# File: fix_data_processing_complete.R
library(dplyr)
library(arrow)
library(stringr)

# Source directory with data files
source_dir <- "C:/Users/kmedv/OneDrive/github/darko/external_share/shiny_folder"

# Create output directories if needed
if (!dir.exists("DARKO/data")) {
  dir.create("DARKO/data", recursive = TRUE)
}

# Step 0: Copy ALL parq files from source
cat("Copying all .parq files from source directory...\n")
parq_files <- c(
  "nba_survivorship.parq",
  "darko_shiny_history.parq",
  "darko_career_box_talent.parq",
  "current_box_score_talent_gsheet.parq",
  "five_man_off.parq",
  "five_man_def.parq"
)

for (file in parq_files) {
  source_file <- file.path(source_dir, file)
  dest_file <- file.path("DARKO/data", file)
  
  if (file.exists(source_file)) {
    file.copy(source_file, dest_file, overwrite = TRUE)
    cat("✓ Copied", file, "\n")
  } else {
    cat("✗ Missing", file, "in source directory\n")
  }
}

# Step 1: Create version_date.rds
cat("\nCreating version_date.rds...\n")
version_date <- vector("list", length = 2)
version_date[[1]] <- Sys.Date()
version_date[[2]] <- Sys.time()
saveRDS(version_date, file = "DARKO/version_date.rds")
cat("✓ Created version_date.rds\n")

# Step 2: Process survival data
cat("\nProcessing survival_data.rds...\n")
survival_data <- arrow::read_parquet("DARKO/data/nba_survivorship.parq") |>
  dplyr::filter(retired_fl == FALSE) |>
  dplyr::filter(seconds_played > 0) |>
  dplyr::mutate(season = season - 1) |>
  dplyr::mutate(current_season = ifelse(season == max(season), 1, 0)) |>
  dplyr::mutate(season_suffix = 1 + as.numeric(str_sub(season, 3, 4))) |>
  dplyr::mutate(season_suffix = ifelse(season_suffix < 10, paste0("0", season_suffix), paste0(season_suffix))) |>
  dplyr::mutate(season = paste0(season, "-", season_suffix)) |>
  dplyr::group_by(nba_id, season) |>
  dplyr::mutate(game_num = 1:n()) |>
  dplyr::mutate(first_game_of_season = ifelse(game_num == min(game_num), 1, 0)) |>
  dplyr::ungroup() |>
  dplyr::mutate(codename = paste0(player_name, ": ", nba_id)) |> 
  dplyr::ungroup()

saveRDS(survival_data, file = "DARKO/survival_data.rds")
cat("✓ Created survival_data.rds\n")

# Step 3: Process first_game data
cat("\nProcessing first_game data...\n")
first_game <- arrow::read_parquet("DARKO/data/darko_shiny_history.parq") |>
  dplyr::filter(season == 2024) |>
  dplyr::group_by(nba_id) |>
  dplyr::mutate(game_num = 1:n()) |>
  dplyr::filter(game_num == 1) |>
  dplyr::select(nba_id, first_dpm = dpm)
cat("✓ Processed first_game data\n")

# Step 4: Process rookie data
cat("\nProcessing rookie data...\n")
rookie <- survival_data |>
  dplyr::select(nba_id, rookie_season) |>
  dplyr::distinct() |>
  dplyr::mutate(Experience = ifelse(rookie_season == 2025, "Rookie",
                                    ifelse(rookie_season == 2024, "Sophomore", "Vet")
  ))
cat("✓ Processed rookie data\n")

# Step 5: Process current_talent data
cat("\nProcessing current_talent.rds...\n")
current_talent <- arrow::read_parquet("DARKO/data/current_box_score_talent_gsheet.parq") |>
  dplyr::mutate(box_dpm = box_ddpm + box_odpm) |>
  dplyr::select(-position) |>
  dplyr::left_join(rookie) |>
  dplyr::left_join(first_game) |>
  dplyr::mutate(dpm_delta = round(dpm - first_dpm, 2)) |> 
  dplyr::mutate(codename = paste0(player_name, ": ", nba_id))

saveRDS(current_talent, file = "DARKO/current_talent.rds")
cat("✓ Created current_talent.rds\n")

# Step 6: Process historical_talent data
cat("\nProcessing historical_talent.csv...\n")
historical_talent <- arrow::read_parquet("DARKO/data/darko_career_box_talent.parq") %>%
  dplyr::rename_with(., ~ stringr::str_remove(.x, "tr_"), starts_with("tr_")) %>% 
  dplyr::mutate(season = season - 1) %>%
  dplyr::mutate(season_numeric = season + 1) %>%
  dplyr::mutate(current_season = ifelse(season == max(season), 1, 0)) %>%
  dplyr::mutate(season_suffix = 1 + as.numeric(str_sub(season, 3, 4))) %>%
  dplyr::mutate(season_suffix = ifelse(season_suffix < 10, paste0("0", season_suffix), paste0(season_suffix))) %>%
  dplyr::mutate(season = paste0(season, "-", season_suffix)) %>%
  dplyr::group_by(nba_id, season) %>%
  dplyr::mutate(game_num = 1:n()) %>%
  dplyr::mutate(first_game_of_season = ifelse(game_num == min(game_num), 1, 0)) %>%
  dplyr::mutate(codename = paste0(player_name, ": ", nba_id)) %>%
  dplyr::ungroup()

readr::write_csv(historical_talent, "DARKO/historical_talent.csv")
cat("✓ Created historical_talent.csv\n")

# Step 7: Process five_man lineup data
cat("\nProcessing five_man.rds...\n")
offense <- arrow::read_parquet("DARKO/data/five_man_off.parq") |> 
  dplyr::mutate(five_man = paste(off1_name, off2_name, off3_name, off4_name, off5_name, sep = " | ")) |> 
  dplyr::select(season, date, delta_offense = off_pts_100_decay, 
                off_combined_poss, off_combined_poss_season, off1, five_man) |> 
  dplyr::left_join(dplyr::select(historical_talent, off1 = nba_id, team_name, date)) |> 
  dplyr::select(-off1)

defense <- arrow::read_parquet("DARKO/data/five_man_def.parq") |> 
  dplyr::mutate(five_man = paste(def1_name, def2_name, def3_name, def4_name, def5_name, sep = " | ")) |> 
  dplyr::select(season, date, delta_defense = def_pts_100_decay, 
                def_combined_poss, def_combined_poss_season, def1, five_man) |> 
  dplyr::left_join(dplyr::select(historical_talent, def1 = nba_id, team_name, date)) |> 
  dplyr::select(-def1)

five_man <- dplyr::left_join(offense, defense) |> 
  dplyr::mutate(total_poss_season = off_combined_poss_season + def_combined_poss_season,
                total_poss = off_combined_poss + def_combined_poss,
                delta_net = delta_offense + delta_defense) |> 
  dplyr::filter(total_poss >= 100) |> 
  dplyr::select(Season = season, 
                Team = team_name, 
                Lineup = five_man, 
                Offense = delta_offense, 
                Defense = delta_defense, 
                Net = delta_net, 
                `Season Possessions` = total_poss_season, 
                `Total Possessions` = total_poss) |> 
  dplyr::arrange(-`Season Possessions`) |> 
  dplyr::mutate(Offense = round(Offense, 1),
                Defense = round(Defense, 1),
                Net = round(Net, 1))

saveRDS(five_man, "DARKO/five_man.rds")
cat("✓ Created five_man.rds\n")

# Step 8: Verify all files exist
cat("\nVerifying all files...\n")
required_files <- c(
  "version_date.rds",
  "survival_data.rds",
  "current_talent.rds",
  "five_man.rds",
  "historical_talent.csv"
)

all_exist <- TRUE
for (file in required_files) {
  if (file.exists(file.path("DARKO", file))) {
    cat("✓ Found", file, "\n")
  } else {
    cat("✗ Missing", file, "\n")
    all_exist <- FALSE
  }
}

if (all_exist) {
  cat("\n✅ SUCCESS: All data files processed and ready for deployment!\n")
} else {
  cat("\n❌ ERROR: Some files are missing. Please check the errors above.\n")
}