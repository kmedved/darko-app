library(dplyr)
library(arrow)
library(stringr)

version_date <- vector("list", length = 2)
version_date[[1]] <- Sys.Date()
version_date[[2]] <- Sys.time()
saveRDS(version_date, file = "darko/DARKO/version_date.rds")

survival_data <- arrow::read_parquet("darko/data/nba_survivorship.parq") |>
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

saveRDS(survival_data, file = "darko/DARKO/survival_data.rds")

first_game <- arrow::read_parquet("darko/data/darko_shiny_history.parq") |>
  dplyr::filter(season == 2024) |>
  dplyr::group_by(nba_id) |>
  dplyr::mutate(game_num = 1:n()) |>
  dplyr::filter(game_num == 1) |>
  dplyr::select(nba_id, first_dpm = dpm)

rookie <- survival_data |>
  dplyr::select(nba_id, rookie_season) |>
  dplyr::distinct() |>
  dplyr::mutate(Experience = ifelse(rookie_season == 2025, "Rookie",
                             ifelse(rookie_season == 2024, "Sophomore", "Vet")
  ))


current_talent <- arrow::read_parquet("darko/data/current_box_score_talent_gsheet.parq") |>
  dplyr::mutate(box_dpm = box_ddpm + box_odpm) |>
  dplyr::select(-position) |>
  dplyr::left_join(rookie) |>
  dplyr::left_join(first_game) |>
  dplyr::mutate(dpm_delta = round(dpm - first_dpm, 2)) |> 
  dplyr::mutate(codename = paste0(player_name, ": ", nba_id)) 
  
saveRDS(current_talent, file = "darko/DARKO/current_talent.rds")

historical_talent <- arrow::read_parquet("darko/data/darko_career_box_talent.parq") %>%
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

readr::write_csv(historical_talent, "darko/DARKO/historical_talent.csv")

offense <- arrow::read_parquet("darko/data/five_man_off.parq") |> 
  dplyr::mutate(five_man = paste(off1_name, off2_name, off3_name, off4_name, off5_name, sep = " | ")) |> 
  dplyr::select(season, date, delta_offense = off_pts_100_decay, 
         off_combined_poss, off_combined_poss_season, off1, five_man) |> 
  dplyr::left_join(dplyr::select(historical_talent, off1 = nba_id, team_name, date)) |> 
  dplyr::select(-off1)

defense <- arrow::read_parquet("darko/data/five_man_def.parq") |> 
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

saveRDS(five_man, "darko/DARKO/five_man.rds")

rm(list = ls())

