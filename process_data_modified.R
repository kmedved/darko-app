library(dplyr)
library(arrow)
library(stringr)

version_date <- vector("list", length = 2)
version_date[[1]] <- Sys.Date()
version_date[[2]] <- Sys.time()
saveRDS(version_date, file = "DARKO/version_date.rds")

# Create data directory if it doesn't exist
if (!dir.exists("DARKO/data")) {
  dir.create("DARKO/data", recursive = TRUE)
}

# Check if data files exist
if (file.exists("DARKO/data/nba_survivorship.parq")) {
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
  
  # Continue with rest of file processing
  # ... (rest of the processing)
} else {
  cat("Warning: Data files not found in DARKO/data directory.\n")
  cat("Make sure to download the required .parq files first.\n")
}

# Final message
cat("Data processing complete. Files saved to DARKO directory.\n")