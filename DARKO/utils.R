
rename_cols <- function(x) {
  
  x %>% mutate(box_dpm = box_ddpm + box_odpm) %>%
    mutate(trb_100 = drb_100 + orb_100) %>%
    rename(
      Team = team_name,
      Age = age,
      Player = player_name,
      Pace = pace,
      `ORB%` = orb_pct,
      `DRB%` = drb_pct,
      `TOV%` = tov_pct,
      `RimFG%` = rim_fg_pct,
      `RimFGA/100` = rim_fga_100,
      `PTS/100` = pts_100,
      `ORB/100` = orb_100,
      `DRB/100` = drb_100,
      `PF/100` = pf_100,
      `DPM` = dpm,
      `O-DPM` = o_dpm,
      `D-DPM` = d_dpm,
      `Box DPM` = box_dpm,
      `Box O-DPM` = box_odpm,
      `Box D-DPM` = box_ddpm,
      `FGA/100` = fga_100,
      `FG2%` = fg2_pct,
      `FG3A/100` = fg3a_100,
      `FG3%` = fg3_pct,
      `FG3ARate%` = fg3_ar,
      `FTA/100` = fta_100,
      `FT%` = ft_pct,
      `FTARate%` = ft_ar,
      `USG%` = usg_pct,
      `REB/100` = trb_100,
      `AST/100` = ast_100,
      `AST%` = ast_pct,
      `BLK/100` = blk_100,
      `BLK%` = blk_pct,
      `STL/100` = stl_100,
      `STL%` = stl_pct,
      `TOV/100` = tov_100
    ) %>%
    mutate(Pos = toupper(gsub("\\_.*", "", .$x_position))) %>%
    mutate(Age = round(Age, 1))
  
}

today_date <- Sys.Date()

basic_cite <- "@kmedved | www.darko.app | @anpatt7"

all_talent_cols <- c(
  "DPM",
  "O-DPM",
  "D-DPM",
  "Box DPM",
  "Box O-DPM",
  "Box D-DPM",
  "FGA/100",
  "FG2%",
  "FG3A/100",
  "FG3%",
  "FG3ARate%",
  "FTA/100",
  "FT%",
  "FTARate%",
  "RimFG%",
  "RimFGA/100",
  "USG%",
  "REB/100",
  "AST/100",
  "AST%",
  "BLK/100",
  "BLK%",
  "STL/100",
  "STL%",
  "TOV/100",
  "Pace"
)

talent_label_link <- c("Pace" = "pace",
                      "ORB%" = "orb_pct",
                      "DRB%" = "drb_pct",
                      "TOV%" = "tov_pct",
                      "RimFG%" = "rim_fg_pct",
                      "RimFGA/100" = "rim_fga_100",
                      "PTS/100" = "pts_100",
                      "ORB/100" = "orb_100",
                      "DRB/100" = "drb_100",
                      "PF/100" = "pf_100",
                      "DPM" = "dpm",
                      "O-DPM" = "o_dpm",
                      "D-DPM" = "d_dpm",
                      "Box DPM" = "box_dpm",
                      "Box O-DPM" = "box_odpm",
                      "Box D-DPM" = "box_ddpm",
                      "FGA/100" = "fga_100",
                      "FG2%" = "fg2_pct",
                      "FG3A/100" = "fg3a_100",
                      "FG3%" = "fg3_pct",
                      "FG3ARate%" = "fg3_ar",
                      "FTA/100" = "fta_100",
                      "FT%" = "ft_pct",
                      "FTARate%" = "ft_ar",
                      "USG%" = "usg_pct",
                      "REB/100" = "trb_100",
                      "AST/100" = "ast_100",
                      "AST%" = "ast_pct",
                      "BLK/100" = "blk_100",
                      "BLK%" = "blk_pct",
                      "STL/100" = "stl_100",
                      "STL%" = "stl_pct",
                      "TOV/100" = "tov_100")

dpm_table_cols <- c("DPM", "O-DPM", "D-DPM", "Box O-DPM", "Box D-DPM", "Box DPM")

style_table_cols <- c("Pace", "USG%", "FTARate%", "FG3ARate%")

per_100_table_cols  <- c("PTS/100", "ORB/100", "DRB/100", "AST/100", "PF/100", "BLK/100", "STL/100",
                         "TOV/100", "FTA/100", "FGA/100", "RimFGA/100", "FG3A/100", "REB/100")

efficiency_table_cols  <- c("FG2%", "FG3%", "FT%", "RimFG%")

box_table_cols  <- c("ORB%", "DRB%", "AST%", "BLK%", "STL%", "TOV%")

theme_surv <- function () {
  theme_bw(base_size = 18) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.caption = element_text(size = 10),
      axis.text.x = element_text(hjust = 0.5, size = 14),
      axis.title.y = element_text(size = 14, angle = 90)
    )
}

theme_new <- function() { 
  theme_bw(base_size = 18) %+replace% 
    theme(
      legend.position = "top",
      panel.grid = element_blank(),
      legend.text = element_text(size = 14),
      plot.caption = element_text(size = 10),
      axis.title.y = element_text(size = 14, angle = 90)
    )
}

theme_talent_comparison <- function() { 
  theme_bw(base_size = 24) %+replace% 
    theme(
      legend.position = "top",
      panel.grid = element_blank(),
      legend.text = element_text(size = 14),
      plot.caption = element_text(size = 12),
      axis.title.y = element_text(angle = 90),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    )
}

theme_talent_comparison_seasons <- function() { 
  theme_bw(base_size = 24) %+replace% 
    theme(
      axis.text.x = element_text(angle = 45, hjust = 0.5, size = 14),
      axis.title.y = element_text(size = 14, angle = 90),
      legend.position = "top",
      panel.grid = element_blank(),
      legend.text = element_text(size = 14),
      plot.caption = element_text(size = 12),
      plot.title = element_text(hjust = 0.5)
    )
}


theme_talent_snap <- function() { 
  theme_bw(base_size = 24) %+replace% 
    theme(
      legend.text = element_text(size = 14),
      plot.caption = element_text(size = 12),
      axis.title.y = element_text(angle = 90),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.minor = element_blank()
    )
}

theme_shap <- function() { 
  theme_bw(base_size = 18) %+replace% 
    theme(
      plot.caption = element_text(size = 10),
      axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 16)
      
    )
}


theme_trend_single_player <- function () { 
  theme_bw(base_size = 18) %+replace% 
    theme(
      legend.position = "bottom",
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 0.5, size = 14),
      plot.caption = element_text(size = 10),
      axis.title.y = element_text(size = 14, angle = 90)
    )
}

theme_pos_dist <- function () { 
  theme_bw(base_size = 18) %+replace% 
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(size = 14),
      plot.caption = element_text(size = 10),
      axis.ticks.y = element_blank()
    )
}

theme_wins <- function () { 
  theme_minimal(base_size = 14) %+replace% 
    theme(
      plot.title = element_text(hjust = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
      
    )
}

theme_new_bottom <- function() { 
  theme_bw(base_size = 18) %+replace% 
    theme(
      legend.position = "top",
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 12),
      plot.caption = element_text(size = 10)
    )
}


theme_new_top <- function() { 
  theme_bw(base_size = 18) %+replace% 
    theme(
      legend.position = "top",
      panel.grid = element_blank(),
      axis.text.x = element_text(size = 16),
      plot.caption = element_text(size = 10)
    )
}
