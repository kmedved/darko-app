version_date <- readRDS("version_date.rds")
current_talent <- readRDS("current_talent.rds")
survival_data <- readRDS("survival_data.rds")
historical_talent <- read_csv("historical_talent.csv")
#projections <- read_csv("win_projections.csv")
#logos <- read_csv("nba_team_logo_urls.csv")
five_man <- readRDS("five_man.rds")

##
brks_lineup_off <- quantile(five_man$Offense, probs = seq(.05, .95, .05), na.rm = TRUE)
clrs_lineup_off <- rev(heat_hcl(length(brks_lineup_off) + 1))

brks_lineup_def <- quantile(five_man$Defense, probs = seq(.05, .95, .05), na.rm = TRUE)
clrs_lineup_def <- rev(heat_hcl(length(brks_lineup_def) + 1))

brks_lineup_net <- quantile(five_man$Net, probs = seq(.05, .95, .05), na.rm = TRUE)
clrs_lineup_net <- rev(heat_hcl(length(brks_lineup_net) + 1))

#### Generate Main Current Talent Table ####
current_talent_clean <- current_talent %>% 
  rename_cols(.) %>% 
  select(nba_id,
         Team,
         Player,
         Experience,
         `DPM`,
         `DPM Improvement` = dpm_delta,
         `O-DPM`,
         `D-DPM`,
         `Box DPM`,
         `Box O-DPM`,
         `Box D-DPM`,
         `FGA/100`,
         `FG2%`,
         `FG3A/100`,
         `FG3%`,
         `FG3ARate%`,
         `RimFGA/100`,
         `RimFG%`,
         `FTA/100`,
         `FT%`,
         `FTARate%`,
         `USG%`,
         `REB/100`,
         `AST/100`,
         `AST%`,
         `BLK/100`,
         `BLK%`,
         `STL/100`,
         `STL%`,
         `TOV/100`
  )
  
brks_bdpm <- quantile(current_talent_clean$`Box DPM`, probs = seq(.05, .95, .05), na.rm = TRUE)
clrs_bdpm <- rev(heat_hcl(length(brks_bdpm) + 1))

brks_dpm <- quantile(current_talent_clean$`DPM`, probs = seq(.05, .95, .05), na.rm = TRUE)
clrs_dpm <- rev(heat_hcl(length(brks_dpm) + 1))

brks_delta_dpm <- quantile(current_talent_clean$`DPM Improvement`, probs = seq(.05, .95, .05), na.rm = TRUE)
clrs_delta_dpm <- rev(heat_hcl(length(brks_delta_dpm) + 1))

#### Per Game DFS Table ####
per_game_table <- current_talent %>% 
  mutate(minutes = ifelse(minutes < 0, 0, minutes)) %>%
  mutate(minutes = minutes * available) %>%
  select(player_name, team_name, Experience, minutes, pace, tidyselect::ends_with("_100")) %>%
  mutate(across(tidyselect::ends_with("_100"), ~ round(((pace / 100) * .x) * (minutes / 48), 1))) %>%
  mutate(pace = round(pace, 1)) %>%
  select(
    Player = player_name,
    Team = team_name,
    Experience,
    Minutes = minutes,
    Pace = pace,
    PTS = pts_100,
    AST = ast_100,
    DREB = drb_100,
    OREB = orb_100,
    BLK = blk_100,
    STL = stl_100,
    TOV = tov_100,
    FGA = fga_100,
    FTA = fta_100,
    FG3A = fg3a_100,
    RimFGA = rim_fga_100,
    PF = pf_100
  ) %>%
  arrange(-PTS)

brks_pts <- quantile(per_game_table$PTS, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_pts <- rev(heat_hcl(length(brks_pts) + 1))

brks_min <- quantile(per_game_table$Minutes, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_min <- rev(heat_hcl(length(brks_min) + 1))

brks_pace <- quantile(per_game_table$Pace, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_pace <- rev(heat_hcl(length(brks_pace) + 1))

brks_ast <- quantile(per_game_table$AST, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_ast <- rev(heat_hcl(length(brks_ast) + 1))

brks_dreb <- quantile(per_game_table$DREB, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_dreb <- rev(heat_hcl(length(brks_dreb) + 1))

brks_oreb <- quantile(per_game_table$OREB, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_oreb <- rev(heat_hcl(length(brks_oreb) + 1))

brks_blk <- quantile(per_game_table$BLK, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_blk <- rev(heat_hcl(length(brks_blk) + 1))

brks_stl <- quantile(per_game_table$STL, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_stl <- rev(heat_hcl(length(brks_stl) + 1))

brks_tov <- quantile(per_game_table$TOV, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_tov <- rev(heat_hcl(length(brks_tov) + 1))

brks_fga <- quantile(per_game_table$FGA, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_fga <- rev(heat_hcl(length(brks_fga) + 1))

brks_fta <- quantile(per_game_table$FTA, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_fta <- rev(heat_hcl(length(brks_fta) + 1))

brks_fg3a <- quantile(per_game_table$FG3A, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_fg3a <- rev(heat_hcl(length(brks_fg3a) + 1))

brks_rfga <- quantile(per_game_table$RimFGA, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_rfga <- rev(heat_hcl(length(brks_rfga) + 1))

brks_pf <- quantile(per_game_table$PF, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_pf <- rev(heat_hcl(length(brks_pf) + 1))

#### Current Season Snapshot ####
current_talent_snapshot <- historical_talent %>% 
  filter(nba_id %in% unique(current_talent$nba_id)) %>% 
  group_by(nba_id) %>% 
  filter(season_numeric == max(season_numeric))


#### Survival Data ####
survival_current_talent <- survival_data %>%
  ungroup() %>%
  group_by(nba_id, player_name) %>%
  filter(career_game_num == max(career_game_num)) %>%
  ungroup() %>%
  mutate(`Est. Retirement Age` = age + projected_years_remaining) %>%
  select(codename,
         #Team = team_name,
         Player = player_name,
         `Rookie Season` = rookie_season,
         `Career Games` = career_game_num,
         Age = age,
         `Est. Retirement Age`,
         `Years Remaining` = projected_years_remaining,
         `+1` = "1",
         `+2` = "2",
         `+3` = "3",
         `+4` = "4",
         `+5` = "5",
         `+6` = "6",
         `+7` = "7",
         `+8` = "8",
         `+9` = "9",
         `+10` = "10",
         `+11` = "11",
         `+12` = "12"
  ) %>%
  mutate(across(paste0("+", seq(1:12)), ~ round(.x, 3))) %>%
  mutate(Age = round(Age, 1)) %>%
  mutate(`Years Remaining` = round(`Years Remaining`, 1)) %>%
  mutate(`Est. Retirement Age` = round(`Est. Retirement Age`, 1)) %>%
  arrange(-`+1`, -`Career Games`, Player) #%>%
  #filter(is.na(Team) == FALSE)

#minutes_played <- daily_talent %>%
#  filter(current_season == 1) %>%
#  group_by(player_name, nba_id) %>%
#  summarise(seconds = sum(seconds_played), .groups = "drop") %>%
#  mutate(minutes = seconds / 60) %>%
#  select(-seconds) %>%
#  ungroup() %>%
#  mutate(minutes = ifelse(is.na(minutes) == TRUE, 0, minutes))

#### Random Player Initialized ####
random_active_player <- current_talent %>%
  ungroup() %>%
  select(codename) %>%
  pull() %>%
  sample(., 1)


#### Label Frame ####
all_talent_labels <- data.frame(
  display = c(
    "DPM", "O-DPM", "D-DPM", "Box DPM", "Box O-DPM", "Box D-DPM", "FGA/100", "FG2%", "FG3A/100",
    "FG3%", "FG3ARate%", "FTA/100", "FT%", "FTARate%", "Usg%", "Reb/100", "Ast/100",
    "Ast%", "Blk/100", "Blk%", "Stl/100", "Stl%", "Tov/100"
  ),
  actual = c(
    "dpm", "o_dpm", "d_dpm", "box_dpm", "box_odpm", "box_ddpm", "fga_100", "fg2_pct", "fg3a_100", 
    "fg3_pct", "fg3_ar","fta_100", "ft_pct", "ft_ar", "usg_pct", "trb_100", "ast_100",
    "ast_pct", "blk_100", "blk_pct", "stl_100", "stl_pct", "tov_100"
  )
) %>%
  mutate(display = as.character(display)) %>%
  mutate(actual = as.character(actual))

#### Scatterplot Data ####
scatter_data <- current_talent %>%
  mutate_if(is.numeric, function(x) round(x, 2)) 

brks_1 <- quantile(survival_current_talent$`+1`, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_1 <- rev(heat_hcl(length(brks_1) + 1))

brks_2 <- quantile(survival_current_talent$`+2`, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_2 <- rev(heat_hcl(length(brks_1) + 1))

brks_3 <- quantile(survival_current_talent$`+3`, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_3 <- rev(heat_hcl(length(brks_1) + 1))

brks_4 <- quantile(survival_current_talent$`+4`, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_4 <- rev(heat_hcl(length(brks_1) + 1))

brks_5 <- quantile(survival_current_talent$`+5`, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_5 <- rev(heat_hcl(length(brks_1) + 1))

brks_6 <- quantile(survival_current_talent$`+6`, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_6 <- rev(heat_hcl(length(brks_1) + 1))

brks_7 <- quantile(survival_current_talent$`+7`, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_7 <- rev(heat_hcl(length(brks_1) + 1))

brks_8 <- quantile(survival_current_talent$`+8`, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_8 <- rev(heat_hcl(length(brks_1) + 1))

brks_9 <- quantile(survival_current_talent$`+9`, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_9 <- rev(heat_hcl(length(brks_1) + 1))

brks_10 <- quantile(survival_current_talent$`+10`, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_10 <- rev(heat_hcl(length(brks_1) + 1))

brks_11 <- quantile(survival_current_talent$`+11`, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_11 <- rev(heat_hcl(length(brks_1) + 1))

brks_12 <- quantile(survival_current_talent$`+12`, probs = seq(.05, .95, .10), na.rm = TRUE)
clrs_12 <- rev(heat_hcl(length(brks_1) + 1))

# brks_dpm <- quantile(current_talent_raw$dpm, probs = seq(.05, .95, .05), na.rm = TRUE)
# clrs_dpm <- rev(heat_hcl(length(brks_dpm) + 1))
# 
# brks_odpm <- quantile(current_talent_raw$o_dpm, probs = seq(.05, .95, .05), na.rm = TRUE)
# clrs_odpm <- rev(heat_hcl(length(brks_odpm) + 1))
# 
# brks_ddpm <- quantile(current_talent_raw$d_dpm, probs = seq(.05, .95, .05), na.rm = TRUE)
# clrs_ddpm <- rev(heat_hcl(length(brks_ddpm) + 1))
# 
# brks_ddpm <- quantile(current_talent_raw$d_dpm, probs = seq(.05, .95, .05), na.rm = TRUE)
# clrs_ddpm <- rev(heat_hcl(length(brks_ddpm) + 1))