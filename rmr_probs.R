library(tidyverse)
library(gt)
library(glue)
library(patchwork)

global_standings <- read_csv("Documents/development/test_rmr_out/eu_ratings.csv")
eu_rmr_b_seeds <- read_csv("Documents/development/test_rmr_out/eu_rmr_b_seeding.csv")
eu_rmr_a_seeds <- read_csv("Documents/development/test_rmr_out/eu_rmr_a_seeding.csv")

getMatchProbability <- function(ratingA, ratingB) {
  1 / (1 + 10^((ratingA - ratingB) / -400))
}

determineRoundWinners <- function(round_matches) {
  match_results <- round_matches %>%
    group_by(match_idx) %>%
    slice_max(win_prob) %>%
    mutate(
      rand_res = runif(n()),
      team_wins = win_prob > rand_res
    )
  
  round_matches %>%
    left_join(
      match_results %>%
        select(match_idx, team_wins, is_win_name = name),
      by = 'match_idx'
    ) %>%
    mutate(
      match_won = case_when(
        name == is_win_name ~ team_wins,
        name != is_win_name ~ !team_wins
      )
    ) %>%
    select(name, match_won)
}

determineMatchupsWithinBucket <- function(potential_matched_teams, previous_matches) {
  buckets_need_reseeded <- potential_matched_teams %>%
    filter(!is.na(match_idx)) %>%
    inner_join(previous_matches, by=c('name', 'opponent')) %>%
    select(round_bucket) %>%
    distinct()
  
  for(rd_bucket in buckets_need_reseeded$round_bucket) {
    # will sometimes try to re-seed 3-0/0-3 games; if either milestone is reached don't need to reseed
    if(grepl('3', rd_bucket)) {
      next
    }
    
    bucket_seeded_teams <- potential_matched_teams %>%
      filter(round_bucket == rd_bucket) %>%
      select(name, round_bucket_seed)
    
    new_matched_teams <- tibble()
    
    new_match_idx <- potential_matched_teams %>%
      filter(round_bucket == rd_bucket) %>%
      slice_min(match_idx) %>%
      select(match_idx) %>%
      distinct() %>%
      pull(match_idx)
    
    att_match_idx <- 0
    
    while(dim(bucket_seeded_teams)[1] > 0) {
      matched_teams <- bucket_seeded_teams %>%
        slice_min(round_bucket_seed) %>%
        crossing(
          bucket_seeded_teams %>% 
            rename(opponent = name, opponent_seed = round_bucket_seed)
        ) %>%
        left_join(previous_matches, by=c('name', 'opponent')) %>%
        filter(is.na(rd), opponent != name) %>%
        slice_max(opponent_seed) %>%
        select(name, opponent)
      
      if(dim(bucket_seeded_teams)[1] == 4){
        unmatched_teams <- bucket_seeded_teams %>%
          filter(!(name %in% matched_teams$opponent), !(name %in% matched_teams$name))
        
        other_teams_matches <- unmatched_teams %>%
          slice_min(round_bucket_seed) %>%
          crossing(
            bucket_seeded_teams %>% 
              rename(opponent = name, opponent_seed = round_bucket_seed)
          ) %>%
          left_join(previous_matches, by=c('name', 'opponent')) %>%
          filter(opponent %in% unmatched_teams$name, opponent != name, !is.na(rd))
        
        if(dim(other_teams_matches)[1] > 0) {
          previous_matches <- previous_matches %>%
            rbind(matched_teams %>% mutate(rd='attempted_match'))

          att_match_idx <- att_match_idx + 1      
          if(att_match_idx > 10) {
            break
          }
          
          next
        }
      }
      
      new_matched_teams <- new_matched_teams %>% rbind(
        matched_teams %>%
          rbind(matched_teams %>% rename(opponent = name, name = opponent)) %>%
          mutate(match_idx = new_match_idx)
      )
      
      bucket_seeded_teams <- bucket_seeded_teams %>%
        filter(!(name %in% new_matched_teams$name))
      
      new_match_idx <- new_match_idx + 1
      att_match_idx <- att_match_idx + 1
      
      if(att_match_idx > 10) {
        break
      }
    }
    
    bucket_with_new_matches <- potential_matched_teams %>%
      filter(round_bucket == rd_bucket) %>%
      select(-match_idx, -opponent) %>%
      left_join(new_matched_teams, by='name')
    
    potential_matched_teams <- potential_matched_teams %>%
      filter(round_bucket != rd_bucket) %>%
      rbind(bucket_with_new_matches)
  }
  
  return(potential_matched_teams)
}

simulateRound2 <- function(initial_round_matrix) {
  round_1_results <- determineRoundWinners(initial_round_matrix)
  
  round_2_games <- initial_round_matrix %>%
    left_join(round_1_results, by='name') %>%
    mutate(
      total_wins = total_wins + match_won,
      total_losses = total_losses + !match_won,
      round_bucket = glue("{total_wins}-{total_losses}")
    ) %>%
    select(-match_idx) %>%
    group_by(round_bucket) %>%
    arrange(rmr_seed) %>%
    mutate(
      round_bucket_seed = rank(rmr_seed),
      match_idx = pmin(round_bucket_seed, 9 - round_bucket_seed) + case_when(
        round_bucket == '0-1' ~ 4,
        T ~ 0
      )
    ) %>%
    ungroup()

  round_2_games %>%
    select(
      name, 
      rmr_seed,
      round_1_opp = opponent, 
      total_wins,
      total_losses, 
      round_bucket, 
      round_bucket_seed,
      match_idx
    ) %>%
    left_join(
      round_2_games %>%
        select(opponent = name, round_bucket, match_idx),
      by = c('match_idx', 'round_bucket'),
      relationship = 'many-to-many'
    ) %>%
    filter(opponent != name) %>%
    left_join(
      win_prob_matrix %>%
        select(
          name = team_a_name, 
          opponent = team_b_name,
          win_prob = team_a_win_prob
        ),
      by = c('name', 'opponent')
    )
}

simulateRound3 <- function(round_2_matrix) {
  round_2_results <- determineRoundWinners(round_2_matrix)
  
  post_round_2_games <- round_2_matrix %>%
    left_join(round_2_results, by='name') %>%
    mutate(
      total_wins = total_wins + match_won,
      total_losses = total_losses + !match_won,
      round_bucket = glue("{total_wins}-{total_losses}")
    ) %>%
    rename(round_2_opp = opponent) %>%
    mutate(team_buchholtz = total_wins - total_losses)
  
  team_buchholtz_values <- post_round_2_games %>%
    pivot_longer(cols = contains('_opp'), values_to = 'opp_name', names_to = 'round_faced') %>%
    left_join(
      post_round_2_games %>% 
        select(opp_name = name, opp_buchholtz = team_buchholtz),
      by = 'opp_name'
    ) %>%
    group_by(name) %>%
    summarise(opp_buchholtz_value = sum(opp_buchholtz))
  
  existing_matches <- round_2_matrix %>% 
    select(name, opponent, round_1_opp) %>% 
    pivot_longer(cols = contains('opp'), values_to = 'opponent', names_to = 'rd')
  
  round_3_games <- post_round_2_games %>%
    left_join(team_buchholtz_values, by = 'name') %>%
    group_by(round_bucket) %>%
    mutate(
      # buchholz*10 to make sure that comes first
      round_bucket_seed = rank(-((10 * opp_buchholtz_value) - rmr_seed)),
      match_idx = case_when(
        round_bucket == "1-1" ~ pmin(round_bucket_seed, 9 - round_bucket_seed),
        T ~ pmin(round_bucket_seed, 5 - round_bucket_seed)
      ) + case_when(
        round_bucket == "0-2" ~ 6,
        round_bucket == "1-1" ~ 2,
        T ~ 0
      )
    ) %>%
    ungroup()
  
  potential_round_3_matchups <- round_3_games %>%
    select(
      name, 
      rmr_seed,
      round_1_opp,
      round_2_opp, 
      total_wins,
      total_losses, 
      round_bucket, 
      round_bucket_seed,
      match_idx
    ) %>%
    left_join(
      round_3_games %>%
        select(opponent = name, round_bucket, match_idx),
      by = c('match_idx', 'round_bucket'),
      relationship = 'many-to-many'
    ) %>%
    filter(opponent != name)
  
  actual_round_3_matchups <- determineMatchupsWithinBucket(
    potential_round_3_matchups, 
    existing_matches
  )
  
  actual_round_3_matchups %>%
    left_join(
      win_prob_matrix %>%
        select(
          name = team_a_name, 
          opponent = team_b_name,
          win_prob = team_a_win_prob
        ),
      by = c('name', 'opponent')
    )
}

simulateRound4 <- function(round_3_matrix) {
  round_3_results <- determineRoundWinners(round_3_matrix)
  
  post_round_3_games <- round_3_matrix %>%
    left_join(round_3_results, by='name') %>%
    mutate(
      total_wins = total_wins + match_won,
      total_losses = total_losses + !match_won,
      round_bucket = glue("{total_wins}-{total_losses}")
    ) %>%
    rename(round_3_opp = opponent) %>%
    mutate(team_buchholtz = total_wins - total_losses)
  
  team_buchholtz_values <- post_round_3_games %>%
    pivot_longer(cols = contains('_opp'), values_to = 'opp_name', names_to = 'round_faced') %>%
    left_join(
      post_round_3_games %>% 
        select(opp_name = name, opp_buchholtz = team_buchholtz),
      by = 'opp_name'
    ) %>%
    group_by(name) %>%
    summarise(opp_buchholtz_value = sum(opp_buchholtz))
  
  existing_matches <- round_3_matrix %>% 
    select(name, contains('opp')) %>% 
    pivot_longer(cols = contains('opp'), values_to = 'opponent', names_to = 'rd')
  
  round_4_games <- post_round_3_games %>%
    left_join(team_buchholtz_values, by = 'name') %>%
    group_by(round_bucket) %>%
    mutate(
      # buchholz*10 to make sure that comes first
      round_bucket_seed = rank(-((10 * opp_buchholtz_value) - rmr_seed)),
      match_idx = case_when(
        (round_bucket == "2-1") | (round_bucket == "1-2") ~ pmin(round_bucket_seed, 7 - round_bucket_seed),
        T ~ NA
      ) + case_when(
        round_bucket == "1-2" ~ 3,
        round_bucket == "2-1" ~ 0,
        T ~ NA
      )
    ) %>%
    ungroup()
  
  potential_round_4_games <- round_4_games %>%
    select(
      name, 
      rmr_seed,
      contains('_opp'), 
      total_wins,
      total_losses, 
      round_bucket, 
      round_bucket_seed,
      match_idx
    ) %>%
    left_join(
      round_4_games %>%
        select(opponent = name, round_bucket, match_idx),
      by = c('match_idx', 'round_bucket'),
      relationship = 'many-to-many'
    ) %>%
    filter(opponent != name)
  
  actual_round_4_games <- determineMatchupsWithinBucket(
    potential_round_4_games,
    existing_matches
  )
  
  actual_round_4_games %>%
    left_join(
      win_prob_matrix %>%
        select(
          name = team_a_name, 
          opponent = team_b_name,
          win_prob = team_a_win_prob
        ),
      by = c('name', 'opponent')
    )
}

simulateRound5 <- function(round_4_matrix) {
  round_4_results <- round_4_matrix %>%
    filter(!is.na(match_idx)) %>%
    determineRoundWinners()
  
  post_round_4_games <- round_4_matrix %>%
    left_join(round_4_results, by='name') %>%
    mutate(
      total_wins = case_when(
        is.na(match_won) ~ total_wins,
        T ~ total_wins + match_won
      ),
      total_losses = case_when(
        is.na(match_won) ~ total_losses,
        T ~ total_losses + !match_won
      ),
      round_bucket = glue("{total_wins}-{total_losses}")
    ) %>%
    rename(round_4_opp = opponent) %>%
    mutate(team_buchholtz = total_wins - total_losses)
  
  team_buchholtz_values <- post_round_4_games %>%
    pivot_longer(cols = contains('_opp'), values_to = 'opp_name', names_to = 'round_faced') %>%
    left_join(
      post_round_4_games %>% 
        select(opp_name = name, opp_buchholtz = team_buchholtz),
      by = 'opp_name'
    ) %>%
    group_by(name) %>%
    summarise(opp_buchholtz_value = sum(opp_buchholtz))
  
  existing_matches <- round_4_matrix %>% 
    select(name, contains('opp')) %>% 
    pivot_longer(cols = contains('opp'), values_to = 'opponent', names_to = 'rd')
  
  round_5_games <- post_round_4_games %>%
    left_join(team_buchholtz_values, by = 'name') %>%
    group_by(round_bucket) %>%
    mutate(
      # buchholz*10 to make sure that comes first
      round_bucket_seed = rank(-((10 * opp_buchholtz_value) - rmr_seed)),
      match_idx = case_when(
        round_bucket == "2-2" ~ pmin(round_bucket_seed, 7 - round_bucket_seed),
        T ~ NA
      )
    ) %>%
    ungroup()
  
  potential_round_5_games <- round_5_games %>%
    select(
      name, 
      rmr_seed,
      contains('_opp'), 
      total_wins,
      total_losses, 
      round_bucket, 
      round_bucket_seed,
      match_idx
    ) %>%
    left_join(
      round_5_games %>%
        select(opponent = name, round_bucket, match_idx),
      by = c('match_idx', 'round_bucket'),
      relationship = 'many-to-many'
    ) %>% 
    filter(opponent != name)
  
  actual_round_5_games <- determineMatchupsWithinBucket(
    potential_round_5_games,
    existing_matches
  )
  
 actual_round_5_games %>%
    left_join(
      win_prob_matrix %>%
        select(
          name = team_a_name, 
          opponent = team_b_name,
          win_prob = team_a_win_prob
        ),
      by = c('name', 'opponent'),
      relationship = 'many-to-many'
    ) %>% 
    distinct(name, .keep_all = T)
}

simulateRound6 <- function(round_5_matrix) {
  round_5_results <- round_5_matrix %>%
    filter(!is.na(match_idx)) %>%
    determineRoundWinners()
  
  post_round_5_games <- round_5_matrix %>%
    left_join(round_5_results, by='name') %>%
    mutate(
      total_wins = case_when(
        is.na(match_won) ~ total_wins,
        T ~ total_wins + match_won
      ),
      total_losses = case_when(
        is.na(match_won) ~ total_losses,
        T ~ total_losses + !match_won
      ),
      round_bucket = glue("{total_wins}-{total_losses}")
    ) %>%
    rename(round_5_opp = opponent) %>%
    mutate(team_buchholtz = total_wins - total_losses)
  
  team_buchholtz_values <- post_round_5_games %>%
    pivot_longer(cols = contains('_opp'), values_to = 'opp_name', names_to = 'round_faced') %>%
    left_join(
      post_round_5_games %>% 
        select(opp_name = name, opp_buchholtz = team_buchholtz),
      by = 'opp_name'
    ) %>%
    group_by(name) %>%
    summarise(opp_buchholtz_value = sum(opp_buchholtz))
  
  existing_matches <- round_5_matrix %>% 
    select(name, contains('opp')) %>% 
    pivot_longer(cols = contains('opp'), values_to = 'opponent', names_to = 'rd')
  
  round_6_games <- post_round_5_games %>%
    left_join(team_buchholtz_values, by = 'name') %>%
    group_by(round_bucket) %>%
    mutate(
      # 7th/8th tiebreak
      round_bucket_seed = rank(-((10 * opp_buchholtz_value) - rmr_seed)),
      match_idx = case_when(
        (round_bucket == "3-2") & (round_bucket_seed != min(round_bucket_seed)) ~ 0,
        T ~ NA
      )
    ) %>%
    ungroup()
  
  round_6_games %>%
    select(
      name, 
      rmr_seed,
      round_5_opp, 
      total_wins,
      total_losses, 
      round_bucket, 
      round_bucket_seed,
      match_idx
    ) %>%
    left_join(
      round_6_games %>%
        select(opponent = name, round_bucket, match_idx),
      by = c('match_idx', 'round_bucket'),
      relationship = 'many-to-many'
    ) %>% 
    filter(opponent != name | is.na(match_idx)) %>%
    left_join(
      win_prob_matrix %>%
        select(
          name = team_a_name, 
          opponent = team_b_name,
          win_prob = team_a_win_prob
        ),
      by = c('name', 'opponent'),
      relationship = 'many-to-many'
    ) %>% 
    distinct(name, .keep_all = T)
}

# in volvo's data, 1 pt win prob approx 0.8 pt observed wins
WIN_PROB_SCALER <- 0.8

win_prob_matrix <- eu_rmr_b_seeds %>%
  select(team_a_name = name, team_a_seed = rmr_seed) %>%
  crossing(
    eu_rmr_b_seeds %>%
      select(team_b_name = name, team_b_seed = rmr_seed)
  ) %>%
  filter(team_a_name != team_b_name) %>%
  left_join(
    global_standings %>%
      select(team_a_name = team_name, team_a_power = power),
    by = c('team_a_name')
  ) %>%
  left_join(
    global_standings %>%
      select(team_b_name = team_name, team_b_power = power),
    by = c('team_b_name')
  ) %>%
  mutate(
    team_a_win_prob = (0.8 * (map2_dbl(team_a_power, team_b_power, getMatchProbability) - 0.5)) + 0.5
  )

initial_round_games <- eu_rmr_b_seeds %>%
  mutate(match_idx = rmr_seed %% 8)

initial_round_probs <- initial_round_games %>%
  left_join(
    initial_round_games %>%
      select(opponent = name, match_idx),
    by = 'match_idx',
    relationship = 'many-to-many'
  ) %>%
  filter(opponent != name) %>%
  left_join(
    win_prob_matrix %>%
      select(
        name = team_a_name, 
        opponent = team_b_name,
        win_prob = team_a_win_prob
      ),
    by = c('name', 'opponent')
  ) %>%
  mutate(total_wins = 0, total_losses = 0)

TOTAL_SIM_COUNT <- 1000

start_time <- Sys.time()

round_2_sims <- tibble(sim_count = 1:TOTAL_SIM_COUNT) %>%
  crossing(initial_round_probs) %>%
  nest(round_1_probs = everything(), .by = sim_count) %>%
  mutate(
    sim_round_2 = map(round_1_probs, simulateRound2),
    sim_round_3 = map(sim_round_2, simulateRound3),
    sim_round_4 = map(sim_round_3, simulateRound4),
    sim_round_5 = map(sim_round_4, simulateRound5),
    sim_round_6 = map(sim_round_5, simulateRound6)
  )

end_time <- Sys.time()

print(glue("Finished {TOTAL_SIM_COUNT} simulations in {end_time - start_time}"))

round_2_results <- round_2_sims %>%
  unnest(sim_round_2)

round_3_results <- round_2_sims %>%
  unnest(sim_round_3)

round_4_results <- round_2_sims %>%
  unnest(sim_round_4)

round_5_results <- round_2_sims %>%
  unnest(sim_round_5)

round_6_results <- round_2_sims %>%
  unnest(sim_round_6)

generateResultsTable <- function(
    display_team_name, 
    display_round_bucket, 
    round_results,
    n_teams_displayed = 6
) {
  round_opponents <- round_results %>%
    filter(
      round_bucket == display_round_bucket, 
      name == display_team_name,
      !is.na(match_idx)
    ) %>%
    mutate(total_in_bucket = n()) %>%
    group_by(opponent) %>%
    summarise(
      baseline_prob = n() / TOTAL_SIM_COUNT,
      given_result_prob = n() / median(total_in_bucket),
      win_prob = median(win_prob)
    ) %>%
    arrange(-given_result_prob) %>%
    ungroup()
  
  displayed_opponents <- round_opponents %>%
    slice_max(given_result_prob, n=n_teams_displayed, with_ties = F) 
    
  other_opponents <- round_opponents %>%
    filter(!(opponent %in% displayed_opponents$opponent)) %>%
    summarise(
      win_prob = sum(baseline_prob * win_prob) / sum(baseline_prob),
      baseline_prob = sum(baseline_prob),
      given_result_prob = sum(given_result_prob)
    ) %>%
    mutate(opponent = "Others")
    
  displayed_opponents %>%
    rbind(other_opponents) %>%
    mutate(
      opponent_disp = case_when(
        opponent == 'Ninjas in Pyjamas' ~ 'NiP',
        opponent == 'Eternal Fire' ~ 'EF',
        opponent == 'Virtus.pro' ~ 'VP',
        opponent == 'Natus Vincere' ~ 'NaVi',
        T ~ opponent
      )
    ) %>%
    select(opponent, opponent_disp, baseline_prob, given_result_prob, win_prob) %>%
    gt() %>%
    applyTableFormat() %>%
    fmt_percent(columns = contains('_prob'), decimals = 1) %>%
    text_transform(
      locations = cells_body(columns=c(opponent)),
      fn = function(x) {
        ifelse(
          !is.na(x),
          local_image(
            filename = glue("Documents/development/test_rmr_out/logos/{x}.png"),
            height = 20
          ),
          ""
        )
      }
    ) %>%
    data_color(
      columns = c(baseline_prob, given_result_prob),
      palette = "viridis",
      reverse = T,
      domain = c(0, 1)
    ) %>%
    data_color(
      columns = c(win_prob),
      palette = "RdBu",
      domain = c(0, 1)
    ) %>%
    cols_label(
      opponent = "",
      opponent_disp = "",
      baseline_prob = "(total)",
      given_result_prob = glue("(in {display_round_bucket})"),
      win_prob = "Win Prob"
    ) %>%
    tab_spanner(
      columns = c(baseline_prob, given_result_prob),
      label = "Matchup Likelihood"
    ) %>%
    tab_style(
      style = cell_borders(side = "right", color = "#333333", weight = px(3)),
      locations = cells_body(
        columns = c(opponent_disp, given_result_prob)
      )
    ) %>%
    tab_style(
      style = cell_borders(side = "right", color = "#333333", weight = px(3)),
      locations = cells_column_labels(
        columns = c(opponent_disp, given_result_prob)
      )
    ) %>%
    tab_style(
      style = cell_borders(side = "left", color = "#333333", weight = px(3)),
      locations = cells_column_labels(
        columns = c(win_prob)
      )
    ) %>%
    tab_options(
      table_body.hlines.color = "#333333",
      table_body.vlines.color = "lightgray",
      table_body.vlines.width = 10,
      table.font.size = 14,
      column_labels.hidden = FALSE
    ) %>%
    tab_header(
      glue("{display_team_name} {display_round_bucket}")
    )
}

generateTerminatingTable <- function(
    display_team_name, 
    display_round_bucket, 
    round_results
) {
  round_results %>%
    filter(
      round_bucket == display_round_bucket, 
      name == display_team_name, 
      is.na(match_idx)
    ) %>%
    mutate(
      total_in_bucket = n(),
      opponent = case_when(
        grepl("-3", round_bucket) ~ "ELIMINATED",
        grepl("3-", round_bucket) ~ "ADVANCED"
      ),
      did_advance = case_when(
        opponent == "ADVANCED" ~ 1,
        T ~ 0
      )
    ) %>%
    group_by(opponent) %>%
    summarise(
      baseline_prob = n() / TOTAL_SIM_COUNT,
      given_result_prob = n() / TOTAL_SIM_COUNT,
      win_prob = mean(did_advance)
    ) %>%
    arrange(-given_result_prob) %>%
    ungroup() %>%
    slice_max(given_result_prob, n=10, with_ties = F)  %>%
    select(opponent, baseline_prob, given_result_prob, win_prob) %>%
    gt() %>%
    applyTableFormat() %>%
    fmt_percent(columns = contains('_prob'), decimals = 1) %>%
    data_color(
      columns = c(baseline_prob, given_result_prob),
      palette = "viridis",
      reverse = T,
      domain = c(0, 1)
    ) %>%
    data_color(
      columns = c(win_prob),
      palette = "RdBu",
      domain = c(0, 1)
    ) %>%
    cols_label(
      opponent = "",
      baseline_prob = "(total)",
      given_result_prob = "(within bucket)",
      win_prob = "Win Prob"
    ) %>%
    tab_spanner(
      columns = c(baseline_prob, given_result_prob),
      label = "Matchup Likelihood"
    ) %>%
    tab_style(
      style = cell_borders(side = "right", color = "#333333", weight = px(3)),
      locations = cells_body(
        columns = c(opponent, given_result_prob)
      )
    ) %>%
    tab_style(
      style = cell_borders(side = "right", color = "#333333", weight = px(3)),
      locations = cells_column_labels(
        columns = c(opponent, given_result_prob)
      )
    ) %>%
    tab_style(
      style = cell_borders(side = "left", color = "#333333", weight = px(3)),
      locations = cells_column_labels(
        columns = c(win_prob)
      )
    ) %>%
    tab_options(
      table_body.hlines.color = "#333333",
      table_body.vlines.color = "lightgray",
      table_body.vlines.width = 10,
      table.font.size = 14,
      data_row.padding = px(3),
      column_labels.hidden = FALSE
    ) %>%
    tab_header(
      glue("{display_team_name} {display_round_bucket}")
    ) 
}

applyTableFormat <- function(gt_obj) {
  gt_obj %>%
    tab_options(
      table.background.color = "#f0f0f0",
      data_row.padding = px(0),
      column_labels.hidden = TRUE
    ) %>%
    opt_table_font(
      font = list(
        google_font("IBM Plex Mono"),
        default_fonts()
      ),
      weight = 500
    )
}

for(team_name_out in eu_rmr_b_seeds$name) {
  table_01 <- generateResultsTable(team_name_out, "0-1", round_2_results)
  table_10 <- generateResultsTable(team_name_out, "1-0", round_2_results)
  
  table_02 <- generateResultsTable(team_name_out, "0-2", round_3_results)
  table_11 <- generateResultsTable(team_name_out, "1-1", round_3_results)
  table_20 <- generateResultsTable(team_name_out, "2-0", round_3_results)
  
  table_21 <- generateResultsTable(team_name_out, "2-1", round_4_results)
  table_12 <- generateResultsTable(team_name_out, "1-2", round_4_results)
  
  table_30 <- generateTerminatingTable(team_name_out, "3-0", round_4_results)
  table_03 <- generateTerminatingTable(team_name_out, "0-3", round_4_results)
  
  table_22 <- generateResultsTable(team_name_out, "2-2", round_5_results)
  table_31 <- generateTerminatingTable(team_name_out, "3-1", round_5_results)
  table_13 <- generateTerminatingTable(team_name_out, "1-3", round_5_results)
  
  table_32_end <- generateTerminatingTable(team_name_out, "3-2", round_6_results)
  table_32_res <- generateResultsTable(team_name_out, "3-2", round_6_results)
  table_23 <- generateTerminatingTable(team_name_out, "2-3", round_6_results)
  
  total_data <- tibble(
    round_1 = list(as_raw_html(applyTableFormat(gt(
      tibble(
        col_1 = list(
          as_raw_html(table_10),
          as_raw_html(table_01)
        )
      )
    )))),
    round_2 = list(as_raw_html(applyTableFormat(gt(
      tibble(
        col_2 = list(
          as_raw_html(table_20),
          as_raw_html(table_11),
          as_raw_html(table_02)
        )
      )
    )))),
    round_3 = list(as_raw_html(applyTableFormat(gt(
      tibble(
        col_3 = list(
          as_raw_html(table_30),
          as_raw_html(table_21),
          as_raw_html(table_12),
          as_raw_html(table_03)
        )
      )
    )))),
    round_4 = list(as_raw_html(applyTableFormat(gt(
      tibble(
        col_4 = list(
          as_raw_html(table_31),
          as_raw_html(table_22),
          as_raw_html(table_13)
        )
      )
    )))),
    round_5 = list(as_raw_html(applyTableFormat(gt(
      tibble(
        col_4 = list(
          as_raw_html(table_32_end),
          as_raw_html(table_32_res),
          as_raw_html(table_23)
        )
      )
    )))),
  )
  
  total_data %>%
    gt() %>%
    tab_header(
      title = glue("Paths through the RMR - {team_name_out}"),
      subtitle = md(
        paste0(
          "Each matchup has two probabilities - total frequency (# occurances / # simulations) ",
          "and within bucket frequency (# occurrances / # times team reaches record (0-1 game, 2-1 game, etc))",
          "<br>Team strength derived from Valve’s 2024-11-13 Global Ranking",
          "<br>Win probability derived from relative glicko difference with scaling applied to match ",
          "actual vs. observed rate from Valve's research"
        ) 
      )
    ) %>%
    applyTableFormat() %>%
    gtsave(
      glue("Documents/development/test_rmr_out/{team_name_out}.png"),
      vwidth = 2000,
      vheight = 1000
    )
}


prob_output_data <- round_6_results %>%
  mutate(is_playing_adv_match = ifelse(is.na(match_idx), 0, 1)) %>%
  group_by(name, round_bucket, is_playing_adv_match) %>%
  summarise(
    prob = n() / TOTAL_SIM_COUNT,
    adv_freq = sum(win_prob) / n()
  ) %>%
  mutate(
    round_bucket = case_when(
      (round_bucket == "3-2") & (is_playing_adv_match == 1) ~ "3-2-elim",
      (round_bucket == "3-2") & (is_playing_adv_match == 0) ~ "3-2-adv",
      T ~ round_bucket
    ),
    # adjust 3-2 elim prob down to only be losses; set advance to be 0
    # so 1 - sum(everything else) is 3-2 advance
    prob = case_when(
      round_bucket == "3-2-elim" ~ prob * (1 - adv_freq),
      round_bucket == "3-2-adv" ~ 0,
      T ~ prob
    )
  ) %>%
  group_by(name) %>%
  mutate(
    # then adjust 3-2 adv up to when the advancement match is won
    prob = case_when(
      round_bucket == "3-2-adv" ~ 1 - sum(prob),
      T ~ prob
    )
  ) %>%
  select(-adv_freq, -is_playing_adv_match) %>%
  pivot_wider(
    names_from = round_bucket,
    values_from = prob
  ) %>%
  replace(is.na(.), 0) %>%
  mutate(
    adv_prob = `3-0` + `3-1` + `3-2-adv`,
    elim_prob = `3-2-elim` + `2-3` + `1-3` + `0-3`
  ) %>%
  arrange(-adv_prob) %>%
  ungroup() %>%
  left_join(
    global_standings,
    by = c('name' = 'team_name')
  ) %>%
  left_join(
    eu_rmr_b_seeds,
    by = c('name')
  ) %>%
  mutate(
    # name_disp = name,
    name_disp = glue("{name}<sub>({rmr_seed})</sub>"),
    rank = glue("#{rank}")
  ) %>% 
  select(
    team_players, rank, name, flag, name_disp, power,
    adv_prob, `3-0`, `3-1`, `3-2-adv`, `3-2-elim`, `2-3`, `1-3`, `0-3`
  ) 


prob_output_data %>%
  # head(2) %>%
  gt() %>%
  cols_hide(team_players) %>%
  fmt_percent(columns = adv_prob:`0-3`, decimals = 1)  %>%
  fmt_markdown(columns = name_disp) %>%
  cols_align(columns = name_disp, align = "left") %>%
  text_transform(
    locations = cells_body(columns=c(name)),
    fn = function(x) {
      ifelse(
        !is.na(x),
        local_image(
          filename = glue("Documents/development/test_rmr_out/logos/{x}.png"),
          height = 30
        ),
        ""
      )
    }
  ) %>%
  text_transform(
    locations = cells_body(columns=c(flag)),
    fn = function(x) {
      ifelse(
        !is.na(x),
        local_image(
          filename = glue("Documents/development/test_rmr_out/flags/{x}.svg"),
          height = 15
        ),
        ""
      )
    }
  ) %>%
  data_color(
    columns = adv_prob:`0-3`,
    palette = "viridis",
    domain = c(0, 1)
  ) %>%
  data_color(
    columns = power,
    palette = "plasma",
    domain = c(min(global_standings$power), max(global_standings$power))
  ) %>%
  tab_style(
    style = cell_borders(sides = "right", color = "#f0f0f0", weight = px(12), style = "solid"),
    locations = cells_body(
      columns = c(power, adv_prob, `3-2-adv`)
    )
  ) %>%
  cols_label(
    rank = "",
    name_disp = html("Team<sub>(seed)</sub>"),
    name = "",
    flag = "",
    power = "Valve Pts",
    adv_prob = "% Advance",
    `3-2-adv` = "{{6^th / 7^th}}",
    `3-2-elim` = "{{8^th}}"
  ) %>%
  tab_spanner(
    columns = c(`3-2-adv`, `3-2-elim`),
    label = "3-2"
  ) %>%
  tab_header(
    title = md(
      '<img src="Documents/development/test_rmr_out/eu_rmr_logo.png" style="height:50px">'
    ),
    subtitle = md(
      paste0(
        "Team strength derived from Valve's 2024-11-13 Global Ranking",
        glue("<br> Probabilities from {TOTAL_SIM_COUNT} Monte Carlo simulations")
      )
    )
  ) %>%
  tab_options(
    table.background.color = "#f0f0f0",
    data_row.padding = px(1)
  ) %>%
  tab_style(
    style = cell_text(align = "center", v_align = "middle"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_footnote(
    footnote = md(
      paste0(
        "Rating does not account for roster changes and can span multiple rosters.",
        "<br>For example, Astralis' rating consders games both with and without cadiaN, makes no adjustment for device's absence."
      )
    ),
    locations = cells_column_labels(columns = power)
  ) %>%
  opt_table_font(
    font = list(
      google_font("IBM Plex Mono"),
      default_fonts()
    ),
    weight = 500
  ) %>%
  gtsave(
    glue("Documents/development/test_rmr_out/eu_rmr_b_odds.png"),
    vwidth = 1000,
    vheight = 1000,
    expand = 0
  )







