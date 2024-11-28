# Add "Speed", "Protocol", and "Stage" columns
assign_intensity <- function(
    data, protocol_1_start, protocol_1_increase, protocol_1_length,
    use_baseline = FALSE, baseline_length = 0, baseline_intensity = 0,
    use_protocol_2 = FALSE, time_of_change = 0, protocol_2_start = 0, protocol_2_increase = 0, protocol_2_length = 0,
    use_break = FALSE, break_length = 0, break_speed = 0) {
  
  # adjust protocol_1_length so it either includes or doesn't include break_length, and also + 0.1 to ensure calculations do not exclude any required time period.
  adjusted_protocol_1_length <- ifelse(use_break, protocol_1_length + break_length + 0.1, protocol_1_length + 0.1)
  
  # adjust time_of_change (when protocol_1 transitions to protocol_2) so that it only includes the first row after the time_of_change 
  adjusted_time_of_change <- time_of_change + 0.1
  
  if (use_baseline) {
    adjusted_time_of_change <- adjusted_time_of_change + baseline_length
  }
  
  # assign "Speed" column, with toggles for baseline, breaks, and change in step parameters
  data <- data %>%
    mutate(Speed = case_when(
      use_baseline & Time <= baseline_length ~ baseline_intensity,
      !use_protocol_2 | (use_protocol_2 & Time <= adjusted_time_of_change) ~ protocol_1_start + protocol_1_increase * ((Time - ifelse(use_baseline, baseline_length, 0)) %/% adjusted_protocol_1_length),
      use_protocol_2 & Time > adjusted_time_of_change ~ protocol_2_start + protocol_2_increase * ((Time - adjusted_time_of_change) %/% protocol_2_length)
    )) %>%
    
    # assign "Protocol" column, with toggles for baseline and change in step parameters
    mutate(Protocol = case_when(
      !(use_baseline | use_protocol_2) ~ "protocol_1",
      use_baseline & !use_protocol_2 ~ ifelse(Time <= baseline_length, "baseline", "protocol_1"),
      !use_baseline & use_protocol_2 ~ ifelse(Time > adjusted_time_of_change, "protocol_2", "protocol_1"),
      use_baseline & use_protocol_2 ~ case_when(
        Time <= baseline_length ~ "baseline",
        Time <= adjusted_time_of_change ~ "protocol_1",
        TRUE ~ "protocol_2"
      )
    )) %>%
    
    # assign "Stage" column based on rank, with - 1 to make sure it always starts at "Stage_0"
    mutate(Stage = paste0("Stage_", dense_rank(Speed) - 1))
  
  # adjust "Speed" column based on whether there are breaks
  if (use_break) {
    data <- data %>%
      group_by(Stage) %>%
      mutate(max_time = max(Time),
             total_stage_time = max_time - min(Time) + mean(diff(Time)),
             Speed = if_else((Protocol == "protocol_1") & (total_stage_time == adjusted_protocol_1_length - 0.1) & (Time > (max_time - break_length)), break_speed, Speed)) %>%
      ungroup() %>%
      select(-max_time, -total_stage_time)
  }
  
  # adjust "Protocol" column so it starts at "Stage_1" if no baseline
  if (data$Protocol[1] == "protocol_1") {
    data <- data %>%
      mutate(Stage = str_replace(Stage, "(\\d+)$", function(x) as.character(as.numeric(x) + 1)))
  }
  
  # adjust "Protocol" column so all "Stage_0" are replaced with "baseline"
  data <- data %>%
    mutate(Stage = str_replace_all(Stage, "Stage_0", "baseline"))
  
  # add in min/km and m:ss, and rename Speed to Speed_kph
  data <- data %>%
    mutate(Speed_minkm_dec = if_else(Speed == 0, NA_real_, 60 / Speed),
           Speed_minkm = if_else(
             is.na(Speed_minkm_dec),
             NA_character_,
             sprintf("%d:%02d", floor(Speed_minkm_dec), round((Speed_minkm_dec %% 1) * 60)))) %>%
    rename(Speed_kph = Speed) %>%
    relocate(Speed_minkm, Speed_minkm_dec, .after = Speed_kph)
  
  return(data)
}
