### SETUP ====================================================

# Reading libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(readxl)
  library(writexl)
  library(fuzzyjoin)
  library(tidyverse)
  library(rhandsontable)
  library(SimplyAgree)
  library(zoo)
  library(plotly)
  library(fresh)
})

# Setting global options 
options(digits = 5, digits.secs = 3, scipen = 3,
        dplyr.summarise.inform = FALSE)

### CREATING FUNCTIONS FOR PROCESSING ========================
## Data Wrangling ============================================

# Create parvo_data list with 2 df (parvo_metadata and parvo_eg_data)
# Add some variables of interest
parvo_process <- 
  function(
    parvo_data_raw,
    col_names = c(
      "Time", "VO2", "VO2kg", "VCO2",
      "RER", "RR", "VT", "VE", "VEVO2", "VEVCO2",
      "FEO2", "FECO2", "HR")
  ) {
    
    # Ensures the raw Parvo export file has all the variables of interest in the expired gas section
    variables_of_interest_row <- which(grepl("---------", parvo_data_raw[[1]]))
    
    required_variables <- c("TIME", "VO2", "VO2/kg", "VCO2", "RER", "RR", "Vt", "VE", "VE/", "VE/", "FEO2", "FECO2", "HR")
    if (variables_of_interest_row - 3 < 1 || !identical(required_variables, as.character(parvo_data_raw[variables_of_interest_row - 3, ]))) {
      stop("Error: The required expired gas variables of interest are not found or do not match the expected order.")
    }
    
    # Ensures the first cell below "---------" contains a colon, which is used to assume time is exported in m:ss (and not in minute decimal format)
    if (!grepl(":", parvo_data_raw[variables_of_interest_row + 1, 1])) {
      stop("Error: Please export the Time column in 'm:ss' format.")
    }
    
    parvo_eg_data <- parvo_data_raw %>%
      # remove metadata details so you only have df if expired gas data
      slice_tail(n = -which(grepl("---------", parvo_data_raw[[1]]))) %>% 
      # rename columns from col_names input
      rename_with(~ col_names) %>% 
      # remove footer (Max, Events, and Summary) details, starting with first NA row in Time column
      slice(-(head(which(is.na(Time)),1):nrow(.))) %>%
      # convert all but Time column to numeric
      mutate(across(-Time, ~ as.numeric(.))) %>% 
      # hide warnings when reading mixed data types
      suppressWarnings() %>%
      # create new Time column in seconds
      mutate(Time_mss = Time) %>%
      relocate(Time_mss, .before = Time) %>%
      mutate(Time = lubridate::period_to_seconds(lubridate::ms(Time, roll = TRUE))) %>%
      
      # filter erroneous data where VO2 or VT below empirical cutoff. will keep the row of data, but replaces all the data with NA
      mutate(
        across(-c(Time, matches("Time_mss")), ~ case_when(
          VO2 < 0.1 | VT < 0.1 ~ NA_real_,
          TRUE ~ .)),
      ) %>%
      
      # create columns with additional variables of interest
      mutate(
        # substrate oxidation in g/min (VO2 & VCO2 in L/min), from Peronnet & Massicotte, 1991
        CHO_gmin = 4.585 * VCO2 - 3.226 * VO2,
        FAT_gmin = 1.695 * VO2 - 1.701 * VCO2,
        # Jeukendrup & Wallis, 2005 (50-75% Intensity)
        # Double check with Trent if go with 9 and 4 kcal/g
        CHO_kcalmin = CHO_gmin * 4,
        FAT_kcalmin = FAT_gmin * 9,
        Total_kcalmin = FAT_kcalmin + CHO_kcalmin,
        Total_kcalhr = Total_kcalmin * 60,
        # calculate O2eq in kJ/L & kcal/L, from Péronnet & Massicotte, 1991
        O2_kJL = 16.8835 + 4.8353 * pmin(pmax(RER, 0.7036), 0.9961),
        O2_kcalL = 4.0372 + 1.1563 * pmin(pmax(RER, 0.7036), 0.9961),
        # Aerobic Work Expenditure in kJ/min (VO2 in L/min, O2kJ in kJ/L)
        WE_kJmin = VO2 * O2_kJL,
        # Aerobic Power in W = J/sec (VO2 in L/min, O2kJ in kJ/L)
        P_Js = VO2 * O2_kJL * 1000/60,
        # Aerobic Energy Expenditure in kcal/min (VO2 in L/min, O2kcal in kcal/L)
        EE_kcalmin = VO2 * O2_kcalL
      )
    
    # create df with test metadata
    parvo_metadata <- 
      tibble(
        Name = as.character(filter(parvo_data_raw, if_any(everything(), ~ grepl("Name", .)))[[which(grepl("Name", parvo_data_raw)) + 1]]),
        Test_Date = as.character(paste(parvo_data_raw[2, 2], sprintf("%02d", as.numeric(parvo_data_raw[2, 4])), sprintf("%02d", as.numeric(parvo_data_raw[2, 6])), sep = "/")),
        Test_Time = paste(paste(parvo_data_raw[2, 7:9], collapse = ""), parvo_data_raw[2, 10], sep = ":"),
        Age = as.numeric(filter(parvo_data_raw, if_any(everything(), ~ grepl("Age", .)))[[which(grepl("Age", parvo_data_raw)) + 1]]),
        Sex = case_when(
          grepl("F", filter(parvo_data_raw, if_any(everything(), ~ grepl("Sex", .)))[[which(grepl("Sex", parvo_data_raw)) + 1]]) ~ "female",
          grepl("M", filter(parvo_data_raw, if_any(everything(), ~ grepl("Sex", .)))[[which(grepl("Sex", parvo_data_raw)) + 1]]) ~ "male"),
        Height = as.numeric(filter(parvo_data_raw, if_any(everything(), ~ grepl("Height", .)))[[which(grepl("Height", parvo_data_raw)) + 3]]),
        Weight = as.numeric(filter(parvo_data_raw, if_any(everything(), ~ grepl("Weight", .)))[[which(grepl("Weight", parvo_data_raw)) + 3]]),
        BMI = round(Weight / (Height/100)^2 , 2),
        Temperature = as.numeric(filter(parvo_data_raw, if_any(everything(), ~ grepl("Insp. temp", .)))[[which(grepl("Insp. temp", parvo_data_raw)) + 1]]),
        RH = as.numeric(filter(parvo_data_raw, if_any(everything(), ~ grepl("Insp. humidity", .)))[[which(grepl("Insp. humidity", parvo_data_raw)) + 1]])
      )
    
    return(list(parvo_eg_data = parvo_eg_data, parvo_metadata = parvo_metadata))
  }

# Remove all rows that exceed the end of the test
remove_post_test_rows <-
  function(
    data,
    input_time,
    use_baseline,
    baseline_length
  ) {
    
    # Adjust the input_time if there is a baseline stage
    if (use_baseline) {
      input_time <- input_time + baseline_length
    }
    
    # Find the index of the maximum value where Time is less than or equal to input_time
    closest_index <- which.max(data$Time[data$Time <= input_time])
    
    # Check if there's an exact match
    if (any(data$Time == input_time)) {
      # If an exact match is found, get its index
      exact_match_index <- which(data$Time == input_time)[1]
      # Keep rows up to and including the exact match
      result_data <- data[1:exact_match_index, ]
    } else {
      # If no exact match, find the closest preceding time and include the first row after it
      if (length(closest_index) == 0) {
        # Handle case when all times are greater than input_time
        result_data <- data
      } else {
        # Ensure we do not exceed the bounds of the dataframe
        end_index <- min(closest_index + 1, nrow(data))
        result_data <- data[1:end_index, ]
      }
    }
    
    return(result_data)
  }

# Round time to make it easier to work with
replace_with_closest_multiple <-
  function(
    data
  ) {
    # Creates a new column to "store" the original "Time" values
    data <- data %>%
      mutate(Time_s = Time, .before = Time)
    
    # Replace the values with rounded "Time" values
    rounded_diff <- round(mean(diff(data$Time)))
    data$Time <- round(data$Time / rounded_diff) * rounded_diff
    return(data)
  }

# Add "Speed", "Protocol", and "Stage" columns
assign_intensity <-
  function(
    data, protocol_1_start, protocol_1_increase, protocol_1_length,
    use_baseline = FALSE, baseline_length = 0, baseline_intensity = 0,
    use_protocol_2 = FALSE, time_of_change = 0, protocol_2_start = 0, protocol_2_increase = 0, protocol_2_length = 0,
    use_break = FALSE, break_length = 0, break_speed = 0
  ) {
    
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

# Assign "Grade/Cadence", "Lactate", and "RPE" columns from Shiny manually inputted table
assign_written_info <-
  function(
    data,
    stage_info,
    exercise_mode
  ) {
    
    stage_info <- stage_info()
    
    # Determine the column name based on the exercise_mode
    # col_name <- if (exercise_mode == "Cycle Ergometer") "Cadence" else "Grade"
    
    col_name <- switch(exercise_mode,
                       "Cycle Ergometer" = "Cadence",
                       "Rowing Ergometer" = "Stroke_Rate",
                       "Grade")
    
    # Match stages and assign Grade/Cadence, Lactate, and RPE values
    data <- data %>%
      mutate(
        !!col_name := stage_info[[col_name]][match(Stage, stage_info$Stage)],
        Lactate = stage_info$Lactate[match(Stage, stage_info$Stage)],
        RPE = stage_info$RPE[match(Stage, stage_info$Stage)]
      )
    
    return(data)
  }

## Creating average stage data file ==========================

# Create the initial average stage df
# Function will not try to average the columns listed
process_stage_avgs <-
  function(
    data,
    averaging_interval,
    include_last_row,
    columns_to_exclude = c(
      "Time_mss", "Time_s", "Time",
      "Speed_minkm", "Speed_minkm_dec", "Protocol",
      "Stage", "Lactate", "RPE",
      "Grade", "Cadence", "Stroke_Rate")
  ) {
    
    # Exclude rows with 0 Speed or "baseline" Stage
    filtered_data <- data %>%
      filter(Speed_kph != 0 & Stage != "baseline")
    
    # Calculate the number of rows for the averaging interval based on the Time column
    rows_for_interval <- averaging_interval / diff(head(filtered_data$Time, 2))
    
    # Group by Speed_kph and calculate the average for all columns except excluded ones
    avg_data <- filtered_data %>%
      group_by(Speed_kph) %>%
      mutate(across(!any_of(columns_to_exclude),
                    ~ rollapplyr(.x, rows_for_interval, mean, align = "right", fill = NA))) %>%
      filter(!is.na(VO2)) %>%
      ungroup()
    
    # Extract the appropriate row based on include_last_row
    final_data <- avg_data %>%
      group_by(Speed_kph) %>%
      slice(if (include_last_row) n() else if (any(Protocol == 'protocol_1')) n() - 1 else n()) %>%
      distinct(Speed_kph, .keep_all = TRUE) %>%
      ungroup()
    
    return(final_data)
  }

# Add running economy and metabolic power variables of interest
add_re_mp <-
  function(
    data,
    athlete_df
  ) {
    
    re_mp_data <- data %>%
      mutate(
        # calculate metabolic power in kJ/s using Peronnet and Massicotte equation
        MP_kJs = (16.89 * VO2 / 60) + (4.84 * VCO2 / 60),
        # expressing metabolic power as W/kg
        MP_Wkg = MP_kJs / athlete_df$Weight * 1000,
        # calculate running economy in ml/kg/km
        RE_mlkgkm = VO2kg * Speed_minkm_dec,
        # calculate running economy in kcal/km
        RE_kcalkm = Total_kcalmin * Speed_minkm_dec,
        # calculate running economy in kcal/kg/km
        RE_kcalkgkm = RE_kcalkm / athlete_df$Weight) %>%
      relocate(RE_mlkgkm, RE_kcalkm, RE_kcalkgkm, MP_kJs, MP_Wkg, .before = Speed_kph)
    
    return(re_mp_data)
  }

## Calculate test summary level variables of interest ===========

# Determine peak values for variables of interest and generate test_summary df
process_peak_data <-
  function(
    data, meta_data, averaging_interval, cols_interest, end_time, 
    protocol_1_increase, protocol_1_length,
    use_break, break_length, 
    use_protocol_2, protocol_2_increase, protocol_2_length,
    use_baseline, baseline_length
  ) {
    
    # Helper function to calculate peak values for a column
    calculate_peak <- function(data, col_interest) {
      
      # Filter data
      filtered_data <- data %>% filter(Speed_kph != 0 & Stage != "baseline")
      
      # Calculate rows for interval
      rows_for_interval <- averaging_interval / diff(data$Time[1:2])
      
      # Compute the rolling average
      filtered_data <- filtered_data %>%
        mutate(rolling_avg = zoo::rollapply(.data[[col_interest]], width = rows_for_interval, FUN = mean, align = "right", fill = NA))
      
      # Check if rolling_avg contains non-NA values. If all values in rolling_avg are NA, then it returns NA as peak value
      if (all(is.na(filtered_data$rolling_avg))) {
        return(NA)
      }
      
      # Find the maximum rolling average
      peak_value <- filtered_data %>%
        summarize(max_rolling_avg = max(rolling_avg, na.rm = TRUE)) %>%
        pull(max_rolling_avg)
      
      return(peak_value)
    }
    
    # Calculate peak values for each column of interest
    peak_values <- cols_interest %>%
      set_names(paste0(., "_peak")) %>%
      map(~ calculate_peak(data, .x)) %>%
      as_tibble() %>%
      select(where(~ !all(is.na(.))))
    
    # Calculate the adjusted peak intensity of the file
    
    # Adjust end_time based on whether there is a baseline stage
    if (use_baseline) {
      adjusted_end_time <- end_time + baseline_length
    } else {
      adjusted_end_time <- end_time
    }
    
    # Make new variable, second_highest_speed' that holds the second highest speed
    second_highest_speed <- data %>%
      arrange(desc(Speed_kph)) %>%
      distinct(Speed_kph) %>%
      slice(2) %>%
      pull(Speed_kph)
    
    # Find the last row corresponding to the second highest speed
    last_row <- data %>% filter(Speed_kph == second_highest_speed) %>% slice(n())
    
    # Calculate the time difference between the end test time and the time associated with the last row corresponding to second highest speed
    time_diff <- as.numeric(difftime(adjusted_end_time, last_row$Time, units = "secs"))
    
    # Determine if there are multiple stages in protocol_2
    has_multiple_protocol_2_stages <- data %>%
      filter(Protocol == 'protocol_2') %>%
      summarise(unique_stages = n_distinct(Stage)) %>%
      pull(unique_stages) > 1
    
    # Calculate new adjusted time difference based on conditions
    if (use_break) {
      if (use_protocol_2) {
        if (!has_multiple_protocol_2_stages) {
          # Adjust time_diff if use_break and use_protocol_2 are TRUE, and protocol_2 has only one stage
          adjusted_time_diff <- time_diff - break_length
        } else {
          # Do not adjust time_diff if protocol_2 has multiple stages
          adjusted_time_diff <- time_diff
        }
      } else {
        # Adjust time_diff if use_break is TRUE and use_protocol_2 is FALSE
        adjusted_time_diff <- time_diff - break_length
      }
    } else {
      # Do not adjust time_diff if use_break is FALSE
      adjusted_time_diff <- time_diff
    }
    
    # Calculate peak intensity based on conditions
    peak_intensity <- if (use_protocol_2) {
      second_highest_speed + (adjusted_time_diff / protocol_2_length * protocol_2_increase)
    } else {
      second_highest_speed + (adjusted_time_diff / protocol_1_length * protocol_1_increase)
    }
    
    # Combine results into a tibble
    test_summary <- bind_cols(
      meta_data,
      Test_duration = sprintf("%d:%02d", end_time %/% 60, end_time %% 60),
      V_peak = peak_intensity,
      peak_values
    )
    
    return(test_summary)
  }

# Assign normative info related to VO2max
assign_VO2max_norms <-
  function(
    athlete_characteristics_df,
    exercise_mode
  ) {
    
    # Define the reference data as a data frame
    vo2max_reference_data <- data.frame(
      Sex = c(rep("male", 18), rep("female", 18)),
      Exercise = c(rep("Treadmill", 9), rep("Cycle Ergometer", 9), rep("Treadmill", 9), rep("Cycle Ergometer", 9)),
      Percentile = rep(c(90, 80, 70, 60, 50, 40, 30, 20, 10), 4),
      Age_20_29 = c(58.6, 54.5, 51.9, 49.0, 46.5, 43.6, 40.0, 35.2, 28.6, 62.2, 57.0, 52.8, 48.3, 44.0, 40.8, 37.4, 34.5, 28.8, 49.0, 44.8, 41.8, 39.0, 36.6, 34.0, 30.8, 27.2, 22.5, 46.0, 40.9, 37.5, 34.3, 31.6, 28.9, 25.6, 21.9, 18.8),
      Age_30_39 = c(55.5, 50.0, 46.4, 43.4, 39.7, 37.0, 33.5, 29.8, 24.9, 50.5, 39.0, 35.5, 31.6, 30.2, 27.9, 25.7, 22.6, 19.1, 42.1, 37.0, 33.6, 31.0, 28.3, 26.4, 24.2, 21.9, 18.6, 32.0, 27.0, 24.5, 22.9, 21.6, 19.9, 18.6, 17.0, 15.0),
      Age_40_49 = c(50.8, 45.2, 40.9, 37.9, 35.3, 32.4, 29.7, 26.7, 22.1, 41.9, 35.1, 31.4, 29.0, 27.4, 25.4, 23.8, 21.9, 19.8, 37.8, 33.0, 30.0, 27.7, 25.7, 23.9, 21.8, 19.7, 17.2, 27.3, 23.5, 21.8, 20.3, 18.8, 17.9, 16.6, 15.4, 13.7),
      Age_50_59 = c(43.4, 38.3, 34.3, 31.8, 29.2, 26.9, 24.5, 22.2, 18.6, 37.1, 31.6, 28.4, 26.3, 24.5, 23.1, 22.0, 20.2, 17.2, 32.4, 28.4, 26.3, 24.6, 22.9, 21.5, 20.1, 18.5, 16.5, 22.4, 20.4, 18.9, 17.8, 16.9, 16.1, 15.2, 14.3, 13.0),
      Age_60_69 = c(37.1, 32.0, 28.7, 26.5, 24.6, 22.8, 20.7, 18.5, 15.8, 31.4, 27.0, 24.5, 23.3, 21.7, 20.7, 19.1, 17.5, 14.7, 27.3, 24.3, 22.4, 20.9, 19.6, 18.3, 17.0, 15.4, 13.4, 20.3, 18.5, 17.4, 16.4, 15.7, 15.0, 14.2, 13.4, 12.2),
      Age_70_79 = c(29.4, 25.9, 23.8, 22.2, 20.6, 19.1, 17.3, 15.9, 13.6, 26.2, 22.6, 20.6, 19.4, 18.3, 17.1, 16.0, 14.7, 11.0, 22.8, 20.8, 19.6, 18.3, 17.2, 16.2, 15.2, 14.0, 12.3, 18.0, 16.8, 15.9, 15.0, 14.5, 13.6, 12.9, 12.0, 10.7),
      Age_80_89 = c(22.8, 21.4, 20.0, 18.4, 17.6, 16.6, 16.1, 14.8, 12.9, 18.7, 17.3, 16.2, 14.6, 13.2, 12.2, 11.1, 9.7, 8.4, 20.8, 18.4, 17.3, 16.0, 15.4, 14.7, 13.7, 12.6, 11.4, 18.1, 14.3, 12.9, 11.3, 10.9, 10.1, 9.4, 8.7, 7.8)
    )
    
    # Myers et al., 2017 calculation for VO2max norms
    VO2max_age_pred <- 79.9 - (0.39 * athlete_characteristics_df$Age) - (13.7 * ifelse(athlete_characteristics_df$Sex == "female", 1, 0)) - (0.127 * athlete_characteristics_df$Weight * 2.20462)
    VO2max_percent_age_pred <- athlete_characteristics_df$VO2kg_peak / VO2max_age_pred * 100
    
    # Determine age group
    age_group <- case_when(
      athlete_characteristics_df$Age >= 20 & athlete_characteristics_df$Age < 30 ~ 'Age_20_29',
      athlete_characteristics_df$Age >= 30 & athlete_characteristics_df$Age < 40 ~ 'Age_30_39',
      athlete_characteristics_df$Age >= 40 & athlete_characteristics_df$Age < 50 ~ 'Age_40_49',
      athlete_characteristics_df$Age >= 50 & athlete_characteristics_df$Age < 60 ~ 'Age_50_59',
      athlete_characteristics_df$Age >= 60 & athlete_characteristics_df$Age < 70 ~ 'Age_60_69',
      athlete_characteristics_df$Age >= 70 & athlete_characteristics_df$Age < 80 ~ 'Age_70_79',
      athlete_characteristics_df$Age >= 80 & athlete_characteristics_df$Age < 90 ~ 'Age_70_79',
      TRUE ~ NA_character_
    )
    
    if (!exercise_mode %in% c("Treadmill", "Cycle Ergometer")) {
      percentile <- "Percentiles not available for the exercise modality"
      percentile_desc <- "Percentiles not available for the exercise modality"
    } else if (is.na(age_group)) {
      percentile <- "Percentiles not available for the age group"
      percentile_desc <- "Percentiles not available for the age group"
    } else {
      # Filter and select relevant reference standards
      relevant_standards <- vo2max_reference_data %>%
        filter(Sex == athlete_characteristics_df$Sex & Exercise == exercise_mode) %>%
        select(Percentile, all_of(age_group))
      
      # Find the percentile corresponding to the athlete's VO2max
      percentile <- relevant_standards %>%
        filter(relevant_standards[[age_group]] <= athlete_characteristics_df$VO2kg_peak) %>%
        arrange(desc(Percentile)) %>%
        slice(1) %>%
        pull(Percentile)
      
      if (length(percentile) == 0) {
        percentile <- "VO2max is below the 5th percentile"
        percentile_desc <- "very poor"
      } else {
        # Assign descriptive category based on the percentile
        percentile_desc <- case_when(
          percentile > 80 ~ "excellent",
          percentile >= 60 & percentile < 80 ~ "good",
          percentile >= 40 & percentile < 60 ~ "fair",
          percentile >= 20 & percentile < 40 ~ "poor",
          percentile < 20 ~ "very poor"
        )
      }
    }
    
    # Combine the results into a tibble
    test_summary <- bind_cols(
      athlete_characteristics_df,
      tibble(
        VO2max_age_pred = VO2max_age_pred,
        VO2max_age_pred_percent = VO2max_percent_age_pred,
        VO2max_percentile = percentile,
        VO2max_percentile_desc = percentile_desc
      )
    )
    
    return(test_summary)
  }

# Calculate vVO2max
workload_at_vo2max <-
  function(
    data,
    summary
  ) {
    # Filter data for Protocol == "protocol_1"
    filtered_data <- data %>%
      filter(Protocol == "protocol_1")
    
    # Perform linear regression with VO2 on the x-axis and Speed_kph on the y-axis
    lm_model <- lm(Speed_kph ~ VO2, data = filtered_data)
    
    # Get the coefficients from the linear model
    intercept <- coef(lm_model)[1]
    slope <- coef(lm_model)[2]
    
    # Solve the regression equation using the VO2_peak value
    vVO2max <- intercept + slope * summary$VO2_peak
    
    summary$vVO2max <- vVO2max
    
    return(summary)
  }

# Calculate Fatmax and MFO
calculate_fatmax <-
  function(
    avg_stage_data,
    test_summary
  ) {
    
    # Filter data so it's only the rows where RER < 1
    filtered_df <- avg_stage_data %>%
      filter(RER <= 1)
    
    # Fit third-degree polynomial models with intercept set to zero
    fit_speed <- lm(FAT_gmin ~ 0 + poly(Speed_kph, 3, raw = TRUE), data = filtered_df)
    fit_hr <- lm(FAT_gmin ~ 0 + poly(Perc_HR_peak, 3, raw = TRUE), data = filtered_df)
    fit_vo2 <- lm(FAT_gmin ~ 0 + poly(Perc_VO2_peak, 3, raw = TRUE), data = filtered_df)
    
    # Create sequences for prediction
    speed_seq <- seq(min(filtered_df$Speed_kph), max(filtered_df$Speed_kph), length.out = 1000)
    hr_seq <- seq(min(filtered_df$Perc_HR_peak), max(filtered_df$Perc_HR_peak), length.out = 1000)
    vo2_seq <- seq(min(filtered_df$Perc_VO2_peak), max(filtered_df$Perc_VO2_peak), length.out = 1000)
    
    # Predict FAT_gmin values using the polynomial models
    pred_speed <- predict(fit_speed, newdata = data.frame(Speed_kph = speed_seq))
    pred_hr <- predict(fit_hr, newdata = data.frame(Perc_HR_peak = hr_seq))
    pred_vo2 <- predict(fit_vo2, newdata = data.frame(Perc_VO2_peak = vo2_seq))
    
    # Find the values that give the maximum FAT_gmin
    max_index_speed <- which.max(pred_speed)
    max_index_hr <- which.max(pred_hr)
    max_index_vo2 <- which.max(pred_vo2)
    
    fatmax_speed <- speed_seq[max_index_speed]
    fatmax_hr <- hr_seq[max_index_hr]
    fatmax_vo2 <- vo2_seq[max_index_vo2]
    
    max_fat_oxidation_speed <- pred_speed[max_index_speed]
    max_fat_oxidation_hr <- pred_hr[max_index_hr]
    max_fat_oxidation_vo2 <- pred_vo2[max_index_vo2]
    
    # Update test_summary with new values
    test_summary <- test_summary %>%
      mutate(
        Fatmax_kph = fatmax_speed,
        Fatmax_HR = fatmax_hr,
        Fatmax_VO2 = fatmax_vo2,
        MFO_gmin = max_fat_oxidation_speed)
    
    return(test_summary)
  }


## Miscellaneous functions ===================================

# Convert time in m:ss to seconds
convert_to_s <-
  function(
    time_str
  ) {
    
    Time = lubridate::period_to_seconds(lubridate::ms(time_str, roll = TRUE))
    return(Time)
  }

# Add % HRpeak and VO2peak columns to avg_stage_data
add_peak_percentages <-
  function(
    avg_stage_data,
    test_summary
  ) {
    
    HR_peak <- pull(test_summary, HR_peak)
    VO2_peak <- pull(test_summary, VO2_peak)
    
    avg_stage_data <- avg_stage_data %>%
      mutate(Perc_HR_peak = HR/HR_peak * 100,
             Perc_VO2_peak = VO2/VO2_peak * 100)
    
    return(avg_stage_data)
  }

# Delete the rounded 'Time' column and mutate a minute decimal time column in the df
fix_time <-
  function(
    data
  ) {
    
    data <- data %>%
      select(-Time) %>%
      mutate(Time_m = Time_s / 60) %>%
      relocate(Time_m, .after = Time_mss)
    
    return(data)
  }

# adjust parvo_eg_data based on exercise modality
adjust_to_modality_parvo_eg_data <-
  function(
    parvo_eg_data,
    test_summary,
    exercise_mode,
    power_unit
  ) {
    
    if (exercise_mode %in% c("Cycle Ergometer", "Rowing Ergometer")) {
      parvo_eg_data <- parvo_eg_data %>%
        select(-Speed_minkm,
               -Speed_minkm_dec) %>%
        rename(Power_W = Speed_kph)
      
      if (power_unit) {
        parvo_eg_data <- parvo_eg_data %>%
          mutate(Power_W_temp = Power_W * test_summary$Weight) %>%
          relocate(Power_W_temp, .before = Power_W) %>%
          rename(Power_Wkg = Power_W, Power_W = Power_W_temp)
      } else {
        parvo_eg_data <- parvo_eg_data %>%
          mutate(Power_Wkg = Power_W / test_summary$Weight) %>%
          relocate(Power_Wkg, .after = Power_W)
      }
    }
    
    return(parvo_eg_data)
  }

# adjust avg_stage_data based on exercise modality
adjust_to_modality_avg_stage_data <-
  function(
    avg_stage_data,
    test_summary,
    exercise_mode,
    power_unit
  ) {
    
    add_avg_cycling_metrics <- function(avg_stage_data, test_summary, power_unit) {
      
      if (power_unit) {
        avg_stage_data <- avg_stage_data %>%
          mutate(
            CE_LminW = VO2 / (Power_W * test_summary$Weight),
            # Used by Lucia, 2002; Millet, 2003; Coyle, 1991 - did they have the units wrong?? Look at Table 4 of Coyle 1991
            CE_WLmin = (Power_W * test_summary$Weight) / VO2,
            # Used by Moseley & Jeukendrup, 2000
            CE_kJL = (Power_W * test_summary$Weight) / VO2 * 60 / 1000,
            # Calculated as ratio of work rate (W) to energy expended (J/s); Moseley & Jeukendrup, 2000
            GE_Js = (Power_W * test_summary$Weight) / P_Js * 100,
            # Calculated as ratio of work rate (kcal/min) to energy expended (kcal/min); Coyle, 1991
            # Is 60/4184 correct conversion? Based on 4.184 J = 1 cal?
            GE_kcalmin = ((Power_W * test_summary$Weight) * 60 / 4184) / EE_kcalmin * 100) %>%
          relocate(CE_LminW, CE_WLmin, CE_kJL, GE_Js, GE_kcalmin, .before = Power_W)
      } else {
        avg_stage_data <- avg_stage_data %>%
          mutate(
            CE_LminW = VO2 / Power_W,
            CE_WLmin = Power_W / VO2,
            CE_kJL = Power_W / VO2 * 60 / 1000,
            GE_Js = Power_W / P_Js * 100,
            GE_kcalmin = (Power_W * 60 / 4184) / EE_kcalmin * 100) %>%
          relocate(CE_LminW, CE_WLmin, CE_kJL, GE_Js, GE_kcalmin, .before = Power_W)
      }
      
      return(avg_stage_data)
    }
    
    if (exercise_mode %in% c("Cycle Ergometer", "Rowing Ergometer")) {
      avg_stage_data <- avg_stage_data %>%
        select(-Speed_minkm,
               -Speed_minkm_dec,
               -RE_mlkgkm,
               -RE_kcalkm,
               -RE_kcalkgkm) %>%
        rename(Power_W = Speed_kph)
      
      avg_stage_data <- add_avg_cycling_metrics(avg_stage_data, test_summary, power_unit)
      
      if (power_unit) {
        avg_stage_data <- avg_stage_data %>%
          mutate(Power_W_temp = Power_W * test_summary$Weight) %>%
          relocate(Power_W_temp, .before = Power_W) %>%
          rename(Power_Wkg = Power_W, Power_W = Power_W_temp)
      } else {
        avg_stage_data <- avg_stage_data %>%
          mutate(Power_Wkg = Power_W / test_summary$Weight) %>%
          relocate(Power_Wkg, .after = Power_W)
      }
    }
    
    return(avg_stage_data)
    
  }

# adjust test_summary based on exercise modality
adjust_to_modality_test_summary <-
  function(
    test_summary,
    exercise_mode,
    power_unit
  ) {
    
    if (exercise_mode %in% c("Cycle Ergometer", "Rowing Ergometer")) {
      test_summary <- test_summary %>%
        rename(pVO2max = vVO2max,
               W_peak = V_peak,
               Fatmax_W = Fatmax_kph)# %>%
      # add delta efficiency code here
      
      if (power_unit) {
        test_summary <- test_summary %>%
          mutate(pVO2max_W_temp = pVO2max * Weight,
                 W_peak_temp = W_peak * Weight,
                 Fatmax_W_temp = Fatmax_W * Weight) %>%
          relocate(pVO2max_W_temp, .before = pVO2max) %>%
          relocate(W_peak_temp, .before = W_peak) %>%
          relocate(Fatmax_W_temp, .before = Fatmax_W) %>%
          rename(pVO2max_Wkg = pVO2max, pVO2max_W = pVO2max_W_temp,
                 Wkg_peak = W_peak, W_peak = W_peak_temp,
                 Fatmax_Wkg = Fatmax_W, Fatmax_W = Fatmax_W_temp)
      } else {
        test_summary <- test_summary %>%
          mutate(pVO2max_Wkg = pVO2max / Weight,
                 Wkg_peak = W_peak / Weight,
                 Fatmax_Wkg = Fatmax_W / Weight) %>%
          relocate(pVO2max_Wkg, .after = pVO2max) %>%
          rename(pVO2max_W = pVO2max) %>%
          relocate(Wkg_peak, .after = W_peak) %>%
          relocate(Fatmax_Wkg, .after = Fatmax_W)
      }
    }
    
    return(test_summary)
    
  }

# Add thresholds and zone info to test_summary
add_threshold_zone <-
  function(
    threshold_zone_info,
    test_summary
  ) {
    
    threshold_zone <- threshold_zone_info()
    
    for (col in names(threshold_zone)) {
      if (!all(is.na(threshold_zone[[col]]))) {
        test_summary[[col]] <- threshold_zone[[col]]
      }
    }
    
    return(test_summary)
    
  }

# # Add marathon race prediction times to test_summary
# add_race_prediction <-
#   function(
    #     exercise_mode,
#     test_summary
#   ) {
#     
#     if (exercise_mode == "Treadmill") {
#       test_summary$
#     }
#     
#     return(test_summary)
#     
#   }

# Remove empty columns
remove_empty_columns <- function(df){
  
  # Remove columns where all values are NA or empty
  df_cleaned <- df[, colSums(!is.na(df) & df != "") > 0]
  
  return(df_cleaned)
}

### CREATE SHINY APP USER INTERFACE ==========================
## Creating sidebar UI =======================================

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About the app", tabName = "about", icon = icon("circle-question"),
             menuSubItem("General Outline", tabName = "app_outline"),
             menuSubItem("Usage Guide", tabName = "instructions"),
             menuSubItem("Glossary", tabName = "glossary"),
             startExpanded = TRUE),
    menuItem("Step Test", tabName = "step_test_processor", icon = icon("stairs"),
             menuSubItem("Options", tabName = "step_test_options"),
             menuSubItem("Test Summary", tabName = "step_test_summary"),
             menuSubItem("Average Stage Data", tabName = "step_avg_stage_data"),
             menuSubItem("Parvo EG Data", tabName = "step_parvo_eg_data"),
             menuSubItem("Graphs", tabName = "graphs")),
    # menuItem("Ramp Test", tabName = "ramp_test_processor", icon = icon("chart-line"),
    #          menuSubItem("Options", tabName = "ramp_test_options"),
    #          menuSubItem("Test Summary", tabName = "ramp_test_summary"),
    #          menuSubItem("Average Stage Data", tabName = "ramp_avg_stage_data"),
    #          menuSubItem("Parvo EG Data", tabName = "ramp_parvo_eg_data")),
    menuItem("ExPhysLab Tools", tabName = "ExPhysLab Tools", lib = "font-awesome", icon = icon("screwdriver-wrench"),
             menuSubItem("Lactate Thresholds App", tabName = "lactate_thresholds"),
             menuSubItem("Exercise Thresholds App", tabName = "exercise_thresholds"))
  )
)

## Creating body UI ==========================================

# Sets the theme of dashboard
my_theme = create_theme(
  adminlte_color(
    light_blue = "#111"
  )
)

# Codes the dashboardBody
body <- dashboardBody(
  use_theme(my_theme),
  tabItems(
    tabItem(tabName = "app_outline",
            includeMarkdown("App Outline.md")
    ),
    tabItem(tabName = "instructions",
            includeMarkdown("Instructions.md")
            
    ),
    tabItem(tabName = "glossary",
            includeMarkdown("Glossary.md")
    ),
    tabItem(tabName = "step_test_options",
            fluidPage(
              fluidRow(
                box(
                  title = "Exercise Modality",
                  width = 6,
                  status = "success", solidHeader = TRUE,
                  radioGroupButtons("modality", "Select the Exercise Modality", choices = c("Treadmill", "Cycle Ergometer", "Rowing Ergometer"), status = "primary")
                ),
                box(
                  title = "Process the Data",
                  width = 6,
                  status = "success", solidHeader = TRUE,
                  actionButton("process", "Process Data")
                )),
              fluidRow(
                box(
                  title = "Upload File",
                  width = 4,
                  "Upload the raw file exported directly from the Parvo Medics' TrueOne",
                  status = "primary", solidHeader = TRUE,
                  fileInput("file", "Upload Parvo File", accept = c(".xls", ".xlsx"))
                ),
                box(
                  title = "Written Info",
                  width = 4,
                  "Fill in the table with the information manually recorded during the test",
                  status = "primary", solidHeader = TRUE,
                  h1("Number of Stages"), 
                  fluidRow(
                    column(2, actionButton("remove_stage", label = NULL, icon = icon("minus"), class = "btn btn-danger")),
                    column(7, numericInput("stage_counter", label = NULL, value = 5, min = 1, width = "100%")),
                    column(2, actionButton("add_stage", label = NULL, icon = icon("plus"), class = "btn btn-success"))),
                  rHandsontableOutput("stage_table")
                ),
                box(
                  title = "Protocol Options",
                  width = 4,
                  "Select the protocol design of the GXT",
                  status = "primary", solidHeader = TRUE,
                  uiOutput("protocol_1_start_input_ui"),
                  uiOutput("protocol_1_increase_input_ui"),
                  uiOutput("power_unit_switch_ui"),
                  textInput("protocol_1_length_input", "Length of Stage (m:ss)", "3:00"),
                  textInput("end_time_input", "End Time (m:ss)")
                )),
              fluidRow(
                box(
                  title = "Baseline Options",
                  width = 4,
                  "If a baseline stage was completed, select the options",
                  status = "warning", solidHeader = TRUE,
                  switchInput("has_baseline", "Has Baseline Stage", TRUE),
                  textInput("baseline_time", "Duration of Baseline Stage (m:ss)", "1:00"),
                  uiOutput("baseline_intensity_input_ui"),
                ),
                box(
                  title = "Break Options",
                  width = 4,
                  "If a break was used between submaximal stages, select the options",
                  status = "warning", solidHeader = TRUE,
                  switchInput("has_break", "Has Break", TRUE),
                  textInput("break_length_input", "Length of Breaks (m:ss)", "0:30"),
                  uiOutput("break_speed_input_ui"),
                ),
                box(
                  title = "Second Protocol Options",
                  width = 4,
                  "If there was a change in protocol in the middle of the test, select the options",
                  status = "warning", solidHeader = TRUE,
                  switchInput("has_protocol_2", "Has Second Protocol (i.e., new step parameters)", TRUE),
                  textInput("when_protocol_2", "When Second Protocol Begins (m:ss)"),
                  uiOutput("protocol_2_start_input_ui"),
                  uiOutput("protocol_2_increase_input_ui"),
                  textInput("protocol_2_length_input", "Length of Stage of Second Protocol (m:ss)", "1:00")
                )),
              fluidRow(
                box(
                  title = "Advanced Options",
                  width = 4,
                  status = "danger", solidHeader = TRUE,
                  #collapsible = TRUE, collapsed = TRUE,
                  numericInput("avg_interval", "Calculation Intervals for Averages during Processing (seconds)", 30),
                  switchInput("include_last_row_input", "Include Last Row of Each Stage in Averaging", FALSE),
                  pickerInput("vars_of_interest", "Variables of Interest", 
                              choices = c("Lactate", "RPE", "HR", "VO2", "VO2kg", "VCO2", "RER", "RR", "VT", "VE"), 
                              selected = c("Lactate", "RPE", "HR", "VO2", "VO2kg"), multiple = TRUE)
                ),
                box(
                  title = "Exercise Thresholds and Zones",
                  width = 8,
                  status = "danger", solidHeader = TRUE,
                  rHandsontableOutput("threshold_table"),
                  rHandsontableOutput("zone_table")
                )
              )
            )
    ),
    tabItem(tabName = "step_test_summary",
            downloadButton("download_step_test_summary_data_excel", "Excel"),
            downloadButton("download_step_test_summary_data_csv", "CSV"),
            fluidRow(
              box(
                title = "Test Summary",
                uiOutput("message_before_processing_tst"),
                rHandsontableOutput("test_summary_table"),
                width = 12,
                style = "height: 85vh; overflow-y: scroll; overflow-x: scroll;"
              )
            )
    ),
    tabItem(tabName = "step_avg_stage_data",
            downloadButton("download_step_avg_stage_data_excel", "Excel"),
            downloadButton("download_step_avg_stage_data_csv", "CSV"),
            fluidRow(
              box(
                title = "Average Stage Data",
                uiOutput("message_before_processing_asdt"),
                rHandsontableOutput("avg_stage_data_table"),
                width = 12,
                style = "height: 85vh; overflow-y: scroll; overflow-x: scroll;"
              )
            )
    ),
    tabItem(tabName = "step_parvo_eg_data",
            downloadButton("download_step_parvo_eg_data_excel", "Excel"),
            downloadButton("download_step_parvo_eg_data_csv", "CSV"),
            fluidRow(
              box(
                title = "Parvo EG Data",
                uiOutput("message_before_processing_pedt"),
                rHandsontableOutput("parvo_eg_data_table"),
                width = 12
                #, style = "height: 85vh; overflow-y: scroll; overflow-x: scroll;"
              )
            )
    ),
    tabItem(tabName = "graphs",
            sidebarLayout(
              sidebarPanel(
                # Controls for Parvo EG Data
                fluidRow(
                  box(
                    title = "Parvo EG Graph Controls",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    uiOutput("parvo_data_y_var_ui"),
                    uiOutput("parvo_data_x_var_ui")
                  )
                ),
                # Controls for Average Stage Data
                fluidRow(
                  box(
                    title = "Average Stage Graph Controls",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    uiOutput("avg_stage_y_var_ui"),
                    uiOutput("avg_stage_x_var_ui")
                  )
                )
              ),
              mainPanel(
                box(
                  title = "Parvo Data Plot",
                  status = "primary",
                  solidHeader = TRUE,
                  width = NULL,
                  plotlyOutput("parvo_eg_plot")
                ),
                box(
                  title = "Average Stage Data Plot",
                  status = "primary",
                  solidHeader = TRUE,
                  width = NULL,
                  plotlyOutput("avg_stage_plot")
                )
              )
            )
    ),
    # tabItem(tabName = "ramp_test_options",
    #         mainPanel("Coming soon")
    # ),
    # tabItem(tabName = "ramp_test_summary",
    #         mainPanel("Coming soon")
    # ),
    # tabItem(tabName = "ramp_avg_stage_data",
    #         mainPanel("Coming soon")
    # ),
    # tabItem(tabName = "ramp_parvo_eg_data",
    #         mainPanel("Coming soon")
    # ),
    tabItem(tabName = "lactate_thresholds",
            tags$iframe(src = "https://www.exphyslab.com/lactate",
                        width = "100%",
                        style = "height: 85vh;")
    ),
    tabItem(tabName = "exercise_thresholds",
            tags$iframe(src = "https://www.exercisethresholds.com/analyze",
                        width = "100%",
                        style = "height: 85vh;")
    )
  )
)

# Code to define UI
ui <- dashboardPage(
  dashboardHeader(title = "Expired Gas Data Processing"),
  sidebar,
  body
)

### DEFINE THE SERVER LOGIC ==================================

server <- function(input, output, session) {
  
  ## Server code related to stage_info table ========================
  
  # Reactive expression to create default stage_info df
  initial_stage_info_data <- reactive({
    if (input$modality == "Cycle Ergometer") {
      data.frame(
        Stage = paste0("Stage_", 1:5),
        Cadence = 90,
        Lactate = as.numeric(NA),
        RPE = as.numeric(NA)
      )
    } else if (input$modality == "Rowing Ergometer") {
      data.frame(
        Stage = paste0("Stage_", 1:5),
        Stroke_Rate = 30,
        Lactate = as.numeric(NA),
        RPE = as.numeric(NA)
      )
    } else {
      data.frame(
        Stage = paste0("Stage_", 1:5),
        Grade = 1,
        Lactate = as.numeric(NA),
        RPE = as.numeric(NA)
      )
    }
  })
  
  # Reactive values to hold the table data
  written_info <- reactiveVal()
  observe({
    written_info(initial_stage_info_data())
  })
  
  # Observe changes in stage_counter and modality to update the table
  observe({
    current_rows <- nrow(written_info())
    if (input$stage_counter > current_rows) {
      additional_rows <-
        if (input$modality == "Cycle Ergometer") {
          data.frame(
            Stage = paste0("Stage_", (current_rows + 1):input$stage_counter),
            Cadence = 90,
            Lactate = as.numeric(NA),
            RPE = as.numeric(NA)
          )
        } else if (input$modality == "Rowing Ergometer") {
          data.frame(
            Stage = paste0("Stage_", (current_rows + 1):input$stage_counter),
            Stroke_Rate = 30,
            Lactate = as.numeric(NA),
            RPE = as.numeric(NA)
          )
        } else {
          data.frame(
            Stage = paste0("Stage_", (current_rows + 1):input$stage_counter),
            Grade = 1,
            Lactate = as.numeric(NA),
            RPE = as.numeric(NA)
          )
        }
      written_info(rbind(written_info(), additional_rows))
    } else if (input$stage_counter < current_rows) {
      written_info(written_info()[1:input$stage_counter, ])
    }
  })
  
  observeEvent(input$add_stage, {
    updateNumericInput(session, "stage_counter", value = input$stage_counter + 1)
  })
  
  observeEvent(input$remove_stage, {
    if (input$stage_counter > 1) {
      updateNumericInput(session, "stage_counter", value = input$stage_counter - 1)
    }
  })
  
  # Observe changes in modality to reset the table with correct column names
  observeEvent(input$modality, {
    written_info(initial_stage_info_data())
  })
  
  # Renders the stage_info table
  output$stage_table <- renderRHandsontable({
    if (input$modality == "Cycle Ergometer") {
      rhandsontable(written_info(), rowHeaders = NULL) %>%
        hot_col("Cadence", type = "numeric") %>%
        hot_col("Lactate", type = "numeric") %>%
        hot_col("RPE", type = "numeric")
    } else if (input$modality == "Rowing Ergometer") {
      rhandsontable(written_info(), rowHeaders = NULL) %>%
        hot_col("Stroke_Rate", type = "numeric") %>%
        hot_col("Lactate", type = "numeric") %>%
        hot_col("RPE", type = "numeric")
    } else {
      rhandsontable(written_info(), rowHeaders = NULL) %>%
        hot_col("Grade", type = "numeric") %>%
        hot_col("Lactate", type = "numeric") %>%
        hot_col("RPE", type = "numeric")
    }
  })
  
  # Update the written_info variable with the contents of the table whenever it's edited
  observe({
    if (!is.null(input$stage_table)) {
      written_info(hot_to_r(input$stage_table))
    }
  })
  
  ## Server code related to exercise thresholds and zones ===========
  
  # Reactive expression to create default info for exercise thresholds
  initial_threshold_data <- reactive({
    if (input$modality == "Treadmill") {
      data.frame(
        Speed_LT1 = as.numeric(NA),
        VO2_LT1 = as.numeric(NA),
        BLa_LT1 = as.numeric(NA),
        Speed_LT2 = as.numeric(NA),
        VO2_LT2 = as.numeric(NA),
        BLa_LT2 = as.numeric(NA)
      )
    } else {
      data.frame(
        Power_LT1 = as.numeric(NA),
        VO2_LT1 = as.numeric(NA),
        BLa_LT1 = as.numeric(NA),
        Power_LT2 = as.numeric(NA),
        VO2_LT2 = as.numeric(NA),
        BLa_LT2 = as.numeric(NA)
      )
    }
  })
  
  # Reactive expression to create default info for exercise zones
  initial_zone_data <- reactive({
    if (input$modality == "Treadmill") {
      data.frame(
        Speed_Z1 = as.character(NA),
        HR_Z1 = as.character(NA),
        RPE_Z1 = as.character(NA),
        Speed_Z2 = as.character(NA),
        HR_Z2 = as.character(NA),
        RPE_Z2 = as.character(NA),
        Speed_Z3 = as.character(NA),
        HR_Z3 = as.character(NA),
        RPE_Z3 = as.character(NA)
      )
    } else {
      data.frame(
        Power_Z1 = as.character(NA),
        HR_Z1 = as.character(NA),
        RPE_Z1 = as.character(NA),
        Power_Z2 = as.character(NA),
        HR_Z2 = as.character(NA),
        RPE_Z2 = as.character(NA),
        Power_Z3 = as.character(NA),
        HR_Z3 = as.character(NA),
        RPE_Z3 = as.character(NA)
      )
    }
  })
  
  # Reactive values to hold the exercise thresholds table data
  threshold_info <- reactiveVal()
  observe({
    threshold_info(initial_threshold_data())
  })
  
  # Reactive values to hold the exercise zones table data
  zone_info <- reactiveVal()
  observe({
    zone_info(initial_zone_data())
  })
  
  # Observe changes in modality to reset the exercise thresholds table with correct column names
  observeEvent(input$modality, {
    threshold_info(initial_threshold_data())
  })
  
  # Observe changes in modality to reset the zones table with correct column names
  observeEvent(input$modality, {
    zone_info(initial_zone_data())
  })
  
  # Renders the exercise thresholds table
  output$threshold_table <- renderRHandsontable({
    if (input$modality == "Treadmill") {
      rhandsontable(threshold_info(), rowHeaders = NULL) %>%
        hot_col("Speed_LT1", type = "numeric") %>%
        hot_col("VO2_LT1", type = "numeric") %>%
        hot_col("BLa_LT1", type = "numeric") %>%
        hot_col("Speed_LT2", type = "numeric") %>%
        hot_col("VO2_LT2", type = "numeric") %>%
        hot_col("BLa_LT2", type = "numeric")
    } else {
      rhandsontable(threshold_info(), rowHeaders = NULL) %>%
        hot_col("Power_LT1", type = "numeric") %>%
        hot_col("VO2_LT1", type = "numeric") %>%
        hot_col("BLa_LT1", type = "numeric") %>%
        hot_col("Power_LT2", type = "numeric") %>%
        hot_col("VO2_LT2", type = "numeric") %>%
        hot_col("BLa_LT2", type = "numeric")
    }
  })
  
  # Renders the exercise zones table
  output$zone_table <- renderRHandsontable({
    if (input$modality == "Treadmill") {
      rhandsontable(zone_info(), rowHeaders = NULL) %>%
        hot_col("Speed_Z1") %>%
        hot_col("HR_Z1") %>%
        hot_col("RPE_Z1") %>%
        hot_col("Speed_Z2") %>%
        hot_col("HR_Z2") %>%
        hot_col("RPE_Z2") %>%
        hot_col("Speed_Z3") %>%
        hot_col("HR_Z3") %>%
        hot_col("RPE_Z3")
    } else {
      rhandsontable(zone_info(), rowHeaders = NULL) %>%
        hot_col("Power_Z1") %>%
        hot_col("HR_Z1") %>%
        hot_col("RPE_Z1") %>%
        hot_col("Power_Z2") %>%
        hot_col("HR_Z2") %>%
        hot_col("RPE_Z2") %>%
        hot_col("Power_Z3") %>%
        hot_col("HR_Z3") %>%
        hot_col("RPE_Z3")
    }
  })
  
  # Update the written_info variable with the contents of the exercise thresholds table whenever it's edited
  observe({
    if (!is.null(input$threshold_table)) {
      threshold_info(hot_to_r(input$threshold_table))
    }
  })
  
  # Update the written_info variable with the contents of the exercise zones table whenever it's edited
  observe({
    if (!is.null(input$zone_table)) {
      zone_info(hot_to_r(input$zone_table))
    }
  })
  
  ## Server code related to exercise modality =======================
  
  # Modify UI based on modality chosen
  renderNumericInput <- function(inputId, labelBase, defaultValues) {
    renderUI({
      if (input$modality == "Cycle Ergometer") {
        unit <- ifelse(input$power_unit_switch, "W/kg", "W")
        defaultValue <- defaultValues[[paste("Cycle Ergometer", unit)]]
        label <- paste(labelBase, " (", unit, ")", sep = "")
      } else if (input$modality == "Rowing Ergometer") {
        unit <- ifelse(input$power_unit_switch, "W/kg", "W")
        defaultValue <- defaultValues[[paste("Rowing Ergometer", unit)]]
        label <- paste(labelBase, " (", unit, ")", sep = "")
      } else {
        unit <- "kph"
        defaultValue <- defaultValues[["Treadmill kph"]]
        label <- paste(labelBase, " (", unit, ")", sep = "")
      }
      numericInput(inputId, label, defaultValue)
    })
  }
  
  observeEvent(input$modality, {
    if (input$modality %in% c("Cycle Ergometer", "Rowing Ergometer")) {
      output$power_unit_switch_ui <- renderUI({
        materialSwitch("power_unit_switch", label = "Input in W/kg", status = "primary", value = FALSE)
      })
    } else {
      output$power_unit_switch_ui <- renderUI({
        NULL
      })
    }
  })
  
  observe({
    modality <- input$modality
    
    # Define labels based on modality
    labels <- list(
      protocol_1_start = if (modality == "Treadmill") "Starting Speed" else "Starting Power",
      protocol_1_increase = if (modality == "Treadmill") "Speed Increments" else "Power Increments",
      baseline_intensity = if (modality == "Treadmill") "Speed during Baseline Stage" else "Power during Baseline Stage",
      break_speed = if (modality == "Treadmill") "Speed during Breaks" else "Power during Breaks",
      protocol_2_start = if (modality == "Treadmill") "Starting Speed of Second Protocol" else "Starting Power of Second Protocol",
      protocol_2_increase = if (modality == "Treadmill") "Speed Increments of Second Protocol" else "Power Increments of Second Protocol"
    )
    
    output$protocol_1_start_input_ui <- renderNumericInput(
      "protocol_1_start_input", labels$protocol_1_start,
      list("Cycle Ergometer W" = NA, "Cycle Ergometer W/kg" = NA, "Rowing Ergometer W" = NA, "Rowing Ergometer W/kg" = NA, "Treadmill kph" = NA)
    )
    output$protocol_1_increase_input_ui <- renderNumericInput(
      "protocol_1_increase_input", labels$protocol_1_increase,
      list("Cycle Ergometer W" = 25, "Cycle Ergometer W/kg" = 0.3, "Rowing Ergometer W" = 25, "Rowing Ergometer W/kg" = 0.3, "Treadmill kph" = 1)
    )
    output$baseline_intensity_input_ui <- renderNumericInput(
      "baseline_intensity_input", labels$baseline_intensity,
      list("Cycle Ergometer W" = 0, "Cycle Ergometer W/kg" = 0, "Rowing Ergometer W" = 0, "Rowing Ergometer W/kg" = 0, "Treadmill kph" = 0)
    )
    output$break_speed_input_ui <- renderNumericInput(
      "break_speed_input", labels$break_speed,
      list("Cycle Ergometer W" = 0, "Cycle Ergometer W/kg" = 0, "Rowing Ergometer W" = 0, "Rowing Ergometer W/kg" = 0, "Treadmill kph" = 0)
    )
    output$protocol_2_start_input_ui <- renderNumericInput(
      "protocol_2_start_input", labels$protocol_2_start,
      list("Cycle Ergometer W" = NA, "Cycle Ergometer W/kg" = NA, "Rowing Ergometer W" = NA, "Rowing Ergometer W/kg" = NA, "Treadmill kph" = NA)
    )
    output$protocol_2_increase_input_ui <- renderNumericInput(
      "protocol_2_increase_input", labels$protocol_2_increase,
      list("Cycle Ergometer W" = 25, "Cycle Ergometer W/kg" = 0.35, "Rowing Ergometer W" = 25, "Rowing Ergometer W/kg" = 0.3, "Treadmill kph" = 1)
    )
  })
  
  
  # Code to create reactive values for graphing
  reactive_avg_stage_data_graphing <- reactiveVal(NULL)
  reactive_parvo_data_graphing <- reactiveVal(NULL)
  
  ## Server code related to 'saving' any edits made to the tables ====
  
  # Code to create reactive values for the table outputs
  reactive_test_summary <- reactiveVal(NULL)
  reactive_avg_stage_data <- reactiveVal(NULL)
  reactive_parvo_eg_data <- reactiveVal(NULL)
  
  # Code that updates each of the table whenever edits are made
  observe({
    if (!is.null(input$test_summary_table)) {
      reactive_test_summary(hot_to_r(input$test_summary_table))
    }
  })
  
  observe({
    if (!is.null(input$avg_stage_data_table)) {
      updated_avg_stage_data <- hot_to_r(input$avg_stage_data_table)
      reactive_avg_stage_data(updated_avg_stage_data)
      
      # Update the graphs to reflect any edits made to the avg_stage_data_table
      current_avg_stage_data <- reactive_avg_stage_data_graphing()
      if (!is.null(current_avg_stage_data)) {
        current_avg_stage_data <- updated_avg_stage_data
        reactive_avg_stage_data_graphing(current_avg_stage_data)
      }
    }
  })
  
  observe({
    if (!is.null(input$parvo_eg_data_table)) {
      updated_parvo_eg_data <- hot_to_r(input$parvo_eg_data_table)
      reactive_parvo_eg_data(updated_parvo_eg_data)
      
      # Update the graphs to reflect any edits made to the parvo_eg_data_table
      current_parvo_data <- reactive_parvo_data_graphing()
      if (!is.null(current_parvo_data)) {
        current_parvo_data$parvo_eg_data <- updated_parvo_eg_data
        reactive_parvo_data_graphing(current_parvo_data)
      }
    }
  })
  
  ## Server code related to message prior to processing data =========
  
  output$message_before_processing_tst <- renderUI({
    if (input$process == 0) {
      h3("To preview the Test Summary table, please go to 'Options' and process the data first.")
    }})
  
  output$message_before_processing_asdt <- renderUI({
    if (input$process == 0) {
      h3("To preview the Average Stage Data table, please go to 'Options' and process the data first.")
    }})
  
  output$message_before_processing_pedt <- renderUI({
    if (input$process == 0) {
      h3("To preview the highest fidelity data table, please go to 'Options' and process the data first.")
    }})
  
  ## Server code that runs after "Process" is initiated =============
  
  # Code that reads in the inputs from the Shiny interface and processes the data
  observeEvent(input$process, {
    
    withProgress(message = "Processing data...", value = 0, {
      incProgress(0.05, detail = "Reading and preparing data...")
      
      file_of_interest <- suppressMessages(read_excel(input$file$datapath))
      modality <- input$modality
      power_unit <- input$power_unit_switch
      has_baseline <- input$has_baseline
      baseline_time <- convert_to_s(input$baseline_time)
      baseline_intensity_input <- input$baseline_intensity_input
      protocol_1_start_input <- input$protocol_1_start_input
      protocol_1_increase_input <- input$protocol_1_increase_input
      protocol_1_length_input <- convert_to_s(input$protocol_1_length_input)
      has_protocol_2 <- input$has_protocol_2
      when_protocol_2 <- convert_to_s(input$when_protocol_2)
      protocol_2_start_input <- input$protocol_2_start_input
      protocol_2_increase_input <- input$protocol_2_increase_input
      protocol_2_length_input <- convert_to_s(input$protocol_2_length_input)
      has_break <- input$has_break
      break_length_input <- convert_to_s(input$break_length_input)
      break_speed_input <- input$break_speed_input
      avg_interval <- input$avg_interval
      include_last_row_input <- input$include_last_row_input
      cols_of_interest <- input$vars_of_interest
      end_time_input <- convert_to_s(input$end_time_input)
      
      incProgress(0.15, detail = "Processing and analyzing data...")
      
      # create parvo_data list with 2 tibbles (parvo_metadata and parvo_eg_data) and add some variables of interest
      parvo_data <-
        parvo_process(
          parvo_data_raw = file_of_interest
        ) %>%
        print()
      
      # Remove all rows that exceed the end of the test
      parvo_data$parvo_eg_data <-
        remove_post_test_rows(
          data = parvo_data$parvo_eg_data,
          input_time = end_time_input,
          use_baseline = has_baseline,
          baseline_length = baseline_time
        ) %>%
        print()
      
      # make time easier to work with by creating new column rounding time to averaging intervals
      parvo_data$parvo_eg_data <-
        replace_with_closest_multiple(
          data = parvo_data$parvo_eg_data
        ) %>%
        print()
      
      # add "Speed", "Protocol", "Stage" columns
      parvo_data$parvo_eg_data <-
        assign_intensity(
          data = parvo_data$parvo_eg_data,
          use_baseline = has_baseline,
          baseline_length = baseline_time,
          baseline_intensity = baseline_intensity_input,
          protocol_1_start = protocol_1_start_input,
          protocol_1_increase = protocol_1_increase_input,
          protocol_1_length = protocol_1_length_input,
          use_protocol_2 = has_protocol_2,
          time_of_change = when_protocol_2,
          protocol_2_start = protocol_2_start_input,
          protocol_2_increase = protocol_2_increase_input,
          protocol_2_length = protocol_2_length_input,
          use_break = has_break,
          break_length = break_length_input,
          break_speed = break_speed_input
        ) %>%
        print()
      
      # assign "Grade/Cadence", "Lactate", and "RPE" data columns from Shiny manually inputted table
      parvo_data$parvo_eg_data <-
        assign_written_info(
          data = parvo_data$parvo_eg_data,
          stage_info = written_info,
          exercise_mode = modality
        ) %>%
        print()
      
      incProgress(0.35, detail = "Creating avg stage data df ...")
      
      # create the average stage data file, while ignoring the columns listed
      avg_stage_data <-
        process_stage_avgs(
          data = parvo_data$parvo_eg_data,
          include_last_row = include_last_row_input,
          averaging_interval = avg_interval
        ) %>%
        print()
      
      # add running economy and metabolic power variables of interest
      avg_stage_data <-
        add_re_mp(
          data = avg_stage_data,
          athlete_df = parvo_data$parvo_metadata
        ) %>%
        print()
      
      incProgress(0.5, detail = "Creating test summary df ...")
      
      # determine peak values for variables of interest and generate new test_summary df
      test_summary <-
        process_peak_data(
          data = parvo_data$parvo_eg_data,
          meta_data = parvo_data$parvo_metadata,
          averaging_interval = avg_interval,
          cols_interest = cols_of_interest,
          end_time = end_time_input,
          protocol_1_increase = protocol_1_increase_input,
          protocol_1_length = protocol_1_length_input,
          use_break = has_break,
          break_length = break_length_input,
          use_protocol_2 = has_protocol_2,
          protocol_2_increase = protocol_2_increase_input,
          protocol_2_length = protocol_2_length_input,
          use_baseline = has_baseline,
          baseline_length = baseline_time
        ) %>%
        print()
      
      # assign normative info related to VO2max
      test_summary <-
        assign_VO2max_norms(
          athlete_characteristics_df = test_summary,
          exercise_mode = modality
        ) %>%
        print()
      
      # add vVO2max to test_summary
      test_summary <-
        workload_at_vo2max(
          avg_stage_data,
          summary = test_summary
        ) %>%
        print()
      
      # add % HRpeak and VO2peak columns to avg_stage_data
      avg_stage_data <-
        add_peak_percentages(
          avg_stage_data,
          test_summary
        ) %>%
        print()
      
      # add FATmax and MFO 
      test_summary <-
        calculate_fatmax(
          avg_stage_data,
          test_summary
        ) %>%
        print()
      
      incProgress(0.7, detail = "Adjusting to the exercise modality ...")
      
      # adjust parvo_eg_data based on exercise modality
      parvo_data$parvo_eg_data <-
        adjust_to_modality_parvo_eg_data(
          parvo_eg_data = parvo_data$parvo_eg_data,
          test_summary = test_summary,
          exercise_mode = modality,
          power_unit = power_unit
        ) %>%
        print()
      
      # adjust avg_stage_data based on exercise modality
      avg_stage_data <-
        adjust_to_modality_avg_stage_data(
          avg_stage_data = avg_stage_data,
          test_summary = test_summary,
          exercise_mode = modality,
          power_unit = power_unit
        ) %>%
        print()
      
      # adjust test_summary based on exercise modality
      test_summary <-
        adjust_to_modality_test_summary(
          test_summary = test_summary,
          exercise_mode = modality,
          power_unit = power_unit
        ) %>%
        print()
      
      # add exercise thresholds from the Shiny manually inputted table
      test_summary <-
        add_threshold_zone(
          threshold_zone_info = threshold_info,
          test_summary = test_summary
        ) %>%
        print()
      
      # add exercise zones from the Shiny manually inputted table
      test_summary <-
        add_threshold_zone(
          threshold_zone_info = zone_info,
          test_summary = test_summary
        ) %>%
        print()
      
      # Delete the rounded 'Time' column and mutate a minute decimal time column in the avg_stage_data df
      avg_stage_data <- 
        fix_time(
          avg_stage_data
        ) %>%
        print()
      
      # Delete the rounded 'Time' column and mutate a minute decimal time column in the parvo_eg_data df
      parvo_data$parvo_eg_data <-
        fix_time(
          parvo_data$parvo_eg_data
        ) %>%
        print()
      
      # Delete any columns consisting entirely of NA values. Used to remove lactate, RPE, thresholds, or zones if not inputted
      avg_stage_data <-
        remove_empty_columns(
          avg_stage_data
        ) %>%
        print()
      
      # Delete any columns consisting entirely of NA values. Used to remove lactate, RPE, thresholds, or zones if not inputted
      parvo_data$parvo_eg_data <-
        remove_empty_columns(
          parvo_data$parvo_eg_data
        ) %>%
        print()
      
      incProgress(0.9, detail = "Making tables pretty ...")
      
      # Render the rhandsontable for test_summary
      output$test_summary_table <- renderRHandsontable({
        rhandsontable(test_summary, rowHeaders = NULL) %>%
          hot_col(col = "Test_Date", dateFormat = "YYYY/MM/DD", type = "date")
      })
      
      # Excel download function for test_summary
      output$download_step_test_summary_data_excel <- downloadHandler(
        filename = function() {
          paste("test_summary.xlsx")
        },
        content = function(file) {
          writexl::write_xlsx(reactive_test_summary(), path = file, col_names = TRUE, format_headers = FALSE)
        }
      )
      
      # CSV download function for test_summary
      output$download_step_test_summary_data_csv <- downloadHandler(
        filename = function() {
          paste("test_summary.csv")
        },
        content = function(file) {
          readr::write_csv(reactive_test_summary(), file, col_names = TRUE)
        }
      )
      
      # Render the rhandsontable for avg_stage_data
      output$avg_stage_data_table <- renderRHandsontable({
        rhandsontable(avg_stage_data, rowHeaders = NULL)
      }
      )
      
      # Excel download function for avg_stage_data
      output$download_step_avg_stage_data_excel <- downloadHandler(
        filename = function() {
          paste("avg_stage_data.xlsx")
        },
        content = function(file) {
          writexl::write_xlsx(reactive_avg_stage_data(), path = file, col_names = TRUE, format_headers = FALSE)
        }
      )
      
      # CSV download function for avg_stage_data
      output$download_step_avg_stage_data_csv <- downloadHandler(
        filename = function() {
          paste("avg_stage_data.csv")
        },
        content = function(file) {
          readr::write_csv(reactive_avg_stage_data(), file, col_names = TRUE)
        }
      )
      
      # Render the rhandsontable for parvo_eg_data
      output$parvo_eg_data_table <- renderRHandsontable({
        rhandsontable(parvo_data$parvo_eg_data, rowHeaders = NULL)
      }
      )
      
      # Excel download function for parvo_eg_data
      output$download_step_parvo_eg_data_excel <- downloadHandler(
        filename = function() {
          paste("parvo_eg_data.xlsx")
        },
        content = function(file) {
          writexl::write_xlsx(reactive_parvo_eg_data(), path = file, col_names = TRUE, format_headers = FALSE)
        }
      )
      
      # CSV download function for parvo_eg_data
      output$download_step_parvo_eg_data_csv <- downloadHandler(
        filename = function() {
          paste("parvo_eg_data.csv")
        },
        content = function(file) {
          readr::write_csv(reactive_parvo_eg_data(), file, col_names = TRUE)
        }
      )
      
      # Updates the x- and y-variables of the plots
      reactive_avg_stage_data_graphing(avg_stage_data)
      
      output$avg_stage_x_var_ui <- renderUI({
        req(reactive_avg_stage_data_graphing())
        selectInput("avg_stage_x_var", "Avg Stage X-axis variable:", choices = names(reactive_avg_stage_data_graphing()), selected = "Time_m")
      })
      
      output$avg_stage_y_var_ui <- renderUI({
        req(reactive_avg_stage_data_graphing())
        selectInput("avg_stage_y_var", "Avg Stage Y-axis variable:", choices = names(reactive_avg_stage_data_graphing()), selected = "VO2")
      })
      
      reactive_parvo_data_graphing(parvo_data)
      
      output$parvo_data_x_var_ui <- renderUI({
        req(reactive_parvo_data_graphing())
        selectInput("parvo_data_x_var", "Parvo Data X-axis variable:", choices = names(reactive_parvo_data_graphing()$parvo_eg_data), selected = "Time_m")
      })
      
      output$parvo_data_y_var_ui <- renderUI({
        req(reactive_parvo_data_graphing())
        selectInput("parvo_data_y_var", "Parvo Data Y-axis variable:", choices = names(reactive_parvo_data_graphing()$parvo_eg_data), selected = "VO2")
      })
      
      # Sweet Alert processing completion indicator
      show_alert(
        title = "Success!",
        text = "Processing Complete",
        type = "success",
        btn_labels = "Ok"
      )
      
    })
  })
  
  # Code to render plots
  output$avg_stage_plot <- renderPlotly({
    req(reactive_avg_stage_data_graphing()) 
    
    plot_ly(data = reactive_avg_stage_data_graphing(), x = ~get(input$avg_stage_x_var), y = ~get(input$avg_stage_y_var), 
            type = 'scatter', mode = 'markers') %>%
      layout(title = 'Avg Stage Data Interactive Plot', 
             xaxis = list(title = input$avg_stage_x_var), yaxis = list(title = input$avg_stage_y_var))
  })
  
  output$parvo_eg_plot <- renderPlotly({
    req(reactive_parvo_data_graphing()) 
    parvo_eg_data <- reactive_parvo_data_graphing()$parvo_eg_data
    
    plot_ly(data = parvo_eg_data, x = ~get(input$parvo_data_x_var), y = ~get(input$parvo_data_y_var), 
            type = 'scatter', mode = 'markers') %>%
      layout(title = 'Parvo Data Interactive Plot', 
             xaxis = list(title = input$parvo_data_x_var), yaxis = list(title = input$parvo_data_y_var))
  })
  
}

### RUN THE SHINY APP ========================================

shinyApp(ui = ui, server = server)
