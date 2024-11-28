# Create parvo_data list with 2 df (parvo_metadata and parvo_eg_data)
# Add some variables of interest
parvo_process <- function(
    parvo_data_raw,
    col_names = c(
      "Time", "VO2", "VO2kg", "VCO2",
      "RER", "RR", "VT", "VE", "VEVO2", "VEVCO2",
      "FEO2", "FECO2", "HR")){
  
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
