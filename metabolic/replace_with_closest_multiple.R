# Round time to make it easier to work with
replace_with_closest_multiple <- function(data) {
  # Creates a new column to "store" the original "Time" values
  data <- data %>%
    mutate(Time_s = Time, .before = Time)
  
  # Replace the values with rounded "Time" values
  rounded_diff <- round(mean(diff(data$Time)))
  data$Time <- round(data$Time / rounded_diff) * rounded_diff
  return(data)
}
