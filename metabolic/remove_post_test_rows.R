# Remove all rows that exceed the end of the test
remove_post_test_rows <- function(data,input_time,use_baseline,baseline_length) {
  
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
