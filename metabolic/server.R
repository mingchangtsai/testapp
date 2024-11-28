
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
