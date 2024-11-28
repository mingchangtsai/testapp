### SETUP ====================================================

# Reading libraries
suppressPackageStartupMessages({
  library(pacman)
  p_load(shiny,shinydashboard,shinyWidgets,readxl,writexl,fuzzyjoin,tidyverse,rhandsontable,SimplyAgree,zoo,plotly,fresh,here)
})

# Setting global options 
options(digits = 5, digits.secs = 3, scipen = 3,
        dplyr.summarise.inform = FALSE)

# load functions
source(here("metabolic","all_functions.R"))




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
