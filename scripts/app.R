library(shiny)
library(tidyverse)

# ---- GLOBAL SECTION ----
data_path <- "../"

# Helper functions
extract_participant <- function(filename) {
  str_match(basename(filename), "-(P\\d+)-")[, 2]
}
extract_group <- function(filename) {
  str_match(basename(filename), "-(G\\d+)-")[, 2]
}

# File list
sd2_files <- list.files(path = data_path, pattern = "\\.sd2$", full.names = TRUE)

# Only process if files exist
if (length(sd2_files) > 0) {
  files_df <- tibble(
    path = sd2_files,
    filename = basename(sd2_files),
    group = extract_group(sd2_files),
    participant = extract_participant(sd2_files)
  )
  
  all_data <- map_dfr(files_df$path, function(file) {
    read_csv(file, show_col_types = FALSE) %>%
      mutate(
        TaskType = as.character(TaskType),  # force to character
        File = basename(file),
        Group = extract_group(file),
        Participant = extract_participant(file)
      )
  })
  
  
  file_tp_means <- all_data %>%
    group_by(Group, File, Participant) %>%
    summarise(mean_tp = mean(`TP(bps)`, na.rm = TRUE), .groups = "drop")
} else {
  files_df <- tibble(path = character(), filename = character(), group = character(), participant = character())
  all_data <- tibble()
  file_tp_means <- tibble()
}

# ---- UI SECTION ----
ui <- fluidPage(
  titlePanel("TP(bps) Histogram by Group and Participant"),
  sidebarLayout(
    sidebarPanel(
      selectInput("group_select", "Select Group (e.g., G00):", choices = unique(files_df$group)),
      uiOutput("participant_select")
    ),
    mainPanel(
      plotOutput("tpPlot"),
      textOutput("avgText")
    )
  )
)

# ---- SERVER SECTION ----
server <- function(input, output, session) {
  
  # Filter participants by selected group
  filtered_participants <- reactive({
    files_df %>%
      filter(group == input$group_select) %>%
      arrange(participant) %>%
      distinct(participant, filename) %>%
      deframe()  # named vector: participant -> filename
  })
  
  # Dynamically update participant dropdown
  output$participant_select <- renderUI({
    selectInput("participant_select", "Select Participant (Pxx):", choices = filtered_participants())
  })
  
  # Subset data by selected group
  group_data <- reactive({
    all_data %>%
      filter(Group == input$group_select)
  })
  
  output$tpPlot <- renderPlot({
    selected_file <- input$participant_select
    if (is.null(selected_file) || nrow(group_data()) == 0) return()
    
    selected_mean <- file_tp_means %>%
      filter(File == selected_file) %>%
      pull(mean_tp)
    
    ggplot(group_data(), aes(x = `TP(bps)`)) +
      geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
      geom_vline(xintercept = selected_mean, linetype = "dashed", color = "red", size = 1) +
      labs(
        title = paste("TP(bps) in", input$group_select, "— Mean for", selected_file),
        x = "TP (bps)",
        y = "Frequency"
      ) +
      theme_minimal()
  })
  
  output$avgText <- renderText({
    selected_file <- input$participant_select
    if (is.null(selected_file)) return("No file selected.")
    selected_mean <- file_tp_means %>%
      filter(File == selected_file) %>%
      pull(mean_tp)
    
    paste("Average TP(bps) for", selected_file, ":", round(selected_mean, 3))
  })
}

# ---- RUN APP ----
shinyApp(ui = ui, server = server)
