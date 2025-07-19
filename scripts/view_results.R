# Load required libraries
library(shiny)
library(tidyverse)

# Set your data folder
data_path <- "../"  # ← Replace with actual path

# Read all .sd2 files
sd2_files <- list.files(path = data_path, pattern = "\\.sd2$", full.names = TRUE)
names(sd2_files) <- basename(sd2_files)

# Prepare a combined dataset and per-file summaries
all_data <- map_dfr(sd2_files, function(file) {
  read_csv(file, show_col_types = FALSE) %>%
    mutate(File = basename(file))
})

file_tp_means <- all_data %>%
  group_by(File) %>%
  summarise(mean_tp = mean(`TP(bps)`, na.rm = TRUE), .groups = "drop")

# --- SHINY APP ---
ui <- fluidPage(
  titlePanel("TP(bps) Histogram from SD2 Files"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_file", "Select a file to highlight average TP(bps):",
                  choices = names(sd2_files))
    ),
    mainPanel(
      plotOutput("tpPlot"),
      textOutput("avgText")
    )
  )
)

server <- function(input, output) {
  output$tpPlot <- renderPlot({
    selected_mean <- file_tp_means %>%
      filter(File == input$selected_file) %>%
      pull(mean_tp)
    
    ggplot(all_data, aes(x = `TP(bps)`)) +
      geom_histogram(binwidth = 0.5, fill = "lightblue", color = "black") +
      geom_vline(xintercept = selected_mean, color = "red", linetype = "dashed", size = 1) +
      labs(
        title = paste("Histogram of TP(bps) with Mean for", input$selected_file),
        x = "TP (bps)",
        y = "Frequency"
      ) +
      theme_minimal()
  })
  
  output$avgText <- renderText({
    selected_mean <- file_tp_means %>%
      filter(File == input$selected_file) %>%
      pull(mean_tp)
    paste("Average TP(bps) for", input$selected_file, ":", round(selected_mean, 3))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
