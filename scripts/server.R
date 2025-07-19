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
      geom_histogram(binwidth = 0.25, fill = "skyblue", color = "black") +
      geom_vline(xintercept = selected_mean, linetype = "dashed", color = "red", size = 1) +
      annotate("segment",
               x = selected_mean + 0.3, xend = selected_mean+0.01,
               y = -1, yend = -0.3,
               arrow = arrow(length = unit(0.3, "cm")),
               color = "red") +
      annotate("text",
               x = selected_mean+0.65,
               y = -2,
               label = paste0(round(selected_mean, 2), " bps"),
               vjust = -0.5,
               hjust = 0.5,
               color = "red",
               size = 8) +
      labs(
        title = paste("TP(bps) in", input$group_select),
        x = "TP (bps)",
        y = "Frequency"
      ) +
      theme_minimal()
  })
  
  output$avgText <- renderText({
    selected_file <- input$participant_select
    if (is.null(selected_file)) return("No file selected.")
    
    entry <- file_tp_means %>%
      filter(File == selected_file)
    
    if (nrow(entry) == 0) return("No data found.")
    
    participant <- entry$Participant
    group <- entry$Group
    tp <- round(entry$mean_tp, 2)
    
    paste0("Throughput for ", participant, " of ", group, " is ", tp, " Bit/s")
  })
  
  
  output$regressionPlot_sd1 <- renderPlot({
    req(nrow(sd1_data) > 1)
    selected_file <- input$participant_select
    req(!is.null(selected_file))
    
    # Get participant ID (e.g., "P00") from the selected .sd2 file name
    selected_participant <- files_df %>%
      filter(filename == selected_file) %>%
      pull(participant)
    
    req(length(selected_participant) == 1)
    
    # Filter sd1_data by participant instead of file
    df <- sd1_data %>%
      filter(Participant == selected_participant) %>%
      filter(!is.na(A), !is.na(W), W > 0) %>%
      mutate(ID = log2((A / W) + 1))
    
    req(nrow(df) > 1)
    print(dim(df))
    ggplot(df, aes(x = ID, y = `MT(ms)`)) +
      geom_point(alpha = 0.6) +
      geom_smooth(method = "lm", color = "blue", se = TRUE) +
      scale_x_continuous(limits = c(2, 7)) +
      scale_y_continuous(limits = c(200, 1500)) +
      labs(
        #title = paste("MT vs ID Regression for", selected_participant),
        x = "Index of Difficulty (ID = log2(A / W + 1))",
        y = "Movement Time (ms)"
      ) +
      theme_minimal()
  })
  
  
  output$regressionPlot <- renderPlot({
    req(nrow(all_data) > 1)
    selected_file <- input$participant_select
    req(!is.null(selected_file))
    
    df <- all_data %>%
      filter(File == selected_file) %>%
      filter(!is.na(`IDe(bits)`), !is.na(`MT(ms)`))
    
    req(nrow(df) > 1)
    
    ggplot(df, aes(x = `IDe(bits)`, y = `MT(ms)`)) +
      geom_point(alpha = 0.6, size=3, color='black') +
      geom_smooth(method = "lm", color = "blue", se = TRUE) +
      labs(
        title = paste("MT vs IDe(bits) Regression for", df$Participant[1]),
        x = "Effective Index of Difficulty (IDe, bits)",
        y = "Movement Time (ms)"
      ) +
      theme_minimal()
  })
  
  
  
  
  # Render the regression formula text
  output$regressionFormula_sd1 <- renderText({
    selected_file <- input$participant_select
    req(!is.null(selected_file))
    
    selected_participant <- files_df %>%
      filter(filename == selected_file) %>%
      pull(participant)
    req(length(selected_participant) == 1)
    
    df <- sd1_data %>%
      filter(Participant == selected_participant) %>%
      filter(!is.na(A), !is.na(W), W > 0) %>%
      mutate(ID = log2((A / W) + 1))
    
    req(nrow(df) > 1)
    
    model <- lm(`MT(ms)` ~ ID, data = df)
    coef <- coefficients(model)
    
    paste0("Regression Formula: MT(ms) = ",
           round(coef[1], 2), " + ",
           round(coef[2], 2), " × ID")
  })
  
  
  output$regressionFormula <- renderText({
    selected_file <- input$participant_select
    req(!is.null(selected_file))
    
    df <- all_data %>%
      filter(File == selected_file) %>%
      filter(!is.na(`IDe(bits)`), !is.na(`MT(ms)`))
    
    req(nrow(df) > 1)
    
    model <- lm(`MT(ms)` ~ `IDe(bits)`, data = df)
    model_summary <- summary(model)
    coef <- coefficients(model)
    r_squared <- model_summary$r.squared
    
    paste0("MT(ms) = ",
           round(coef[1], 2), " + ",
           round(coef[2], 2), " × IDe(bits)",
           "  |  R² = ", round(r_squared, 3))
  })
  
  
}
