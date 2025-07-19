ui <- fluidPage(
  titlePanel("Throughput Histogram & MT-ID Function"),
  sidebarLayout(
    sidebarPanel(
      selectInput("group_select", "Select Group (e.g., G00):",
                  choices = unique(files_df$group)),
      uiOutput("participant_select")
    ),
    mainPanel(
      plotOutput("tpPlot", height= "200px"),
      textOutput("avgText"),
      hr(),
      h4("MT vs. ID Regression"),
      plotOutput("regressionPlot", height = "200px", width="320px"),
      textOutput("regressionFormula")  # add this below the plot
      
    )
  )
)
