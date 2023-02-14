require(shiny)


comp_ui <- fluidPage( 
  sidebarLayout(
    sidebarPanel(
      selectInput('comparison_cat','Choose Comparison Category',
                  choices = c('Program Type', 'College', "Program"),
                  multiple = FALSE),
      uiOutput('main_comparison')
    ),
    mainPanel(
      plotOutput('comparison_plot'),
      tableOutput('table')
    )
  )
)

comp_server <- function(input, output, session) {
  require(ggplot2)
  require(ggpubr)
  require(ggusc)
  require(ggpubr)
  require(ggtext)
  # Build data, would be replaced by the csv loading in your case
  stipend_data <- readRDS('./data/aggregated_data.rds')
  full_range_data <- read.table('./data/full_phd_data.tsv', header = T,
                                na.strings = '<NA>')
  
  comp_cat <- reactive({
    if(input$comparison_cat == 'Program Type') {
      'program_type'
    } else {
      input$comparison_cat
    }
  })
  
  # Render selectInput 
  output$main_comparison <- renderUI({

    poss_comp_selections <- as.vector(unique(stipend_data$usc[[comp_cat()]]))
    selectInput('selected_comp','Select Comparison Group', 
                choices = poss_comp_selections, multiple = TRUE)
  })
  
  comparison_data <- reactive({
    stipend_data$comparison[stipend_data$comparison[[comp_cat()]] %in% input$selected_comp, ]
  })
  
  usc_data <- reactive({
    stipend_data$usc[stipend_data$usc[[comp_cat()]] %in% input$selected_comp, ]
  })
  
  output$comparison_plot <- renderPlot({
    
    ggplot() +
      geom_point(data = comparison_data(),
                 aes(x = living, y = stipend),
                 color = 'grey', alpha = 0.5) +
      geom_point(data = usc_data(),
                 aes(x = 35609.6, y = mean),
                 color = usc_cols(1)) +
      stat_smooth(data = comparison_data(),
                  aes(x = living, y = stipend),
                  color = 'black', method = 'lm', se = F) +
      stat_smooth(data = full_range_data,
                  aes(x = living, y = living),
                  method = 'lm', fullrange = T, se = F,
                  color = 'green')+
      stat_smooth(data = full_range_data,
                  aes(x = living, y = poverty),
                  fullrange = T,
                  color = 'red', method = 'lm', se = F)+
      labs(x = 'MIT Living Wage [$]', y = 'Graduate Annual Stipend [$]',
           title = paste0('Graduate Stipend Comparison for ', 
                          input$selected_comp, ' Students'),
           subtitle = "Comparison universities are shown in grey. 
           USC stipends are shown in <span style='color:#73000a;'>**garnet**</span>")+
      theme_pubr() +
      theme(plot.subtitle = element_markdown())
  })
  
}

compStipendApp <- function() {
  shinyApp(ui = comp_ui, server = comp_server)
}
