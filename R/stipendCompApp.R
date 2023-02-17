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
      textOutput('comparison_summary'),
      tags$head(tags$style('#comparison_summary{font-size: 20px;
                                                font-style: bold;
                                                text-align: center}'))
    )
  )
)

comp_server <- function(input, output, session) {
  require(ggplot2)
  require(ggpubr)
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
    
    if(length(input$selected_comp) <= 1) {
      title_lab <- paste0('Graduate Stipend Comparison for ', 
                           input$selected_comp, ' Students')
    } else {
      title_lab <- paste0('Graduate Stipend Comparison for ', 
                          paste0(input$selected_comp, collapse = ' & '),
                          ' Students')
    }
    
    
    ggplot() +
      geom_point(data = comparison_data(),
                 aes(x = living, y = stipend),
                 color = 'grey', alpha = 0.5) +
      geom_point(data = usc_data(),
                 aes(x = 35609.6, y = mean),
                 color = '#73000a') +
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
           title = title_lab, # need to change for multiple inputs
           subtitle = "Comparison universities are shown in <span style='color: #999999;'>**grey**</span>.
           <br>
           USC stipends are shown in <span style='color:#73000a;'>**garnet**</span>
           <br>
           The <span style='color: green;'>**green**</span> line indicates the MIT cost of living for a single person
           <br>
           The <span style='color: #FF0000;'>**red**</span> line indicates the poverty line.")+
      theme_pubr(base_size = 15) +
      theme(plot.subtitle = element_markdown())
  })
  
  
  output$comparison_summary <- renderText({
    
    if(length(comparison_data() > 0)) {
      
      mod_selected <- lm(stipend ~ living, data = comparison_data())
      
      predicted_salary <- summary(mod_selected)$coef[2] * 35609.6 + summary(mod_selected)$coef[1]
      
      average_usc <- mean(usc_data()$mean)
      
      if(predicted_salary >= average_usc) {
        paste0('Adjusting for the cost of living, the average USC stipend is $', 
               round(predicted_salary - average_usc, 2),
               ' lower than other universities!')
      } else {
        "This is a rare case where USC stipends exceed the average competitor"
      }
    }
  })
}
