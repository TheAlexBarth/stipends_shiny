library(shiny)

source('./R/stipendCompApp.R')

shinyApp(ui = comp_ui, server = comp_server)
