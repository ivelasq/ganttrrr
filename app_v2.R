#########################
# Creating ganttrrr app #
#########################

# resources:

# https://stackoverflow.com/questions/22272571/data-input-via-shinytable-in-r-shiny-application
# https://rdrr.io/cran/DiagrammeR/man/grVizOutput.html
# https://shiny.rstudio.com/articles/action-buttons.html

# libraries ---------------------------------------------------------------

library(tidyverse)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(rhandsontable)
library(htmlwidgets)
library(DiagrammeR)
library(glue)
library(here)

# app ---------------------------------------------------------------------

ui <- fluidPage(
  
  # css 
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  # header panel
  
  titlePanel(
    fluidRow(
      column(3, 
             h1("ganttrrr")), 
      column(9, 
             h2("A Shiny App for Creating Gantt Charts using DiagrammeR::mermaid"))
    )
  ),
  
  # sidebar layout
  
  sidebarLayout(
    sidebarPanel(
      helpText("Right-click on the table to delete/insert rows.", 
               tags$br(),
               "Double-click on a cell to edit.",
               tags$br(),
               "Once edited, save table and create chart.",
               tags$br(),
               tags$br(),
               actionButton("load", "Load Example")),
      wellPanel(
        h4("Save & Create Chart"), 
        actionButton("save", "Save Table"),
        tags$br(),
        tags$br(),
        actionButton("create", "Create Chart"),
        tags$br(),
        tags$br(),
        downloadButton("export", "Export PDF")
      )        
      
    ), # sidebarPanel
    
    mainPanel(rHandsontableOutput("hot"),
              DiagrammeROutput("gantt_render"))
    
  ) # sidebarLayout
) # fluid page

server <- function(input, output) {
  
  
  
}