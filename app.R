# resources:
# https://stackoverflow.com/questions/22272571/data-input-via-shinytable-in-r-shiny-application
# libraries ---------------------------------------------------------------

library(tidyverse)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(rhandsontable)

(DF <- data.frame(stringsAsFactors=FALSE,
                 task = c("Task1", "Task2", "Task3", "Task4", "Task5", "Task6", "Task7"),
                 status = c("active, crit", "active, crit", "crit", "active", "crit", "crit", "active, crit"),
                 pos = c("one", "two", "two", "thr", "thr1", "thr2", "thr3"),
                 start = c("2019-04-05", "after one", "after one", "after two", "after thr", "after thr1", "after thr2"),
                 end = c("1d", "7d", "3d", "4d", "3d", "2d", "1d")
))

editTable <- function(DF, outdir=getwd(), outfilename="table"){
  ui <- shinyUI(fluidPage(
    
    titlePanel("Edit and save a table"),
    sidebarLayout(
      sidebarPanel(
        helpText("Shiny app based on an example given in the rhandsontable package.", 
                 "Right-click on the table to delete/insert rows.", 
                 "Double-click on a cell to edit"),
        
        wellPanel(
          h3("Table options"),
          radioButtons("useType", "Use Data Types", c("TRUE", "FALSE"))
        ),
        br(), 
        
        wellPanel(
          h3("Save"), 
          actionButton("save", "Save table")
        )        
        
      ),
      
      mainPanel(
        
        rHandsontableOutput("hot")
        
      )
    )
  ))
  
  server <- shinyServer(function(input, output) {
    
    values <- reactiveValues()
    
    ## Handsontable
    observe({
      if (!is.null(input$hot)) {
        DF = hot_to_r(input$hot)
      } else {
        if (is.null(values[["DF"]]))
          DF <- DF
        else
          DF <- values[["DF"]]
      }
      values[["DF"]] <- DF
    })
    
    output$hot <- renderRHandsontable({
      DF <- values[["DF"]]
      if (!is.null(DF))
        rhandsontable(DF, useTypes = as.logical(input$useType), stretchH = "all")
    })
    
    ## Save 
    observeEvent(input$save, {
      finalDF <- isolate(values[["DF"]])
      saveRDS(finalDF, file=file.path(outdir, sprintf("%s.rds", outfilename)))
    })
    
  })
  
  ## run app 
  runApp(list(ui = ui, server = server))
  return(invisible())
}

editTable(DF)
