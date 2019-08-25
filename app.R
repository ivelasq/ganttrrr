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

# read the most recent gantt data file
df <- file.info(list.files(here::here("data"), full.names = T))
df <- read_rds(rownames(df)[which.max(df$mtime)])

ui <- fluidPage(
    
    titlePanel("ganttrrr"),
    sidebarLayout(
      sidebarPanel(
        helpText("Shiny App for Creating Gantt Charts",
                 tags$br(),
                 tags$br(),
                 "Right-click on the table to delete/insert rows.", 
                 tags$br(),
                 "Double-click on a cell to edit."),
        
        #wellPanel(
          #h3("Table options"),
          #radioButtons("useType", "Use Data Types", c("TRUE", "FALSE"))
        #),
        #br(), 
        
        wellPanel(
          h3("Save & Create Chart"), 
          actionButton("save", "Save Table"),
          actionButton("create", "Create Chart")
        )        
        
      ),
      
      mainPanel(
        
        rHandsontableOutput("hot"),
        DiagrammeROutput("gantt")
        
    )
  ))
  
  
server <- function(input, output) {
    
    values <- reactiveValues(df = NULL)
    
    ## Handsontable
    observe({
      if (!is.null(input$hot)) {
        df = hot_to_r(input$hot)
      } else {
        if (is.null(values[["df"]]))
          df <- df
        else
          df <- values[["df"]]
      }
      values[["df"]] <- df
    })
    
    output$hot <- renderRHandsontable({
      df <- values[["df"]]
      if (!is.null(df))
        rhandsontable(df, stretchH = "all")
    })
    
    ## Save 
    observeEvent(input$save, {
      finaldf <- isolate(values[["df"]])
      saveRDS(finaldf, file = file.path(here::here("data", sprintf("%s.rds", Sys.Date()))))
    })

    
    ## Create Chart
    diagram <- 
      eventReactive(input$create, {
        df <- readRDS(here::here("data", sprintf("%s.rds", Sys.Date()))) %>% data.frame
        one <- df %>% filter(pos %in% str_subset(df$pos, "^one")) # Category 1
        two <- df %>% filter(pos %in% str_subset(df$pos, "^two")) # Category 2
        thr <- df %>% filter(pos %in% str_subset(df$pos, "^thr")) # Category 3
        fou <- df %>% filter(pos %in% str_subset(df$pos, "^fou")) # Category 4
        fiv <- df %>% filter(pos %in% str_subset(df$pos, "^fiv")) # Category 5
        six <- df %>% filter(pos %in% str_subset(df$pos, "^six")) # Category 6
        sev <- df %>% filter(pos %in% str_subset(df$pos, "^sev")) # Category 7
        
        gantt <-
          DiagrammeR::mermaid(
            paste0(
              "gantt", "\n",
              "dateFormat YYYY-MM-DD", "\n",
              "section Category 1", "\n",
              paste(one %>%
                      unite(i, task, status, sep = ":") %>%
                      unite(j, i, pos, start, end, sep = ",") %>%
                      .$j,
                    collapse = "\n"
              ), "\n",
              "section Category 2", "\n",
              paste(two %>%
                      unite(i, task, status, sep = ":") %>%
                      unite(j, i, pos, start, end, sep = ",") %>%
                      .$j,
                    collapse = "\n"
              ), "\n",
              "section Category 3", "\n",
              paste(thr %>%
                      unite(i, task, status, sep = ":") %>%
                      unite(j, i, pos, start, end, sep = ",") %>%
                      .$j,
                    collapse = "\n"
              ), "\n",
              "section Category 4", "\n",
              paste(fou %>%
                      unite(i, task, status, sep = ":") %>%
                      unite(j, i, pos, start, end, sep = ",") %>%
                      .$j,
                    collapse = "\n"
              ), "\n",
              "section Category 5", "\n",
              paste(fiv %>%
                      unite(i, task, status, sep = ":") %>%
                      unite(j, i, pos, start, end, sep = ",") %>%
                      .$j,
                    collapse = "\n"
              ), "\n",
              "section Category 6", "\n",
              paste(six %>%
                      unite(i, task, status, sep = ":") %>%
                      unite(j, i, pos, start, end, sep = ",") %>%
                      .$j,
                    collapse = "\n"
              ), "\n",
              "section Category 7", "\n",
              paste(sev %>%
                      unite(i, task, status, sep = ":") %>%
                      unite(j, i, pos, start, end, sep = ",") %>%
                      .$j,
                    collapse = "\n"
              ), "\n"
            ), width = 1000
          )
        gantt$x$config = list(ganttConfig = list(
          axisFormatter = list(list(
            "%d%b%y"
            ,htmlwidgets::JS(
              'function(d){ return d.getDay() == 1 }'
            )
          ))
        ))
        gantt
    })
    
    output$gantt <- renderDiagrammeR({
      req(diagram())
      diagram()
    })
    
}

  
  ## run app 
shinyApp(ui = ui, server = server)
