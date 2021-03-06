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

values <- list()
setHot <- function(x) 
  values[["hot"]] <<- x 

df <-
  data.frame(
    Category = c(rep(NA_character_, 7)),
    Task =  c(rep(NA_character_, 7)),
    Status = c(rep(NA_character_, 7)),
    Critical = c(rep(FALSE, 7)),
    Start = c(rep(NA_character_, 7)),
    Duration = c(rep(NA_integer_, 7)),
    stringsAsFactors = FALSE
  )

ui <- fluidPage(
  
  # css 
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")), 
  
  # header panel
  
  titlePanel(windowTitle = "ganttrrrrrrrrrrr",
             fluidRow(column(3,
                             h1("ganttrrr")),
                      column(
                        8,
                        h2(
                          "A Shiny App for Creating Gantt Charts Using DiagrammeR::mermaid()"
                        )
                      ),
                      column(1,
                             ( tags$a(
                                 img(src = "Download Code.png",
                                     align = "right",
                                     style = "width:150px;height:150px;"),
                                 href = "https://raw.githubusercontent.com/ivelasq/ganttrrr/master/code/agenda_gantt.R"
                                 )
                             ),
                             )
                      ) # end Fluid Row
             ), # end Title Panel 
  
  # sidebar layout
  
    sidebarLayout(
      sidebarPanel(
        helpText("Right-click on the table to delete/insert rows.", 
                 tags$br(),
                 "Double-click on a cell to edit.",
                 tags$br(),
                 "Once edited, save table and create chart."
                 ),
        wellPanel(
          actionButton("load1", "Load Example 1: Creating an R Package"),
          actionButton("load2", "Load Example 2: rstudio::conf(2020) Agenda"),
          actionButton("save", "Save Table & Create Chart"),
          actionButton("clear", "Clear Table")
          # tags$br(),
          # tags$br(),
          # downloadButton("export", "Export PDF")
        )        
        
      ), # sidebarPanel
      
      mainPanel(rHandsontableOutput("hot"),
                DiagrammeROutput("gantt_render"))
      
    ) # sidebarLayout
  ) # fluid page
  
  
server <- function(input, output) {
  
  # Load Example 1
  
  observeEvent(input$load1, {

    df <- readRDS(here::here("data", "example1.rds"))
    
    output$hot <- renderRHandsontable({
      rhandsontable(df, stretchH = "all") %>%
        hot_col(
          col = "Status",
          type = "dropdown",
          source = c("To Do", "In Progress", "Done")
        ) %>%
        hot_col(col = "Critical", halign = "htCenter") %>%
        hot_col(col = "Start",
                type = "date",
                dateFormat = "YYYY-MM-DD") %>%
        hot_context_menu(customOpts = list(csv = list(
          name = "Download to CSV",
          callback = htmlwidgets::JS(
            "function (key, options) {
                         var csv = csvString(this);
                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                           encodeURIComponent(csv));
                         link.setAttribute('download', 'data.csv');
                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }"
          )
        )))
    })
  })
  
  # Load Example 2
  
  observeEvent(input$load2, {
    
    df <- readRDS(here::here("data", "example2.rds"))
    
    output$hot <- renderRHandsontable({
      rhandsontable(df, stretchH = "all") %>%
        hot_col(
          col = "Status",
          type = "dropdown",
          source = c("To Do", "In Progress", "Done")
        ) %>%
        hot_col(col = "Critical", halign = "htCenter") %>%
        hot_col(col = "Start",
                type = "date",
                dateFormat = "YYYY-MM-DD") %>%
        hot_context_menu(customOpts = list(csv = list(
          name = "Download to CSV",
          callback = htmlwidgets::JS(
            "function (key, options) {
                         var csv = csvString(this);
                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                           encodeURIComponent(csv));
                         link.setAttribute('download', 'data.csv');
                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }"
          )
        )))
    })
  })
  
  # Clear Table
  
  observeEvent(input$clear, {
    
    df <-
      data.frame(
        Category = c(rep(NA_character_, 7)),
        Task =  c(rep(NA_character_, 7)),
        Status = c(rep(NA_character_, 7)),
        Critical = c(rep(FALSE, 7)),
        Start = c(rep(NA_character_, 7)),
        Duration = c(rep(NA_integer_, 7)),
        stringsAsFactors = FALSE
      )
    
    output$hot <- renderRHandsontable({
      rhandsontable(df, stretchH = "all") %>%
        hot_col(
          col = "Status",
          type = "dropdown",
          source = c("To Do", "In Progress", "Done")
        ) %>%
        hot_col(col = "Critical", halign = "htCenter") %>%
        hot_col(col = "Start",
                type = "date",
                dateFormat = "YYYY-MM-DD") %>%
        hot_context_menu(customOpts = list(csv = list(
          name = "Download to CSV",
          callback = htmlwidgets::JS(
            "function (key, options) {
                         var csv = csvString(this);
                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                           encodeURIComponent(csv));
                         link.setAttribute('download', 'data.csv');
                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }"
          )
        )))
    })
  })
  
  ## Handsontable
    
  observe({
    if (!is.null(input$hot)) {
      df <- (hot_to_r(input$hot))
      setHot(df)
    }
  })
  
  output$hot <- renderRHandsontable({
    rhandsontable(df, stretchH = "all") %>%
      hot_col(
        col = "Status",
        type = "dropdown",
        source = c("To Do", "In Progress", "Done")
      ) %>%
      hot_col(col = "Critical", halign = "htCenter") %>%
      hot_col(col = "Start",
              type = "date",
              dateFormat = "YYYY-MM-DD") %>%
      hot_context_menu(customOpts = list(csv = list(
        name = "Download to CSV",
        callback = htmlwidgets::JS(
          "function (key, options) {
                         var csv = csvString(this);
                         var link = document.createElement('a');
                         link.setAttribute('href', 'data:text/plain;charset=utf-8,' +
                           encodeURIComponent(csv));
                         link.setAttribute('download', 'data.csv');
                         document.body.appendChild(link);
                         link.click();
                         document.body.removeChild(link);
                       }"
        )
      )))
  })
    
    diagram <- 
      eventReactive(input$save, {
        
        if (!is.null(values[["hot"]])) { # if there's a table input
          df <- values$hot
        }
        
        # make table mermaid-friendly
        
        df <-
          df %>% 
          data.frame %>% 
          mutate(status = case_when(Status == "To Do" & Critical == TRUE ~ "crit",
                                    Status == "To Do" & Critical == FALSE ~ "",
                                    Status == "In Progress" & Critical == TRUE ~ "active, crit",
                                    Status == "In Progress" & Critical == FALSE ~ "active",
                                    Status == "Done" & Critical == TRUE ~ "done, crit",
                                    Status == "Done" & Critical == FALSE ~ "done"
          ),
          start = as.Date(Start, "%Y-%m-%d"),
          end = paste0(Duration, "d")) %>% 
          select(-Status, -Critical, -Start, -Duration) %>% 
          rename(task = Task,
                 pos = Category)
        
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
    
    output$gantt_render <- renderDiagrammeR({
      req(diagram())
      diagram()
    })
    
    # output$export = downloadHandler(
    #   filename = function() {"gantt_chart.pdf"},
    #   content = function(file) {
    #     pdf(file, onefile = TRUE)
    #     values$gantt %>%
    #       htmltools::html_print() %>%
    #       webshot::webshot(file = "gantt_chart.pdf") 
    #     dev.off()
    #   }
    # )
    
#     output$Save_diagrammeR_plot <- downloadHandler(
#       filename = "gantt_chart.html",
#       content = function(file) {
#         save_html(renderDiagrammeR(req(diagram())))
#       }
# )
}

  
  ## run app 
shinyApp(ui = ui, server = server)
