# gantt chart agenda

# setup
library(DiagrammeR)
library(dplyr)
library(glue)
library(here)
library(htmltools)
library(stringr)
library(tidyr)

# locals
here <- here::here()

# input
df <- data.frame(stringsAsFactors=FALSE,
                 task = c("Task1", "Task2", "Task3", "Task4", "Task5", "Task6", "Task7"),
                 status = c("active, crit", "active, crit", "crit", "active", "crit", "crit", "active, crit"),
                 pos = c("one", "two", "two", "thr", "thr1", "thr2", "thr3"),
                 start = c("2019-04-05", "after one", "after one", "after two", "after thr", "after thr1", "after thr2"),
                 end = c("1d", "7d", "3d", "4d", "3d", "2d", "1d")
)

# data management
one <- df %>% filter(pos %in% str_subset(df$pos, "^one")) # Category 1
two <- df %>% filter(pos %in% str_subset(df$pos, "^two")) # Category 2
thr <- df %>% filter(pos %in% str_subset(df$pos, "^thr")) # Category 3
fou <- df %>% filter(pos %in% str_subset(df$pos, "^fou")) # Category 4
fiv <- df %>% filter(pos %in% str_subset(df$pos, "^fiv")) # Category 5
six <- df %>% filter(pos %in% str_subset(df$pos, "^six")) # Category 6
sev <- df %>% filter(pos %in% str_subset(df$pos, "^sev")) # Category 7

# gantt chart
gantt <-
  mermaid(
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
save_html(gantt, file = glue("{here}/agenda_gantt.html"))
