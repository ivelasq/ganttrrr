# old example
# df <- data.frame(
#   stringsAsFactors = FALSE,
#               Task = c("hfjk", "jksjf", "jksjf", "jksjf", "jksjf", "jksjf"),
#             Status = c("In Progress", "To Do","To Do","In Progress","Done", "Done"),
#           Critical = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
#           Category = c("one", "two", "two", "thr", "thr1", "thr2"),
#              Start = c("2019-02-01", "2019-02-02",
#                        "2019-02-01", "2019-02-03", "2019-02-01", "2019-02-01"),
#                Duration = c(1, 7, 3, 4, 3, 2)
# )

# example 1

ex1 <-
  data.frame(
  stringsAsFactors = FALSE,
                   Category = c("one","two","two2",
                          "thr","thr2","thr3","fou","fiv"),
                   Task = c("Create Framework",
                          "Add Functions","Create External Dependencies",
                          "Document Functions","Add Testing","Make Vignettes",
                          "Ask for Feedback","Submit to CRAN"),
                   Status = c("Done","Done","Done",
                          "In Progress","In Progress","In Progress","To Do",
                          "To Do"),
                Critical = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE),
                   Start = c("2020-03-01",
                          "2020-03-02","2020-03-02","2020-03-04","2020-03-04",
                          "2020-03-06","2020-03-06","2020-03-15"),
                Duration = c(1L, 5L, 3L, 5L, 3L, 2L, 7L, 5L)
   )

# checking mutate

df <-
  ex2 %>% 
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

# example 2

ex2 <-
  data.frame(
    stringsAsFactors = FALSE,
    Category = c("one", "two", "thr", "thr2", "thr3"),
    Task = c(
      "Tidy Time Series Analysis and Forecasting Workshop",
      "RStudio Instructor Training Workshop",
      "R-Ladies kickoff breakfast",
      "rstudio conf Day 1",
      "rstudio conf Day 2"
    ),
    Status = c("Done", "Done", "In Progress", "In Progress", "To Do"),
    Critical = c(FALSE, TRUE, FALSE, TRUE, TRUE),
    Start = c(
      "2020-01-27",
      "2020-01-28",
      "2020-01-29",
      "2020-01-29",
      "2020-01-30"
    ),
    Duration = c(2L, 1L, 1L, 1L, 1L)
  )
