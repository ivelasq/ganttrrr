df <- data.frame(
  stringsAsFactors = FALSE,
              Task = c("hfjk", "jksjf", "jksjf", "jksjf", "jksjf", "jksjf"),
            Status = c("Active","Active",
                       "Not Active","Not Active","Done","Done"),
          Critical = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
          Category = c("one", "two", "two", "thr", "thr1", "thr2"),
             Start = c("2019-02-01", "2019-02-02",
                       "2019-02-01", "2019-02-03", "2019-02-01", "2019-02-01"),
               Duration = c(1, 7, 3, 4, 3, 2)
)

