Calendar <- list(
  "Misc Calendar Data",
  headers = c("Month", "NumDays", "Seasons"),
  rows = list(
    list("Feb", 29, "Winter"),
    list("Mar", 15, "Spring")
  )
  
)

str(Calendar)
Calendar[1]
Calendar[["rows"]][[2]][[1]]
Calendar[[3]][[2]][[1]]

numbers <- c(1,2,3,4)
numbers
numbers <- append(numbers, 5)
numbers
numbers <- append(numbers, c(2,2), after = 3)
numbers