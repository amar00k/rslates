




wizard <- function(name) {

  list(
    name = name
  )
}



rename.columns <- wizard(
  name = "Rename Columns"
)



wizard.list <- list(
  rename.columns
)
names(wizard.list) <- sapply(wizard.list, "[[", "name")

