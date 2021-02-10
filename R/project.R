



slatesProject <- function(title, authors = "") {
  uid <- b64.uid(32)
  now <- Sys.time()

  list(type = "project",
       uid = uid,
       title = title,
       authors = authors,
       date.created = now,
       date.modified = now,
       date.saved = NULL,
       datasets = list(),
       slates = list()
  )
}

