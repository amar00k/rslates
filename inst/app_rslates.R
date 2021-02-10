


# blueprint.dir <- system.file("builtin_blueprints", package="rslates")
# blueprint.files <- list.files(blueprint.dir, pattern = '\\.R$', full.names = TRUE)
#
# e <- new.env()
# for (f in blueprint.files) {
#
#   eval(parse(f), envir = e)
# }
#
# options(
#   slate.blueprints = as.list(e)
#   #slate.data.blueprints = create_data_blueprints()
# )

slatesApp()

