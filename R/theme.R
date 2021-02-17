



rslate.themes <- list(
  # https://coolors.co/f8f6f2-2a7f62-ec4e20-3772ff-092b36
  "Natural (soft light)" = bslib::bs_theme(
    bootswatch = "solar", version = "4",
    bg = "#F8F6F2", fg = "#000000",
    primary = "#092B36", secondary = "#2A7F62") %>%
    bslib::bs_add_rules("body { background-image: linear-gradient(to bottom, #F8F6F2, #DBD1BD, #B09A6D); }")
)


loadTheme <- function(name) {
  if (name %in% names(rslate.themes))
    return(rslate.themes[[ name ]])

  return(bslib::bs_theme(bootswatch = name, version = "4"))
}
