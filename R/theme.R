



rslate.themes <- list(
  # https://coolors.co/f8f6f2-2a7f62-182825-171b0e-390805-092b36
  "Natural (soft light)" = bslib::bs_theme(
    bootswatch = "minty", version = "4",
    bg = "#F1ede4", fg = "#000000",
    primary = "#092B36", secondary = "#2A7F62",
    #base_font = "Noto Sans JP, sans-serif",
    "input-border-color" = "#6c757d"
    ) %>%
    bslib::bs_add_variables(
      "theme-colors" = "('light': #f1ede4, 'dark': #0b0d07, 'title': #26403B)"
    ) %>%
    bslib::bs_add_rules("
      /*body { background-image: linear-gradient(
        to bottom, #eae3d7, #DBD1BD 1000px, #B09A6D 2000px, #D64933 100%) ;
      }*/

      body {
       background: #eae3d7;
      }

      .slate {
        background: #F1ede4;
      }

      .jstree-proton .jstree-wholerow-clicked {
        background: $primary;
      }
    ")
)


loadTheme <- function(name) {
  if (name %in% names(rslate.themes))
    return(rslate.themes[[ name ]])

  return(bslib::bs_theme(bootswatch = name, version = "4"))
}
