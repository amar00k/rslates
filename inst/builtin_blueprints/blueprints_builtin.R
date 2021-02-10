

inputs =  list(
  x = slateInput("x", "expression", "1:10",
             description = "x coordinates for the plot."),
  y = slateInput("y", "expression", NULL,
             description = "y coordinates for the plot."),
  type = slateInput("type", "choices", "p",
             choices = list("Points"="p",
                            "Lines"="l",
                            "Both"="b",
                            "Empty points"="c",
                            "Overplotted"="o",
                            "Stair steps (low)"="s",
                            "Stair steps (high)"="S",
                            "Vertical lines"="h"),
             description = "Type of plot."),
  main = slateInput("main", "character", "", description = "Main title for the plot."),
  xlab = slateInput("xlab", "character", "", description = "Label for the x-axis."),
  ylab = slateInput("ylab", "character", "", description = "Label for the y-axis."),
  mar_bottom = slateInput("mar_bottom", "numeric", 5.1, description = "Bottom margin in lines."),
  mar_left = slateInput("mar_left", "numeric", 4.1, description = "Left margin in lines."),
  mar_top = slateInput("mar_top", "numeric", 4.1, description = "Top margin in lines."),
  mar_right = slateInput("mar_right", "numeric", 2.1, description = "Right margin in lines.")
)

scatterplot.blueprint <- slateBlueprint(
  title = "Scatterplot",
  input.layout = inputLayout(
    main.page = inputPage(
      name = "Main",
      inputGroup(inputs$x, inputs$y)
    ),
    pages = list(
      inputPage(
        name = "Plot options",
        inputGroup(inputs$type, inputs$main, inputs$xlab, inputs$ylab)
      ),
      inputPage(
        name = "Graphical parameters",
        description = "Additional graphical parameters for the plot output.",
        inputGroup(inputs$mar_bottom, inputs$mar_left, inputs$mar_top, inputs$mar_right)
      )
    )
  ),
  outputs = list(
    slateOutput(
      "Plot", "plot",
      source = list(
        "par(mar=c(${mar_bottom, mar_left, mar_top, mar_right}))",
        "plot(${dn:: x:x, y=y, q:type=type, q:main=main, q:xlab=xlab, q:ylab=ylab})"
      )
    )
  )
)






# slate_scatterplot <- slate(
#   title = "Scatterplot",
#   inputs = list(
#     slate_input_page(
#       title = "Scatterplot options",
#       slate_input_group(
#         slate_input("x", "expression", "1:10",
#                     description = "x coordinates for the plot."),
#         slate_input("y", "expression", NULL,
#                     description = "y coordinates for the plot."),
#         slate_input("type", "choices", "p",
#                     choices = list("Points"="p",
#                                    "Lines"="l",
#                                    "Both"="b",
#                                    "Empty points"="c",
#                                    "Overplotted"="o",
#                                    "Stair steps (low)"="s",
#                                    "Stair steps (high)"="S",
#                                    "Vertical lines"="h"),
#                     description = "Type of plot."),
#         slate_input("main", "character", "", description = "Main title for the plot."),
#         slate_input("xlab", "character", "", description = "Label for the x-axis."),
#         slate_input("ylab", "character", "", description = "Label for the y-axis.")
#       )
#     ),
#     slate_input_page(
#       title = "Graphical parameters",
#       description = "Additional graphical parameters for the plot output.",
#       slate_input_group(
#         slate_input("customize_margins", "logical", FALSE, description = "Customize plot margin sizes.")
#       ),
#       slate_conditional_input_group(
#         condition = expression(customize_margins == TRUE),
#         slate_input("mar_bottom", "numeric", 5.1, description = "Bottom margin in lines."),
#         slate_input("mar_left", "numeric", 4.1, description = "Left margin in lines."),
#         slate_input("mar_top", "numeric", 4.1, description = "Top margin in lines."),
#         slate_input("mar_right", "numeric", 2.1, description = "Right margin in lines.")
#       )
#     )
#   ),
#   outputs = list(
#     slate_output("plot", "Plot", "plot",
#                  source = slate_source(
#                    source_call("plot",
#                                args = c("x", "y", "type", "main", "xlab", "ylab"),
#                                opts = list("x"=c("inline-default")))
#                  )
#     ),
#     sources = list(plot="
#       %if%customize_margins == TRUE%
#         par(mar=c(mar_bottom, mar_left, mar_top, mar_right))
#       %fi%
#
#       plot( %li %a::x, %a:o:y, %a:o:type, %a:o:main, %a:o:xlab, %a:o:ylab %il )
#       ")
#     # source = source_conditional(
#     #   condition = "set.margins == TRUE",
#     #   source_gsub("par(mar=")
#     #
#     # )
#   )
# )


#
#   slate_histogram <- slate(
#     title = "Histogram",
#     inputs = list(
#       slate_input_page(
#         title = "Histogram options",
#         slate_input_group(
#           slate_input("x", "expression", "rnorm(50, mean = 2)",
#                       description = "x coordinates for the plot."),
#           # TODO: complex input, e.g.:
#           # breaks = slate_input_function(
#           #   value = function() { if (breaks.type == "numeric") return(breaks.num) else return(breaks.funcname) }
#           # )
#           slate_input("breaks", "character", "Sturges",
#                       description = "A vector giving the breakpoints between histogram cells or;
#                        a single number giving the number of cells for the histogram or;
#                        name of an algorithm to compute the number of cells."),
#           # TODO: make it a choices and map result to a logical
#           # e.g. freq = slate_input("display", "logical-choices", choices = list("Frequency", "Density"))
#           slate_input("freq", "logical", TRUE,
#                       description = "Whether to display frequencies or probability densities."),
#           slate_input("main", "character", "",
#                       description = "Main title for the plot."),
#           slate_input("xlab", "character", "", description = "Label for the x-axis."),
#           slate_input("ylab", "character", "", description = "Label for the y-axis.")
#         )
#       )
#     ),
#     outputs = list(
#       slate_output("plot", "Plot", "plot",
#                    source = slate_source(
#                      source_call("hist",
#                                  args = c("x", "breaks", "freq", "main", "xlab", "ylab"),
#                                  opts = list("x"=c("inline-default")))
#                    )
#       )
#     )
#   )
#
#
#
#   slate_import_csv <- slate(
#     title = "CSV dataset",
#     inputs = list(
#       slate_input_page(
#         title = "",
#         main = TRUE,
#         slate_input_group(
#           layout = "vertical",
#           slate_input("file", "file", NULL, description = "CSV file to load."),
#           slate_input("varname", "character", "my_table", description = "Variable name for this table.")
#         )
#       ),
#       slate_input_page(
#         "Options",
#         slate_input_group(
#           slate_input("header", "logical", TRUE),
#           slate_input("sep", "choices", ",",
#                       choices = list("Comma (,)"=",",
#                                      "Semicolon (;)"=";",
#                                      "Tab"="\t",
#                                      "Space"=" ")),
#           slate_input("dec", "choices", ".",
#                       choices=list("."=".",
#                                    ","=",")),
#           slate_input("skip", "numeric", 0)
#         )
#       ),
#       slate_input_page(
#         "Filters",
#         slate_input_group(
#           slate_input("sample_rows", "logical", FALSE),
#           slate_input("sample_size", "numeric", 50)
#         )
#       )
#     ),
#     datasets = list(
#       slate_dataset("dataframe",
#                     source = slate_source(
#                       source_call2("read.table", "file", c("header", "sep", "dec", "skip"))
#                     )
#       )
#     ),
#     outputs = list(
#       slate_output("preview", title = "Preview", "table",
#                    #urce = source_asis("head(iris, 10)")
#                    source_call2("read.table", "file", c("header", "sep", "dec", "skip"))
#       ),
#                    #source = source_asis("read.table(file$datapath, header=header, sep=sep, dec=dec, skip=skip)"))
#       # slate_output("text", title = "Text", "print",
#       #              source = source_asis("print(sep)")),
#       slate_output("debug", title = "Debug", "debug")
#
#       #source = source_asis("read.table(file$datapath, header=header, sep=sep, dec=dec, skip=skip)"))
#     )
#   )
#







