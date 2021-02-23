

blueprint <- blueprintFromJSON(system.file("blueprints/Scatterplot.json", package="rslates"))

testServer(
  slateServer,
  args = list(blueprint = blueprint, slate.options = NULL, global.options = NULL), {

    inputs <- getInputs(blueprint)

    # get all pairs (input_id, initial_value)
    args <- lapply(inputs,
                   function(x) getHandler(x)$get.inputs(x, session, x$default)) %>%
      unlist(recursive = FALSE) %>%
      set_names(sapply(inputs, "[[", "id"))

    # set initial input values
    do.call(session$setInputs, args)

    # ui.ready()
    expect_true(ui.ready())

    # input.list()
    expect_equal(length(inputs), length(input.list()))
    expect_equal(names(inputs), names(input.list()))
    expect_equal(sapply(input.list(), "[[", "default"),
                 sapply(input.list(), "[[", "value"))
    expect_true(all(sapply(input.list(), function(x) !is.null(x$source))))

    # print(input.list())
    #do.call(session$setInputs, lapply(input.list, "[[", "default"))
    #session$setInputs(charf = "Some text")
    #print(names(input.list))
    #print(names(inputs()))
    #expect_equal(inputs()$character, "Some text")
    #expect_equal(inputs()$expression, c(1:10, 1:10))
  }
)
