#' Call for shiny example where the user can test verifyr package functions
#'
#' \code{runExample} returns simple Shiny App where user can see how the verifyr functions work
#'
#'

custom_file_input <- function (input_id, label, value = "", ...) {
  div(class = "form-group shiny-input-container", style = "padding-right: 50px; position: relative; width: 100%;",
    shinyFiles::shinyDirButton(paste0(input_id, "_directory"), NULL, "Please select a folder", style = "position: absolute; right: 0px; top: 25px;", icon = icon("folder-open")),
    tags$label(label, `for` = input_id, class = "control-label"),
    tags$input(id = input_id, type = "text", class = "shiny-input-text form-control shiny-bound-input", value = value, ...),
  )
}

ui <- shiny::fluidPage(
  shiny::headerPanel("File Location Input"),
  shiny::wellPanel(
    shiny::fluidRow(
      shiny::column(6,
        custom_file_input("old_file_location", "Old File Folder", system.file("/extdata/base_files", package = "verifyr")),
        custom_file_input("new_file_location", "New File Folder", system.file("/extdata/compare_files", package = "verifyr")),
        shiny::actionButton("go", "Go")
      ),
      shiny::column(6,
        shiny::textInput("file_name_patter", "File Name Pattern"),
        shiny::textInput("omit_rows", "Omit rows with text", "Source:"),
      ),
    ),
  ),
  shiny::fluidRow(
    shiny::column(12,
      h2("Initial comparison (using function initial_comparison):"),
      DT::dataTableOutput("initial_out"),
    ),
    shiny::column(12,
      h2("Side-by side comparison (using function full_comparison):"),
      shiny::htmlOutput("full_out")
    ),
  ),
  style = "padding: 20px;",
)

server <- function(input, output, session) {

  roots <- c(Home = fs::path_home(), Examples = fs::path_package("verifyr", "extdata"))

  shinyFiles::shinyDirChoose(input, "old_file_location_directory", roots = roots, session = session, restrictions = system.file(package = "base"), allowDirCreate = FALSE)
  shinyFiles::shinyDirChoose(input, "new_file_location_directory", roots = roots, session = session, restrictions = system.file(package = "base"), allowDirCreate = FALSE)

  observe({
    if (!is.integer(input$old_file_location_directory)) {
      shiny::updateTextInput(session, "old_file_location",
        label <- NULL,
        value <- parseDirPath(roots, input$old_file_location_directory)
      )
    }

    if (!is.integer(input$new_file_location_directory)) {
      shiny::updateTextInput(session, "new_file_location",
        label <- NULL,
        value <- parseDirPath(roots, input$new_file_location_directory)
      )
    }
  })

  list_of_files <- shiny::eventReactive(input$go, {
    if (file.exists(input$old_file_location) && file.exists(input$new_file_location)) {
      verifyr::list_files(input$old_file_location, input$new_file_location, input$file_name_patter)
    }
  })

  initial_verify <- shiny::reactive({
    if (!is.null(list_of_files())) {
      tibble::tibble(list_of_files()) %>%
      dplyr::mutate(omitted = input$omit_rows) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(comparison = verifyr::initial_comparison(old =old_path,new=new_path, omit = omitted))
    }
  })

  full_verify <- shiny::reactive({
    if (file.exists(input$old_file_location) && file.exists(input$new_file_location)) {
      verifyr::full_comparison(input$old_file_location, input$new_file_location)
    }
  })

  output$initial_out <-  DT::renderDataTable({
    if (!is.null(initial_verify())) {
      DT::datatable(initial_verify(), selection = "single")
    } else {
      "No folder selected or folders do not exist"
    }
  })

  shiny::observe({
    shiny::req(input$initial_out_rows_selected)
    selRow <- initial_verify()[input$initial_out_rows_selected,]

    output$full_out <- shiny::renderUI({

    #list side-by-side comparison
      shiny::HTML(
        as.character(
          verifyr::full_comparison(paste0(selRow[2]), paste0(selRow[3]))
        )
      )
    })
  })
}

shiny::shinyApp(ui, server)
