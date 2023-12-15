#' Call for shiny example where the user can test verifyr package functions
#'
#' \code{run_example} returns simple Shiny App where user can see how the verifyr functions work
#'
#'

ui <- shiny::fluidPage(
  shiny::headerPanel("File Location Input"),
  shiny::sidebarPanel(
    shiny::textInput("old_file", "Old File Folder", system.file("/extdata/base_files", package = "verifyr")),
    shiny::textInput("new_file", "New File Folder", system.file("/extdata/compare_files", package = "verifyr")),
    shiny::textInput("file_name_pattern", "File Name Pattern"),
    shiny::textInput("omit_rows", "Omit rows with text", "Source:"),
    shiny::actionButton("go", "Go")
  ),
  shiny::mainPanel(
    h4("Initial comparison (using function initial_comparison):"),
    DT::dataTableOutput("initial_out"),
    h4("Side-by side comparison (using function full_comparison):"),
    shiny::htmlOutput("full_out")
  )
)

server <- function(input, output) {

  list_of_files <- shiny::eventReactive(input$go, {
    if (file.exists(input$old_file) && file.exists(input$new_file)) {
      verifyr::list_files(input$old_file, input$new_file, input$file_name_pattern)
    }
  })

  initial_verify <- shiny::reactive({
    if (!is.null(list_of_files())) {
      tibble::tibble(list_of_files()) %>%
        dplyr::mutate(omitted = input$omit_rows) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(comparison = verifyr::initial_comparison(old = old_path, new = new_path, omit = omitted)) # nolint
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
    sel_row <- initial_verify()[input$initial_out_rows_selected, ]
    output$full_out <- shiny::renderUI({

      #list side-by-side comparison
      shiny::HTML(
        as.character(
          verifyr::full_comparison(paste0(sel_row[2]), paste0(sel_row[3]))
        )
      )
    })
  })
}

shiny::shinyApp(ui, server)
