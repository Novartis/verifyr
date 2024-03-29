#' Call for shiny example where the user can test verifyr package functions
#'
#' \code{runExample} returns simple Shiny App where user can see how the verifyr functions work
#'
#'

ui <- shiny::fluidPage(
  shiny::headerPanel("File Location Input"),
  shiny::sidebarPanel(
    shiny::textInput("old_file_location", "Old File Folder", paste0(system.file("/extdata/base_files", package = "verifyr"))),
    shiny::textInput("new_file_location", "New File Folder", paste0(system.file("/extdata/compare_files", package = "verifyr"))),
    shiny::textInput("file_name_patter", "File Name Pattern"),
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


list_of_files <- eventReactive(input$go, {
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
