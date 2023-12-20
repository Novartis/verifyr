#' Call for shiny example where the user can test verifyr package functions
#'
#' \code{runExample} returns simple Shiny App where user can see how the verifyr functions work
#'
#'

custom_file_input <- function(input_id, label, value = "", ...) {
  label_class  <- "control-label"
  input_class  <- "shiny-input-text form-control shiny-bound-input"
  folder_value <- system.file(value, package = "verifyr")

  shiny::div(class = "form-group form-group-custom shiny-input-container",
    shinyFiles::shinyDirButton(paste0(input_id, "_select"), NULL, "Select folder", icon = shiny::icon("folder-open")),
    tags$label(label, `for` = input_id, class = label_class),
    tags$input(id = input_id, type = "text", class = input_class, value = folder_value, ...),
  )
}

ui <- shiny::fluidPage(
  shiny::includeCSS("styles.css"),
  shiny::headerPanel("File Location Input"),
  shiny::wellPanel(
    shiny::fluidRow(
      shiny::column(6,
        custom_file_input("old_folder", "Old File Folder", "/extdata/base_files"),
        custom_file_input("new_folder", "New File Folder", "/extdata/compare_files"),
        shiny::actionButton("go", "Go"),
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
      shiny::textOutput("initial_out_placeholder"),
    ),
    shiny::column(12,
      h2("Side-by side comparison (using function full_comparison):"),
      shiny::fluidRow(
        shiny::column(6,
          shiny::textOutput("full_out_placeholder1"),
          shiny::downloadLink("open_old_file_link", shiny::textOutput("open_old_file_link_text")),
        ),
        shiny::column(6,
          shiny::textOutput("full_out_placeholder2"),
          shiny::downloadLink("open_new_file_link", shiny::textOutput("open_new_file_link_text")),
        ),
      ),
      shiny::htmlOutput("full_out"),
      shiny::textOutput("full_out_placeholder"),
    ),
  ),
)
server <- function(input, output, session) {

  roots  <- c(Home = fs::path_home(), Examples = fs::path_package("verifyr", "extdata"))
  params <- list(roots = roots, session = session, restrictions = system.file(package = "base"), allowDirCreate = FALSE)

  do.call(shinyFiles::shinyDirChoose, c(list(input, "old_folder_select"), params))
  do.call(shinyFiles::shinyDirChoose, c(list(input, "new_folder_select"), params))

  open_old_file_link <- shiny::reactiveVal("")
  open_new_file_link <- shiny::reactiveVal("")

  default1 <- "Select the compared file folders and execute the initial comparison by clicking on the 'Go' button."
  initial_out_placeholder_text <- shiny::reactiveVal(default1)

  default2 <- "Click on a row in the initial comparison result to view the detailed side-by-side comparison."
  full_out_placeholder_text <- shiny::reactiveVal(default2)

  shiny::observe({
    if (!is.integer(input$old_folder_select)) {
      shiny::updateTextInput(session, "old_folder",
        NULL,
        shinyFiles::parseDirPath(roots, input$old_folder_select)
      )
    }

    if (!is.integer(input$new_folder_select)) {
      shiny::updateTextInput(session, "new_folder",
        NULL,
        shinyFiles::parseDirPath(roots, input$new_folder_select)
      )
    }
  })

  list_of_files <- shiny::eventReactive(input$go, {
    if (file.exists(input$old_folder) && file.exists(input$new_folder)) {
      initial_out_placeholder_text("")
      verifyr::list_files(input$old_folder, input$new_folder, input$file_name_patter)
    }
  })

  initial_verify <- shiny::reactive({
    if (!is.null(list_of_files())) {
      tibble::tibble(list_of_files()) %>%
        dplyr::mutate(omitted = input$omit_rows) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(comparison = verifyr::initial_comparison(old =old_path,new=new_path, omit = omitted)) # nolint
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
    full_out_placeholder_text("")

    #list side-by-side comparison
    output$full_out <- shiny::renderUI({
      shiny::HTML(
        as.character(
          verifyr::full_comparison(paste0(sel_row[2]), paste0(sel_row[3]))
        )
      )
    })

    if (!is.na(sel_row[2])) {
      open_old_file_link(paste0("Open ", sel_row[1], " (old)"))
      output$open_old_file_link <- downloadHandler(
        filename = function() {
          paste0("old_", sel_row[1])
        },
        content = function(file) {
          file.copy(paste0(sel_row[2]), file)
        }
      )
    } else {
      open_old_file_link("")
    }

    if (!is.na(sel_row[3])) {
      open_new_file_link(paste0("Open ", sel_row[1], " (new)"))
      output$open_new_file_link <- downloadHandler(
        filename = function() {
          paste0("new_", sel_row[1])
        },
        content = function(file) {
          file.copy(paste0(sel_row[3]), file)
        }
      )
    } else {
      open_new_file_link("")
    }
  })

  # set up the reactive value bindings to default outputs
  output$initial_out_placeholder <- shiny::renderText({
    initial_out_placeholder_text()
  })

  output$full_out_placeholder <- shiny::renderText({
    full_out_placeholder_text()
  })

  output$open_old_file_link_text <- shiny::renderText(open_old_file_link())
  output$open_new_file_link_text <- shiny::renderText(open_new_file_link())
}

shiny::shinyApp(ui, server)
