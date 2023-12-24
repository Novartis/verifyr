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

dt_global_file_list <- NULL
sel_row_index <- NULL

ui <- shiny::fluidPage(
  useShinyjs(),
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
      shiny::textOutput("initial_text_output"),
      shiny::downloadButton("download_csv", "Download comparison results as CSV"),
    ),
    shiny::column(12,
      h2("Side-by side comparison (using function full_comparison):"),
      shiny::fluidRow(
        shiny::column(6,
          shiny::downloadLink("open_old_file_link", shiny::textOutput("open_old_file_link_output")),
        ),
        shiny::column(6,
          shiny::downloadLink("open_new_file_link", shiny::textOutput("open_new_file_link_output")),
        ),
      ),
      shiny::htmlOutput("full_out"),
      shiny::textOutput("full_text_output"),
      shiny::fluidRow(
        shiny::column(12,
          shiny::textAreaInput("full_out_comments", "Comments", width = "100%"),
          shiny::actionButton("save_comments", "Save comments"),
          shiny::actionButton("clear_comments", "Clear comments"),
        ),
        class = "comparison_comments",
      ),
    ),
  ),
)
server <- function(input, output, session) {

  # ===============================================================================================
  # Element initializations
  # ===============================================================================================

  roots  <- c(Home = fs::path_home(), Examples = fs::path_package("verifyr", "extdata"))
  params <- list(roots = roots, session = session, restrictions = system.file(package = "base"), allowDirCreate = FALSE)

  do.call(shinyFiles::shinyDirChoose, c(list(input, "old_folder_select"), params))
  do.call(shinyFiles::shinyDirChoose, c(list(input, "new_folder_select"), params))

  default1 <- "Select the compared file folders and execute the initial comparison by clicking on the 'Go' button."
  default2 <- "Click on a row in the initial comparison result to view the detailed side-by-side comparison."

  initial_text <- shiny::reactiveVal(default1)
  full_text    <- shiny::reactiveVal(default2)
  open_old_file_link <- shiny::reactiveVal("")
  open_new_file_link <- shiny::reactiveVal("")

  dt_proxy <- dataTableProxy("initial_out")

  output$initial_text_output <- shiny::renderText(initial_text())
  output$full_text_output    <- shiny::renderText(full_text())

  output$open_old_file_link_output <- shiny::renderText(open_old_file_link())
  output$open_new_file_link_output <- shiny::renderText(open_new_file_link())

  output$download_csv <- shiny::downloadHandler(
    filename = function() {
      paste0("verifyr_comparison_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv")
    },
    content = function(file) {
      dt_subset <- dt_global_file_list[, !(names(dt_global_file_list) %in% "comments")]
      write.csv(dt_subset, file, row.names = FALSE)
    }
  )

  output$initial_out <- DT::renderDataTable({
    req(initial_verify())
    options <- list(columnDefs = list(list(visible = FALSE, targets = c("comments_full"))))
    DT::datatable(initial_verify(), selection = "single", options = options)
  })

  # ===============================================================================================
  # Reactive elements and observe triggers
  # ===============================================================================================

  list_of_files <- shiny::eventReactive(input$go, {
    if (file.exists(input$old_folder) && file.exists(input$new_folder)) {
      shinyjs::runjs("$('.comparison_comments').hide();")
      shinyjs::runjs("$('#download_csv').css('display', 'inline-block');")
      set_reactive_text("initial_text", "")
      verifyr::list_files(input$old_folder, input$new_folder, input$file_name_patter)
    } else {
      set_reactive_text("initial_text", "No folder selected or folders do not exist")
      NULL
    }
  })

  shiny::observeEvent(input$save_comments, {
    if (is.null(dt_global_file_list)) {
      dt_global_file_list <<- initial_verify()
    }

    dt_global_file_list[sel_row_index, "comments_full"] <- input$full_out_comments
    dt_global_file_list[sel_row_index, "comments"] <- ifelse(input$full_out_comments != "", "yes", "no")
    dt_global_file_list <<- dt_global_file_list

    DT::replaceData(dt_proxy, dt_global_file_list)
    DT::selectRows(dt_proxy, sel_row_index)
  })

  shiny::observeEvent(input$clear_comments, {
    if (!is.null(dt_global_file_list)) {
      if ("" != dt_global_file_list[sel_row_index, "comments_full"]) {
        dt_global_file_list[sel_row_index, "comments_full"] <- ""
        dt_global_file_list[sel_row_index, "comments"] <- "no"
        dt_global_file_list <<- dt_global_file_list

        DT::replaceData(dt_proxy, dt_global_file_list)
        DT::selectRows(dt_proxy, sel_row_index)
      }
    }

    shiny::updateTextAreaInput(session, "full_out_comments", value = "")
  })

  initial_verify <- shiny::reactive({
    req(list_of_files())
    dt_global_file_list <<- tibble::tibble(list_of_files()) %>%
      dplyr::mutate(omitted = input$omit_rows) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(comparison = verifyr::initial_comparison(old =old_path,new=new_path, omit = omitted)) %>% # nolint
      dplyr::mutate(comments = "no") %>%
      dplyr::mutate(comments_full = "")
  })

  shiny::observe({
    # handle changes in folder selections
    update_folder_selections()

    # handle changes related to selecting a comparison row
    shiny::req(input$initial_out_rows_selected)
    new_row_index <- input$initial_out_rows_selected

    # clear/initialize the comparison specific comment value when selecting a new row
    if (!is.null(sel_row_index) && sel_row_index != new_row_index) {
      row_comment <- paste0(dt_global_file_list[new_row_index, "comments_full"])
      shiny::updateTextAreaInput(session, "full_out_comments", value = row_comment)
    }

    shinyjs::runjs("$('.comparison_comments').show();")

    sel_row_index <<- new_row_index
    sel_row <- initial_verify()[new_row_index, ]

    # list side-by-side comparison
    update_full_comparison(sel_row)

    # set up the file download links for the compared files
    update_download_links(sel_row)
  })

  # ===============================================================================================
  # Helper functions
  # ===============================================================================================

  set_reactive_text <- function(reactive_id, text, class = "") {
    do.call(reactive_id, list(text))
  }

  update_folder_selections <- function() {
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
  }

  update_full_comparison <- function(sel_row) {
    set_reactive_text("full_text", "")

    output$full_out <- shiny::renderUI({
      shiny::HTML(
        as.character(
          verifyr::full_comparison(paste0(sel_row[2]), paste0(sel_row[3]))
        )
      )
    })
  }

  update_download_links <- function(sel_row) {
    open_old_file_link("")
    open_new_file_link("")

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
    }
  }
}

shiny::shinyApp(ui, server)
