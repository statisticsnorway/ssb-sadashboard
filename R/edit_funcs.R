#' Function from fellesr to fetch initials of user (Internal)
#'
#' @returns String with initials
#' @noRd
#' @keywords Internal
initialer_funk <- function() {
  if (grepl("ON_PREM", user_agent())) {
    initialer <- Sys.getenv('USER')
  }
  if (grepl("BIP", user_agent())) {
    initialer <- gsub("@ssb.no", "", Sys.getenv('JUPYTERHUB_USER'))
  }
  if (grepl("DAPLA_LAB", user_agent())) {
    initialer <- gsub("@ssb.no", "", Sys.getenv('DAPLA_USER'))
  }
  if (!exists("initialer")) {
    warning("No user found")
  }
  return(initialer)
}


#' Convert date (Internal)
#'
#' @param date String variable for the date
#'
#' @return String variable for the formated date
#' @noRd
#' @keywords Internal
convert_date <- function(date) {
  # Check if the date is in 'dd/mm/yyyy' format
  if (grepl("^\\d{2}/\\d{2}/\\d{4}$", date)) {
    date <- format(lubridate::dmy(date), "%Y-%m-%d")
    return(as.character(date))
  }

  # Assume the date is in 'yyyy-mm-dd' format and return it as is
  date
}


#' Read in yaml file with constraints information
#'
#' @param path String variable for the path and name of yaml contraints file.
#'
#' @returns A data frame object with the constrainst data as a data.frame object
#' @export
read_yaml_constraints <- function(path){
  yaml_content <- yaml.load_file(path)
  df_rows <- list()

  # Initialize an empty list to store column names
  column_names <- list()

  # Iterate over the list to process each series/line
  for (key in names(yaml_content)) {
    # Extract the keys for this series/line
    keys <- names(yaml_content[[key]])

    # Add the keys to the list of column names (avoid duplicates)
    column_names <- unique(c(column_names, keys))

    # Initialize an empty list for this row
    row_data <- list(name = key)

    # Populate the row data with values for each column
    for (col in column_names) {
      if (col %in% keys) {
        row_data[[col]] <- yaml_content[[key]][[col]]
      } else {
        row_data[[col]] <- ""  # Set to NA if the column is missing for this series/line
      }
    }

    # Combine the row data into a data frame
    row_df <- do.call(data.frame, row_data)
    df_rows[[key]] <- row_df
  }

  # Combine all the row DataFrames into a single DataFrame
  df <- do.call(rbind, df_rows)
  df <- df[, c("name", setdiff(names(df), "name"))]

  row.names(df) <- NULL
  df
}

#' Write constraints data.frame object to disk as a yaml file
#'
#' @param df Data frame object
#' @param path Path and file name to save the object as
#'
#' @returns NULL
#' @export
write_yaml_constraints <- function(df, path) {
  # Create an empty list to store YAML content
  yaml_content <- list()

  # Iterate over rows in the DataFrame
  for (i in 1:nrow(df)) {
    # Get the name and spec values for this row
    name <- df$name[i]

    # Create a list for this observation
    observation <- list()

    # Iterate over column names (excluding 'name' and 'spec')
    for (col_name in setdiff(names(df), c("name"))) {
      col_value <- df[i, col_name]
      # Only add the column if it is not blank
      if (!is.na(col_value) && col_value != "") {
        observation[[col_name]] <- col_value
      }
    }

    # Add this observation to the YAML content
    yaml_content[[name]] <- observation
  }

  # Convert the YAML content to a YAML string
  yaml_string <- as.yaml(yaml_content, handlers = list(logical = verbatim_logical))

  # Write the YAML string to the file
  write(yaml_string, file = path)
}

#' Get a port address used to open editor in (Internal)
#'
#' @returns A four-digit integer with a valid port address
#' @noRd
#' @keywords Internal
get_port <- function(){
  port <- NULL
  while (is.null(port)){
    port <- shiny:::p_randomInt(3000, 8000)
    if (!port %in% c(3659, 4045, 5060, 5061, 6000,
                     6566, 6665:6669, 6697)) {
      port <- NULL
    }
  }
  port
}


#' Function to get correct url address for the environment (Internal)
#'
#' @param port 4-digit port address
#'
#' @returns Character object with the url
#' @noRd
#' @keywords Internal
get_url <- function(port){
  if (Sys.getenv("DAPLA_REGION") == "ON_PREM") {
    usr <- initialer_funk()
    app_url <- paste("https://sl-jupyter-p.ssb.no/user/", usr, "/proxy/", port, "/", sep = "")
  } else if (Sys.getenv("DAPLA_REGION") == "DAPLA_LAB"){
    domain <- gsub("/+$", "", Sys.getenv('JUPYTERHUB_HTTP_REFERER'))     # Clean trailing /
    usr <- Sys.getenv('JUPYTERHUB_SERVICE_PREFIX', '/')
    app_url <- paste0(domain, usr, "proxy/", port, "/")
  }
  app_url
}


#' Function to get list of column names and standard types
#'
#' @param dt Data frame for editing
#'
#' @returns List of column names and types to use in editor
#' @noRd
#' @keywords Internal
get_column_list <- function (dt){
  column_list <- list(easter.enabled = c(TRUE,FALSE),
                      usrdef.varEnabled = c(TRUE,FALSE),
                      outlier.enabled = c(TRUE,FALSE),
                      outlier.ao = c(TRUE,FALSE),
                      outlier.ls = c(TRUE,FALSE),
                      outlier.tc = c(TRUE,FALSE),
                      usrdef.outliersEnabled = c(TRUE,FALSE),
                      automdl.enabled = c(TRUE,FALSE),
                      identify_outliers = c(TRUE,FALSE),
                      transform.function=c('"Log"', '"Auto"'),
                      usrdef.var = c("kalendergml", "kalender6"),
                      outlier.from="date"
  )

  #  update with 'chr' for columns not in the original list
  for (col in names(dt)) {
    if(!col %in% names(column_list)) {
      column_list[[col]] = "chr"
    }
  }

  # Drop original col_options not in the data
  column_list <- column_list[names(column_list) %in% names(dt)]

  column_list
}

#' Edit a constraint data frame object
#'
#' @param dt Data frame object
#'
#' @returns Updated data frame object
#' @export
edit_constraints <- function(dt){
  port <- get_port()
  url <- get_url(port)
  message(cat("Edit data at: ", url, "\n"))

  suppressMessages(
    new_data <- data_edit_ssb(dt, viewer = "browser", port = port, col_options = get_column_list(dt))
  )

  if("outlier.from" %in% names(new_data)){ # Maybe need to do this for all dates?
    new_data$outlier.from <- sapply(new_data$outlier.from, convert_date)
  }

  new_data
}


#' Add a constraint to a constraint data frame object and open for editing.
#'
#' @param dt Data frame object
#' @param constraint Name of the new constraint to be added
#' @param type The type of constrain it is. "int" for integer, "bool" for boolean
#' @param default The default value to give the constraint
#'
#' @returns Updated constraint data frame object
#' @export
add_constraint <- function(dt, constraint, type = "chr", default = ""){
  port <- get_port()
  url <- get_url(port)

  if (type == "int" & default == "") default = NA
  if (type == "bool" & default == "") default = FALSE
  new_col <- matrix(rep(default, nrow(dt)),
                    ncol = 1,
                    dimnames = list(NULL, constraint))

  message(cat("Edit data at: ", url))

  col_options_list <- get_column_list(dt)

  if (type == "bool") col_options_list[constraint] = c(TRUE,FALSE)
  if (type == "date") col_options_list[constraint] = "date"

  suppressMessages(
    new_data <- data_edit_ssb(dt,
                              viewer = "browser",
                              port = port,
                              col_bind = new_col,
                              col_options = col_options_list
    )
  )
  if("outlier.from" %in% names(new_data)){ # Maybe need to do this for all dates?
    new_data$outlier.from <- sapply(new_data$outlier.from, convert_date)
  }

  new_data
}



# Code below is an edited version of the function data_edit from the package DataEditR
## DATA_EDIT -------------------------------------------------------------------

#' An interactive editor for viewing, entering and editing data
#'
#' code{data_edit} is a shiny application built on \code{rhandsontable} that is
#' designed to make it easy to interactively view, enter or edit data without
#' any coding. \code{data_edit} is also a wrapper for any reading or writing
#' function to make it easy to interactively update data saved to file.
#'
#' @param x a matrix, data.frame, data.table or the name of a csv file to edit.
#'   Tibbles are also supported but will be coerced to data.frames. An empty
#'   table can be created by specifying the dimensions in a vector of the form
#'   \code{c(nrow, ncol)} or the names of the columns to include in the
#'   template.
#' @param col_bind additional columns to add to the data prior to loading into
#'   editor, can be either an array containing the new data, a vector containing
#'   the new column names for empty columns or a named list containing a vector
#'   for each new column.
#' @param col_edit logical indicating whether columns can be added or removed,
#'   set to TRUE by default.
#' @param col_options named list containing the options for columns that use
#'   dropdown menus, dates, checkboxes or passwords.
#' @param col_stretch logical indicating whether columns should be stretched to
#'   fill the full width of the display, set to FALSE by default.
#' @param col_factor logical indicating whether character columns should be
#'   converted to factors prior to returning the edited data, set to FALSE by
#'   default.
#' @param col_names logical indicating whether column names can be edited or a
#'   vector of column names that cannot be edited, set to TRUE by default to
#'   allow editing of column names.
#' @param col_readonly names of columns that cannot be edited. Users will be
#'   able to edit values but these will be reverted to the original values.
#'   Column names for these column cannot be edited either.
#' @param row_bind additional rows to add to the data prior to loading into
#'   editor, can be either an array containing the new data, a vector containing
#'   the new row names for empty rows or a named list containing a vector for
#'   each new column.
#' @param row_edit logical indicating whether rows can be added or removed, set
#'   to TRUE by default.
#' @param save_as name of a csv file to which the edited data should be saved.
#' @param title optional title to include above the data editor.
#' @param logo optional package logo to include in title above the data editor,
#'   must be supplied as path to logo png.
#' @param logo_size width of the logo in pixels, set to 30 pixels by default.
#' @param logo_side can be either \code{"left"} or \code{"right"} to determine
#'   the position of the logo relative to the title, set to \code{"left"} by
#'   default.
#' @param viewer can be either \code{"dialog"}, \code{"browser"} or
#'   \code{"pane"} to open the application in a dialog box, browser or RStudio
#'   viewer pane. First letter abbreviations are allowed, set to \code{"dialog"}
#'   by default.
#' @param viewer_height numeric to control the height of the viewer in pixels
#'   when \code{viewer} is set to \code{"dialog"}, set 800 by default.
#' @param viewer_width numeric to control the width of the viewer in pixels when
#'   \code{viewer} is set to \code{"dialog"}, set to 1200 by default.
#' @param theme valid shinytheme name, set to "yeti" by default.
#' @param read_fun name of the function to use to read in the data when \code{x}
#'   is the name of a file, set to \code{read.csv} by default.
#' @param read_args a named list of additional arguments to pass to
#'   \code{read_fun}.
#' @param write_fun name of the function to use to write the edited version of
#'   \code{x} to a file, set to \code{write.csv} by default. Only requirement is
#'   that the first argument accepts the edited data and the second argument
#'   accepts the file name supplied to \code{save_as}.
#' @param write_args a named list of additional arguments to pass to
#'   \code{write_fun}.
#' @param quiet logical indicating whether messages should be suppressed, set to
#'   FALSE by default.
#' @param hide logical indicating whether the \code{dataInput} and
#'   \code{dataOutput} modules should be visible to the user within the
#'   application. If \code{hide = FALSE} and \code{save_as} is specified, the
#'   edited data will be written to file after the application is closed.
#' @param code logical indicating whether the code required to generate the
#'   edited data should be printed to the console, set to \code{FALSE} by
#'   default. Alternatively, users can supply the name of an R script to create
#'   and store this code.
#' @param cancel optional value to return when the user hits the \code{cancel}
#'   button, set to the supplied data by default.
#' @param ... not in use.
#'
#' @return the edited data as a matrix or data.frame.
#'
#' @importFrom rstudioapi getActiveDocumentContext
#' @importFrom htmltools img span br div HTML
#' @importFrom shiny runGadget dialogViewer browserViewer paneViewer splitLayout
#'   fluidPage column stopApp reactiveValues actionButton insertUI
#' @importFrom shinyjs useShinyjs hidden show
#' @importFrom bslib bs_theme
#' @importFrom miniUI gadgetTitleBar
#' @importFrom shinyBS bsButton updateButton addTooltip
#' @importFrom rhandsontable %>%
#'
#' @author Dillon Hammill, \email{Dillon.Hammill@anu.edu.au}
#'
#' @examples
#' if(interactive()) {
#'
#'   data_edit(mtcars)
#'
#' }
#'
#' @export
data_edit_ssb <- function(x = NULL,
                          col_bind = NULL,
                          col_edit = TRUE,
                          col_options = NULL,
                          col_stretch = FALSE,
                          col_factor = FALSE,
                          col_names = TRUE,
                          col_readonly = NULL,
                          row_bind = NULL,
                          row_edit = TRUE,
                          save_as = NULL,
                          title = NULL,
                          logo = NULL,
                          logo_size = 30,
                          logo_side = "left",
                          viewer = "dialog",
                          viewer_height = 800,
                          viewer_width = 1200,
                          theme = "yeti",
                          read_fun = "read.csv",
                          read_args = NULL,
                          write_fun = "write.csv",
                          write_args = NULL,
                          quiet = FALSE,
                          hide = FALSE,
                          code = FALSE,
                          cancel,
                          port,
                          ...) {

  # DATA ENVIRONMENT -----------------------------------------------------------

  # SEARCH DATA OUTSIDE DATA_EDIT
  envir <- parent.frame()

  # PREPARE DATA ---------------------------------------------------------------

  # RSTUDIO ADDIN/DATA
  if(Sys.getenv("RSTUDIO") == "1") {
    context <- getActiveDocumentContext()$selection[[1]]$text
    # CHECK DATA_EDIT() CALL HIGHLIGHTED
    if(nzchar(context)) {
      if(!exists(context, envir = envir)) {
        context <- ""
      }
    }
  } else {
    context <- ""
  }

  # LOAD DATA THROUGH RSTUDIO ADDIN
  if(is.null(x) & nzchar(context)) {
    data <- context
  } else {
    if(!is.null(dim(x))) {
      data <- as.character(substitute(x))
    } else {
      data <- x
    }
  }

  # CANCEL
  if(missing(cancel)) {
    cancel <- x
  }

  # PREPARE SHINY COMPONENTS ---------------------------------------------------

  # DATAEDITR LOGO
  if(is.null(logo)) {
    logo <- paste0(
      "https://raw.githubusercontent.com/DillonHammill/DataEditR/master",
      "/vignettes/logo.png"
    )
  }

  # LOGO IMAGE
  if(!is.null(logo)) {
    logo <- htmltools::img(
      src = logo,
      width = logo_size
    )
  }

  # TITLE
  if(is.null(title)) {
    title <- "Data Editor"
  }

  # TITLE PANEL
  if(is.null(logo)) {
    title <- miniUI::gadgetTitleBar(
      title
    )
    # TITLE + LOGO PANEL
  } else {
    # LOGO ON LEFT
    if(grepl("^l", logo_side, ignore.case = TRUE)) {
      title <- miniUI::gadgetTitleBar(
        htmltools::span(logo,
                        title)
      )
      # LOGO ON RIGHT
    } else if(grepl("^r", logo_side, ignore.case = TRUE)) {
      title <- miniUI::gadgetTitleBar(
        htmltools::span(title,
                        logo)
      )
    }
  }

  # SHINY APPLICATION ----------------------------------------------------------

  # USER INTERFACE
  ui <- shiny::fluidPage(
    title,
    theme = if(is.null(theme)) {
      NULL
    } else {
      if("bs_theme" %in% class(theme)) {
        theme
      } else {
        bslib::bs_theme(
          version = 3, # version 4 places DONE on left
          bootswatch = theme
        )
      }
    },
    shinyjs::useShinyjs(),
    shiny::fluidRow(
      shiny::column(
        7,
        style = "padding-right: 5px;",
        dataInputUI("input1",
                    cellWidths = c("50%", "50%"))
      ),
      shiny::column(
        5,
        style = "padding-left: 5px; margin-top: 35px;",
        dataSelectUI("select1"),
        dataFilterUI("filter1"),
        dataSyncUI("sync1"),
        dataOutputUI("output-active"),
        dataOutputUI("output-update",
                     icon = "save"),
        shinyjs::hidden(
          shinyBS::bsButton(
            "cut",
            label = NULL,
            icon = shiny::icon(
              "glyphicon glyphicon-scissors",
              lib = "glyphicon"
            ),
            style = "danger",
            type = "action"
          )
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        dataEditUI("edit1"),
        htmltools::br()
      )
    )
  )

  # SERVER
  server <- function(input,
                     output,
                     session) {

    # SHOW BUTTONS
    if(!hide) {
      shinyjs::show("sync")
      shinyBS::addTooltip(
        session = session,
        id = "sync",
        title = "sychronise"
      )
      shinyjs::show("cut")
      shinyBS::addTooltip(
        session = session,
        id = "cut",
        title = "crop to selection"
      )
    }

    # DATA STORAGE
    values <- shiny::reactiveValues(
      data = NULL, # original data
      data_active = NULL, # displayed data
      rows = NULL,
      columns = NULL,
      cut = FALSE,
      row_index = NULL
    )

    # DATA INPUT
    data_input <- dataInputServer(
      "input1",
      data = data,
      read_fun = read_fun,
      read_args = read_args,
      hide = hide,
      envir = envir # search in parent frame
    )

    # RESET FILTERS
    shiny::observeEvent(data_input(), {
      # RESET FILTERS
      values$rows <- NULL
      values$columns <- NULL
      # BIND ROWS/COLUMNS
      values$data <- data_input() %>%
        DataEditR:::data_bind_rows(row_bind = row_bind) %>%
        DataEditR:::data_bind_cols(col_bind = col_bind)
    })

    # FILTERS ALWAYS RESET ON DATA SYNC

    # DATA SELECT
    data_select <- dataSelectServer(
      "select1",
      data = shiny::reactive(values$data),
      hide = hide,
      hover_text = "select columns"
    )

    # DATA FILTER
    data_filter <- dataFilterServer(
      "filter1",
      data = shiny::reactive(values$data),
      hide = hide,
      hover_text = "filter rows"
    )

    # UPDATE FILTERS
    shiny::observe({
      values$rows <- data_filter$rows()
      values$columns <- data_select$columns()
    })

    # DATA FILTERING
    shiny::observe({
      # ENTIRE DATA
      if(length(values$rows) == 0 & length(values$columns) == 0) {
        values$data_active <- values$data
        # DATA SUBSET
      } else {
        # ROWS
        if(length(values$rows) != 0 & length(values$columns) == 0) {
          values$data_active <- values$data[values$rows,
                                            ,
                                            drop = FALSE]
          # COLUMNS
        } else if(length(values$rows) == 0 & length(values$columns) != 0) {
          values$data_active <- values$data[ ,
                                             values$columns,
                                             drop = FALSE]
          # ROWS & COLUMNS
        } else if(length(values$rows) != 0 & length(values$columns) != 0) {
          values$data_active <- values$data[values$rows,
                                            values$columns,
                                            drop = FALSE]
        }
      }
    })

    # ROW INDEX - ROWS IN MASTER COPY
    shiny::observe({
      values$row_index <- nrow(values$data)
    })

    # DATAEDIT - ENTIRE DATASET
    data_update <- dataEditServer(
      "edit1",
      data = shiny::reactive({values$data_active}),
      col_bind = NULL, # endless loop!
      col_edit = col_edit,
      col_options = col_options,
      col_stretch = col_stretch,
      col_names = col_names,
      col_readonly = col_readonly,
      col_factor = col_factor,
      row_bind = NULL, # endless loop!
      row_edit = row_edit,
      row_index = shiny::reactive({values$row_index}), # row_index + 1 for new rows
      quiet = quiet
    )

    # UPDATE ACTIVE DATA
    shiny::observe({
      values$data_active <- data_update()
    })

    # SYNC
    data_sync <- dataSyncServer(
      "sync1",
      data = shiny::reactive(values$data),
      data_subset = shiny::reactive(values$data_active),
      rows = shiny::reactive(values$rows),
      columns = shiny::reactive(values$cols),
      hide = hide,
      hover_text = "synchronise"
    )

    # DATASYNC - ONLY UPDATE MASTER - REMOVE FILTERS FOR DISPLAY
    shiny::observe({
      values$data <- data_sync()
    })

    # DATA OUTPUT - DATA ACTIVE
    dataOutputServer(
      "output-active",
      data = shiny::reactive({values$data_active}),
      save_as = save_as,
      write_fun = write_fun,
      write_args = write_args,
      hide = hide,
      hover_text = "save selection \n to file"
    )

    # DATA OUTPUT - DATA ENTIRE
    dataOutputServer(
      "output-update",
      data = shiny::reactive({values$data}),
      save_as = save_as,
      write_fun = write_fun,
      write_args = write_args,
      hide = hide,
      hover_text = "save to file"
    )

    # CUT
    shiny::observeEvent(input$cut, {
      if(values$cut) {
        values$cut <- FALSE
        shinyBS::updateButton(
          session,
          "cut",
          NULL,
          block = FALSE,
          style = "danger"
        )
      } else {
        values$cut <- TRUE
        shinyBS::updateButton(
          session,
          "cut",
          NULL,
          block = FALSE,
          style = "success"
        )
      }
    })

    # CANCEL
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(NULL)
    })

    # DONE
    shiny::observeEvent(input$done, {

      # HIDDEN INPUTS - SYNC & RETURN
      if(hide == TRUE) {
        if(!is.null(values$data_active) & !is.null(save_as)) {
          do.call(
            write_fun,
            c(list(values$data_active, save_as), write_args)
          )
        }
        shiny::stopApp(values$data_active)
        # VISIBLE INPUTS
      } else {
        # DATA ACTIVE
        if(values$cut) {
          if(!is.null(values$data_active) & !is.null(save_as)) {
            do.call(
              write_fun,
              c(list(values$data_active, save_as), write_args)
            )
          }
          shiny::stopApp(values$data_active)
          # DATA UPDATE
        } else {
          if(!is.null(values$data) & !is.null(save_as)) {
            do.call(
              write_fun,
              c(list(values$data, save_as), write_args)
            )
          }
          shiny::stopApp(values$data)
        }
      }

    })

  }

  # DIALOG
  if(grepl("^d", viewer, ignore.case = TRUE)){
    viewer <- dialogViewer("DataEditR",
                           width = viewer_width,
                           height = viewer_height)
    # BROWSER
  } else if (grepl("^b", viewer, ignore.case = TRUE)) {
    viewer <- shiny::browserViewer()
    # VIEWER PANE
  } else if (grepl("^v", viewer, ignore.case = TRUE) |
             grepl("^p", viewer, ignore.case = TRUE)) {
    viewer <- paneViewer()
    # UNSUPPORTED VIEWER
  } else {
    viewer <- paneViewer()
  }

  # RUN APPLICATION
  x_edit <- shiny::runGadget(ui,
                             server,
                             port=port,
                             viewer = viewer,
                             stopOnCancel = FALSE)


  # RETURN DATA
  if(is.null(x_edit)) {
    return(cancel)
  } else {
    # CODE
    if(is.character(code)) {
      if(!file.exists(code)) {
        file.create(code)
      }
      dput(x_edit, code)
    } else if(code == TRUE) {
      dput(x_edit)
    }
    return(x_edit)
  }

}