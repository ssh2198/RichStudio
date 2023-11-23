
uploadDegTabUI <- function(id, tabName) {
  ns <- NS(id)
  tabItem(tabName = tabName,
    # UPLOAD TAB CONTENTS
    useShinyjs(),
    column(width = 4,
      fluidRow( 
          box(title=span(icon("upload"), "File upload"), status="primary", solidHeader=TRUE, width=NULL,
          helpText("Accepted formats: .txt, .csv, .tsv"),      
          fileInput(ns('deg_files'), 'Select files', multiple=TRUE, accept=c('.csv', '.tsv', '.xls', '.txt')),
        )
      ),
      fluidRow(
        box(title=span(icon("pencil"), "Text input"), status="primary", solidHeader=TRUE, width=NULL,
          textAreaInput(ns('deg_text'), "Text Input", placeholder="Paste list of significant genes or dataframe-like object"),
          textInput(ns('degtext_name'), "Name", placeholder="Set name for pasted DEG set"),
          actionButton(ns('upload_degtext'), "Upload")
        )
      ),
    ),
    column(width = 8,
      shinyjs::hidden(tags$div(
        id=ns("deglist_box"),
        box(title="Uploaded files", status="primary", solidHeader=TRUE, width=NULL,
          DT::DTOutput(ns('deg_list_table')),
          actionButton(ns('remove_btn'), "Delete")
        ))
      ), 
      box(title="View DEG sets", status="primary", solidHeader=TRUE, width=NULL,
        fluidRow(
          column(4,
            selectInput(ns('deg_table_select'), "Select DEG set", choices=NULL),
          ),
          column(4,
            selectInput(ns('deg_export_type'), "Export as", choices=c(".txt", ".csv", ".tsv"))
          )
        ),
        downloadButton(ns("download_deg"), "Download"),
        br(),
        br(),
        br(),
        DT::dataTableOutput(ns('deg_table'))
      )
    )
  )
}


uploadDegTabServer <- function(id, u_degnames, u_degdfs, u_big_degdf) {
  
  moduleServer(id, function(input, output, session) {
    # create reactive objs to make accessible in other modules
    u_degnames_reactive <- reactive(u_degnames$labels) 
    u_degdfs_reactive <- reactive(u_degdfs)
    
    # create reactive to store dataframe of uploaded files
    u_big_degdf_reactive <- reactive(u_big_degdf)
    
    # update select inputs based on # file inputs
    observe({
      updateSelectInput(session=getDefaultReactiveDomain(), 'deg_table_select', choices= u_degnames_reactive())
    })
    
    # when deg upload button clicked
    observeEvent(input$deg_files, {
      for (i in seq_along(input$deg_files$name)) {
        lab <- input$deg_files$name[i]
        
        ext <- tools::file_ext(input$deg_files$name[i])
        path <- input$deg_files$datapath[i]
        # try to read file as csv
        csv_ncol <- tryCatch({
          csvdf <- read.csv(path)
          ncol(csvdf)
        }, error = function(err) {
          0
        })
        # try to read file as tsv
        tsv_ncol <- tryCatch({
          tsvdf <- read.delim(path)
          ncol(tsvdf)
        }, error = function(err) {
          0
        })
        # decide which df to store
        if (tsv_ncol == 0 || csv_ncol > tsv_ncol) {
          df <- read.csv(path)
        } else {
          df <- read.delim(path)
        }
        
        u_degdfs[[lab]] <- df # set u_degdfs
        u_degnames$labels <- c(u_degnames$labels, lab) # set u_degnames 
        print("does this work?")
        u_big_degdf[['df']] <- add_file_degdf(u_big_degdf[['df']], lab, df)
        print("yes!")
      }
      
      # Show file list
      if (!is.null(u_big_degdf[['df']])) {
        shinyjs::show("deglist_box")
        print("showing...")
      }
      
    })
    
    # Parse pasted text inputs
    observeEvent(input$upload_degtext, {
      req(input$deg_text, input$degtext_name)
      x <- strsplit(input$deg_text, "[^[:alnum:]]+")
      df <- data.frame(GeneID = x)
      colnames(df) <- c("GeneID")
      lab <- input$degtext_name
      
      u_degdfs[[lab]] <- df # set u_degdfs
      u_degnames$labels <- c(u_degnames$labels, lab)
      u_big_degdf[['df']] <- add_file_degdf(u_big_degdf[['df']], lab, df)
      
      # Show file list
      if (!is.null(u_big_degdf[['df']])) {
        shinyjs::show("deglist_box")
        print("showing...")
      }
      
    })

    # Reactively update uploaded file dataframe
    big_degdf_to_table <- reactive({
      u_big_degdf[['df']]
    })
    # Output uploaded file table
    output$deg_list_table = DT::renderDT(
      big_degdf_to_table(), editable='cell'
    )
    # Code from https://github.com/rstudio/DT/pull/480
    proxy = dataTableProxy('deg_list_table')
    observeEvent(input$deg_list_table_cell_edit, {
      info = input$deg_list_table_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      print(info$col)
      
      # Rename deg if changing a value in column 1 (name col)
      if (info$col == 1) {
        new_name <- v
        old_name <- u_big_degdf[['df']][i, j]
        if (nchar(new_name) > 0 && nchar(old_name) > 0) {
          u_degnames$labels <- c(u_degnames$labels, new_name)
          u_degnames$labels <- setdiff(u_degnames$labels, old_name) # remove old name
          
          u_degdfs[[new_name]] <- u_degdfs[[old_name]]
          u_degdfs <- setdiff(names(u_degdfs), old_name)
        }
      }
      
      u_big_degdf[['df']][i, j] <<- DT::coerceValue(v, u_big_degdf[['df']][i, j])
    })
    
    # Remove deg from uploaded degs
    observeEvent(input$remove_btn, {
      req(input$deg_list_table_rows_selected)
      
      # Also remove from degdfs and degnames
      for (deg in input$deg_list_table_rows_selected) {
        deg_to_rm <- u_big_degdf[['df']][deg, ]
        deg_to_rm <- deg_to_rm$name
        
        u_degdfs <- setdiff(names(u_degdfs), deg_to_rm)
        u_degnames$labels <- setdiff(u_degnames$labels, deg_to_rm)
      }
      
      # Remove selection from u_big_degdf
      rm_vec <- u_big_degdf[['df']][input$deg_list_table_rows_selected, ]
      u_big_degdf[['df']] <- rm_file_degdf(u_big_degdf[['df']], rm_vec)
      
      if (is.null(u_big_degdf[['df']])) {
        shinyjs::hide("deglist_box")
        print("hide")
      }
    })
    
    # Reactively update which deg table is read based on selection
    deg_to_table <- reactive ({
      req(input$deg_table_select)
      df <- u_degdfs[[input$deg_table_select]]
      #df <- round_tbl(df, 3)
      return(df)
    })
    
    # Output individual deg preview table
    output$deg_table = DT::renderDataTable({
      deg_to_table()
    })
    
    # download deg
    output$download_deg <- downloadHandler(
      filename = function() {
        req(input$deg_table_select)
        paste0(input$deg_table_select, input$deg_export_type)
      },
      content = function(file) {
        ext_type <- input$deg_export_type
        if (ext_type == ".txt") {
          write.table(u_degdfs[[input$deg_table_select]], file, sep='\t', row.names=FALSE)
        } else if (ext_type == ".csv") {
          write.csv(u_degdfs[[input$deg_table_select]], file, row.names=FALSE)
        } else if (ext_type == ".tsv") {
          write.table(u_degdfs[[input$deg_table_select]], file, sep='\t', row.names=FALSE)
        }
      }
    )
    
    
  })
  
}