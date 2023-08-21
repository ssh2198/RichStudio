library(shiny)
library(tools)
library(readxl)
# add reset button
# automatic column based on col name

uploadTabUI <- function(id, tabName) {
  ns <- NS(id)
  tabItem(tabName = tabName,
    # UPLOAD TAB CONTENTS
    fluidRow(
      column(width = 4, 
        tabBox(title="Upload Files", id='upload_box', width=12,
          # DEG upload panel
          tabPanel("DEG Sets",
            br(),
            fileInput(ns('deg_files'), 'File Input', multiple=TRUE, accept=c('.csv', '.tsv', '.xls', '.txt')),
            helpText("Accepted formats: .txt, .csv, .tsv"),
            selectInput(ns('deg_sep'), "Element separator", c(Comma=",", Space=" ", Tab="\t", "Guess"), selected="Guess"),
            actionButton(ns('upload_deg_file'), "Upload")
          ),
          # Rich Result upload panel
          tabPanel("Enrichment Results",
            br(),
            fileInput(ns('rr_files'), 'File Input', multiple=TRUE, accept=c('.csv', '.tsv', '.xls', '.txt')),
            helpText("Accepted formats: .txt, .csv, .tsv"),
            selectInput(ns('rr_sep'), "Element separator", c(Comma=",", Space=" ", Tab="\t", "Guess"), selected="Guess"),
            actionButton(ns('upload_rr_file'), "Upload")
          )
        ),
        tabBox(title="Text input", id='text_upload', width=12,
          tabPanel("DEG Sets",
            br(),
            textAreaInput(ns('deg_text'), "Text Input", placeholder="Paste list of significant genes"),
            textInput(ns('degtext_name'), "Name", placeholder="Set name for pasted gene list"),
          ),
          tabPanel("Enrichment Results",
          )
        )
      ),
      column(width = 8,
        # Uploaded File View
        h3("File View"),
        tabsetPanel(
          # deg set view tab
          tabPanel("DEG Sets",
            br(),
            fluidRow(
              column(6,
                box(title = "Rename DEG sets", status = "primary", width = 12, collapsible = TRUE,
                  selectInput(ns("rename_deg_select"), "Select DEG set", choices=NULL, multiple=FALSE),
                  textInput(ns("new_deg_name"), "Name", placeholder="New name"),
                  actionButton(ns("rename_deg"), "Update name")
                )
              ),
              column(6,
                box(title = "Remove DEG sets", status = "primary", width=12, collapsible = TRUE,
                  selectInput(ns("remove_deg_select"), "Select DEG sets to remove", choices=NULL, multiple=TRUE),
                  actionButton(ns("remove_deg"), "Remove selection")
                )
              )
            ),
            box(title = "Table view/download", status = "primary", width=12,
              solidHeader = TRUE,
              fluidRow(
                column(4,
                  selectInput(ns('deg_table_select'), "Select DEG set", choices=NULL),
                ),
                column(4,
                  selectInput(ns('deg_export_type'), "Export as", choices=c(".txt", ".csv", ".tsv"))
                )
              ),
              DT::dataTableOutput(ns('deg_table')),
              br(),
              downloadButton(ns("download_deg"), "Download"),
              br()
            )
          ),
          # rich result view tab
          tabPanel("Enrichment Results",
            br(),
            fluidRow(
              column(6,
                box(title = "Rename enrichment results", status = "primary", width = 12, collapsible = TRUE,
                  selectInput(ns("rename_rr_select"), "Select enrichment result", choices=NULL, multiple=FALSE),
                  textInput(ns("new_rr_name"), "Name", placeholder="New name"),
                  actionButton(ns("rename_rr"), "Update name")
                )
              ),
              column(6,
                box(title = "Remove enrichment results", status = "primary", width=12, collapsible = TRUE,
                  selectInput(ns("remove_rr_select"), "Select enrichment results to remove", choices=NULL, multiple=TRUE),
                  actionButton(ns("remove_rr"), "Remove selection")
                )
              )
            ),
            box(title = "Table view/download", status = "primary", width=12,
              solidHeader = TRUE,
              fluidRow(
                column(4,
                  selectInput(ns('rr_table_select'), "Select enrichment result", choices=NULL),
                ),
                column(4,
                  selectInput(ns('rr_export_type'), "Export as", choices=c(".txt", ".csv", ".tsv"))
                )
              ),
              DT::dataTableOutput(ns('rr_table')),
              br(),
              downloadButton(ns("download_rr"), "Download"),
              br()
            )
          ),
          # cluster result view tab
          tabPanel("Cluster Result",
           br(),
           fluidRow(
              column(6,
                box(title = "Rename cluster results", status = "primary", width = 12, collapsible = TRUE,
                  selectInput(ns("rename_clus_select"), "Select cluster result", choices=NULL, multiple=FALSE),
                  textInput(ns("new_clus_name"), "Name", placeholder="New name"),
                  actionButton(ns("rename_clus"), "Update name")
                )
              ),
              column(6,
                box(title = "Remove cluster results", status = "primary", width=12, collapsible = TRUE,
                  selectInput(ns("remove_clus_select"), "Select cluster results to remove", choices=NULL, multiple=TRUE),
                  actionButton(ns("remove_clus"), "Remove selection")
                )
              )
           ),
           box(title = "Table view/download", status = "primary", width=12,
              solidHeader = TRUE,
              fluidRow(
                column(4,
                  selectInput(ns('clus_table_select'), "Select cluster result", choices=NULL),
                ),
                column(4,
                  selectInput(ns('clus_export_type'), "Export as", choices=c(".txt", ".csv", ".tsv"))
                )
              ),
              DT::dataTableOutput(ns('clus_table')),
              br(),
              downloadButton(ns("download_clus"), "Download"),
              br()
           )
          )
        )
      )
    )
  )
}


uploadTabServer <- function(id, u_degnames, u_degdfs, u_rrnames, u_rrdfs, u_clusnames, u_clusdfs, u_cluslists) {
  
  moduleServer(id, function(input, output, session) {
    
    # create reactive objs to make accessible in other modules
    u_degnames_reactive <- reactive(u_degnames$labels) 
    u_degdfs_reactive <- reactive(u_degdfs) 
    u_rrnames_reactive <- reactive(u_rrnames$labels) 
    u_rrdfs_reactive <- reactive(u_rrdfs)
    u_clusnames_reactive <- reactive(u_clusnames$labels)
    u_clusdfs_reactive <- reactive(u_clusdfs)
    u_cluslists_reactive <- reactive(u_cluslists)
    
    # update select inputs based on # file inputs
    observe({
      updateSelectInput(session=getDefaultReactiveDomain(), 'rename_deg_select', choices=u_degnames_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'remove_deg_select', choices=u_degnames_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'deg_table_select', choices=u_degnames_reactive())
      
      updateSelectInput(session=getDefaultReactiveDomain(), 'rename_rr_select', choices=u_rrnames_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'remove_rr_select', choices=u_rrnames_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'rr_table_select', choices=u_rrnames_reactive())
      
      updateSelectInput(session=getDefaultReactiveDomain(), 'rename_clus_select', choices=u_clusnames_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'remove_clus_select', choices=u_clusnames_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'clus_table_select', choices=u_clusnames_reactive())
    })
    
    
    # <!----- DEG FILE MANAGEMENT -----!>
    # when deg upload button clicked
    observeEvent(input$upload_deg_file, {
      req(input$deg_files) # Make sure file uploaded
      
      for (i in seq_along(input$deg_files$name)) {
        lab <- input$deg_files$name[i]
        
        # read file based on file extension
        ext <- tools::file_ext(input$deg_files$name[i])
        df <- switch(ext,
          csv = read.csv(input$deg_files$datapath[i]),
          tsv = read.delim(input$deg_files$datapath[i]),
          txt = read.csv(input$deg_files$datapath[i], sep=input$deg_sep),
          xls = readxl::read_excel(input$deg_files$datapath[i])
        )
        
        u_degdfs[[lab]] <- df # set u_degdfs
        u_degnames$labels <- c(u_degnames$labels, lab) # set u_degnames 
      }
      
    })
    
    # when rename deg button clicked
    observeEvent(input$rename_deg, {
      req(input$rename_deg_select)
      req(input$new_deg_name)
      
      new_name <- input$new_deg_name
      old_name <- input$rename_deg_select
      
      u_degnames$labels <- c(u_degnames$labels, new_name)
      u_degnames$labels <- setdiff(u_degnames$labels, old_name) # remove old name
      
      u_degdfs[[new_name]] <- u_degdfs[[old_name]]
      u_degdfs <- setdiff(names(u_degdfs), old_name)
    })
    
    # when remove deg button clicked
    observeEvent(input$remove_deg, {
      req(input$remove_deg_select) # Make sure DEG selected
      
      # remove selected files from u_rrdfs and u_rrnames 
      u_degdfs <- setdiff(names(u_degdfs), input$remove_deg_select)
      u_degnames$labels <- setdiff(u_degnames$labels, input$remove_deg_select)
    })
    
    # reactively update which deg table is read based on selection
    deg_to_table <- reactive ({
      req(input$deg_table_select)
      df <- u_degdfs[[input$deg_table_select]]
      return(df)
    })
    # output deg table
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
          write.table(u_degdfs[[input$deg_table_select]], file, sep=' ', row.names=FALSE)
        } else if (ext_type == ".csv") {
          write.csv(u_degdfs[[input$deg_table_select]], file, row.names=FALSE)
        } else if (ext_type == ".tsv") {
          write.table(u_degdfs[[input$deg_table_select]], file, sep='\t', row.names=FALSE)
        }
      }
    )
    
    
    # <!----- RICH RESULT FILE MANAGEMENT -----!>
    # when rich result upload button clicked
    observeEvent(input$upload_rr_file, {
      req(input$rr_files) # Make sure file uploaded
      
      for (i in seq_along(input$rr_files$name)) {
        lab <- input$rr_files$name[i]
        
        # read file based on file extension
        ext <- tools::file_ext(input$rr_files$name[i])
        df <- switch(ext,
          csv = read.csv(input$rr_files$datapath[i]),
          tsv = read.delim(input$rr_files$datapath[i]),
          txt = read.csv(input$rr_files$datapath[i], sep=input$rr_sep),
          xls = readxl::read_excel(input$rr_files$datapath[i])
        )
        
        u_rrdfs[[lab]] <- df # set u_rrdfs
        u_rrnames$labels <- c(u_rrnames$labels, lab) # set u_rrnames 
      }
      
    })
    
    # when rename rr button clicked
    observeEvent(input$rename_rr, {
      req(input$rename_rr_select)
      req(input$new_rr_name)
      
      new_name <- input$new_rr_name
      old_name <- input$rename_rr_select
      
      u_rrnames$labels <- c(u_rrnames$labels, new_name)
      u_rrnames$labels <- setdiff(u_rrnames$labels, old_name) # remove old name
      
      u_rrdfs[[new_name]] <- u_rrdfs[[old_name]]
      u_rrdfs <- setdiff(names(u_rrdfs), old_name)
    })
    
    # when remove rr button clicked
    observeEvent(input$remove_rr, {
      req(input$remove_rr_select) # Make sure DEG selected
      
      # remove selected files from u_rrdfs and u_rrnames 
      u_rrdfs <- setdiff(names(u_rrdfs), input$remove_rr_select)
      u_rrnames$labels <- setdiff(u_rrnames$labels, input$remove_rr_select)
    })
    
    # reactively update which rr table is read based on selection
    rr_to_table <- reactive ({
      req(input$rr_table_select)
      df <- u_rrdfs[[input$rr_table_select]]
      return(df)
    })
    
    # output rr table
    output$rr_table = DT::renderDataTable({
      rr_to_table()
    })
    
    # download rr
    output$download_rr <- downloadHandler(
      filename = function() {
        req(input$rr_table_select)
        paste0(input$rr_table_select, input$rr_export_type)
      },
      content = function(file) {
        ext_type <- input$rr_export_type
        if (ext_type == ".txt") {
          write.table(u_rrdfs[[input$rr_table_select]], file, sep=' ', row.names=FALSE)
        } else if (ext_type == ".csv") {
          write.csv(u_rrdfs[[input$rr_table_select]], file, row.names=FALSE)
        } else if (ext_type == ".tsv") {
          write.table(u_rrdfs[[input$rr_table_select]], file, sep='\t', row.names=FALSE)
        }
      }
    )
    
    
    # <!----- CLUSTER RESULT LOGIC -----!>
    # when rename cluster result button clicked
    observeEvent(input$rename_clus, {
      req(input$rename_clus_select)
      req(input$new_clus_name)
      
      new_name <- input$new_clus_name
      old_name <- input$rename_clus_select
      
      # update clusnames
      u_clusnames$labels <- c(u_clusnames$labels, new_name)
      u_clusnames$labels <- setdiff(u_clusnames$labels, old_name) # remove old name
      # update clusdfs
      u_clusdfs[[new_name]] <- u_clusdfs[[old_name]]
      u_clusdfs <- setdiff(names(u_clusdfs), old_name)
      # update cluslists
      u_cluslists[[new_name]] <- u_cluslists[[old_name]]
      u_cluslists <- setdiff(names(u_cluslists), old_name)
    })
    
    # when remove cluster result button clicked
    observeEvent(input$remove_clus, {
      req(input$remove_clus_select) # Make sure cluster result selected
      
      # remove selected files from u_clusdfs, u_cluslists, u_clusnames 
      u_clusdfs <- setdiff(names(u_clusdfs), input$remove_clus_select)
      u_cluslists <- setdiff(names(u_cluslists), input$remove_clus_select)
      u_clusnames$labels <- setdiff(u_clusnames$labels, input$remove_clus_select)
    })
    
    # reactively update which cluster table is read based on selection
    clus_to_table <- reactive ({
      req(input$clus_table_select)
      df <- u_clusdfs[[input$clus_table_select]]
      return(df)
    })
    
    # output cluster result table
    output$clus_table = DT::renderDataTable({
      clus_to_table()
    })
    
    # download cluster result
    output$download_clus <- downloadHandler(
      filename = function() {
        req(input$clus_table_select)
        paste0(input$clus_table_select, input$clus_export_type)
      },
      content = function(file) {
        ext_type <- input$clus_export_type
        if (ext_type == ".txt") {
          write.table(u_clusdfs[[input$clus_table_select]], file, sep=' ', row.names=FALSE)
        } else if (ext_type == ".csv") {
          write.csv(u_clusdfs[[input$clus_table_select]], file, row.names=FALSE)
        } else if (ext_type == ".tsv") {
          write.table(u_clusdfs[[input$clus_table_select]], file, sep='\t', row.names=FALSE)
        }
      }
    )
    
  })
  
}