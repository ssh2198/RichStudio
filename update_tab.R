
updateTabUI <- function(id, tabName) {
  ns <- NS(id)
  tabItem(tabName = tabName,
    fluidRow(
      column(width = 6,
        box(title="Rename", status="primary", width=NULL, solidHeader=TRUE,
          selectInput(ns("rename_deg_select"), "Select DEG set", choices=NULL, multiple=FALSE),
          textInput(ns("new_deg_name"), "Name", placeholder="New DEG set name"),
          hr(),
          # rename enrichment results
          selectInput(ns("rename_rr_select"), "Select enrichment result", choices=NULL, multiple=FALSE),
          textInput(ns("new_rr_name"), "Name", placeholder="New enrichment result name"),
          hr(),
          # rename cluster results
          selectInput(ns("rename_clus_select"), "Select cluster result", choices=NULL, multiple=FALSE),
          textInput(ns("new_clus_name"), "Name", placeholder="New cluster result name"),
          hr(),
          br(),
          actionButton(ns('rename_btn'), "Rename selection")
        )
      ),
      column(width = 6,
        box(title="Remove", status="danger", width=NULL, solidHeader=TRUE,
          selectInput(ns("remove_deg_select"), "Select DEG sets to remove", choices=NULL, multiple=TRUE),
          selectInput(ns("remove_rr_select"), "Select enrichment results to remove", choices=NULL, multiple=TRUE),
          selectInput(ns("remove_clus_select"), "Select cluster results to remove", choices=NULL, multiple=TRUE),
          br(),
          actionButton(ns('remove_btn'), "Remove selection")
        )
      )
    )
  )
}


updateTabServer <- function(id, u_degnames, u_degdfs, u_rrnames, u_rrdfs, u_clusnames, u_clusdfs, u_cluslists) {
  
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
      
      updateSelectInput(session=getDefaultReactiveDomain(), 'rename_rr_select', choices=u_rrnames_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'remove_rr_select', choices=u_rrnames_reactive())
      
      updateSelectInput(session=getDefaultReactiveDomain(), 'rename_clus_select', choices=u_clusnames_reactive())
      updateSelectInput(session=getDefaultReactiveDomain(), 'remove_clus_select', choices=u_clusnames_reactive())
    })
    
    # <!----- EDIT FILES -----!>
    # Rename files
    observeEvent(input$rename_btn, {
      # rename deg
      new_name <- input$new_deg_name
      old_name <- input$rename_deg_select
      if (nchar(new_name) > 0 && nchar(old_name) > 0) {
        u_degnames$labels <- c(u_degnames$labels, new_name)
        u_degnames$labels <- setdiff(u_degnames$labels, old_name) # remove old name
        
        u_degdfs[[new_name]] <- u_degdfs[[old_name]]
        u_degdfs <- setdiff(names(u_degdfs), old_name)
      }
      
      # rename rr
      new_name <- input$new_rr_name
      old_name <- input$rename_rr_select
      if (nchar(new_name) > 0 && nchar(old_name) > 0) {
        u_rrnames$labels <- c(u_rrnames$labels, new_name)
        u_rrnames$labels <- setdiff(u_rrnames$labels, old_name) # remove old name
        
        u_rrdfs[[new_name]] <- u_rrdfs[[old_name]]
        u_rrdfs <- setdiff(names(u_rrdfs), old_name)
      }
      
      # rename clus
      new_name <- input$new_clus_name
      old_name <- input$rename_clus_select
      if (nchar(new_name) > 0 && nchar(old_name) > 0) {
        # update clusnames
        u_clusnames$labels <- c(u_clusnames$labels, new_name)
        u_clusnames$labels <- setdiff(u_clusnames$labels, old_name) # remove old name
        
        # update clusdfs
        u_clusdfs[[new_name]] <- u_clusdfs[[old_name]]
        u_clusdfs <- setdiff(names(u_clusdfs), old_name)
        
        # update cluslists
        u_cluslists[[new_name]] <- u_cluslists[[old_name]]
        u_cluslists <- setdiff(names(u_cluslists), old_name) 
      }
    })
    
    # Remove files
    observeEvent(input$remove_btn, {
      if (!is.null(input$remove_deg_select)) {
        u_degdfs <- setdiff(names(u_degdfs), input$remove_deg_select)
        u_degnames$labels <- setdiff(u_degnames$labels, input$remove_deg_select)
      }
      if (!is.null(input$remove_rr_select)) {
        u_rrdfs <- setdiff(names(u_rrdfs), input$remove_rr_select)
        u_rrnames$labels <- setdiff(u_rrnames$labels, input$remove_rr_select)
      }
      if (!is.null(input$remove_clus_select)) {
        u_clusdfs <- setdiff(names(u_clusdfs), input$remove_clus_select)
        u_cluslists <- setdiff(names(u_cluslists), input$remove_clus_select)
        u_clusnames$labels <- setdiff(u_clusnames$labels, input$remove_clus_select)
      }
    })
    
  })
  
}