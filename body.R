body <- function() {
      
      dashboardBody(
            useShinyjs(),
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
            tabItems(
                  # First tab content
                  tabItem(tabName = "pag1",
                          fluidRow( title = "Add Artists",
                                column(width = 1),
                                box(  title = "Running Playlist",
                                      div(style = 'overflow-y: auto; overflow-x: auto', dataTableOutput("playlist_to_run")),
                                      width = 5
                                ),
                                
                                box(  title = "Relaxing Playlist",
                                      div(style = 'overflow-y: auto; overflow-x: auto', dataTableOutput("playlist_to_relax")),
                                      width = 5
                                )
                          )
                  ),
                  # Second tab content
                  tabItem(tabName = "pag2",
                          uiOutput("data_exp")
                  )
            )
      )
}