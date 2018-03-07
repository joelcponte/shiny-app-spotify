sidebar <- function() {
      sidebar <- dashboardSidebar(
            sidebarMenu(id = "tabs",
                  tags$div(style = "display: none;", menuItem("Data Exploration", tabName = "pag2", icon = icon("dashboard"))),
                  tags$div(style = "display: none;", menuItem("Playlist Creator",tabName = "pag1", icon = icon("th"))),
                  tags$div(style = "margin-top: 16px", add_artist_sidebarUI("add"))
            )
      )
      return(sidebar)
}