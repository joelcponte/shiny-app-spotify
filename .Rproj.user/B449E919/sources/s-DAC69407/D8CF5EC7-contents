header <- function() {
      header <- dashboardHeader()
      anchor <- tags$a(href='http://www.spotify.com',
                       tags$img(src='https://broadwaydollamusic.files.wordpress.com/2012/09/6274-spotify-logo-horizontal-white-rgb.png', height='40'),
                       '')
      
      header$children[[2]]$children <- tags$div(
            tags$head(tags$style(HTML(".name { background-color: black }"))),
            anchor,
            class = 'name')
      
      header$children[[3]]$children[[1]] <- tags$div(style = "padding-left: 40px",
                                                     actionButton('btn_tab_exp', 'Data Exploration', style = "height: 50px; font-family: Fixedsys; background-color: #121212; border: 0; color: #ddd; border-bottom: 3px solid; border-color:#ddd"),
                                                     actionButton('btn_tab_play', 'Playlist Creator', style = "height: 50px; font-family: Fixedsys; background-color: #121212; border: 0; color: #ddd; border-bottom: 3px solid; border-color:#ddd")
                                                     )
      header$children[[3]]$children[[2]] <- NULL
    return(header)
}