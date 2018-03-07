library(tidyverse)
library(httr)
library(stringr)
library(lubridate)
library(plotly)
library(shiny)
library(shinydashboard)
library(caret)
library(reshape2)
library(formattable)
library(data.table)
library(plotly)
library(highcharter)
library(RColorBrewer)
library(htmltools)
library(shinyjs)
source("spotify_functions.R")
source("loading_button.R")


artist_inputUI <- function(id) {
      
      ns = NS(id)
      
      uiOutput(ns("artist_input"))
      
      tagList(
            tags$div(style = "padding: 0cm", textInput(ns("artist"), "Artist")),
            tags$div(style = "padding: 0cm", withBusyIndicatorUI(actionButton(ns("search_artist"), "Search", icon = icon("spotify"))))
      )
      
}


artist_input <- function(input, output, session) {
      
      ns = session$ns
      
      
      artist_list <- eventReactive(input$search_artist, {
            withBusyIndicatorServer(ns("search_artist"), {
                  if (nchar(input$artist) > 0) 
                        return(get_artists(input$artist, token))
            })
      })
      return(artist_list)
}

artist_chooseUI  <- function(id) {
      
      ns = NS(id)
      
      uiOutput(ns("pick_artist"))
}


artist_choose <- function(input, output, session, artist_list = NULL) {
            
      ns = session$ns
      
      output$pick_artist <- renderUI({

            if(!is.null(artist_list())) {
                  
              
                  artists = artist_list()$artist_uri %>% simplify %>% as.list
                  names(artists) = artist_list()$artist_name
                  list(br(),
                       selectInput(inputId = ns("chosen_artist"), 
                                   label = "Results", 
                                   choices = artists),
                             div(style="display:inline-block",withBusyIndicatorUI(actionButton(ns("search_albums2"), "Choose Albums"))),
                             div(style="display:inline-block",withBusyIndicatorUI(actionButton(ns("search_tracks_all_albums"), "Get All Tracks")))

                  )
                  
            }
      })
      selected_artist <- reactive(artist_list() %>% filter(artist_uri == input$chosen_artist))
            
      observe(print(input$asdasd))
      
      return(selected_artist)
      
}

choose_artist_input <- function(input, output, session) {
      return(input)
}

artist_img_printUI  <- function(id) {
      
      ns = NS(id)
      
      uiOutput(ns("print_artist_img"))
}


artist_img_print <- function(input, output, session, artist_list = NULL, choose_artist_input = NULL) {

      script = "$('.img_asd').each(function () {
        var imgwidth = $(this).width();
        var imgheight = $(this).height();

        if (imgwidth > imgheight) {
            $(this).css('width', 'auto');
            $(this).css('height', '230px');
        }
        else {
                $(this).css('width', '230px');
                $(this).css('height', 'auto');
            }
    });"

      
      output$print_artist_img <- renderUI({
            validate(need(choose_artist_input$chosen_artist, F))
            if(!is.null(choose_artist_input$chosen_artist) & !is.null(artist_list())) {
                              img_link = artist_list() %>% filter(artist_uri == choose_artist_input$chosen_artist) %>% select(artist_img) %>% simplify()
                              if(length(img_link)>0) {
                                    tags$div(style = "width: 230px; height: 230px; position: relative; overflow: hidden; margin: 0px; border-radius: 50%;",tags$img(class = "img_asd", style = "display: inline; margin: 0;", id = paste0("artist_image_sidebar_", choose_artist_input$chosen_artist), onload = script,  src = img_link, width = "50%", align = "middle"))
                              }
            } else{
                  print("No artists found.")
            }
      })

}


pick_albumsUI  <- function(id) {
      
      ns = NS(id)
      
      uiOutput(ns("albums_list"))
}


pick_albums <- function(input, output, session, selected_artist = NULL, choose_artist_input) {
      
      ns = session$ns
      
      albums_list = reactiveValues(df = NULL)
      
      observe({
            print(choose_artist_input$search_albums2)
      })
      
      observe({
            print(choose_artist_input$search_tracks_all_albums)
      })
      
      observeEvent({
                  choose_artist_input$search_tracks_all_albums
      },{
            if(is.null(selected_artist())) return()
            albums_list$df = get_albums(selected_artist()$artist_uri, token)
      })
            
      
      #       
      observeEvent({
            choose_artist_input$search_albums2
      },{
            if(is.null(selected_artist())) return()
            albums_list$df = get_albums(selected_artist()$artist_uri, token)
      })
      
      
      output$albums_list <- renderUI({
            if(!is.null(albums_list$df)) {
                  if(choose_artist_input$search_albums2 >0) {
                        albums = albums_list$df %>% data.frame() %>% select(album_name) %>% as_vector() %>% as.character()
                        list(
                              selectizeInput(inputId = ns("chosen_albums"), 
                                             label = "Choose an album:", 
                                             choices = albums,
                                             multiple = T,
                                             selected = albums,
                                             options = list('plugins' = list('remove_button'))),
                              withBusyIndicatorUI(actionButton(ns("select_albums"), "Select"))
                        )
                  }
            }
      })
      
      return(reactive(albums_list$df))
}

pick_albums_input <- function(input, output, session) {
      return(input)
}


tracks_list <- function(input, output, session, chosen_artist = NULL, albums_list = NULL, choose_artist_input, pick_albums_input, spotify) {

      ns = session$ns
      
            tracks = reactiveValues()
      
            observeEvent(pick_albums_input$select_albums, {
                  withBusyIndicatorServer("add-alb-select_albums", {
                        tracks$df = rbind(spotify$df, get_tracks(chosen_artist(), albums_list() %>% filter(album_name  %in% pick_albums_input$chosen_albums), token) %>% data.frame)
                  })
            })
            
            observeEvent(choose_artist_input$search_tracks_all_albums, {
                  withBusyIndicatorServer("add-choo-search_tracks_all_albums", {
                        tracks$df = rbind(spotify$df, get_tracks(chosen_artist(), albums_list(), token) %>% data.frame)
                  })
            })
            
      return(tracks)
}


module_server <- function(input, output, session) {
      
      spotify_df = reactiveValues()
      
      artist_list <- callModule(artist_input, "art")
      chosen_artist <- callModule(artist_choose, "choo", artist_list)
      ca_setup <- callModule(choose_artist_input, "choo")
      callModule(artist_img_print, "img", artist_list, ca_setup)
      selected_albums <- callModule(pick_albums, "alb", chosen_artist, ca_setup)
      spotify_df$df <- callModule(tracks_list, "fin", chosen_artist, selected_albums)
      
      return(spotify_df)
}


moduleUI <- function(id) {
      ns <- NS(id)
      
      tagList(
      artist_inputUI(ns("art")),
      br(),
      artist_chooseUI(ns("choo")),
      br(),
      artist_img_printUI(ns("img")),
      br(),
      pick_albumsUI(ns("alb"))
      )
}

delete_artist <- function(input, output, session, spotify) {
      
      ns = session$ns 
      
      artists = reactive({
            if (is.null(unique(spotify$df$artist_name))) {
                  return("")
            } else {
                  return(unique(spotify$df$artist_name))
            }
      })
      
      
      
      output$delete_UI <- renderUI({
            list(
                  selectizeInput(inputId = ns("artists_to_keep"), 
                                 label = "Selected Artists", 
                                 choices = artists(),
                                 multiple = T,
                                 selected = artists(),
                                 options = list('plugins' = list('remove_button'))),
                  actionButton(ns("filter_artists"), "Filter")
            )
      })
      
      
      return(input)
      
}

delete_artistUI <- function(id) {
      ns <- NS(id)
      
      uiOutput(ns("delete_UI"))
}

all_imgs_print <- function(input, output, session, spotify) {
      
      output$print_artist_img <- renderUI({
            if(!is.null(spotify)) {
                  lapply(unique(spotify$df$album_img), function(x) {
                             tags$img(src = x, style = "float: left; margin-right = 1%; margin-bottom = 0.5em", width = "24%", align = "middle")
                        }
                  )
            }
      })
      
      reactive({
            spotify
      })
      
}

all_imgs_printUI  <- function(id) {
      
      ns = NS(id)
      
      uiOutput(ns("print_artist_img"))
}


add_artist <- function(input, output, session, df_old) {
      
      spotify <- reactiveValues(df = structure(list(danceability = numeric(0), energy = numeric(0), 
                                                       key = numeric(0), loudness = numeric(0), mode = numeric(0), 
                                                       speechiness = numeric(0), acousticness = numeric(0), instrumentalness = numeric(0), 
                                                       liveness = numeric(0), valence = numeric(0), tempo = numeric(0), 
                                                       track_uri = character(0), duration_ms = numeric(0), time_signature = numeric(0), 
                                                       album_uri = character(0), track_number = numeric(0), track_name = character(0), 
                                                       album_name = character(0), album_img = character(0), album_release_date = character(0), 
                                                       album_release_year = numeric(0), artist_img = character(0)), .Names = c("danceability", 
                                                                                                                               "energy", "key", "loudness", "mode", "speechiness", "acousticness", 
                                                                                                                               "instrumentalness", "liveness", "valence", "tempo", "track_uri", 
                                                                                                                               "duration_ms", "time_signature", "album_uri", "track_number", 
                                                                                                                               "track_name", "album_name", "album_img", "album_release_date", 
                                                                                                                               "album_release_year", "artist_img"), row.names = integer(0), class = "data.frame"))
      
      artist_list <- callModule(artist_input, "art")
      chosen_artist <- callModule(artist_choose, "choo", artist_list)
      ca_setup <- callModule(choose_artist_input, "choo")
      callModule(artist_img_print, "img", artist_list, ca_setup)
      selected_albums <- callModule(pick_albums, "alb", chosen_artist, ca_setup)
      
      
      pa_setup <- callModule(pick_albums_input, "alb")
      spotify <- callModule(tracks_list, "fin", chosen_artist, selected_albums, ca_setup, pa_setup, spotify)
      
      delete <- callModule(delete_artist, "temp", spotify)
      
      callModule(all_imgs_print, "all_imgs", spotify)
      
      observeEvent(delete$filter_artists, {
            if (is.null(delete$artists_to_keep)) {
                  spotify$df = spotify$df[0,]
            } else {
                  spotify$df = spotify$df %>% filter(artist_name %in% delete$artists_to_keep)
            }
      })
      
      return(spotify)
      
}



add_artistUI <- function(id) {
      ns <- NS(id)
      
      tagList(
            box(title = "Pick Artists",
                  artist_inputUI(ns("art")),
                  artist_chooseUI(ns("choo")),
                  br(),
                  artist_img_printUI(ns("img")),
                  br(),
                  pick_albumsUI(ns("alb")),
            width = 12),
            box(
                  delete_artistUI(ns("temp")),
                  br(),
                  all_imgs_printUI(ns("all_imgs")),
                  width = 12
            )
      )
}



add_artist_sidebarUI <- function(id) {
      ns <- NS(id)
      
      tagList(
            box(title = "Pick Artists",
                artist_inputUI(ns("art")),
                artist_chooseUI(ns("choo")),
                br(),
                artist_img_printUI(ns("img")),
                br(),
                pick_albumsUI(ns("alb")),
                width = 12),
            box(  style = "padding-bottom: 50px",
                  delete_artistUI(ns("temp")),
                  br(),
                  all_imgs_printUI(ns("all_imgs")),
                  width = 12
            ),
            tags$div(class = "footnote", tags$div("Created by Joel Ponte"), tags$div("joelcponte@gmail.com"), align = "center")
      )
}
