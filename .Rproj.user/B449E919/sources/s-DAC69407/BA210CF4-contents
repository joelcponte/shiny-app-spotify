

server <- function(input, output, session) {
      
      spotify_df <- callModule(add_artist, "add")
      
      model_run <- readRDS("logistic_regression_chill_run")
      
      observeEvent(input$btn_tab_play, {
            updateTabItems(session, "tabs", "pag1")
      })
      
      observeEvent(input$btn_tab_exp, {
            updateTabItems(session, "tabs", "pag2")
      })
      
      
      output$similar_table <- renderDataTable({
            validate(need(spotify_df$df, message = FALSE))
            if(nrow(spotify_df$df)>0) {
                  features = c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness','instrumentalness', 'liveness', 'valence', 'tempo', 'time_signature')
                  x = spotify_df$df
                  x = x[,features]
                  x = predict(preProcess(x, method = c("center", "scale")), x)
                  dd = dist(x)
                  dd = as.matrix(dd)
                  dd = melt(dd)
                  dd = dd %>% filter(Var1>Var2)
                  dd$track_1 = spotify_df$df[dd$Var1,]$track_name
                  dd$album_1 = spotify_df$df[dd$Var1,]$album_name
                  dd$track_2 = spotify_df$df[dd$Var2,]$track_name
                  dd$album_2 = spotify_df$df[dd$Var2,]$album_name
                  dd$Var1 = NULL
                  dd$Var2 = NULL
                  dd = dd %>% arrange(value)
                  names(dd) = c("Songs distance", "Track 1", "Album 1", "Track 2", "Album 2")
                  dd$`Songs distance` = sprintf("%.2f", dd$`Songs distance`) %>% as.numeric()
                  data.table(dd)
                  # formattable(dd, list(area(col = "Indice de diferenca") ~ normalize_bar("pink", 0.2)))
            }
            
      })
      
      
      output$playlist_to_run <- renderDataTable({
            validate(need(spotify_df$df, message = FALSE))
            if(nrow(spotify_df$df)>0) {
                  features = c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness','instrumentalness', 'liveness', 'valence', 'tempo', 'time_signature')
                  run_index = sprintf("%.2f", predict(model_run, spotify_df$df, type = "prob")[,2]) %>% as.numeric()
                  temp = cbind(spotify_df$df, run_index = run_index)
                  temp = temp[temp$run_index > 0.87,]
                  temp %>% 
                        arrange(desc(run_index)) %>%
                        select(track_name, album_name, artist_name) %>%
                        data.table()
            }
            
      })
      
      
      output$playlist_to_relax <- renderDataTable({
            validate(need(spotify_df$df, message = FALSE))
            if(nrow(spotify_df$df)>0) {
                  features = c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness','instrumentalness', 'liveness', 'valence', 'tempo', 'time_signature')
                  run_index = sprintf("%.2f", predict(model_run, spotify_df$df, type = "prob")[,2]) %>% as.numeric()
                  temp = cbind(spotify_df$df, run_index = run_index)
                  temp = temp[temp$run_index < 0.20,]
                  temp %>% 
                        arrange(run_index) %>%
                        select(track_name, album_name, artist_name) %>%
                        data.table()
            }
            
      })
      
      
      output$plot_2dpca <- renderHighchart({
            validate(need(spotify_df$df, message = FALSE))

            if(nrow(spotify_df$df)>0) {
                  features = c("danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo")
                  
                  pca = prcomp(spotify_df$df[,features], scale = T)
                  
                  
                  var_explained = sum((pca$sdev^2)[1:2])/sum(pca$sdev^2)
                  
                  rotation = pca$rotation %>% data.frame
                  
                  rotation$names = row.names(pca$rotation)
                  
                  df_plot = cbind(pca$x, spotify_df$df[,c("artist_name", "track_name", "album_name", "album_release_date", "album_img")])
                  
                  
                  pcs = rotation %>%
                        rownames_to_column("name") %>%
                        select(PC1, PC2, name) %>%
                        mutate(PC1 = 10*PC1, PC2 = 10*PC2) %>%
                        group_by(name) %>%
                        do(data = list(c(0,0), c(.$PC1, .$PC2)), marker = list(enabled = F), showInLegend = F, tooltip = "") %>%
                        list_parse()
                  
                  
                  pcs_labels = rotation %>%
                        rownames_to_column("name") %>%
                        select(PC1, PC2, name) %>%
                        mutate(PC1 = 10*PC1, PC2 = 10*PC2) %>%
                        group_by(name) %>%
                        do(label = .$name, data = list(c(.$PC1, .$PC2)), dataLabels = list(enabled = T, format = '{series.name}'), marker = list(symbol = "diamond"), showInLegend = F, tooltip = "") %>%
                        list_parse()
                  
                  
                  
                  df_plot = df_plot %>%
                        mutate(tooltip = paste0('<a style = "margin-right:', max(max(nchar(track_name), nchar(album_name)) * 7, 55), 'px">', # dynamic sizing
                                                '<b>Album:</b> ', album_name,
                                                '<br><b>Track:</b> ', track_name))
                  
                  grouped_by = ifelse(n_distinct(df_plot$artist_name)>1, "artist_name", "album_name")
                  
                  names(df_plot)[names(df_plot) == grouped_by] = "grouped_by"
                  
                  set.seed(10)
                  pal <- sample(brewer.pal(12, 'Paired'), n_distinct(spotify_df$df[grouped_by]), replace = T)
                  
                  
                  
                  
                  hchart(df_plot, "scatter", hcaes(PC1, PC2, group = grouped_by)) %>%
                        hc_add_series_list(pcs_labels) %>%
                        hc_add_series_list(pcs) %>%
                        hc_colors(color = c(pal, rep('white', 2*nrow(rotation)))) %>%
                        hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")))
            }

      })
      
      output$plot_3dpca <- renderPlotly({
            validate(need(spotify_df$df, message = FALSE))
            if(nrow(spotify_df$df)>0) {
                  
                  features = c("danceability", "energy", "loudness", "speechiness", "acousticness", "instrumentalness", "liveness", "valence", "tempo")
                  
                  pca = prcomp(spotify_df$df[,features], scale = T)
                  
                  var_explained = sum((pca$sdev^2)[1:2])/sum(pca$sdev^2)
                  
                  rotation = pca$rotation %>% data.frame  %>% select(PC1, PC2, PC3)
                  rotation$names = row.names(rotation)
                  
                  df_plot = cbind(pca$x, spotify_df$df[,c("track_name", "album_name", "album_release_date", "album_img", "artist_name")])
                  
                  rotation2 = apply(rotation[,c("PC1", "PC2", "PC3", "names")], 2, function(x) rbind(c(0), x, c(NA))) %>% data.frame(stringsAsFactors = F)
                  rotation2[which(rotation2$PC1==0),]$names = NA
                  rotation2$PC1 = as.numeric(rotation2$PC1)
                  rotation2$PC2 = as.numeric(rotation2$PC2)
                  rotation2$PC3 = as.numeric(rotation2$PC3)
                  
                  library(caret)
                  # df_plot[,c("PC1", "PC2", "PC3")] = df_plot[,c("PC1", "PC2", "PC3")] %>% apply(2, function(x) (x - mean(x))/(max(x) - min(x)))
                  
                  df_plot[,c("PC1", "PC2", "PC3")] = df_plot[,c("PC1", "PC2", "PC3")]/max(df_plot[,c("PC1", "PC2", "PC3")])
                  
                  grouped_by = ifelse(n_distinct(df_plot$artist_name)>1, "artist_name", "album_name")
                  
                  names(df_plot)[names(df_plot) == grouped_by] = "grouped_by"
                  
                  set.seed(10)
                  pal <- sample(brewer.pal(12, 'Paired'), n_distinct(spotify_df$df[grouped_by]), replace = T)
                  
                  
                  plot_ly(df_plot, x = ~PC1, y = ~PC2, z = ~PC3, color = ~grouped_by, colors = pal, text = ~paste("<b>Song:</b>", track_name)) %>%
                        add_markers() %>%
                        layout(scene = list(xaxis = list(title = 'PC1'),
                                            yaxis = list(title = 'PC2'),
                                            zaxis = list(title = 'PC3')),
                               legend = list(x = 0.5, y = 0, orientation = "h")) %>%
                        add_trace(data = rotation2, x = ~PC1, y = ~PC2, z = ~PC3, color = "Components", text = ~paste("<b>Component:</b>", names), line = list(color = "#FFFFFF"), text = NULL, type = 'scatter3d', mode = "lines", width = 2) %>% 
                        layout(paper_bgcolor='transparent')
            }
      })
      
      output$plot_features <- renderUI({
            validate(need(spotify_df$df, message = FALSE))
            if(nrow(spotify_df$df)>0) {
                  progress <- shiny::Progress$new()
                  progress$set(message = "Computing data", value = 0)
                  # Close the progress when this reactive exits (even if there's an error)
                  on.exit(progress$close())
                  i=1
                  features = c("danceability", "energy", "loudness", "speechiness", "acousticness", 
                               "instrumentalness", "liveness", "valence", "tempo")
                  map(features, function(x){
                        progress$inc(1/i, detail = paste("Doing part", i))
                        i = i+1
                        spotify_df$df[[x]] %>% 
                              hchart(showInLegend = FALSE) %>% 
                              hc_add_theme(hc_theme_smpl()) %>% 
                              hc_title(text = x) %>% 
                              hc_yAxis(title = list(text = ""))
                  }) %>% 
                        hw_grid(rowheight = 150, ncol = 9)
            }
            
            
      })
      
      output$data_exp <- renderUI({
            validate(need(spotify_df$df, message = "Please, pick at least one artist."))
            if(nrow(spotify_df$df)>0) {
                  tagList(
                        fluidRow(box(htmlOutput("plot_features"), width = 12)),
                        fluidRow(
                              box(  title = "Most similar songs",
                                    div(style = 'height: 670px; overflow-y: auto; overflow-x: auto', dataTableOutput("similar_table")),
                                    width = 4, height = 710
                              ),
                              
                              tabBox(title = "Principal Components Analysis",
                                     tabPanel("2D", {
                                           highchartOutput("plot_2dpca", height = 670)
                                     }),
                                     tabPanel("3D", plotlyOutput("plot_3dpca", height = 670)),
                                     width = 8,
                                     height = 710)
                        )
                  )
            } else {
                  tags$div(class = "no-artist-chosen", print("Please, pick at least one artist."))
            }
      })
      
}