library(dplyr)
get_artists <- function(artist_name, token) {
      
      # Search Spotify API for artist name
      res <- GET('https://api.spotify.com/v1/search', query = list(q = artist_name, type = 'artist', access_token = token)) %>%
            content %>% .$artists %>% .$items
      
      # Clean response and combine all returned artists into a dataframe
      artists <- map_df(seq_len(length(res)), function(x) {
            list(
                  artist_name = res[[x]]$name,
                  artist_uri = str_replace(res[[x]]$uri, 'spotify:artist:', ''), # remove meta info from the uri string
                  artist_img = ifelse(length(res[[x]]$images) > 0, res[[x]]$images[[1]]$url, NA)
            )
      })
      
      if(nrow(artists)>0)
            return(artists)
      
      return(NULL)
}


get_albums <- function(artist_uri, token) {
      albums <- GET(paste0('https://api.spotify.com/v1/artists/', artist_uri,'/albums'),
                    query = list(access_token = token, market = "US", limit = 50, album_type = "album")) %>% content
      
      if(length(albums$items)==0) stop("Sorry. No albums were found for this artist.")
      
      map_df(1:length(albums$items), function(x) {
            tmp <- albums$items[[x]]
            
            # Make sure the album_type is not "single"
            if (tmp$album_type == 'album') {
                  data.frame(album_uri = str_replace(tmp$uri, 'spotify:album:', ''),
                             album_name = str_replace_all(tmp$name, '\'', ''),
                             album_img = albums$items[[x]]$images[[1]]$url,
                             stringsAsFactors = F) %>%
                        mutate(album_release_date = GET(paste0('https://api.spotify.com/v1/albums/', str_replace(tmp$uri, 'spotify:album:', '')), query = list(access_token = token)) %>% content %>% .$release_date, # you need a separate call to on "albums" to get release date.
                               album_release_year = ifelse(nchar(album_release_date) == 4, year(as.Date(album_release_date, '%Y')), year(as.Date(album_release_date, '%Y-%m-%d'))) # not all album_release_dates have months, so I created album_release year for sorting
                        )
            } else {
                  NULL
            }
            
      }) %>% filter(!duplicated(tolower(album_name))) %>%  # Sometimes there are multiple versions (just with different capitalizations) of the same album
            arrange(album_release_year)
}

get_tracks <- function(artist_info, album_info, token) {
      
      
      track_info <- map_df(album_info$album_uri, function(x) {
            tracks <- GET(paste0('https://api.spotify.com/v1/albums/', x, '/tracks'), query = list(access_token = token)) %>% 
                  content %>% 
                  .$items 
            
            uris <- map(1:length(tracks), function(z) {
                  gsub('spotify:track:', '', tracks[z][[1]]$uri)
            }) %>% unlist %>% paste0(collapse=',')
            
            res <- GET(paste0('https://api.spotify.com/v1/audio-features/?ids=', uris),
                       query = list(access_token = token)) %>% content %>% .$audio_features
            
            df = plyr::rbind.fill(lapply(lapply(res, lapply, function(x)ifelse(is.null(x), NA, x)) , as.data.frame))#plyr::rbind.fill(lapply(lapply(res, Filter, f = Negate(is.null)) , as.data.frame))
            df = data.frame(do.call("cbind", lapply(df, function(x) if(is.numeric(x)) {round(x,2)} else {x})), stringsAsFactors = F)
#             df <- unlist(res) %>% 
#                   matrix(nrow = length(res), byrow = T) %>% 
#                   as.data.frame(stringsAsFactors = F
                                # )
            names(df) <- names(res[[1]])
            library(dplyr)
            df <- df %>% 
                  mutate(album_uri = x,
                         track_number = row_number()) %>% 
                  rowwise %>% 
                  mutate(track_name = tracks[[track_number]]$name) %>%
                  ungroup %>% 
                  left_join(album_info, by = 'album_uri') %>% 
                  rename(track_uri = id) %>% 
                  select(-c(type, track_href, analysis_url, uri))
            return(df)
      }) %>%
            drop_na() %>%
            mutate(artist_img = artist_info$artist_img, artist_name = artist_info$artist_name) %>% 
            mutate_at(c('album_uri', 'track_uri', 'album_release_date', 'track_name', 'album_name', 'artist_img'), funs(as.character)) %>%
            mutate_at(c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness', 'album_release_year',
                        'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature', 'track_number'), funs(as.numeric(gsub('[^e0-9.-]+', '', as.character(.))))) # for some reason parse_number() from readr doesn't work here
      return(track_info)
}


client_id <- '09419aa4748c4b5b94099c5bd1a3451e'
client_secret <- '2179e36d865f42488ff4bee8551d619c'
token <- POST('https://accounts.spotify.com/api/token',
              accept_json(), authenticate(client_id, client_secret),
              body = list(grant_type='client_credentials'),
              encode = 'form', httr::config(http_version=2)) %>% content %>% .$access_token     





get_playlists <- function(playlist_name, token) {
      
      # Search Spotify API for a1rtist name
      res <- GET('https://api.spotify.com/v1/search', query = list(q = playlist_name, type = 'playlist', access_token = token)) %>%
            content %>% .$playlists %>% .$items
      
      
      
      
      # Clean response and combine all returned artists into a dataframe
      playlists <- map_df(seq_len(length(res)), function(x) {
            list(
                  playlist_id = res[[x]]$id,
                  user_id = str_replace(res[[x]]$owner$uri, 'spotify:user:', '') # remove meta info from the uri string
            )
      })
      
      return(playlists)
}



get_playlists_tracks <- function(playlist_list, token) {
      
      
      GET('https://api.spotify.com/v1/users/claireunderwood2017/playlists/40HeD8KNiKZKiXzDARAzfN/tracks', query = list(access_token = token)) %>% 
            content 
      
      # Clean response and combine all 1returned artists into a dataframe
      tracks <- map_df(seq_len(nrow(playlist_list)), function(i) {
            tracks = GET(paste0('https://api.spotify.com/v1/users/', playlist_list[i,2], '/playlists/', playlist_list[i,1], '/tracks'), query = list(access_token = token)) %>% 
                  content %>% .$items
            print(i)
            tracks = map(seq_len(length(tracks)), function(x) {
                  # print(x)
                  list(
                        track_name = tracks[[x]]$track$name,
                        artist_name = tracks[[x]]$track$artists[[1]]$name,
                        # playlist_id = playlist_list[i,1] %>% simplify(),
                        track_id = tracks[[x]]$track$id
                  ) 
            })
            
            # tracks = do.call(rbind.data.frame, tracks)
            
            tracks = plyr::rbind.fill(lapply(lapply(tracks, Filter, f = Negate(is.null)) , as.data.frame))
            
            get_tracks_features(tracks, token)
            
      })
      
      # tracks = tracks %>% drop_na() %>% group_by(track_id, artist_name, track_name) %>% summarise(appearances = n()) %>% arrange(desc(appearances))
      
      return(tracks)
}


get_tracks_features <- function(tracks, token) {
      
      features = GET(paste0('https://api.spotify.com/v1/audio-features/'), query = list(access_token = token, ids = paste(tracks$track_id, collapse = ","))) %>% 
                  content %>% 
                  .$audio_features
      
      
            
            
      features = plyr::rbind.fill(lapply(lapply(features, Filter, f = Negate(is.null)) , as.data.frame))
            
      
      features = features %>%
            mutate_at(c('danceability', 'energy', 'key', 'loudness', 'mode', 'speechiness', 'acousticness',
                        'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature'), funs(as.numeric(gsub('[^e0-9.-]+', '', as.character(.))))) 
      
      
           
      features = merge(tracks, features, by.x = "track_id", by.y = "id", all = F)
      
      return(features)
}

# 
# playlist_list = get_playlists("running", token)
# playlist_tracks = get_playlists_tracks(playlist_list = playlist_list, token)
# # features = get_tracks_features(playlist_tracks, token)
# 
# 
# relax_p = get_playlists("relax", token)
# relax_tracks = get_playlists_tracks(relax_p, token)
# 
# 
# run_tracks$label = "run"
# relax_tracks$label = "relax"
# 
# tracks = rbind(run_tracks, relax_tracks)
# names(tracks)
# 
# ggplot(tracks[order(rnorm(nrow(tracks))),], aes(x = instrumentalness, energy, color = label, text = track_name)) +
#       geom_point(alpha = 0.6)
# ggplotly(ggplot(tracks, aes(x = loudness, energy, color = label, text = track_name)) +
#       geom_point())
# 
# 
# require(caret)
# 
# inTrain = createDataPartition(tracks$label, p = 0.7, list = F)
# train = tracks[inTrain,]
# test = tracks[-inTrain,]
# 
# ctrl = trainControl(method = "repeatedcv", number = 5, repeats = 5, verboseIter = T)
# 
# dput(names(train))
# 
# features = c("danceability", "energy", 
#   "key", "loudness", "mode", "speechiness", "acousticness", "instrumentalness", 
#   "liveness", "valence", "tempo", "duration_ms", "time_signature", "label")
# 
# model = train(label ~ ., train[,features], method = "glm", preProcess = c("center", "scale"), trControl = ctrl)
# modelrf = train(label ~ ., train[,features], method = "rf", preProcess = c("center", "scale"), tuneGrid = expand.grid(mtry = 2))
# modelsvm = train(label ~ ., train[,features], method = "svmLinear", preProcess = c("center", "scale"), trControl = ctrl, tuneGrid = expand.grid(C = c(1, 2,5)))
# 
# confusionMatrix(model)
# model$finalModel
# 
# saveRDS(model, "../Desktop/Projetos/Estudos/Shiny Apps/Machine Learning App/logistic_regression_chill_run")
# 
# 
# radiohead = get_artists(artist_name = "Radiohead", token)
# radiohead = radiohead[1,]
# r_albums = get_albums(radiohead$artist_uri, token)
# r_tracks = get_tracks(radiohead, r_albums, token)
# 
# 
# cbind(r_tracks, run = predict(model, r_tracks, type = "prob")[,2]) %>%  select(track_name, run) %>% arrange(run)
