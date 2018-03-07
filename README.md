# shiny-app-spotify
Shiny app that connects with spotify's API and provides personalized playlist recommendation through machine learning and data visualization.

The app can be seen at joelcponte.shinyapps.io/spotifyapp.

It is a bit messy now as I used it to learn some stuff, but it could be helpful. I will clean it up in the short future.

Some of the code to acess the Spotify data and inspiration is due this post from RCharlie: http://www.rcharlie.com/post/fitter-happier/. Dean Attali's blog was also helpful https://deanattali.com/.

### Pick one or multiple artists

Pick one artist to compare their songs and albums or pick multiple artists to compare them.

### Playlist recommendation

The app recommends two playlists using songs from the selected artists. The recomendation is very naive and it was developed using data from Spotify.

First, I downloaded playlists with the keywords "running" and "relax" through Spotify's api and labelled the songs accordingly. Spotify provides a set of features for all of their songs, so we used these to create a training set where the Spotify features were the inputs and the labels "running" and "relax" as outputs. A logistic regression was trained to predict to which playlist new songs are most likely to belong.
