
# loading required packages
library(spotifyr)
library(plyr)
library(tidyverse)
library(httr)
library(rvest)
library(stringr)
library(ggthemes)
library(tidytext)
library(wordcloud)
library(ggridges)
library(wesanderson)
library(yarrr)
library(knitr)
library(kableExtra)
library(radarchart)


# set up Spotify client ID and client secret
Sys.setenv(SPOTIFY_CLIENT_ID = 'your client ID here')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'your client secret here')


#using spoitfyr
taylor <- get_artist_audio_features('taylor swift')

taylor <- taylor %>% filter(album_name == "Taylor Swift" | album_name == "Fearless Platinum Edition" | 
                              album_name == "Speak Now" | album_name == "Red" |
                              album_name == "1989" | album_name == "reputation") 
# The Spotify data for Taylor Swift changed a little in the week between my pulling it and posting this code.



# Getting artist ID on Genius
token <- 'your token here'

genius_get_artists <- function(artist_name, n_results = 10) {
  baseURL <- 'https://api.genius.com/search?q=' 
  requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
                       '&per_page=', n_results,
                       '&access_token=', token)
  
  res <- GET(requestURL) %>% content %>% .$response %>% .$hits
  
  map_df(1:length(res), function(x) {
    tmp <- res[[x]]$result$primary_artist
    list(
      artist_id = tmp$id,
      artist_name = tmp$name
    )
  }) %>% unique
}

genius_artists <- genius_get_artists('taylor swift')



# Getting track urls
baseURL <- 'https://api.genius.com/artists/' 
requestURL <- paste0(baseURL, genius_artists$artist_id[1], '/songs')

track_lyric_urls <- list()
i <- 1
while (i > 0) {
  tmp <- GET(requestURL, query = list(access_token = token, per_page = 50, page = i)) %>% content %>% .$response
  track_lyric_urls <- c(track_lyric_urls, tmp$songs)
  if (!is.null(tmp$next_page)) {
    i <- tmp$next_page
  } else {
    break
  }
}



# Filtering to get urls only for tracks on which Taylor Swift is the primary artist
filtered_track_lyric_urls <- c()
filtered_track_lyric_titles <- c()
index <- c()


for (i in 1:length(track_lyric_urls)) {
  if (track_lyric_urls[[i]]$primary_artist$name == "Taylor Swift") {
    filtered_track_lyric_urls <- append(filtered_track_lyric_urls, track_lyric_urls[[i]]$url)
    filtered_track_lyric_titles <- append(filtered_track_lyric_titles, track_lyric_urls[[i]]$title)
    
    index <- append(index, i)
  }
}




# Fixing inconsistencies between track names on Spotify and Genius

filtered_track_lyric_titles[12] <- "All You Had To Do Was Stay"
filtered_track_lyric_titles[176] <- "...Ready For It?"
filtered_track_lyric_titles[53] <- "Cold As You"
filtered_track_lyric_titles[54] <- "Come Back... Be Here"
filtered_track_lyric_titles[57] <- "Come In With The Rain"
filtered_track_lyric_titles[86] <- "Forever & Always"
filtered_track_lyric_titles[91] <- "Girl At Home"
filtered_track_lyric_titles[131] <- "King Of My Heart"
filtered_track_lyric_titles[140] <- "Look What You Made Me Do"
filtered_track_lyric_titles[170] <- "Out Of The Woods"
filtered_track_lyric_titles[110] <- "I Knew You Were Trouble"
taylor$track_name[9] <- "Should've Said No"
filtered_track_lyric_titles[216] <- "State Of Grace"
filtered_track_lyric_titles[254] <- "The Other Side Of The Door"
filtered_track_lyric_titles[257] <- "The Story Of Us"
filtered_track_lyric_titles[288] <- "Welcome To New York"
filtered_track_lyric_titles[68] <- "Don’t Blame Me"
filtered_track_lyric_titles[265] <- "This Is Why We Can’t Have Nice Things"
filtered_track_lyric_titles[164] <- "New Year’s Day"
taylor$track_name[4] <- "A Place In This World"
taylor$track_name[3] <- "Teardrops On My Guitar"
taylor$track_name[7] <- "Tied Together With A Smile"

# (some indexes might need to be changed since Spotify data has changed)

taylor_lyrics <- data.frame(filtered_track_lyric_urls, filtered_track_lyric_titles)
taylor_lyrics <- taylor_lyrics[filtered_track_lyric_titles %in% taylor$track_name, ]

taylor_lyrics$filtered_track_lyric_urls <- as.character(taylor_lyrics$filtered_track_lyric_urls)
taylor_lyrics$filtered_track_lyric_titles <- as.character(taylor_lyrics$filtered_track_lyric_titles)



# Webscraping lyrics using rvest 
lyric_text <- rep(NA, 94)
for (i in 1:94) {
  lyric_text[i] <- read_html(taylor_lyrics$filtered_track_lyric_urls[i]) %>% 
    html_nodes(".lyrics p") %>% 
    html_text()
}


# Cleaning and standardizing lyrics
for (i in 1:94) {
  lyric_text[i] <- gsub("([a-z])([A-Z])", "\\1 \\2", lyric_text[i])
  lyric_text[i] <- gsub("\n", " ", lyric_text[i])
  lyric_text[i] <- gsub("\\[.*?\\]", " ", lyric_text[i])
  lyric_text[i] <- tolower(lyric_text[i])
  lyric_text[i] <- gsub("[ [:punct:] ]", " ", lyric_text[i])
  lyric_text[i] <- gsub(" {2,}", " ", lyric_text[i])
}

genius_data <- data.frame(track_name = taylor_lyrics$filtered_track_lyric_titles, lyrics = lyric_text)
genius_data$track_name <- as.character(genius_data$track_name)
genius_data$lyrics <- as.character(genius_data$lyrics)



# Removing tracks that would interfere with lyric analysis
taylor <- taylor[!(taylor$track_name == "Forever & Always - Piano Version" | 
                     taylor$track_name == "Treacherous - Original Demo Recording" |
                     taylor$track_name == "Teardrops on My Guitar - Pop Version"),]


# joining Spotify and Genius data
spotify_genius <- full_join(genius_data, taylor, by = "track_name")


# adding "ordered_albums", with album names as factors
spotify_genius$album_name[spotify_genius$album_name == "Fearless Platinum Edition"] <- "Fearless"

ordered_albums <- factor(spotify_genius$album_name)
ordered_albums <- factor(ordered_albums,levels(ordered_albums)[c(4,1,3,5,2,6)])
spotify_genius$ordered_albums <- ordered_albums

options("digits" = 3)


# valence ridge plot (I used fig.height = 6, fig.width = 6 in an rmd)
spotify_genius %>% ggplot(aes(x = valence, y = ordered_albums, fill = ..x..)) + 
  geom_density_ridges_gradient(scale = 0.9) + 
  scale_fill_gradient(low = "white", high = "maroon3") + 
  theme_fivethirtyeight() + 
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  xlim(0,1) +
  theme(legend.position = "none")


# table: album by mean valence
spotify_genius %>% 
  group_by(album_name) %>% 
  summarise(mean(valence)) %>% 
  arrange(desc(`mean(valence)`)) %>% 
  kable() %>% 
  kable_styling("striped", full_width = F, position = "left") %>% 
  row_spec(row = 1:6, background = "#fffce4", color = "red")

# table: top 5 songs by valence
spotify_genius %>% 
  group_by(album_name) %>% 
  summarise(mean(valence)) %>% 
  arrange(desc(`mean(valence)`)) %>% 
  kable() %>% 
  kable_styling("striped", full_width = F, position = "left") %>% 
  row_spec(row = 1:6, background = "#fffce4", color = "red")

# sonic score graph
pirateplot(valence + danceability + energy ~ album_release_year, spotify_genius,
           pal = c(wes_palettes$GrandBudapest2, wes_palettes$Moonrise3[1:2]), 
           xlab = "album", ylab = "sonic score",
           theme = 0, point.o = 0.7, avg.line.o = 1, jitter.val = .05, 
           bty = "n", cex.axis = 0.6, xaxt = "n") 
axis(1, cex.axis = 0.6, lwd = 0)
legend("topright", c("1: Taylor Swift", "2: Fearless", "3: Speak Now", "4: Red", "5: 1989", "6: reputation"), bty = "n", cex = 0.6) 


# 1989 sonic scores
spotify_genius %>% 
  mutate(sonic_score = valence + danceability + energy) %>% 
  select(album_name, track_name, sonic_score) %>% 
  arrange(desc(sonic_score)) %>% 
  filter(album_name == "1989") %>% 
  kable() %>% 
  kable_styling(full_width = F, position = "left") %>% 
  row_spec(row = 1:13, background = "seashell", color = "#b39db2")


# album by danceability
spotify_genius %>% 
  group_by(album_name) %>% 
  summarise(mean(danceability)) %>% 
  arrange(desc(`mean(danceability)`)) %>% 
  kable() %>% 
  kable_styling(full_width = F, position = "left") %>% 
  row_spec(row = 1, background = "seashell", color = "#b39db2")


# tokenized and cleaned datasets of lyrics for textual analysis
tidy_taylor <- spotify_genius %>% unnest_tokens(word, lyrics)
tidier_taylor <- tidy_taylor %>% anti_join(rbind(stop_words[1], "uh", "yeah", "hey", "baby", "ooh", "wanna", "gonna", "ah", "ahh", "ha", "la", "mmm", "whoa", "haa"))
tidier_taylor$word[tidier_taylor$word == "don" | tidier_taylor$word == "didn"] <- NA
tidier_taylor$word[tidier_taylor$word == "ain"] <- NA
tidier_taylor$word[tidier_taylor$word == "isn"] <- NA
tidier_taylor$word[tidier_taylor$word == "usin"] <- "using"
tidier_taylor$word[tidier_taylor$word == "wouldn"] <- "wouldn't"
tidier_taylor$word[tidier_taylor$word == "couldn"] <- "couldn't"
tidier_taylor$word[tidier_taylor$word == "shouldn"] <- "shouldn't"
tidier_taylor$word[tidier_taylor$word == "won"] <- "won't"
tidier_taylor$word[tidier_taylor$word == "ve" | tidier_taylor$word == "ll"] <- NA
tidier_taylor <- na.omit(tidier_taylor)
tidier_taylor$word[tidier_taylor$word == "ileft"] <- "left"


# wordcloud: all
word_count <- tidier_taylor %>%
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>%
  ungroup()

wordcloud(words = word_count$word, freq = word_count$n,
          max.words=100, random.order=FALSE, 
          colors= c(wes_palettes$Moonrise3[c(1:2,5)], wes_palettes$Royal2[5]))


# how many tracks does the word "remember" appear in?
tidier_taylor %>% 
  select(track_name, word) %>% 
  filter(word == "remember") %>% 
  unique() %>% 
  select(track_name)

# wordcloud: Taylor Swift the album
word_count_ts <- tidier_taylor %>%
  filter(album_name == "Taylor Swift") %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>%
  ungroup()

wordcloud(words = word_count_ts$word, freq = word_count_ts$n,
          max.words=25, random.order=FALSE, 
          colors= c(wes_palettes$GrandBudapest2[3:1]))

# wordcloud: reputation
word_count_rep <- tidier_taylor %>%
  filter(album_name == "reputation") %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>%
  ungroup()

wordcloud(words = word_count_rep$word, freq = word_count_rep$n,
          max.words=25, random.order=FALSE, 
          colors= c(wes_palettes$GrandBudapest2[3:1]))


# more cleaning, can be done earlier
tidier_taylor$album_release_year <- as.character(tidier_taylor$album_release_year)
tidier_taylor$album_release_year <- as.numeric(substr(tidier_taylor$album_release_year, 1, 4))

tidy_taylor$album_release_year <- as.character(tidy_taylor$album_release_year)
tidy_taylor$album_release_year <- as.numeric(substr(tidy_taylor$album_release_year, 1, 4))


# creating a "lexical diversity" dataset
lexical_diversity <- tidy_taylor %>% group_by(track_name, album_release_year) %>% 
  mutate(lex_div = length(unique(word))/length(word)) %>% 
  select(track_name, lex_div, album_release_year) %>% 
  distinct()


# lexical diversity plot
pirateplot(lex_div ~ album_release_year, lexical_diversity,
           pal = c("cyan3", "darkgoldenrod1", "maroon4", "red3", "#b39db2", "black"),
           xlab = "album", ylab = "lexical diversity",
           theme = 0, point.o = 0.5, avg.line.o = 1, jitter.val = .05, 
           bty = "n", cex.axis = 0.6, xaxt = "n") 
axis(1, cex.axis = 0.6, lwd = 0)
legend("topright", c("1: Taylor Swift", "2: Fearless", "3: Speak Now", "4: Red", "5: 1989", "6: reputation"), bty = "n", cex = 0.6)


# least lexically diverse tracks
tidy_taylor %>% group_by(track_name, album_name) %>% 
  mutate(lex_div = length(unique(word))/length(word)) %>% 
  select(track_name, lex_div, album_name) %>% 
  arrange(lex_div) %>% 
  distinct() %>% 
  head(5) %>% 
  kable() %>% 
  kable_styling(full_width = F, position = "left") %>% 
  row_spec(row = 1:5, background = "azure", color = "palevioletred")



# joining the tokenized, tidied lyric dataset with sentiment lexicons
taylor_nrc_sub <- tidier_taylor %>%
inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative"))

taylor_AFINN <- tidier_taylor %>% 
  inner_join(get_sentiments("afinn"))

taylor_bing <- tidier_taylor %>% 
  inner_join(get_sentiments("bing"))



# sentiment scores using AFINN
dim <- taylor_AFINN %>% 
  count(album_name)
taylor_AFINN %>%
  group_by(ordered_albums) %>% 
  summarise(sum(score)) %>% 
  mutate(scaled = `sum(score)` * 229 / dim$n) %>% 
  ggplot(aes(x = ordered_albums, y = scaled, fill = ordered_albums)) +
  geom_bar(stat = "identity") +
  ylim(-200,200) +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  scale_fill_manual(values = c("palevioletred", "violetred3", "greenyellow", "lightpink", "olivedrab3", "mediumseagreen")) +
  theme(legend.position="none")



# 1989 pyramid plot
sent_taylor_1989 <- taylor_bing %>%
  unique() %>% 
  group_by(track_name, sentiment, album_name) %>%
  count(track_name, sentiment) %>%
  filter(album_name == "1989")

for(i in 1:24) {
  if(sent_taylor_1989$sentiment[i] == "negative")
    sent_taylor_1989$n[i] <- -sent_taylor_1989$n[i]
}

sent_taylor_1989 %>% 
  ggplot(aes(x = track_name, y = n, fill = sentiment)) + 
  geom_bar(subset = .(sentiment == "positive"), stat = "identity") + 
  geom_bar(subset = .(sentiment == "negative"), stat = "identity") + 
  scale_y_continuous(breaks = seq(-20, 20, 5)) +
  coord_flip() +
  theme_fivethirtyeight() +
  ylim(-20,10) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(plot.background = element_rect(fill = "white")) +
  scale_fill_manual(values = c("palevioletred", "olivedrab3")) +
  theme(legend.position="none")



# all-album radar chart
sentiment_nrc <- taylor_nrc_sub %>%
  group_by(ordered_albums, sentiment) %>%
  count(ordered_albums, sentiment) %>% 
  select(ordered_albums, sentiment, sentiment_total = n)

album_nrc <- taylor_nrc_sub %>%
  count(ordered_albums) %>% 
  select(ordered_albums, album_total = n)

radar_chart <- sentiment_nrc %>% 
  inner_join(album_nrc, by = "ordered_albums") %>% 
  mutate(percent = round((sentiment_total/album_total * 100), 3)) %>% 
  select(-sentiment_total, -album_total) %>%
  spread(ordered_albums, percent)

radar_chart <- radar_chart[c(2,7,5,8,4,3,1,6), c(1, 7:2)]

chartJSRadar(radar_chart, polyAlpha = 0.1, lineAlpha = 0.8, maxScale = 25,
             colMatrix = matrix(c(0, 255, 255, 255, 185, 15, 139, 0, 139, 
                                  255, 0, 0, 201, 167, 198, 0, 0, 0), byrow = F, nrow = 3))


