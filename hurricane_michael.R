install.packages("devtools")
library(devtools)

devtools::install_github("dkahle/ggmap") 
library(ggmap)

devtools::install_github('thomasp85/gganimate')
library(gganimate)

install.packages(c("jsonlite", "quanteda", "syuzhet", "tm"))

library(jsonlite)
library(ggplot2)
library(quanteda)
library(syuzhet)
library(tm)
setwd("/Users/cooperlogerfo/desktop/python_workspace/")


#read 


# Returns vector of sizes (number of tweets) for each dataframe in city list
get_size <- function(city_list){
  size <- rep(0, length(city_list) )
  for( i in 1:length(city_list) ){
    if(is.null(dim(city_list[[i]])[1])){
      size[i] <- 0
    }
    else{
      size[i] <- dim(city_list[[i]])[1]
    }
  }
  return(size)
}

# Positive or Negative
get_sent <- function(city_list){
  sent_vector <- rep(0, length(city_list) )
  for( i in 1:length(city_list) ){
    if( length(city_list[[i]]) < 1 ){
      #skip
    }
    else{
      sent_vector[i] <- sum(get_sentiment(city_list[[i]]$Tweets))/dim(city_list[[i]])[1]
    }
  }
  return(sent_vector)
}
 
# Type : "anger", "anticipation", "disgust", "fear",
#"joy", "sadness", "surprise", "trust", "negative", "positive."
get_nrc_sent <- function(city_list, type){
  return_v <- rep(0, length(city_list))
  for( i in 1:length(city_list) ){
    if( length(city_list[[i]]) < 1 ){
      #skip
    }
    else{
      return_v[i] <- sum(get_nrc_sentiment(city_list[[i]]$Tweets)$type)
    }
  }
  return(return_v)
}

# Use to tokenize dataframe/vector of tweets of type "character"
# Currently not used, keeping for later
tokenize_bow <- function(tweets){
  bag_of_words <-
    tweets %>% 
    tokens( what = "word", remove_punct = TRUE, remove_hyphens = TRUE, 
            remove_numbers = TRUE, remove_url = TRUE, remove_twitter = TRUE) %>%
    tokens_tolower() %>%
    tokens_select(stopwords(), selection = "remove") %>%
    tokens_wordstem(language = "english")
  
  return(bag_of_words)
} 




#1 read in data
setwd("/Users/cooperlogerfo/desktop/Alb_data/")
Albany_dfs <- lapply(list.files(pattern = "A_Oct"), 
                  function(x) jsonlite::fromJSON(sprintf("[%s]", paste(readLines(x),collapse=","))))
setwd("/Users/cooperlogerfo/desktop/Col_data/")
Col_dfs <- lapply(list.files(pattern = "Col_Oct"),
                  function(x) jsonlite::fromJSON(sprintf("[%s]", paste(readLines(x),collapse=","))))
setwd("/Users/cooperlogerfo/desktop/Dot_data/")
Dot_dfs <- lapply(list.files(pattern = "D_Oct"),
                  function(x) jsonlite::fromJSON(sprintf("[%s]", paste(readLines(x),collapse=","))))
setwd("/Users/cooperlogerfo/desktop/MB_data/")
MB_dfs <- lapply(list.files(pattern = "MB_Oct"),
                   function(x) jsonlite::fromJSON(sprintf("[%s]", paste(readLines(x),collapse=","))))
setwd("/Users/cooperlogerfo/desktop/M_data/")
Mont_dfs <- lapply(list.files(pattern = "M_Oct"),
                   function(x) jsonlite::fromJSON(sprintf("[%s]", paste(readLines(x),collapse=","))))
setwd("/Users/cooperlogerfo/desktop/PC_data/")
PC_dfs <- lapply(list.files(pattern = "PC_Oct"),
                 function(x) jsonlite::fromJSON(sprintf("[%s]", paste(readLines(x),collapse=","))))
setwd("/Users/cooperlogerfo/desktop/P_data/")
Pen_dfs <- lapply(list.files(pattern = "P_Oct"),
                  function(x) jsonlite::fromJSON(sprintf("[%s]", paste(readLines(x),collapse=","))))
setwd("/Users/cooperlogerfo/desktop/Tal_data/")
Tal_dfs <- lapply(list.files(pattern = "TAL_Oct"),
                  function(x) jsonlite::fromJSON(sprintf("[%s]", paste(readLines(x),collapse=","))))


#2 get lengths, populate matrix with lengths

size_Albany <- get_size(Albany_dfs)
size_Col <- get_size(Col_dfs)
size_Dot <- get_size(Dot_dfs)
size_MB <- get_size(MB_dfs)
size_Mont <- get_size(Mont_dfs)
size_PC <- get_size(PC_dfs)
size_Pen <- get_size(Pen_dfs)
size_Tal <- get_size(Tal_dfs)
  
# Set (long,lat) coordiantes
loc_Albany <- c(-84.1557, 31.5785)
loc_Col <- c(-84.9877, 32.4610)
loc_Dot <- c(-85.3905, 31.2232)
loc_MB <- c(-88.0399, 30.6954)
loc_M <- c(-86.3077, 32.3792)
loc_PC <- c(-85.6602, 30.1588)
loc_Pen <- c(-87.2169, 30.4213)
loc_Tal <- c(-84.2807, 30.4383)

sent_Albany <- get_sent(Albany_dfs)
sent_Col <- get_sent(Col_dfs)
sent_Dot <- get_sent(Dot_dfs)
sent_MB <- get_sent(MB_dfs)
sent_Mont <- get_sent(Mont_dfs)
sent_PC <- get_sent(PC_dfs)
sent_Pen <- get_sent(Pen_dfs)
sent_Tal <- get_sent(Tal_dfs)


dates <- seq(1, 12, 1)
duration <- length(size_Albany)

# Function to build the dataframe ggplot and gganimate will work with
build_df <- function(){
  Albany <- cbind( x = rep(loc_Albany[1], duration), y = rep(loc_Albany[2], duration),
                   size_Albany, as.factor(dates), sent_Albany)
  Col <- cbind( x = rep(loc_Col[1], duration), y =rep(loc_Col[2], duration), 
                size_Col, as.factor(dates), sent_Col)
  Dot <- cbind( x = rep(loc_Dot[1], duration), y =rep(loc_Dot[2], duration),
                size_Dot, as.factor(dates), sent_Dot)
  MB <- cbind( x = rep(loc_MB[1], duration), y =rep(loc_MB[2], duration), 
               size_MB, as.factor(dates), sent_MB)
  Mont <- cbind( x = rep(loc_M[1], duration),y = rep(loc_M[2], duration), 
                 size_Mont, as.factor(dates), sent_Mont)
  PC <- cbind(  x =rep(loc_PC[1], duration),y = rep(loc_PC[2], duration), 
                size_PC, as.factor(dates), sent_PC)
  Pen <- cbind( x = rep(loc_Pen[1], duration),y = rep(loc_Pen[2], duration), 
                size_Pen, as.factor(dates), sent_Pen)
  Tal <- cbind( x = rep(loc_Tal[1], duration), y =rep(loc_Tal[2], duration), 
                size_Tal, as.factor(dates), sent_Tal)
  return(as.data.frame(rbind(Albany, Col, Dot, MB, Mont, PC, Pen, Tal)))
}

# Put dataframe together
full_df <- build_df()
city_labels <- c( rep("Albany", duration), rep("Columbia", duration), rep("Dotham", duration), 
             rep("Mobile", duration), rep("Mont.", duration), rep("P.C.", duration), 
             rep("Pens.", duration), rep("Tallahassee", duration) )
full_df <- cbind(full_df, city_labels)
colnames(full_df) <- c("x_c", "y_c", "sizes", "date", "avg_sent" ,"city_labels")


# Use ggmap, "stamen" source to get map of southeast USA
michael_map <- get_map(location = c(-90, 25.7617, -76.2, 33.8), source = "stamen", maptype = "terrain-background")


# gganimate, plotting sentiment and number of tweets from each city, on each day
full_plot <- ggmap(michael_map) + geom_point(aes(x = x_c, y = y_c, size = sizes, colour = avg_sent ), alpha = 0.5, data = full_df) +
  geom_text(aes(x = x_c, y = y_c, label=city_labels), hjust=-0.2, vjust=0, data = full_df) +
  transition_states(full_df$date, transition_length = 1, state_length = 12) +
  labs(title = 'date: 10-{closest_state}-2018', size = "Num Tweets") +
  scale_colour_gradient(low = "violet", high = "violetred4")

full_plot
anim_save("sent_and_size.gif")

# gganimate, plotting number of tweets from each city, on each day
size_plot <- ggmap(michael_map) + geom_point(aes(x = x_c, y = y_c, size = sizes), alpha = 0.5, data = full_df) +
  geom_text(aes(x = x_c, y = y_c, label=city_labels), hjust=-0.2, vjust=0, data = full_df) +
  transition_states(full_df$date, transition_length = 1, state_length = 12) +
  labs(title = 'date: 10-{closest_state}-2018', size = "Num Tweets")

size_plot
anim_save("size.gif")


# gganimate, plotting sentiment of tweets from each city, on each day
sent_plot <- ggmap(michael_map) + geom_point(aes(x = x_c, y = y_c, size = 5, colour = avg_sent ), alpha = 0.5, data = full_df) +
  geom_text(aes(x = x_c, y = y_c, label=city_labels), hjust=-0.2, vjust=0, data = full_df) +
  transition_states(full_df$date, transition_length = 1, state_length = 12) +
  labs(title = 'date: 10-{closest_state}-2018') +
  scale_colour_gradient(low = "violet", high = "violetred4")

sent_plot
anim_save("sent.gif")

# Two different options for converting list of dataframes into single dataframe
# Albany_dfs <- lapply(Albany_dfs, function(x) as.data.frame(x))
# Albany_fulldf <- ldply(Albany_dfs, data.frame)


# All tweets sent on 10/10/18, the day Hurricane Michael made landfall, from our 8 cities.
tweets <- bind_rows(Albany_dfs[[10]], Col_dfs[[10]], Dot_dfs[[10]],
                    MB_dfs[[10]], Mont_dfs[[10]], PC_dfs[[10]],
                    Pen_dfs[[10]], Tal_dfs[[10]])


twcorpus <- corpus(tweets$Tweets)
tw_dfm <- dfm(tw_corp, remove_punct = TRUE, tolower = TRUE,
              remove = c(stopwords("english"), "hurricane", "michael", "#", 
                         "com", "rt", "https", ":://",
                        "storm", "weather", "tropical",
                        "monday", "tuesday", "wednesday", "thursday",
                        "friday","saturday","sunday"),
              remove_url = TRUE, 
              verbose = TRUE)

textplot_wordcloud(tw_dfm, rotation=.1, min_size=.75, max_size=4, max_words=40)


