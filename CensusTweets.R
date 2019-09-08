#load libraries

library(rtweet) #twitter library
#All you need is a Twitter account (user name and password) and you can be up in running in minutes!
#rtweet compares and has beeter fxn compared to other twitter APIS like twitteR, streamR, 
library(ggplot2) #plotting
library(dplyr) #pipes  tidyverse
library(tidytext) # text mining
library(igraph) #plotting
library(ggraph) #plotting
#library(widyr) # ngrams
#library(tidyr)
library(stopwords)

theme_set(theme_classic())

censusTweets <- search_tweets(q="#Censuskenya2019", n=10000, 
                              include_rts = FALSE, lang='en')
#returns tweets in the past 6-9 days

#post census tweets

#search_30day() is for premium accounts and requires env_name


censusKE <- censusTweets

"#Census" %in% censusTweets$text

#Top Tweet Unique Words

head(censusKE$text)
glimpse(censusKE)

#making sure we have only unique words all in lower case


censusKE %>%
  dplyr::select(text) %>%
  unnest_tokens(Words, text) %>%
  filter(!Words %in% stop_words$word) %>%
  count(Words, sort=TRUE)


#https - links have been included -> remove

#some users have included links removing them below

censusKE$strpWords <- gsub("https.*", "", censusKE$text) #removing https.* links

censusKE %>%
  dplyr::select(strpWords) %>%
  unnest_tokens(word, strpWords) %>%
  filter(!word %in% stop_words$word) %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(censusKE, mapping =  aes(reorder(word, n), n)) +
    geom_bar(stat = 'identity', aes(fill=word), show.legend = FALSE) +
    coord_flip()+
    labs(title = "Count of unique words in #Censuskenya2019 tweets ", x="Unique Words", y="Count")




#Top Users Tweeting, location etc

users <- users_data(censusKE)
users %>%
  dplyr::select(location) %>%
  unnest_tokens(Location, location) %>%
  count(Location, sort =TRUE) %>%
  top_n(10) %>%
  ggplot(users, mapping = aes(reorder(Location, n), n)) +
    geom_bar(stat = 'identity', aes(fill=Location), show.legend = FALSE) + 
    coord_flip() +
    labs(title = "Location of users in #Censuskenya2019 tweets", x="Location", y="Count") 
  

#Top user scren name

users %>% 
  dplyr::select(screen_name) %>%
  count(screen_name, sort = TRUE) %>%
  top_n(15) %>%
  ggplot(users, mapping = aes(reorder(screen_name, n), n)) +
    geom_bar(stat = 'identity', aes(fill=screen_name), show.legend = FALSE)  +
    labs(title="Top users in #Censuskenya2019 tweets", x="Screen Names", y="Count")+  
    coord_flip()


#Accounts that are verified

ggplot(users, aes(verified))+
  geom_bar(aes(fill=verified), show.legend = FALSE) +
  labs(title = "Count of verified accounts in #Censuskenya2019 tweets", x="Verified ", y="Count")



#Ts plot > time series trend of the tweets


ts_plot(censusKE)

