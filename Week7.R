#Guiding Questions
#1. What is the public sentiment expressed toward the US Airlines in 2015?
#2. How does sentiment show differences for United and American Airlines?

library(dplyr)
library(readr)
library(tidyr)
library(rtweet)
library(writexl)
library(readxl)
library(tidytext)
library(textdata)
library(ggplot2)
library(textdata)
library(scales)
library(wordcloud2)

Tweetsdata <- read_excel("Data /Tweetsdata.xlsx")
View(Tweetsdata)

##### Wrangle #####
Tweets_American <-
  Tweetsdata %>%
  select(tweet_created, tweet_location, text) %>%
  mutate(Airline = "American") %>%
  relocate(Airline)

Tweets_United <-
  Tweetsdata %>%
  select(tweet_created, tweet_location, text) %>%
  mutate(Airline = "United") %>%
  relocate(Airline)

tweets<- bind_rows(Tweets_American, Tweets_United)
head(tweets)
tail(tweets)

#### Removing the missing data ####
tweets<- na.omit(tweets)

tweet_tokens <- 
  tweets %>%
  unnest_tokens(output = word, 
                input = text, 
                token = "tweets")


tweets_counts <- count(tweet_tokens, word, sort = TRUE)
tweets_counts
view(tweets_counts)

tidy_tweets <-
  tweet_tokens %>%
  anti_join(stop_words, by = "word")%>%
  filter(!word == "im")

tweets_count<-count(tidy_tweets, word, sort = T)

##### Explore #####
ts_plot(tweets, by="days")

wordcloud2(tweets_count,
           color = ifelse(tweets_count[, 2] > 1000, 'black', 'gray'))

tweets_count %>%
  filter(n > 1000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() + 
  labs(x = "Word Counts", y = NULL, title = "Frequently Used Words") + 
  theme_minimal()

#### Get Sentiments ####
bing <- get_sentiments("bing")
bing

loughran <- get_sentiments("loughran")
loughran

##### Join Sentiments ####
sentiment_bing <- inner_join(tidy_tweets, bing, by = "word")
sentiment_bing

sentiment_loughran <- inner_join(tidy_tweets, loughran, by = "word")
sentiment_loughran


##### Sentiment Summaries ######

summary_bing <- count(sentiment_bing, sentiment, sort = TRUE)
summary_bing

summary_bing <- sentiment_bing %>% 
  group_by(Airline) %>% 
  count(sentiment, sort = TRUE) %>% 
  spread(sentiment, n) 

summary_bing

summary_bing <- sentiment_bing %>% 
  group_by(Airline) %>% 
  count(sentiment, sort = TRUE) %>% 
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(lexicon = "bing") %>%
  relocate(lexicon)
summary_bing

summary_loughran <- sentiment_loughran %>% 
  group_by(Airline) %>% 
  count(sentiment, sort = TRUE) %>% 
  spread(sentiment, n) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(lexicon = "loughran") %>%
  relocate(lexicon)

summary_loughran

summary_bing2 <- sentiment_bing %>% 
  group_by(Airline) %>% 
  count(sentiment, sort = TRUE) %>% 
  mutate(method = "bing")

summary_loughran2 <- sentiment_loughran %>% 
  filter(sentiment %in% c("positive", "negative")) %>%
  group_by(Airline) %>% 
  count(sentiment, sort = TRUE) %>% 
  mutate(method = "loughran") 

summary_sentiment <- bind_rows(summary_bing2,
                               summary_loughran2) %>%
  arrange(method, Airline) %>%
  relocate(method)

summary_sentiment

total_counts <- 
  summary_sentiment %>%
  group_by(method, Airline) %>%
  summarise(total = sum(n))

sentiment_counts <- left_join(summary_sentiment, total_counts)
sentiment_counts


sentiment_percents <- sentiment_counts %>%
  mutate(percent = n/total * 100)

sentiment_percents

sentiment_percents %>%
  ggplot(aes(x = Airline, y = percent, fill=sentiment)) +
  geom_bar(width = .8, stat = "identity") +
  facet_wrap(~method, ncol = 1) +
  coord_flip() +
  labs(title = "Public Sentiment on Twitter", 
       subtitle = "American & United",
       x = "Airline", 
       y = "Percentage of Words")

#### Presentation Part ####

#cleaning
rm(list = ls())


