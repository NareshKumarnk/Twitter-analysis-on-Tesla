# Load Requried Packages
library(tm)
library(lda)
library(httr)
library(dplyr)
library(tidyr)
library(anytime)
library(stringi)
library(twitteR)
library(syuzhet)
library(tidytext)
library(tidyverse)
library(SnowballC)
library(wordcloud)
library(topicmodels)
library(BiocManager)
library(rtweet)

stocks_raw <- read.csv("H:/google classroom/SEM6/Data mining Lab/Cat 2/BTC-USD.csv")
stocks_raw1 <- read.csv("H:/google classroom/SEM6/Data mining Lab/Cat 2/DOGE-USD.csv")
stocks_raw2 <- read.csv("H:/google classroom/SEM6/Data mining Lab/Cat 2/TSLA.csv")

# Update Date column into date format.
stocks_raw$Date <- as.Date(stocks_raw$Date)
stocks_raw1$Date <- as.Date(stocks_raw1$Date)
stocks_raw2$Date <- as.Date(stocks_raw2$Date)


# Select data from 01/01/2018 to 11/20/2019 and calculate price change percentage between closing and opening price.
stocks.df <- stocks_raw %>% 
  filter(between(Date, as.Date("2015-04-23"),as.Date("2021-04-23"))) %>%
  mutate(Pct_Change=(Close-Open)/Open*100)

stocks.df1 <- stocks_raw1 %>% 
  filter(between(Date, as.Date("2015-04-23"),as.Date("2021-04-23"))) %>%
  mutate(Pct_Change=(Close-Open)/Open*100)

stocks.df2 <- stocks_raw2 %>% 
  filter(between(Date, as.Date("2015-04-23"),as.Date("2021-04-23"))) %>%
  mutate(Pct_Change=(Close-Open)/Open*100)

head(stocks.df)

app <- "Financial_tweet_analysis"
api_key <- 'XXX'
api_secret_key <- 'XXX'
access_token <- 'XXX'
access_token_secret <- 'XXX'

## authenticate via web browser
token <- create_token(
  app = "twitteranalysisR",
  consumer_key = api_key,
  consumer_secret = api_secret_key,
  access_token = access_token,
  access_secret = access_token_secret)

get_token()

#vignette("auth", package = "rtweet")

rt <- search_tweets("#Tesla", n = 5000, include_rts = FALSE)
names(rt)
rt
elon_musk <- search_tweets("#elonmusk", n = 500, include_rts = FALSE)

ts_plot(rt, "3 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of #rstats Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

data1 = rt %>% select(text)
head(data1)

tweets_text <- rt %>% select(text)

# Remove meaningless characters and symbols.
tweets_text$text <- gsub("&amp","", tweets_text$text)
tweets_text$text <- gsub("the","",tweets_text$text)
tweets_text$text <- gsub("(RT)((?:\\b\\w*@\\w+)+)","", tweets_text$text)
tweets_text$text <- gsub("^RT","", tweets_text$text)
tweets_text$text <- gsub("@\\w+","", tweets_text$text)
tweets_text$text <- gsub("[[:punct:]]","", tweets_text$text)
tweets_text$text <- gsub("[[:digit:]]+\\s","", tweets_text$text)
tweets_text$text <- gsub("http\\w+","", tweets_text$text)
tweets_text$text <- gsub("[ \t]{2,}"," ", tweets_text$text)

tweets_text$text

# Remove all non-ASCII characters 
tweets_text$text <- iconv(tweets_text$text, "UTF-8", "ASCII", sub="")

# Delete empty text column.
tweets_text <- tweets_text %>% na_if("") %>% na_if(" ") %>% na.omit()

# Tweets that contained less than 20 characters were treated as noise.
tweets_text <- tweets_text %>% filter(nchar(text)>20)

# Add id column to consider each text row as a document.
tweets_text$doc_id <- seq.int(nrow(tweets_text))
head(tweets_text)

# Tokenize the text and see frequency of words.
tweets_text %>% 
  unnest_tokens(word, text)%>%
  anti_join(stop_words) %>%
  count(word, sort=TRUE) 
head(tweets_text)

# Creating tweets frequency dataframe.
top_words <- tweets_text %>% 
  unnest_tokens(word, text)%>%
  anti_join(stop_words) %>% 
  count(word, sort=TRUE)

# Visualizing words which frequency are greater than 300.
top_words <- filter(top_words, n>200)
head(top_words)

#Creating Document Term Matrix
# Select text and id column.
tweetscorpus.df <- tweets_text %>% select(doc_id, text)

# Create a corpus for document term matrix.
tweetscorpus <- VCorpus(DataframeSource(tweetscorpus.df))

# Remove all punctuation from the corpus.
tweetscorpus <- tm_map(tweetscorpus, removePunctuation)

# Remove all English stopwords from the corpus.
tweetscorpus <- tm_map(tweetscorpus, removeWords, stopwords("en"))
tweetscorpus <- tm_map(tweetscorpus, removeWords, stopwords("SMART"))

# Remove all number from the corpus.
tweetscorpus <- tm_map(tweetscorpus, removeNumbers)

# Strip extra white spaces in the corpus.
tweetscorpus <- tm_map(tweetscorpus, stripWhitespace)

#stemming the text 
tweetscorpus <- tm_map(tweetscorpus, stemDocument)

# Build a document term matrix.
tweetsdtm <- DocumentTermMatrix(tweetscorpus)
dim(tweetsdtm)

# Remove sparse terms which don't appear very often. Limit the document term matrix to contain terms appearing in at least 2% of documents.
tweetsdtm <- removeSparseTerms(tweetsdtm, 0.98)
dim(tweetsdtm)

freq<- sort(colSums(as.matrix(tweetsdtm)), decreasing=TRUE)
findFreqTerms(tweetsdtm, lowfreq=100) 
#identifying terms that appears more than 100times

wf<- data.frame(word=names(freq), freq=freq)
head(wf)

# Find the sum of words in each document and remove all docs without words.
rowTotals <- apply(tweetsdtm , 1, sum)
tweetsdtm.new   <- tweetsdtm[rowTotals> 0, ]

# Put the document in the format lda package required.
tweetsdtm.matrix <- as.matrix(tweetsdtm.new)

head(tweetsdtm.matrix, n=5)

# Visualizing wordcloud.
wordcloud(tweetscorpus, max.words = 150, random.order = FALSE, rot.per = 0.15, min.freq = 5, colors = brewer.pal(8, "Dark2"))

# Which words are associated with 'tesla'?
rtesla <- findAssocs(tweetsdtm.new, "tesla", 0.05)
relon <- findAssocs(tweetsdtm.new, "elonmusk", 0.05)

freq_terms <- findFreqTerms(tweetsdtm.new, lowfreq = 300)  

# Visualizing the association.
plot(tweetsdtm.new, term = freq_terms, corThreshold = 0.10, weighting = T)

#visualizing bitcoin stock
ggplot(stocks.df, aes(x=Date))+
  geom_line(aes(y=Open))+
  labs(title = "Bitcoin-USD Stock Market Trend")+
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        panel.grid.minor = element_blank())

#visualizing dogecoin stock
ggplot(stocks.df1, aes(x=Date))+
  geom_line(aes(y=Open))+
  labs(title = "Dogecoin-USD Stock Market Trend")+
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        panel.grid.minor = element_blank())


#visualizing Tesla stock
ggplot(stocks.df2, aes(x=Date))+
  geom_line(aes(y=Open))+
  labs(title = "Tesla Stock Market Trend")+
  theme(axis.text.x = element_text(angle=90, vjust=0.5),
        panel.grid.minor = element_blank())


# Create a LDA model with Gibbs method for 30 topics.
tweetsLDA <- LDA(tweetsdtm.matrix, 30, method="Gibbs", control = list(seed = 123))

# Top 30 words per topic.
terms(tweetsLDA, 10)

# Per-topic-per-word probabilities.
tweetsLDA.topicword.prob <- tidy(tweetsLDA, matrix="beta")
head(tweetsLDA.topicword.prob)

# Find the 10 terms that are most common within each topic.
tweetsLDA.topterms <- tweetsLDA.topicword.prob %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

head(tweetsLDA.topterms)

# Plot per-topic-per-word probabilities for topic #9.
tweetsLDA.topterms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  filter(topic==6) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# Classify the selected topic #9 per document.
tweetsLDA.class <- data.frame(topics(tweetsLDA))
tweetsLDA.class <- cbind(tweetsLDA.class, 1:nrow(tweetsLDA.class))
colnames(tweetsLDA.class)[ncol(tweetsLDA.class)] <-'doc_id'
tweetsLDA.class <- tweetsLDA.class %>% filter(topics.tweetsLDA.==1)

head(tweetsLDA.class)

# Inner join selected classified topic with original dataframe.
tweets.final <- inner_join(tweetsLDA.class, tweets_text)

# Turn tweets text into vector.
tweets.df <- as.vector(tweets.final$text)

# Getting emotion score for each tweet.
tweets.emotion <- get_nrc_sentiment(tweets.df)
tweets.emotion <- cbind(tweets.final, tweets.emotion) 
head(tweets.emotion)

# Getting sentiment score for each tweet.
tweets.score <- get_sentiment(tweets.df)
tweets.score <- cbind(tweets.final,tweets.score )
head(tweets.score)

category_senti <- ifelse(tweets.score <= 0, "Negative", ifelse(tweets.score > 0, "Positive"))

head(category_senti)

positive <- tweets.score %>%  
  filter(tweets.score > 0)

negative <- tweets.score %>%  
  filter(tweets.score < 0)

tweets_words = tweets_text %>% 
  select(text)%>%
  unnest_tokens(word,text)

tweets_words <- tweets_words %>%
  anti_join(stop_words)

score = tweets_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(word,sentiment,sort = TRUE) %>%
  ungroup()

score %>% 
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word,n,fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment,scales = "free_y")+
  labs(title = "Tweets containing",
       x= "sentiment",
       y= NULL)+
  coord_flip()+ theme_bw()

bing = get_sentiments("bing")

tweets_words %>%
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "dark green"),
                   max.words = 100)
