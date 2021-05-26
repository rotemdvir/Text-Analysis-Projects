## Twitter project: Text analysis ##
## May 2021

library(twitteR)
library(ROAuth)
library(tidyverse)
library(tm)
library(syuzhet)
library(rtweet)
library(wordcloud)
library(lubridate)
library(SnowballC)
library(viridis)

### Twitter API (Keys)
consumer_key <- XXXXX
consumer_secret <- XXXXX
access_token <- XXXXX
access_secret <- XXXXX
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
  
### Data: pull tweets (key word = #recycling) and build dataset 
pull.dat <- searchTwitter("#recycling", since = '2021-05-13', until = '2021-05-20', 
                          n=4000, lang = "en")

mydata <- twListToDF(pull.dat)
sum(duplicated(mydata)) 

####### timing of tweets    #######
mydata$date <- day(mydata$created)
mydata$hour <- hour(mydata$created)

# Timeline: plot time series of all tweets
ts.p <- ts_plot(mydata, "hours") +
  labs(
    x = "Date and time",
    y = "Frequency of tweets",
    title = "Time series of #recycling tweets",
    subtitle = "Frequency of tweets calculated in one-hour intervals") +
  scale_color_viridis() +
  theme_classic()

ts.p <- ts.p + theme(plot.title = element_text(face = "bold", size = 16),
                     plot.subtitle = element_text(face = "italic", size = 10))

# Timing plot: When people tweet the most in an average day?
ggplot(mydata, aes(x = date)) + 
  geom_density()  # general data outlook

t.dat <- mydata %>%
  mutate(likehigh = ifelse(favoriteCount > 3,"high","low")) # create variable for high/low number of likes

t.plot <- ggplot(t.dat, aes(x = hour)) + 
  geom_density(aes(fill = likehigh), alpha = 0.4) +
  xlab("24 Hour format") + ylab("Density") + ggtitle(label = "When do people tweet the most about #recycling?",
                                                     subtitle = "Tweets seaprated by total number of likes") +
  labs(fill = "Number of Likes") +
  theme_classic()

t.plot <- t.plot + theme(legend.position = "bottom",
                         plot.title = element_text(face = "bold", size = 16, color = "Darkblue"),
                         plot.subtitle = element_text(face = "italic", size = 10, color = "black")) +
  scale_fill_manual(breaks = c("high", "low"),
                    labels = c("High", "Less than 3"),
                    values = c("#56B4E9","red")) 

### Top hashtags
library(tidytext)
library(reactable)
library(reactablefmtr)

hash.tab1 <- mydata %>%
  unnest_tokens(hashtag,text,"tweets") %>%
  filter(str_detect(hashtag, "^#")) %>%
  count(hashtag, sort = T) %>%
  filter(n<1000) %>%  # remove #recycling 
  top_n(15)

tab1 <- reactable(hash.tab1, bordered = TRUE, striped = TRUE,
                  pagination = FALSE, # display all rows on one page
                  defaultColDef = colDef(align = "left",
                                         cell = data_bars(hash.tab1, c("purple", "blue","limegreen", "orange"))))


#####   Text analysis: Top Words    #####

## Text data cleaning
data.text <- mydata$text
data.text <- tolower(data.text)

# Replace blank space (“rt”)/
data.text <- gsub("rt", "", data.text)

# Replace @UserName
data.text <- gsub("@\\w+", "", data.text)

# Remove punctuation
data.text <- gsub("[[:punct:]]", "", data.text)

# Remove links
data.text <- gsub("http\\w+", "", data.text)

# Remove tabs
data.text <- gsub("[ |\t]{2,}", "", data.text)

# Remove blank spaces at the beginning
data.text <- gsub("^ ", "", data.text)

# Remove blank spaces at the end
data.text <- gsub(" $", "", data.text)

# Removing stop words (Corpus method)
corp <- Corpus(VectorSource(data.text))
mydata.text.corpus <- tm_map(corp, function(x)removeWords(x,stopwords()))

### Create wordcloud
cloud.p <- wordcloud(mydata.text.corpus,min.freq = 20, scale = c(4,0.3),
                     colors=brewer.pal(8, "Set1"),random.color = T)

### Top words over time

# Unnest text to dataset and remove stop words (Tidytext method)
wd.tab <- mydata %>%
  select(text,date,hour) %>%
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) 

# Add counter: most frequent words
wd.tab1 <- wd.tab %>%
  count(word, sort = T) 

# Remove more stop words (manual, after inspecting data)
mystopwords <- data_frame(word = c("https", "t.co", "rt", "amp"))
wd.tab1 <- anti_join(wd.tab1, mystopwords, by = "word")

# Generate data with tweets times for each word 
wd.tab2 <- left_join(wd.tab1,wd.tab) %>%
  arrange(-n,word,date,hour)

# Focus on most frequent (top 4), added relevant frequent words (packaging, bottles)
# Add counters for tweets by date and hour
wd.tab3 <- wd.tab2 %>%
  filter(word == "recycling" | word == "plastic" | word == "sustainability" | word == "waste" |
           word == "packaging" | word == "bottles") %>%
  select(-n) %>%
  count(word,date,hour) %>%
  arrange(word,date,hour,n)

# Labels for dates
date.labs <- c("13" = "May 13","14" = "May 14","15" = "May 15","16" = "May 16",
               "17" = "May 17","18" = "May 18","19" = "May 19")

# Plot trend for recycling (outlier in data, also removed 5/13 for weird hours)
word.trend.plot1 <- wd.tab3 %>%
  filter(word == "recycling") %>%
  filter(date > 13) %>%
  ggplot(aes(x=hour, y=n)) +
  geom_point(aes(size = n)) + 
  geom_smooth(span = 1) + 
  xlab("24 Hour format") + ylab("Number of tweets (n)") + 
  ggtitle(label = "Top words - recycling: Tweeting over time",
  subtitle = "The frequency of tweets using recycling term (May 2021)") +
  facet_wrap(~ date, nrow = 2, scale = "free_y", labeller = labeller(date = date.labs))+
  theme_bw()

word.trend.plot1 <- word.trend.plot1 + theme(
  legend.position = "bottom",
  plot.title = element_text(face = "bold", size = 18),
  plot.subtitle = element_text(face = "italic", size = 12))

# Plot trend in other top words
word.trend.plot2 <- wd.tab3 %>%
  filter(!(word == "recycling")) %>%
  ggplot(aes(x=hour, y=n)) +
  geom_point(aes(size = n, colour = factor(date))) + 
  geom_smooth(span = 1) + 
  xlab("24 Hour format") + ylab("Number of tweets (n)") + 
  ggtitle(label = "Top words: Tweeting over time",
          subtitle = "The frequency of tweets (May 2021)") +
  labs(colour = "Day") +
  facet_wrap(~ word, nrow = 2, scale = "free_y") +
  theme_bw()

word.trend.plot2 <- word.trend.plot2 + theme(
  legend.position = "bottom",
  legend.box = "vertical",
  legend.background = element_rect(color = "darkblue", linetype = "solid"),
  plot.title = element_text(face = "bold", size = 18),
  plot.subtitle = element_text(face = "italic", size = 12)) +
  scale_colour_discrete(labels = c("May 13","May 14","May 15","May 16",
                                   "May 17","May 18","May 19")) 


##### sentiment analysis (syuzhet package, nrc lexicon)  #####
mysent <- get_nrc_sentiment((data.text))

# Compute total score for each sentiment
mysentscores <- data.frame(colSums(mysent[,]))
names(mysentscores) <- "Score"
mysentscores <- cbind("sentiment" = rownames(mysentscores),mysentscores)
rownames(mysentscores) <- NULL

newscore <- mysentscores %>%
  arrange(Score)

# Plot total sentiment scores across data 
sent.plot1 <- ggplot(newscore, aes(x=sentiment, y=Score)) +
  geom_bar(aes(fill=sentiment), stat = "identity") +
  ggtitle(label = "Sentiment scores for #recycling",
          subtitle = "4000 Tweets collected in May 2021") + 
  xlab("Sentiment Type") +
  theme_classic()

sent.plot1 <- sent.plot1 + theme(legend.position = "none",
                                 plot.title = element_text(face = "bold", size = 16, color = "Darkblue"),
                                 plot.subtitle = element_text(face = "italic", size = 10, color = "blue")) +
  scale_fill_viridis(discrete=TRUE)


### Top 10 words: positive and negative words (sentiment using bing lexicon)
# Unnest tweets and remove stop words
dat.all <- mydata %>%
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) 

# Join with sentiment lexicon and count words
bing_word_counts <- dat.all %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# Focus on top 10 per sentiment and plot
sent.lab <- c("negative" = "Negative Words", "positive" = "Positive Words")

sent.plot2 <- bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  geom_label(aes(label = n), hjust = -0.3, fill = "lightgrey") +
  ggtitle("Top 10 words contributing to tweets sentiment") +
  facet_wrap(~sentiment, scales = "free_y", nrow = 2, labeller = labeller(sentiment = sent.lab)) +
  labs(x = "Contribution to sentiment",
       y = NULL) +
  theme_bw() +
  scale_fill_manual(values = c("#D16103","#52854C"))

