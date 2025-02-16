library(RColorBrewer) # to obtain color palettes
library(syuzhet) # to carry out sentiment analysis
library(ggplot2) # for plotting graphs
library(stringi) # to un-escape all known escape sequences such as the
# trademark symbol for the ice cream flavours

library(dplyr) # data manipulation
library(tidytext) # applies the principles of the tidyverse to analyzing text


# read in the Ben & Jerry's reviews data set
ic_text <- read.csv("C:/Users/User/Desktop/Analitik Data Tak Berstruktur/Project_2/Task_3/reviews.csv")
# save only the reviews column
ic_reviews <- ic_text$text

# read in the Ben & Jerry's products data set
ic_products <- read.csv("C:/Users/User/Desktop/Analitik Data Tak Berstruktur/Project_2/Task_3/products.csv")

# convert Unicode escape sequences to the trademark symbol in the name column,
# which is the flavour of the ice cream
ic_products <- ic_products %>%
  mutate(name = stri_unescape_unicode(name))

# merge the reviews and products data set by the common "key" column
merged_ic <- merge(ic_products, ic_text, by = "key", all.x = TRUE)

# subset only the "name" (flavour) and "text" (review) columns
merged_ic <- merged_ic[, c("name", "text")]

#_______________________________________________________________________________

### Obtain Sentiment Scores Using Different Lexicons

# syuzhet
syuzhet_vector <- get_sentiment(ic_reviews, method="syuzhet")
summary(syuzhet_vector) 

# bing
bing_vector <- get_sentiment(ic_reviews, method="bing")
summary(bing_vector)

# afinn
afinn_vector <- get_sentiment(ic_reviews, method="afinn")
summary(afinn_vector)

#________________________________________________________________________________

### Most Common Positive and Negative Words

# The tidy format makes it simple to manipulate and analyse individual words or 
# tokens within a text corpus, making it popular for text analysis applications.

# Take the reviews and convert the text into a tidy format using unnest_tokens().
# Keep track of which line the words come from in linenumber column.
tidy_ic <- merged_ic %>%
  group_by(name) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) # tokenizing reviews into words

# Obtain how much each word contributes to each sentiment (positive/negative).
# Use bing because it has only negative or positive sentiments
bing_word_counts <- tidy_ic %>%
  inner_join(get_sentiments("bing")) %>%
  # Filter out the word "fudge" because it appears as a negative sentiment when 
  # in fact it is only a type of flavour in our case.
  # Filter out the word "limited" because it appears as a negative sentiment 
  # when in fact it portrays that customers want Ben & Jerry's to expand
  # their limited edition batches of ice cream. 
  filter(!word %in% c("fudge", "limited")) %>% 
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

# show the top 10 positive and negative words based on their contributions to 
# the sentiments
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  # one plot for negative words and one plot for positive words
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Number of Times the Word Contributes to the Sentiment",
       y = NULL, title = "Top 10 Most Common Positive and Negative Words") +
  theme(plot.title = element_text(hjust = 0.5))

# Visualise the positive and negative words in a comparison cloud
library(reshape2)
library(wordcloud)

# Modify the sentiment of the word "addicted", "addicting" and indulge from 
# negative to positive.
# This is because when a reviewer says these about the ice cream,
# it is a good thing for Ben & Jerry's.
new_bing <- get_sentiments("bing") %>%
  mutate(sentiment = ifelse(word %in% c("addicting", "addicted", "indulge"), "positive", sentiment))

# Plot the comparison cloud with a maximum of 80 words, with positive words
# in green and negative words in red.
tidy_ic %>%
  inner_join(new_bing) %>%
  filter(!word %in% c("fudge", "limited")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("darkred", "darkgreen"),
                   max.words = 80)

#________________________________________________________________________________

### Emotion Classification

emotion_ic <- get_nrc_sentiment(ic_reviews)

#Visualization
emotion_ic_df <- data.frame(t(emotion_ic)) # transpose emotion_ic
# The function rowSums computes column sums across rows for each level of a 
# grouping variable.
emotion_ic_df <- data.frame(rowSums(emotion_ic_df)) 
names(emotion_ic_df)[1] <- "count" # change column name to count
# combine sentiment column with count column before setting rownames as NULL
emotion_ic_df <- cbind("sentiment" = rownames(emotion_ic_df), emotion_ic_df)
rownames(emotion_ic_df) <- NULL
emotion_ic_df <- emotion_ic_df[1:8,] # exclude positive and negative sentiments

# get the percentage proportion for each emotion
emotion_ic_df$percentage <- emotion_ic_df$count/sum(emotion_ic_df$count)*100


# plot the emotion classification for ice cream reviews
quickplot(sentiment, data = emotion_ic_df, weight = count, geom = "bar", 
          fill = sentiment, ylab = "count") + 
  geom_text(aes(label = paste0(round(percentage, 2), "%"), y = count), vjust = -0.5, 
            color = "black", size = 3) +
  ggtitle("Ben & Jerry's Reviews' Emotion Sentiments") +
  labs(x = "Emotion Sentiment", y = "Count", fill = "Sentiment") +
  theme(plot.title = element_text(hjust = 0.5))

#________________________________________________________________________________

## Other analyses

# Get the top 5 most reviewed ice cream flavours and the top 5 least reviewed 
# ice cream flavours. Then, plot their percentage contribution of negative and 
# positive sentiments based on the reviews in two separate plots. 
library(tidytext)
library(dplyr)
library(tidyr)

# Carry out sentiment analysis using the bing lexicon since it only involves
# positive and negative sentiments.
# Get the total number of negative and positive sentiments for each flavour 
# and their total reviews.
sentiment_scores <- tidy_ic %>%
  inner_join(get_sentiments("bing")) %>%
  count(name, sentiment) %>%
  group_by(name) %>%
  mutate(total_reviews = sum(n)) %>%
  ungroup()

# Calculate the percentage of positive and negative reviews and save them in a 
# column called "percentage"
sentiment_scores <- sentiment_scores %>%
  mutate(percentage = n / total_reviews * 100)

# Select the top 5 flavors with the most reviews
popular_flavours <- sentiment_scores %>%
  top_n(10, wt = total_reviews) %>%
  arrange(desc(total_reviews))

# Select the top 5 flavors with the least reviews
least_popular_flavours <- sentiment_scores %>%
  top_n(10, wt = -total_reviews) %>%
  arrange(total_reviews)

# Bar plot for Top 5 most popular ice cream flavours
ggplot(popular_flavours, aes(x = name, y = percentage, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Ice Cream Flavours", y = "Percentage Contribution to Sentiment", 
       fill = "Sentiment") +
  scale_fill_manual(values = c("positive" = "green", "negative" = "red")) +
  ggtitle("Percentage of Positive and Negative Sentiments for the Top 5 Most Popular Ice Cream Flavours") +
  theme(plot.title = element_text(hjust = 0.5))

# Bar plot for Top 5 least popular ice cream flavours
ggplot(least_popular_flavours, aes(x = name, y = percentage, fill = sentiment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Ice Cream Flavours", y = "Percentage Contribution to Sentiment", 
       fill = "Sentiment") +
  scale_fill_manual(values = c("positive" = "green", "negative" = "red")) +
  ggtitle("Percentage of Positive and Negative Sentiments for the Top 5 Least Popular Ice Cream Flavours") +
  theme(plot.title = element_text(hjust = 0.5))


### For the top 5 most popular ice cream flavours, plot the net sentiment scores
# for their reviews to get a sense of whether the reviews are predominantly 
# positive, negative or neutral. Comparison between flavours can be done. 
popular_sentiment <- tidy_ic %>%
  inner_join(get_sentiments("bing")) %>%
  count(name, index = linenumber, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative) %>%
  filter(name %in% popular_flavours$name) 

# plot for each flavour
ggplot(popular_sentiment, aes(index, sentiment, fill = name)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~name, ncol = 2, scales = "free_x") + 
  labs(x = "Index", y = "Net Sentiment") +
  ggtitle("Net Sentiment Scores for the Top 5 Most Popular Ice Cream Flavors") +
  theme(plot.title = element_text(hjust = 0.5))

