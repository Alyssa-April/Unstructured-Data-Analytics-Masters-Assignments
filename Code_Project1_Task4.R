# import the libraries needed
library(tm)
library(stringr)
library(SnowballC)
library(wordcloud2)
library(huxtable) # this is to create a table for the word frequencies

# read in the csv file containing all three songs lyrics 
# change the file path accordingly
song_lyrics <- read.csv("C:/Users/User/Desktop/Analitik Data Tak Berstruktur/Project 1/Lyrics_Project1_P125528/All_Lyrics_Task4_P125528.csv",
                        header = FALSE)

# this for loop is to create doc_id 1 to number of sentences available
doc_number <- c()
for (i in 1:nrow(song_lyrics)){
  doc_number = append(doc_number, paste0("doc_", i))
}

# a function to create a vector of as.character functions to convert each 
# sentence to characters
f <- function(x){
  text_func <- c()
  for (y in 1:nrow(x)){
    text_func <- append(text_func, as.character(x[y,]))
  }
  return(text_func)
}

# create a vector of aa, ab, ac, ... , zz
dmeta2_letters <- as.vector(sapply(letters, function(x) sapply(letters, function(y) paste0(x,y))))

# create a data frame that can be converted to corpus
task4_docs <- data.frame(doc_id = doc_number, text = f(song_lyrics),
                   dmeta1 = 1:nrow(song_lyrics), dmeta2 = dmeta2_letters[1:nrow(song_lyrics)],
                   stringsAsFactors = FALSE)

# create a data frame source since it is a csv.file input
task4_text <- DataframeSource(task4_docs) 

# create a corpus
task4_corpus <- VCorpus(task4_text)

# display detailed information on a corpus
inspect(task4_corpus)

## Clean and Pre-process

# transform corpus to lower case letters
task4_corpus <- tm_map(task4_corpus, content_transformer(tolower)) 

# remove english stopwords
task4_corpus <-tm_map(task4_corpus, removeWords,stopwords("english")) 

# remove punctuation
task4_corpus <- tm_map(task4_corpus, removePunctuation)

# remove numbers
task4_corpus <- tm_map(task4_corpus, removeNumbers)

# Remove a vector of words that bring no meaning to the text.
# some are just filler words or to make the songs sound better, 
# aint is short for are not 
task4_corpus <- tm_map(task4_corpus, removeWords, 
                       c("oh", "ha", "whoa", "like", "just", "aint"))

# remove any extra whitespace
task4_corpus <- tm_map(task4_corpus, stripWhitespace)

# Stem words in a text document using Porter's stemming algorithm.
# The Porter algorithm identifies the stem of some words as not a regular English 
# word but instead a special stem object.
task4_docs2 <- tm_map(task4_corpus,stemDocument) 

# create a function to substitute a particular pattern with the word "rule"
to_rule <- content_transformer(function(x,pattern){return(gsub(pattern,"rule",x))})

# The stemDocument function recognizes ruler and rule as two different words.
# Thus, here we substitute all the words "ruler" with "rule" as it quite an
# important word in the song Royals by Lorde.
task4_docs2 <- tm_map(task4_docs2, to_rule, "ruler")

# constructs or coerces to a document-term matrix
dtm <- DocumentTermMatrix(task4_docs2)

# get the frequency of each term
freq <- colSums(as.matrix(dtm))

# order the terms in decreasing order of frequency
ord <- order(freq, decreasing=T)
freq[ord]

# create a data frame of all the terms and their frequencies
df <- data.frame(term = as.vector(names(freq[ord])), freq = as.vector(freq[ord]))

# create a table for the terms and their frequencies (only terms that 
# appear at least 5 times)
as_hux(df[df$freq >= 5, ]) %>%
  set_caption("Table: Terms that appear in three song lyrics and their frequencies in descending order") %>%
  set_right_border(everywhere, everywhere) %>%
  set_left_border(everywhere, 1) %>%
  set_bottom_border(everywhere, everywhere) %>%
  set_top_border(1, everywhere) %>%
  set_align(everywhere, everywhere, "centre") %>%
  set_bold(1, everywhere, value = TRUE) 

# set the colours where terms with the same frequency have the same colour
colours <- c("green", "white", "blue", "purple", "orange", "yellow", "hotpink",
            "khaki", "lightblue", "red")

# set the frequencies as factors and then convert to numeric to set the colours
clr <- colours[as.numeric(as.factor(df$freq))]

# Create a wordcloud using a music note png (customised shape). 
# Open the wordcloud in browser and refresh if it does not appear.
# Might need to run code several times before the wordcloud appears. 
wordcloud2(df, figPath = "music_note.png", color = clr, backgroundColor = "black")

#__________________________________________________________________________________

### Carry out the same steps for each song separately

## The Scientist by Coldplay

song_1 <- read.csv("C:/Users/User/Desktop/Analitik Data Tak Berstruktur/Project 1/Lyrics_Project1_P125528/TheScientist_Coldplay_P125528.csv",
                        header = FALSE)

doc_number <- c()
for (i in 1:nrow(song_1)){
  doc_number = append(doc_number, paste0("doc_", i))
}

f <- function(x){
  text_func <- c()
  for (y in 1:nrow(x)){
    text_func <- append(text_func, as.character(x[y,]))
  }
  return(text_func)
}

dmeta2_letters <- as.vector(sapply(letters, function(x) sapply(letters, function(y) paste0(x,y))))

song1_docs <- data.frame(doc_id = doc_number, text = f(song_1),
                         dmeta1 = 1:nrow(song_1), dmeta2 = dmeta2_letters[1:nrow(song_1)],
                         stringsAsFactors = FALSE)

song1_text <- DataframeSource(song1_docs) 

song1_corpus <- VCorpus(song1_text)

inspect(song1_corpus)

## Clean and Pre-process Song 1

song1_corpus <- tm_map(song1_corpus, content_transformer(tolower)) 

song1_corpus <-tm_map(song1_corpus, removeWords, stopwords("english")) 

song1_corpus <- tm_map(song1_corpus, removePunctuation)

song1_corpus <- tm_map(song1_corpus, removeWords, 
                       c("oh","just"))

song1_corpus <- tm_map(song1_corpus, stripWhitespace)

song1_docs2 <- tm_map(song1_corpus,stemDocument) 

dtm_song1 <- DocumentTermMatrix(song1_docs2)

freq_song1 <- colSums(as.matrix(dtm_song1))

ord_song1 <- order(freq_song1, decreasing=T)
freq_song1[ord_song1]

df_song1 <- data.frame(term = as.vector(names(freq_song1[ord_song1])), 
                       freq = as.vector(freq_song1[ord_song1]))

as_hux(df_song1[df_song1$freq >= 4, ]) %>%
  set_caption("Table: Terms that appear in The Scientist and their frequencies in descending order") %>%
  set_right_border(everywhere, everywhere) %>%
  set_left_border(everywhere, 1) %>%
  set_bottom_border(everywhere, everywhere) %>%
  set_top_border(1, everywhere) %>%
  set_align(everywhere, everywhere, "centre") %>%
  set_bold(1, everywhere, value = TRUE) 

colours_song1 <- c("green", "white", "blue", "purple", "orange", "yellow")

clr_song1 <- colours_song1[as.numeric(as.factor(df_song1$freq))]

wordcloud2(df_song1, shape = "star", color = clr_song1, backgroundColor = "black")

#____________________________________________________________________________________

## Better Than Revenge by Taylor Swift

song_2 <- read.csv("C:/Users/User/Desktop/Analitik Data Tak Berstruktur/Project 1/Lyrics_Project1_P125528/BetterThanRevenge_TaylorSwift_P125528.csv",
                   header = FALSE)

doc_number <- c()
for (i in 1:nrow(song_2)){
  doc_number = append(doc_number, paste0("doc_", i))
}

f <- function(x){
  text_func <- c()
  for (y in 1:nrow(x)){
    text_func <- append(text_func, as.character(x[y,]))
  }
  return(text_func)
}

dmeta2_letters <- as.vector(sapply(letters, function(x) sapply(letters, function(y) paste0(x,y))))

song2_docs <- data.frame(doc_id = doc_number, text = f(song_2),
                         dmeta1 = 1:nrow(song_2), dmeta2 = dmeta2_letters[1:nrow(song_2)],
                         stringsAsFactors = FALSE)

song2_text <- DataframeSource(song2_docs) 

song2_corpus <- VCorpus(song2_text)

inspect(song2_corpus)

## Clean and Pre-process Song 2

song2_corpus <- tm_map(song2_corpus, content_transformer(tolower)) 

song2_corpus <-tm_map(song2_corpus, removeWords, stopwords("english")) 

song2_corpus <- tm_map(song2_corpus, removePunctuation)

song2_corpus <- tm_map(song2_corpus, removeWords, 
                       c("oh", "ha", "whoa", "like", "just"))

song2_corpus <- tm_map(song2_corpus, stripWhitespace)

song2_docs2 <- tm_map(song2_corpus,stemDocument) 

dtm_song2 <- DocumentTermMatrix(song2_docs2)

freq_song2 <- colSums(as.matrix(dtm_song2))

ord_song2 <- order(freq_song2, decreasing=T)
freq_song2[ord_song2]

df_song2 <- data.frame(term = as.vector(names(freq_song2[ord_song2])), 
                       freq = as.vector(freq_song2[ord_song2]))

as_hux(df_song2[df_song2$freq >= 6, ]) %>%
  set_caption("Table: Terms that appear in Better Than Revenge and their frequencies in descending order") %>%
  set_right_border(everywhere, everywhere) %>%
  set_left_border(everywhere, 1) %>%
  set_bottom_border(everywhere, everywhere) %>%
  set_top_border(1, everywhere) %>%
  set_align(everywhere, everywhere, "centre") %>%
  set_bold(1, everywhere, value = TRUE) 

colours_song2 <- c("green", "white", "blue", "purple", "orange", "yellow",
                   "hotpink", "khaki")

clr_song2 <- colours_song2[as.numeric(as.factor(df_song2$freq))]

wordcloud2(df_song2, shape = "star", color = clr_song2, backgroundColor = "black")

#____________________________________________________________________________________

## Royals by Lorde

song_3 <- read.csv("C:/Users/User/Desktop/Analitik Data Tak Berstruktur/Project 1/Lyrics_Project1_P125528/Royals_Lorde_P125528.csv",
                   header = FALSE)

doc_number <- c()
for (i in 1:nrow(song_3)){
  doc_number = append(doc_number, paste0("doc_", i))
}

f <- function(x){
  text_func <- c()
  for (y in 1:nrow(x)){
    text_func <- append(text_func, as.character(x[y,]))
  }
  return(text_func)
}

dmeta2_letters <- as.vector(sapply(letters, function(x) sapply(letters, function(y) paste0(x,y))))

song3_docs <- data.frame(doc_id = doc_number, text = f(song_3),
                         dmeta1 = 1:nrow(song_3), dmeta2 = dmeta2_letters[1:nrow(song_3)],
                         stringsAsFactors = FALSE)

song3_text <- DataframeSource(song3_docs) 

song3_corpus <- VCorpus(song3_text)

inspect(song3_corpus)

## Clean and Pre-process Song 3

song3_corpus <- tm_map(song3_corpus, content_transformer(tolower)) 

song3_corpus <-tm_map(song3_corpus, removeWords, stopwords("english")) 

song3_corpus <- tm_map(song3_corpus, removePunctuation)

song3_corpus <- tm_map(song3_corpus, removeWords, 
                       c("oh", "like", "just", "aint"))

song3_corpus <- tm_map(song3_corpus, stripWhitespace)

song3_docs2 <- tm_map(song3_corpus,stemDocument) 

to_rule <- content_transformer(function(x,pattern){return(gsub(pattern,"rule",x))})

song3_docs2 <- tm_map(song3_docs2, to_rule, "ruler")

dtm_song3 <- DocumentTermMatrix(song3_docs2)

freq_song3 <- colSums(as.matrix(dtm_song3))

ord_song3 <- order(freq_song3, decreasing=T)
freq_song3[ord_song3]

df_song3 <- data.frame(term = as.vector(names(freq_song3[ord_song3])), 
                       freq = as.vector(freq_song3[ord_song3]))

as_hux(df_song3[df_song3$freq >= 5, ]) %>%
  set_caption("Table: Terms that appear in Royals and their frequencies in descending order") %>%
  set_right_border(everywhere, everywhere) %>%
  set_left_border(everywhere, 1) %>%
  set_bottom_border(everywhere, everywhere) %>%
  set_top_border(1, everywhere) %>%
  set_align(everywhere, everywhere, "centre") %>%
  set_bold(1, everywhere, value = TRUE) 

colours_song3 <- c("green", "white", "blue", "purple", "orange", "yellow",
                   "hotpink")

clr_song3 <- colours_song3[as.numeric(as.factor(df_song3$freq))]

wordcloud2(df_song3, shape = "star", color = clr_song3, backgroundColor = "black")

