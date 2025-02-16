### TASK 1

# load the necessary packages
library(tm)
library(tidytext)
library(topicmodels)
library(tidyr)
library(ggplot2)
library(dplyr)
library(wordcloud2)
library(SnowballC)

# create a corpus of documents from a directory
docs <- Corpus(DirSource("C:/Users/User/Desktop/Analitik Data Tak Berstruktur/Project_2/P2_NewsArticles",
                         encoding = 'UTF-8'))

#_______________________________________________________________________________

### Data Preprocessing and Cleaning

docs <- tm_map(docs,content_transformer(tolower)) # transform text to lower case
# remove english stopwords
docs <- tm_map(docs, removeWords, stopwords("english")) 
docs <- tm_map(docs, removePunctuation) # remove punctuation
docs <- tm_map(docs, removeNumbers) # remove digits
docs <- tm_map(docs, stripWhitespace) # remove extra whitespaces

# replace specific patterns in the document
docs <- tm_map(docs, content_transformer(gsub),pattern = "rains", 
               replacement = "rain")
docs <- tm_map(docs, content_transformer(gsub),pattern = "areas", 
               replacement = "area")
docs <- tm_map(docs, content_transformer(gsub),pattern = "storms", 
               replacement = "storm")
docs <- tm_map(docs, content_transformer(gsub),pattern = "earthquakes", 
               replacement = "quake")
docs <- tm_map(docs, content_transformer(gsub),pattern = "earthquake", 
               replacement = "quake")
docs <- tm_map(docs, content_transformer(gsub),pattern = "quakes", 
               replacement = "quake")
docs <- tm_map(docs, content_transformer(gsub),pattern = "towns", 
               replacement = "town")
docs <- tm_map(docs, content_transformer(gsub),pattern = "northern", 
               replacement = "north")
docs <- tm_map(docs, content_transformer(gsub),pattern = "southern", 
               replacement = "south")

# additional stopwords that we want to remove
self_stopwords <- c("said", "people", "one", "two", "according", "least",
                    "saturday", "sunday", "also", "told", "statement", "new",
                    "still", "near", "area", "week", "monday", "around", 
                    "away", "just")
docs2 <- tm_map(docs, removeWords, self_stopwords) # remove custom stopwords


dtm <- DocumentTermMatrix(docs2) # create document term matrix

#_______________________________________________________________________________

### (1 - i) Create Four Topics

# create an LDA model with 4 topics using dtm
ap_lda <- LDA(dtm, k = 4, control = list(seed = 40)) 

### (2 - i) 

# extract per-topic-per-word probabilities from the LDA model and convert
# them into a tidy format
ap_topics <- tidy(ap_lda, matrix = "beta")

# Find at least the top 8 terms that are most common within each topic.
# Group ap_topics by topic, select the top 8 terms with the highest
# beta within each topic, then arrange them in descending order. 
ap_top_terms <- ap_topics %>% group_by(topic) %>% top_n(8, beta) %>% 
  ungroup () %>% arrange (topic, -beta)

# grouped bar chart to visualise the top terms for each of the four topics
ap_top_terms %>% mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) + geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales = "free") + coord_flip() + # visualize the above
  ggtitle("Most Common Terms Within Each Topic") +
  theme(plot.title = element_text(hjust = 0.5))
### (2 - ii) ~ Analyse the beta spreas between pairs of topics.
# Choose only one to be explained in the report (Topic 2 and Topic 4)

# Topic 1 and Topic 2
beta_spread <- ap_topics %>% mutate (topic = paste0("topic", topic)) %>% spread(topic, beta) %>%
  filter (topic1 > 0.004 | topic2 > 0.0045) %>% mutate(log_ratio = log2(topic2/topic1))
beta_spread %>% mutate(term = reorder(term,log_ratio)) %>%
  ggplot(aes(term,log_ratio)) + geom_col(show.legend=FALSE) + coord_flip()

# Topic 1 and Topic 3
beta_spread <- ap_topics %>% mutate (topic = paste0("topic", topic)) %>% spread(topic, beta) %>%
  filter (topic1 > 0.004 | topic3 > 0.004) %>% mutate(log_ratio = log2(topic3/topic1))
beta_spread %>% mutate(term = reorder(term,log_ratio)) %>%
  ggplot(aes(term,log_ratio)) + geom_col(show.legend=FALSE) + coord_flip()

# Topic 1 and Topic 4
beta_spread <- ap_topics %>% mutate (topic = paste0("topic", topic)) %>% spread(topic, beta) %>%
  filter (topic1 > 0.0036 | topic4 > 0.0038) %>% mutate(log_ratio = log2(topic4/topic1))
beta_spread %>% mutate(term = reorder(term,log_ratio)) %>%
  ggplot(aes(term,log_ratio)) + geom_col(show.legend=FALSE) + coord_flip()

# Topic 2 and Topic 3
beta_spread <- ap_topics %>% mutate (topic = paste0("topic", topic)) %>% spread(topic, beta) %>%
  filter (topic2 > 0.0045 | topic3 > 0.0045) %>% mutate(log_ratio = log2(topic3/topic2))
beta_spread %>% mutate(term = reorder(term,log_ratio)) %>%
  ggplot(aes(term,log_ratio)) + geom_col(show.legend=FALSE) + coord_flip()

# Topic 2 and Topic 4
beta_spread <- ap_topics %>% mutate (topic = paste0("topic", topic)) %>% spread(topic, beta) %>%
  filter (topic2 > 0.006 | topic4 > 0.004) %>% mutate(log_ratio = log2(topic4/topic2))
beta_spread %>% mutate(term = reorder(term,log_ratio)) %>%
  ggplot(aes(term,log_ratio)) + geom_col(show.legend=FALSE) + 
  coord_flip() +
  ggtitle("Beta Spread for Topic 2 and Topic 4") +
  ylab("log ratio") +
  theme(plot.title = element_text(hjust = 0.5))

# Topic 3 and Topic 4
beta_spread <- ap_topics %>% mutate (topic = paste0("topic", topic)) %>% spread(topic, beta) %>%
  filter (topic3 > 0.004 | topic4 > 0.004) %>% mutate(log_ratio = log2(topic4/topic3))
beta_spread %>% mutate(term = reorder(term,log_ratio)) %>%
  ggplot(aes(term,log_ratio)) + geom_col(show.legend=FALSE) + coord_flip()

### (2 - iii)

# extract the per-document-per-topic probabilities from the LDA model 
ap_documents <- tidy(ap_lda, matrix = "gamma") 
# arrange the probabilities in ascending order based on "document"
ap_documents_sorted <- ap_documents %>% arrange(document)
# print all rows to the console
print(ap_documents_sorted, n = 140)

# Stacked bar chart to visualise gamma. Every bar is a document, whereas the 
# height of the bar is determined by the probability of the document belonging
# to a specific topic. 
ggplot(ap_documents_sorted, aes(x = document, y = gamma, fill = factor(topic))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Document", y = "Gamma", fill = "Topic") +
  ggtitle("Per-Document-Per-Topic, Gamma Probabilities for Each News Article") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

# check the most common words in EarthquakeGardez.txt
tidy(dtm) %>%
  filter(document == "EarthquakeGardez.txt") %>%
  arrange(desc(count))


### (2 - iv)

### Word Cloud

# perform stemming in docs to reduce words to their root words
docs3 <- tm_map(docs,stemDocument) 

# replace specific patterns in the document
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "peopl", replacement = "people")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "quak", replacement = "quake")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "disast", replacement = "disaster")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "countri", replacement = "country")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "evacu", replacement = "evacuate")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "resid", replacement = "resident")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "peopl", replacement = "people")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "offici", replacement = "official")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "peopl", replacement = "people")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "citi", replacement = "city")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "peopl", replacement = "people")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "natur", replacement = "nature")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "hous", replacement = "house")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "sever", replacement = "severe")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "damag", replacement = "damage")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "includ", replacement = "include")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "avalanch", replacement = "avalanche")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "capit", replacement = "capital")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "author", replacement = "authority")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "agenc", replacement = "agency")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "cyclon", replacement = "cyclone")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "rescu", replacement = "rescue")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "death", replacement = "dead")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "emerg", replacement = "emergency")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "landslid", replacement = "landslide")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "polic", replacement = "police")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "climat", replacement = "climate")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "heavi", replacement = "heavy")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "activ", replacement = "active")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "provinc", replacement = "province")
docs3 <- tm_map(docs3, content_transformer(gsub), pattern = "peopleeee", replacement = "people")

# create a custom list of stopwords 
cloud_stopwords <- c("said", "two", "accord", "one", "told", "also", "show",
                     "caus", "around", "last", "away", "mile", "just", "three")

docs3 <- tm_map(docs3, removeWords, cloud_stopwords) # remove custom stopwords

# create a Document-Term Matrix from the preprocessed corpus, docs3 
dtm2 <- DocumentTermMatrix(docs3)
# calculate the term frequencies 
freq <- colSums(as.matrix(dtm2))
# sort the terms and their frequencies in descending order
ord <- order(freq, decreasing=T)
# create a data frame with the words and their frequencies
wc_df <- data.frame(Term = names(freq[ord]), Freq = freq[ord])
# subset ofr terms with frequencies greater than or equal to 15
wc_df_filtered <- subset(wc_df, Freq >= 15)


# set the colours, where terms with the same frequency have the same colour
colours <- c("green", "white", "blue", "purple", "orange", "yellow", "hotpink",
             "green", "white", "blue", "purple", "orange", "yellow", "hotpink",
             "green", "white", "blue", "purple", "orange", "yellow", "hotpink",
             "green", "white", "blue", "purple", "orange", "yellow", "hotpink")

# set the frequencies as factors and then convert to numeric to set the colours
clr <- colours[as.numeric(as.factor(wc_df_filtered$Freq))]

# generate a word cloud 
wordcloud2(wc_df_filtered, color = clr, backgroundColor = "black")

# find terms that are associated with the top 3 most frequent words in the 35
# news articles
findAssocs(dtm2, c("people", "area", "quake"), 0.55)

#_______________________________________________________________________________

### Task 2

library(proxy) # for distance measures
# extract and visualize the output of multivariate data analyses
library(factoextra) # for silhoutte analysis
library(dbscan) # for hdbscan
library(cluster) # provides methods for cluster analysis
library(colorspace)

# convert the document-term matrix to a regular matrix
dtm_mat <- as.matrix(dtm) 

# compute distance matrix that is the cosine distance between the rows 
# of dtm_mat 
dist_mat <- dist(dtm_mat, method = "cosine") 

# perform the Average Silhoutte method and plot it to get the optimal number
# of clusters
fviz_nbclust(x = dtm_mat, FUNcluster = kmeans, method = "silhouette")

# choose the number of clusters to be 4
truth_K = 4 

# perform k-means clustering on dtm_mat with 4 clusters
kmeans_cluster <- kmeans(dtm_mat, 4) 

# perform hierarchical clustering on dist_mat using the ward.D2 method
hierarchical_cluster <- hclust(dist_mat, method = "ward.D2") 

# perform hdbscan clustering on dist_mat with the minimum number of 
# points set to 5. minPts is the minimum size of clusters
hdbscan_cluster <- hdbscan(dist_mat, minPts = 5) 

# extract the cluster memberships from the respective clustering results
kmeans_member <- kmeans_cluster$cluster 
hierarchical_member <- cutree(hierarchical_cluster, k = 6) 
hdbscan_member <- hdbscan_cluster$cluster 

# Create a cluster plot of dist_mat using the k-means cluster memberships. 
clusplot(as.matrix(dist_mat), kmeans_cluster$cluster, color = T, 
         shade = T,labels = 2,lines = 0, main = "K-means Cluster", cex = 0.6)
# Display the number of news articles in each cluster.
table(kmeans_member)

# plot the hierarchical clustering dendrogram and draw rectangles around
# the selected number of clusters 
plot(hierarchical_cluster, main = "Hierarchical Clustering Dendrogram")
rect.hclust(hierarchical_cluster, 6)  
table(hierarchical_member)

# plot dist_mat with each point colored according to its HDBScan 
# cluster membership
plot(as.matrix(dist_mat), col = hdbscan_cluster$cluster+1L,
     main = "HDBScan Cluster Plot", xlab = "", ylab = "")
table(hdbscan_member)

# Plot scatter plots of all three clustering algorithms.
# Perform multidimensional scaling on dist_mat to obtain a 2-dimensional 
# representation of the data. 
points <- cmdscale(dist_mat, k = 2) 
# create a colour palette with 4 colours
palette <- diverge_hcl(truth_K) 

# generate three scatter plots, one each for the k-means clustering, 
# hierarchical clustering and HDBScan clustering respectively.
layout(matrix(1:3, ncol = 1))
plot(points, main = 'K-Means clustering', col = as.factor(kmeans_member), 
     pch = 16, mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')  # Q33
plot(points, main = 'Hierarchical clustering', col = as.factor(hierarchical_member), 
     pch = 16, mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),  
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Density-based clustering', col = as.factor(hdbscan_member), 
     pch = 16, mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 



