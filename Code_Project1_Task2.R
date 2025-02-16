####### Task 2 #######

#_______________________________Romance Data Set________________________________

# load the required packages
library(rvest) # for web scraping
library(tidyr) # to use pipe operator
library(purrr) # to use map_df() to return data frames for the data set

# link for three pages of romance movies in IMDb sorted by the number of votes in descending order
# seq() is used to navigate between pages
rom_pages <- paste0('https://www.imdb.com/search/title/?title_type=movie&genres=romance&sort=num_votes,desc&start=', 
                seq(1, 101, by = 50), '&explore=title_type,genres&ref_=adv_nxt')

# create an empty data frame to combine later on
rom_dataset <- data.frame()

# Here, a for loop is created to navigate between the three pages.
# The SelectorGadget is used to obtain the path for each of the 8 variables to 
# be scraped.
# Since, some movie entries lack the variable meant to be scraped, it will be 
# set to NA so that the number of rows available for each variable are the same
# and can be combined in a data frame.
for (i in 1:3){
  
  rom_df <- read_html(rom_pages[i]) %>% 
    html_nodes(".mode-advanced") %>% 
    map_df(~list(Title = html_nodes(.x, '.lister-item-header a') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 'Release Year' = html_nodes(.x, '.text-muted.unbold') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 Certification = html_nodes(.x, '.certificate') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 'Runtime (minutes)' = html_nodes(.x, '.runtime') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 'User Rating' = html_nodes(.x, '.ratings-imdb-rating strong') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 Synopsis = html_nodes(.x, '.ratings-bar+ .text-muted') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 'Number of Votes' = html_nodes(.x, '.sort-num_votes-visible span:nth-child(2)') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 'Gross (in millions of US Dollars)' = html_nodes(.x, '.ghost~ .text-muted+ span') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .}))
  
# In the first loop, combine the empty romance data set with the results of the first page.
# In the following loops, row bind the data set with results from the next page.
  rom_dataset <- rbind(rom_dataset, rom_df)
}


# convert data set into a data frame
rom_dataset <- as.data.frame(rom_dataset)

# replace everything except numbers 0-9 and "." from Gross column with whitespace
rom_dataset$`Gross (in millions of US Dollars)` <- gsub("[^0-9.]", "",
                                                        rom_dataset$`Gross (in millions of US Dollars)`)

# replace everything except numbers 0-9 from Release Year, Runtime and 
# Number of Votes column with whitespace
for (x in c(2, 4, 7)){
  rom_dataset[,x] <- gsub("[^0-9]", "", rom_dataset[,x])
}

# convert character vectors to numeric for the specified columns
for (y in c(4, 5, 7, 8)){
  rom_dataset[,y] <- as.numeric(rom_dataset[,y])
}

# convert character vectors to factor for the Certification column
rom_dataset$Certification <- as.factor(rom_dataset$Certification)

# convert character vectors to integer for the Release Year column
rom_dataset$`Release Year` <- as.integer(rom_dataset$`Release Year`)

# view the romance data set
View(rom_dataset)

# observe the structure of the romance data set
str(rom_dataset)

#_______________________________Horror Data Set________________________________

# link for three pages of romance movies in IMDb sorted by the number of votes in descending order
# seq() is used to navigate between pages
hor_pages <- paste0('https://www.imdb.com/search/title/?title_type=movie&genres=horror&sort=num_votes,desc&start=', 
                    seq(1, 101, by = 50), '&explore=title_type,genres&ref_=adv_nxt')

# create an empty data frame to combine later on
hor_dataset <- data.frame()

# Here, a for loop is created to navigate between the three pages.
# The SelectorGadget is used to obtain the path for each of the 8 variables to 
# be scraped.
# Since, some movie entries lack the variable meant to be scraped, it will be 
# set to NA so that the number of rows available for each variable are the same
# and can be combined in a data frame.
for (i in 1:3){
  
  hor_df <- read_html(hor_pages[i]) %>% 
    html_nodes(".mode-advanced") %>% 
    map_df(~list(Title = html_nodes(.x, '.lister-item-header a') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 'Release Year' = html_nodes(.x, '.text-muted.unbold') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 Certification = html_nodes(.x, '.certificate') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 'Runtime (minutes)' = html_nodes(.x, '.runtime') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 'User Rating' = html_nodes(.x, '.ratings-imdb-rating strong') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 Synopsis = html_nodes(.x, '.ratings-bar+ .text-muted') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 'Number of Votes' = html_nodes(.x, '.sort-num_votes-visible span:nth-child(2)') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .},
                 'Gross (in millions of US Dollars)' = html_nodes(.x, '.ghost~ .text-muted+ span') %>% 
                   html_text() %>% 
                   {if(length(.) == 0) NA else .}))
  
# In the first loop, combine the empty horror data set with the results of the first page.
# In the following loops, row bind the data set with results from the next page.
  hor_dataset <- rbind(hor_dataset, hor_df)
}

# convert data set into a data frame
hor_dataset <- as.data.frame(hor_dataset)

# replace everything except numbers 0-9 and "." from Gross column with whitespace
hor_dataset$`Gross (in millions of US Dollars)` <- gsub("[^0-9.]", "",
                                                        hor_dataset$`Gross (in millions of US Dollars)`)

# replace everything except numbers 0-9 from Release Year, Runtime and 
# Number of Votes column with whitespace
for (x in c(2, 4, 7)){
  hor_dataset[,x] <- gsub("[^0-9]", "", hor_dataset[,x])
}

# convert character vectors to numeric for the specified columns
for (y in c(4, 5, 7, 8)){
  hor_dataset[,y] <- as.numeric(hor_dataset[,y])
}

# convert character vectors to factor for the Certification column
hor_dataset$Certification <- as.factor(hor_dataset$Certification)

# convert character vectors to integer for the Release Year column
hor_dataset$`Release Year` <- as.integer(hor_dataset$`Release Year`)

# view the horror data set
View(hor_dataset)

# observe the structure of the horror data set
str(hor_dataset)


#_______________________________Analysis________________________________________

### Cleaning the Data Sets

library(dplyr)

# observe if there are any NA values for each variable in the romance data set
colSums(is.na(rom_dataset))
# turns out there are six NAs for the Certification column and one NA for the Gross column

# remove all rows with NA values from romance dataset
rom_dataset <- na.omit(rom_dataset)

colSums(is.na(rom_dataset))
nrow(rom_dataset) 
# There are no more NA values.The romance data set has 143 movie entries. 


# observe if there are any NA values for each variable in the horror data set
colSums(is.na(hor_dataset))
# turns out there are five NAs for the Certification column and eight NAs for the Gross column

# remove all rows with NA values from horror data set
hor_dataset <- na.omit(hor_dataset)

colSums(is.na(hor_dataset))
nrow(hor_dataset) 
# There are no more NA values.The horror data set has 138 movie entries. 

inner_join(rom_dataset, hor_dataset)
# Notice that there are four entries appearing in both data sets. This is 
# because they are classified in both the horror and romance genres.

# remove the similar entries and save in a new variable for romance data set.
rom_data <- anti_join(rom_dataset, hor_dataset)
nrow(rom_data) # the final romance data set has 139 entries.

# remove the similar entries and save in a new variable for horror data set.
hor_data <- anti_join(hor_dataset, rom_dataset)
nrow(hor_data) # the final horror data set has 134 entries.

# create one data set that combines both genres
romance <- rom_data
romance$Genre <- "Romance" # create a new Genre column for romance data

horror <- hor_data
horror$Genre <- "Horror" # create a new Genre column for horror data

combined_dataset <- rbind(romance, horror)

# convert Genre column to factor
combined_dataset$Genre <- as.factor(combined_dataset$Genre)

# sort the data by "Number of Votes" column in decreasing order
combined_dataset <- combined_dataset[order(combined_dataset$`Number of Votes`, decreasing = TRUE),]
nrow(combined_dataset) # This combined dataset has 273 entries.

# view the combined data set
View(combined_dataset)

#_________________________________________________________________________________

library(psych) # to use describe()
library(ggplot2)
library(huxtable) # to create a table

### Summary 

# Get the descriptive statistics for the runtime of the romance and horror data sets
runtime_df <- as.data.frame(rbind(describe(rom_data$`Runtime (minutes)`), 
              describe(hor_data$`Runtime (minutes)`)))

# add a column differentiating the romance and horror data
runtime_df <- cbind(c("Romance", "Horror"), runtime_df)

# choose the columns needed
runtime_df <- runtime_df[, c(1,3,4,5,6,9,10,11,12)]

# compute the q1 and q3 of runtime for both genres
rom_q1 <- quantile(rom_data$`Runtime (minutes)`, probs = 0.25)
rom_q3 <- quantile(rom_data$`Runtime (minutes)`, probs = 0.75)
hor_q1 <- quantile(hor_data$`Runtime (minutes)`, probs = 0.25)
hor_q3 <- quantile(hor_data$`Runtime (minutes)`, probs = 0.75)

# combine the quartile data with the data frame
runtime_df <- cbind(runtime_df, c(rom_q1, hor_q1), c(rom_q3, hor_q3))

# change the names of the data frame columns
names(runtime_df) <- c("Genre", "Number of Entries", "Mean", "Standard Deviation",
                       "Median", "Min", "Max", "Range", "Skewness", "Q1", "Q3")

# create a table
as_hux(runtime_df) %>%
  set_caption("Table: Descriptive statistics of movies from the romance and horror genre 
              in terms of runtime") %>%
  set_right_border(everywhere, everywhere) %>%
  set_left_border(everywhere, 1) %>%
  set_bottom_border(everywhere, everywhere) %>%
  set_top_border(1, everywhere) %>%
  set_align(everywhere, everywhere, "centre") %>%
  set_bold(1, everywhere, value = TRUE) 


### Boxplot of Runtime Based on Movie Genres

boxplot(rom_data$`Runtime (minutes)`, hor_data$`Runtime (minutes)`,
        names = c("Romance", "Horror"), ylab = "Runtime (minutes)",
        col = c("rosybrown", "darkred"), 
        main = "Boxplot of Runtime Based on Movie Genres")

#_________________________________________________________________________________

### Top 15 Movies by Number of Votes

# Get the first 15 movie entries of the combined data set that was already 
# sorted in descending order of the Number of Votes. Only the Title, 
# Number of Votes and Gross columns are retrieved.
top_15 <- combined_dataset[1:15, c(1,7,9)]

ggplot(data = top_15, 
       aes(x = reorder(top_15$Title, top_15$`Number of Votes`, decreasing = TRUE), 
           y = top_15$`Number of Votes`, fill = Genre)) + # Differentiate Genre by colour of bar
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Top 15 Movies by Number of Votes", y = "Number of Votes",
       x = 'Title') +
  theme(plot.title = element_text(hjust = 0.5), # centre the title
        axis.text.x = element_text(angle = 30, hjust = 1, size = 10)) + # adjust x-axis text labels
  # add number of votes on top of the bars
  geom_text(aes(label = top_15$`Number of Votes`), vjust = -0.5, size = 3)

#_________________________________________________________________________________

### Line Plot of the Number of Top Movies by Genre for Each Year Available

# Group by release year and genre before getting the number of entries for 
# each group.
combined_year <- combined_dataset %>% 
  group_by(`Release Year`, Genre) %>% 
  count(`Release Year`) 

# change the names of the columns
names(combined_year) <- c("Year", "Genre", "Count")

ggplot(combined_year, aes(x = Year, y = Count, group = Genre)) +
  geom_line(aes(color = Genre), size = 0.8)+ # add lines with colour according to genre
  geom_point(aes(color = Genre), size = 2.5) + # add points with colour according to genre
  labs(title = "Number of Top Movies by Genre Over Time", y = "Number of Top Movies") +
  theme(plot.title = element_text(hjust = 0.5), # centre the title
        axis.text.x = element_text(angle = 30, hjust = 1)) + # adjust x-axis text labels
  # set x-axis text label boundaries
  scale_x_continuous(breaks = seq(min(combined_year$Year), max(combined_year$Year), by = 2)) +
  scale_y_continuous(breaks = seq(min(combined_year$Count), max(combined_year$Count), by = 1))

#_________________________________________________________________________________

### Stacked Bar Plot of Percentage of Certifications by Genre

# Obtain the percentage of romance and horror movies for each type of certification
percent_bar <- combined_dataset %>% 
  group_by(Certification) %>% 
  count(Genre) %>%
  mutate(ratio = scales::percent(n/sum(n)))

percent_bar

percentage_breaks <- c(0, 0.25, 0.5, 0.75, 1)

# Plot a stacked bar plot for the percentage of romance and horror movies
# based on their respective certifications
ggplot(combined_dataset, aes(x = Certification, fill = Genre)) + # the genres are represented by two different colours
  geom_bar(position = "fill", color = 'black') +
  geom_text(data = percent_bar, aes(y = n,label = ratio),
            position = position_fill(vjust = 0.5)) + # set text showing the percentage in each bar
  labs(title = "Percentage of Certifications by Genre", y = "Percentage") +
  theme(plot.title = element_text(hjust = 0.5))  + # centre the title 
  scale_y_continuous(breaks = percentage_breaks, 
                     labels = scales::percent(percentage_breaks))

#_________________________________________________________________________________

### Scatterplot Matrix of User Rating, Number of Votes and Gross (in millions of US Dollars)


# Insert the correlation value of the variables involved in each panel (romance)
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = "rosybrown", cex = 1.5)
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt, cex = 1.8)
}

# scatterplot matrix for romance data
pairs(rom_data[,c(7,8,5)], # use only the rating, votes and gross columns
      lower.panel = upper.panel, 
      upper.panel = upper.panel,
      main = "Scatterplot Matrix of Number of Votes, Gross and User Rating for the Romance Data Set",
      cex.labels = 2)

# Insert the correlation value of the variables involved in each panel (horror)
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = "darkred", cex = 1.5)
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt, cex = 1.8)
}

# scatterplot matrix for horror data
pairs(hor_data[,c(7,8,5)], # use only the rating, votes and gross columns
      lower.panel = upper.panel, 
      upper.panel = upper.panel,
      main = "Scatterplot Matrix of Number of Votes, Gross and User Rating for the Horror Data Set",
      cex.labels = 2)


#____________________________________________________________________________________

## Median Gross by Genre

median(rom_data$`Gross (in millions of US Dollars)`)

median(hor_data$`Gross (in millions of US Dollars)`)

## Median User Rating by Genre

median(rom_data$`User Rating`)

median(hor_data$`User Rating`)

#____________________________________________________________________________________
