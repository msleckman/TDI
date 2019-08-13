#######################
# Project Proposal    #
#######################

library(tidyverse)
# install.packages("rtweet", "ggplot2", "dplyr", "tidytext", "skimr")
# install.packages("textdata")
#library(textdata)
# library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
library(readr)
library(dplyr)
library(stringr)
library(skimr)
library(wordcloud)
library(RColorBrewer)

getwd()
# 
df_1 = read_csv("C:/github/TDI/consumer-reviews-of-amazon-products/1429_1.csv")
df_2 = read_csv("C:/github/TDI/consumer-reviews-of-amazon-products/Datafiniti_Amazon_Consumer_Reviews_of_Amazon_Products.csv")
df_3 = read_csv("C:/github/TDI/consumer-reviews-of-amazon-products/Datafiniti_Amazon_Consumer_Reviews_of_Amazon_Products_May19.csv")

# 
# #### Cleaning Data ####
names(df_1)
names(df_2)
names(df_3)
# 
# 

# All I care about is the review text, so I remove other variables 

df_1 <- df_1 %>% 
  select(name, brand, categories, reviews.date, reviews.rating, reviews.text)
df_2 <- df_2 %>% 
  select(name, brand, categories, reviews.date, reviews.rating, reviews.text)
df_3 <- df_3 %>% 
  select(name, brand, categories, reviews.date, reviews.rating, reviews.text)

consumer_review <- rbind(df_1, df_2, df_3)

write_csv(consumer_review, 'C:/github/TDI/consumer_review.csv')

# write_csv(df_1, "C:/Users/Jocelyne/Downloads/consumer-reviews-of-amazon-products/df_1.csv")
# write_csv(df_2, "C:/Users/Jocelyne/Downloads/consumer-reviews-of-amazon-products/df_2.csv")
# write_csv(df_3, "C:/Users/Jocelyne/Downloads/consumer-reviews-of-amazon-products/df_3.csv")
# 
# 
# df_1 <- read_csv("C:/Users/Jocelyne/Downloads/consumer-reviews-of-amazon-products/df_1.csv")
# df_2 <- read_csv("C:/Users/Jocelyne/Downloads/consumer-reviews-of-amazon-products/df_2.csv")
# df_3 <- read_csv("C:/Users/Jocelyne/Downloads/consumer-reviews-of-amazon-products/df_3.csv")
# 
## explore date structure

read_csv("C:/github/TDI/consumer_review.csv")

lapply(consumer_review, class)
str(consumer_review)
skim(consumer_review)
# head(consumer_review)

### Add column to extract first part of name 
consumer_review$name_extract <- str_extract(pattern = "^((\\w+\\W+){4}\\w+)",
                                            string = consumer_review$name)

### Get individual words
text_consumer_reviews <- data.frame(Product_name = as.character(consumer_review$name_extract), Product_details = as.character(consumer_review$name), Text = as.character(consumer_review$reviews.text), Date =  consumer_review$reviews.date)

### Edit class
text_consumer_reviews$Text <- as.character(text_consumer_reviews$Text)
text_consumer_reviews$Product_name <- as.character(text_consumer_reviews$Product_name)

# Unnest words 
words_consumer_reviews <- text_consumer_reviews %>% 
  tidytext::unnest_tokens(output = word, input = Text, token = "words", to_lower = F)

# head(words_consumer_reviews)

### Edit words if needed
class(text_consumer_reviews$Text)

#words_consumer_reviews$Product_name <- as.character(words_consumer_reviews$Product_name)
#words_consumer_reviews$Product_details <- as.character(words_consumer_reviews$Product_details)

### Match with sentiment analysis dictionary

tidytext::get_sentiments("afinn") # associates word with a sentiment score
#afinn scores/ranks from -5 to +5 for positive or negative sentiment.

get_sentiments("nrc") # associated word with another sentiment feeling word

consumer_review_join <- words_consumer_reviews %>%
  left_join(get_sentiments("afinn"), by = "word") %>% 
  filter(!is.na(value))
  
### What is the mean sentiment score per product
mean_sentiment_consumer_review <- consumer_review_join %>% 
  group_by(Product_name, Product_details) %>% 
  summarise(mean_sentiment = mean(value))

#View(head(mean_sentiment_consumer_review))

### Take a subset of only Kindle products for plot:

selected_products <- consumer_review_join %>% 
  filter(grepl(pattern = "kindle", x = Product_name, ignore.case = T)) %>% 
  group_by(Product_name, Product_details) %>% 
  summarise(mean_sentiment = mean(value))  
  
colnames(selected_products) <- c("Product Name", "Product Details", "Mean Sentiment Score")

#View(selected_products)

if(length(selected_products$`Product Name`) != length(unique(selected_products$`Product Name`))){
  
  selected_products <- selected_products %>% 
    group_by(`Product Name`) %>% 
    summarise(`Mean Sentiment Score` = mean(`Mean Sentiment Score`))
}

names(selected_products)


### Plot 1 - bar graph of means 

hist_plot_kindle <- ggplot(selected_products, aes(x = `Product Name`,
                                           y = `Mean Sentiment Score`))+
  geom_bar(stat = 'identity', color="black", fill = "blue")+
  theme_classic()+
  theme(axis.text.x=element_text(angle=75, hjust = 1))+ 
  labs(title = "Average Sentiment Score of customer reviews for Amazon Kindle related Products", subtitle = "Dates: 2009-2017\n")


### Plot 2 - Create wordcloud from all words from all customer reviews of the amazon products

count_word_cons_reviews <- words_consumer_reviews %>%
  group_by(word) %>% 
  anti_join(get_stopwords()) %>% 
  filter(word != "I") %>% 
  summarise(count = n())

set.seed(100000)
wordcloud_kindle <- wordcloud(words = count_word_cons_reviews$word, freq = count_word_cons_reviews$count, min.freq = 1000, max.words=50, scale = c(3, 0.2), random.order=F, rot.per=0.35, colors=brewer.pal(8, "Dark2"), main = "Hello")

