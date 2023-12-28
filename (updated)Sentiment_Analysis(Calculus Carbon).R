
#Loading dataset
file_path <- "/Users/vidyasagar/Downloads/result_df.csv"
column_names <- read.csv(file_path, header = FALSE, nrows = 1)
youtube_comments_df <- read.csv(file_path, col.names = column_names)

#install.packages("wordcloud")
#install.packages("tm")
#install.packages("syuzhnet")
library(tm)
library(wordcloud)
library(syuzhet)

# Create a new data frame with the top 30000 records
df1<- youtube_comments_df[1:30000, ]

#checking structure of file
str(df1)

#creating corpus
corpus <-iconv(df1$text)
corpus <- Corpus(VectorSource(corpus))


#cleaning corpus
corpus <- tm_map(corpus,tolower)
corpus <- tm_map(corpus,removeNumbers)
corpus <- tm_map(corpus,removeWords,stopwords("english"))

# Create a custom function to remove emojis
remove_emojis <- function(x) {
  # Use a regular expression to remove emojis
  x <- gsub("[\U00002600-\U000027BF\U0001f300-\U0001f64F\U0001f680-\U0001f6FF]", "", x, perl = TRUE)
  return(x)
}
# Create a custom function to remove special characters
remove_special_characters <- function(x) {
  # Use a regular expression to remove non-alphanumeric characters
  x <- gsub("[^a-zA-Z0-9 ]", " ", x)
  return(x)
}

corpus <- tm_map(corpus, content_transformer(remove_emojis))
corpus <- tm_map(corpus, content_transformer(remove_special_characters))
corpus <- tm_map(corpus,stripWhitespace)
final_df <- corpus

#Create Term Document
tdm <- TermDocumentMatrix(final_df)
tdm <- as.matrix(tdm)
tdm[1:10,1:5]

#Bar plot of words
w <- rowSums(tdm)
w <- subset(w,w>=25)
barplot(w,las=2,col="blue")

w <- sort(rowSums(tdm),decreasing = T)
set.seed(2000)
wordcloud(words = names(w),
          freq=w,
          max.words = 500,
          random.order = T,
          min.freq = 25,
          colors = brewer.pal(8,"Dark2"),
          scale = c(3,0,3)
          )

sentiment_data <- iconv(df1$text)
s <- get_nrc_sentiment(sentiment_data)
s[1:10,]

s$score <- s$positive - s$negative
s[1:10,]

comment_score <- colSums(s[,])
print(comment_score)

barplot(colSums(s),
        las=2,
        col=rainbow(10),
        ylab = 'Count',
        main = 'Sentiment')

# Add a new column "sentiment" based on the values in the 's' data frame
df1$sentiment <- ifelse(s$score > 0, "positive", ifelse(s$score < 0, "negative", "irrelevant"))



# Display the updated data frame with the new 'sentiment' column
head(df1)

######################NOTE################################
#Process_comments is the funtion to do the  exact tasks what happened above, the above code is written to understand the step step analysis of sentiment analysis

#The input dataset is very huge , it will exhaust the RAM, so the input data is divivded into 5 parts (subsets), and the analysis is performed
process_comments <- function(df) {
  # Creating corpus
  corpus <- iconv(df$text, to = "UTF-8")
  corpus <- Corpus(VectorSource(corpus))
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  
  # Create a custom function to remove emojis
  remove_emojis <- function(x) {
    # Use a regular expression to remove emojis
    x <- gsub("[\U00002600-\U000027BF\U0001f300-\U0001f64F\U0001f680-\U0001f6FF]", "", x, perl = TRUE)
    return(x)
  }
  
  remove_special_characters <- function(x) {
    # Use a regular expression to remove non-alphanumeric characters
    x <- gsub("[^a-zA-Z0-9 ]", " ", x)
    return(x)
  }
  
  corpus <- tm_map(corpus, content_transformer(remove_emojis))
  corpus <- tm_map(corpus, content_transformer(remove_special_characters))
  corpus <- tm_map(corpus,removeWords,c("blackpink"))
  corpus <- tm_map(corpus, stripWhitespace)
  final_df <- corpus
  
  # Create Term-Document Matrix
  tdm <- TermDocumentMatrix(final_df)
  tdm <- as.matrix(tdm)
  
  # Word Frequency Visualization
  w <- rowSums(tdm)
  w <- subset(w, w >= 25)
  barplot(w, las = 2, col = "blue")
  
  w <- sort(rowSums(tdm), decreasing = TRUE)
  set.seed(2000)
  wordcloud(words = names(w),
            freq = w,
            max.words = 500,
            random.order = TRUE,
            min.freq = 25,
            colors = brewer.pal(8, "Dark2"),
            scale = c(3, 0, 3)
  )
  
  # Sentiment Analysis
  sentiment_data <- iconv(df$text, to = "UTF-8")
  s <- get_nrc_sentiment(sentiment_data)
  
  s$score <- s$positive - s$negative
  
  comment_score <- colSums(s[,])
  print(comment_score)
  
  # Visualization of Sentiment
  barplot(colSums(s),
          las = 2,
          col = rainbow(10),
          ylab = 'Count',
          main = 'Sentiment')
  
  # Add a new column "sentiment" based on the values in the 's' data frame
  df$sentiment <- ifelse(s$score > 0, "positive", ifelse(s$score < 0, "negative", "irrelevant"))
  
  return(df)
}


#Remaing 4 subsets of input datasets
df2<- youtube_comments_df[30001:50000, ]
df2_result <- process_comments(df2)
df3<- youtube_comments_df[50001:70000, ]
df3_result <- process_comments(df3)
df4<- youtube_comments_df[70001:90000, ]
df4_result <- process_comments(df4)
df5<- youtube_comments_df[90001:95428, ]
df5_result <- process_comments(df5)

#"final_result" ,single big output  dataframe created by concating all subsets dataframes output
final_result <- rbind(df1, df2_result, df3_result, df4_result, df5_result)
print(final_result)

#code to save the output in csv format
write.csv(final_result, file = "/Users/vidyasagar/Downloads/output.csv", row.names = FALSE)
######Plots################

# Calculate the proportions
#install.packages("plotrix")
library(plotrix)
library(graphics)

create_sentiment_pie_chart <- function(df, sentiment_column, plot_title) {
  # Create sentiment counts and proportions
  sentiment_counts <- table(df[[sentiment_column]])
  proportions <- prop.table(sentiment_counts) * 100
  
  # Set alpha value
  alpha <- 0.8
  
  # Create a 2D pie chart with reduced color intensity
  pie(
    proportions,
    labels = paste(names(proportions), "\n", round(proportions, 2), "%"),
    main = plot_title,
    col = adjustcolor(rainbow(length(proportions)), alpha = alpha),
    cex = 1,
    radius = 1
  )
  
  # Create a legend below the pie chart with counts
  legend(
    "topright",
    legend = paste(names(proportions), sprintf(" (%d)", sentiment_counts), sep = ""),
    fill = adjustcolor(rainbow(length(proportions)), alpha = alpha),
    cex = 0.8,
    horiz = FALSE
  )
}
create_sentiment_pie_chart(df2_result, "sentiment","For Video 2")