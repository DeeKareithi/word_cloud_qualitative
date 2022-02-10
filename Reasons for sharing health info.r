#Dorcas Kareithi
#Feb2019
#Creating word clouds from qualitative data


#clearing everything
rm(list=ls())


#loading needed packages
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("NLP")
library("tm")
library("ggplot2")
library(wordcloud2)
library(dplyr)


#STEP 1: load the data
text <- readLines("C:\\Users\\Dorcas\\Desktop\\Dee Sandbox\\Sandbox 5 - Assignments\\VIAMO - Kenya\\Data\\Qualitative interview responses\\Reasons for sharing health info.txt")


#Step 2: converting to a corpus for analysis
docs <- Corpus(VectorSource(text))


#Step 3: transform and clean your data
#### Transformation is performed using tm_map() function to replace, for example, special characters from the text. Replacing "/", "@" and "|" with space.

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
#docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, ",")
docs <- tm_map(docs, toSpace, "\\|")

#### code used to clean your text:

docs <- docs %>%
   tm_map(removeNumbers) %>%
   tm_map(removePunctuation) %>%
   tm_map(stripWhitespace)
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

#Remove your own stop words
# specify your stopwords as a character vector
docs <- tm_map(docs, removeWords, c("can","also", "got", "service", "feel")) 


# Text stemming
# docs <- tm_map(docs, stemDocument)

#### Step 4 : Build a term-document matrix Document matrix is a table containing the frequency of the words. Column names are words and row names are documents. The function TermDocumentMatrix() from text mining package can be used as follow 

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, length(text))

#### Step 5 : Generate the Word cloud The importance of words can be illustrated as a word cloud as follow

set.seed(10022022)
wc_one<-wordcloud(words = d$word, freq = d$freq,scale=c(2,0.25), min.freq = 1,
                  max.words=150, random.order=FALSE, rot.per=0.15, 
                  colors=brewer.pal(8, "Dark2")); wc_one



wc_two<-wordcloud2(data=d, size=0.6, color='random-dark');wc_two

wc_three<-wordcloud2(data=d, size = 0.7, shape = 'diamond'); wc_three

#circle, cardioid, diamond, triangle-forward, triangle, pentagon and star.


wc_four<-wordcloud2(data=d, size = 0.7, shape = 'circle'); wc_four


#### words : the words to be plotted freq : their frequencies min.freq : words with frequency below min.freq will not be plotted; max.words : maximum number of words to be plotted; random.order : plot words in random order. If false, they will be plotted in decreasing frequency; rot.per : proportion words with 90 degree rotation (vertical text); colors : color words from least to most frequent. Use, for example, colors ="black" for single color.