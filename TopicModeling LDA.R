library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming
library(pdftools)#PDF Reader
library(ggplot2)
library(quanteda)
library(tokenizers)

corpus <- corpus(data_corpus_inaugural)
corpus <- Corpus(DirSource("~/R/DRPI Research Reports/Africa/Cameroon"), readerControl=list(reader=readPDF))


corpus <- tm_map(corpus, removeNumbers)
#corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)


sansinteret <- as.matrix.data.frame(read.csv("~/R/ListeDeTermes.csv"))

corpus = tm_map(corpus, removeWords, stopwords("en"))
corpus = tm_map(corpus, removeWords, sansinteret)

sentences <- tokenize_sentences(corpus[[1]]$content)

par =  sentences[[1]][[1]]
par






Y <- top_terms_by_topic_LDA((par), number_of_topics = 2,plot= F)
Y
Y[1,2]
for (i in 1:5){
  print(Y[i,2])
}


