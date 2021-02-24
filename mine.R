#install.packages("tidyverse")
#install.packages("tokenizers")
library(tidyverse)
library(tokenizers)
library(tm) 
library(qdap) 
library(qdapDictionaries)
library(dplyr) 
library(RColorBrewer) 
library(ggplot2)
library(scales) 
library(pdftools)
library(Rgraphviz)
library("stringr")
library(udpipe)
library(textrank)
library(fastTextR)

options(width=100L)
fn <- "dbpedia_csv.tar.gz"

if ( !file.exists(fn) ) {
  download.file("https://github.com/le-scientifique/torchDatasets/raw/master/dbpedia_csv.tar.gz",
                fn)
  untar(fn)
}

train <- sample(sprintf("__label__%s", readLines("dbpedia_csv/train.csv")))
head(train, 2)

train <- ft_normalize(train)
writeLines(train, con = "dbpedia.train")

test <- readLines("dbpedia_csv/test.csv")
labels <- trimws(gsub(",.*", "", test))
table(labels)

test <- ft_normalize(test)
test <- trimws(sub(".*?,", "", test))
head(test, 2)

cntrl <- ft_control(word_vec_size = 10L, learning_rate = 0.1, max_len_ngram = 2L, 
                    min_count = 1L, nbuckets = 10000000L, epoch = 5L, nthreads = 4L)

model <- ft_train(file = "dbpedia.train", method = "supervised", control = cntrl)
ft_save(model, "dbpedia.bin")

model <- ft_load("dbpedia.bin")
model

test_pred <- ft_predict(model, newdata=corpus[[1]]$content)
test_pred

confusion_matrix <- table(truth=as.integer(labels), 
                          predicted=as.integer(gsub("\\D", "", test_pred$label)))
print(confusion_matrix)


corpus <- Corpus(DirSource("~/R/DRPI Research Reports/Africa/Cameroon"), readerControl=list(reader=readPDF))
(corpus[[1]]$content)

corpus <- tm_map(corpus, removeNumbers)
#corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)


sansinteret <- as.matrix.data.frame(read.csv("~/R/ListeDeTermes.csv"))

corpus = tm_map(corpus, removeWords, stopwords("en"))
corpus = tm_map(corpus, removeWords, sansinteret)

dataframe<-data.frame(text=unlist(sapply(corpus, `[`, "content")), stringsAsFactors=F)





dataframe[1,]
n <- length(corpus)
for (i in 1:n){
  Encoding(corpus[[i]]$content) <- "UTF8-BOM" # "WINDOWS-1252" # "ISO-8859-1"
}
corpus

sentences <- tokenize_sentences(corpus[[1]]$content)
par=sentences[[1]]
par
for (i in 1:n){
  paragraph[n] <- sentences[1,n]
}


sentences[[1]][[2]]#select specific paragraph
words <- tokenize_words(sentences[[1]][[2]])
length(words[[1]])
tab <- table(words[[1]])
tab <- tibble(word = names(tab), count = as.numeric(tab))
tab <- arrange(tab, desc(count))
tab


filter(tab, frequency < 0.1)

dim(par)
n <- length(par)
n
par[[2]]
sentences[1,3,1]
for (i in 1:n){
  sentences[[i]]
}


words <- tokenize_words(corpus[[1]]$content)
words 

tab <- table(sentences[[1]])
tab <- tibble(word = names(tab), count = as.numeric(tab))
tab <- arrange(tab, desc(count))
tab
paragraphs <- tokenize_paragraphs(corpus[[1]]$content)
paragraphs[[1]]
