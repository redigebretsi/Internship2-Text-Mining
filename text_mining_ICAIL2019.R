# Chargement des packages (bibliothèques de codes spécifiques)

library(tm) # Framework for text mining.
library(qdap) # Quantitative discourse analysis of transcripts.
library(qdapDictionaries)
library(dplyr) # Data wrangling, pipe operator %>%().
library(RColorBrewer) # Generate palette of colours for plots.
library(ggplot2) # Plot word frequencies.
library(scales) # Include commas in numbers.
library(pdftools)
library(Rgraphviz) # Correlation plots.
library("stringr")
library(udpipe)
library(textrank)
library(tidyverse)
library(tokenizers)

library("quanteda") 

#"C:/Users/redig/AppData/Local/Programs/Python/Python39/Lib/site-packages/spacy/util.py"

# ptm <- proc.time()


corpus <- Corpus(DirSource("~/R/DRPI Research Reports/Africa/Cameroon"), readerControl=list(reader=readPDF))

# corpus <- Corpus(DirSource("~/R/data/decisions"), readerControl=list(reader=readPDF))


text <- pdf_text("~/R/DRPI Research Reports/Africa/Cameroon")
text2 <- strsplit(Corpus[1], "\n")
head(text2[[1]])



# Contenu du premier document lu
corpus[[1]]$content
x <- as.data.frame(corpus[[1]]$content)

x

win.graph(800, 600, 10)
hist(nchar(corpus[[1]]$content), 2)

#splitting text with /n character
corpus[[1]]$content[nchar(corpus[[1]]$content) < 1000] <- paste0(corpus[[1]]$content[nchar(corpus[[1]]$content) < 1000], "\n")
txt <- paste(corpus[[1]]$content, collapse = " ")
txt <- strsplit(txt, "\n")[[1]]
txt

grep("disability", txt, value = TRUE)

library(lattice)
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "cadetblue", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Freq")


text2 <- strsplit(corpus[[1]]$content, "\\n\\n")
a <- strsplit(corpus[[1]]$content, "\n+\\s+")[[1]]
a
#The output of strsplit is a list
text2
head(text2[[1]])
# problèmes de caractères accentués 

# Transformation des caractères accentués dans le bon mode d'encodage
#Transformation of accented characters into the right encoding mode
n <- length(corpus)
for (i in 1:n){
  Encoding(corpus[[i]]$content) <- "UTF8-BOM" # "WINDOWS-1252" # "ISO-8859-1"
}
corpus[[1]]$content
n <- length(corpus)
for (i in 1:n){
  corpus[[i]]$content <- str_replace_all(corpus[[i]]$content, "ï¿½", "fi")
  }

# Contenu du premier document lu
corpus[[2]]$content
# contenu correct


# Informations générales sur les objets
str(corpus)
corpus
class(corpus)
class(corpus[[1]])
summary(corpus)

inspect(corpus[1])
summary(corpus[1])

# Noms des fichiers employés
names(corpus)

# Traitements du corpus : transformation en minuscules...
corpus = tm_map(corpus, content_transformer(tolower))
corpus[[1]]$content

corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, stripWhitespace)
# corpus <- tm_map(corpus, tolower)
# corpus <- tm_map(corpus, content_transformer(tolower))
# Suppressions des mots vides, nombres, espaces...

sansinteret <- as.matrix.data.frame(read.csv("~/R/ListeDeTermes.csv"))
# sansinteret

# stopwords("en")

corpus = tm_map(corpus, removeWords, stopwords("en"))
corpus = tm_map(corpus, removeWords, sansinteret)


#""","'s","-","´","-","'","´","''","½","""
corpus[[1]]$content


# library("SnowballC", lib.loc="~/R/R-3.5.1/library")
# corpus <- tm_map(corpus, stemDocument, language = "english")


#*********************
# save(corpus, file= '~/R/data/corpus_ICAIL2017.rdata')


adtm <-DocumentTermMatrix(corpus) 
adtm <- removeSparseTerms(adtm, 0.3)


# save(corpus, file= '~/R/data/corpus_justice.rdata')

# round(memory.limit()/2^20, 2)
# memory.limit()
# memory.limit(size = 8000)


# load('~/R/data/corpus_justice.rdata')
sentences <- tokenize_sentences(corpus[[1]]$content)
sentences
par =  sentences[[1]][[1]]
par
x <- VCorpus(par)

# Creation de la matrice termes-documents
TDM = TermDocumentMatrix(par)
inspect(TDM)
matrixDecision2Justice = as.matrix(TDM)
# save(matrixDecision2Justice, file= '~/R/data/matrice_decision_de_justice.rdata')

# Tri de la matrice des décisions de justice
sortedmatrix = sort(rowSums(matrixDecision2Justice),decreasing=TRUE)
# Tri par fréquence
frequence = data.frame(word = names(sortedmatrix),freq=sortedmatrix)
# Quels sont les 10 mots les plus fréquents ?
tete = head(frequence, 10)
tete

# Présentation sous forme d'histogramme

win.graph(800,600,10)


barplot(height=tete$freq, names.arg=tete$word,
        xlab="Mots", ylab="Fréquence", 
        col="#973232", main="Analyse des mots présents dans les décisions de justice")


frequence[frequence$word=="critical",]



# write.table(frequence, file = "~/R/data/Frequence.CSV")


library(wordcloud2)

# Présentation sous forme de nuage de mots
wordcloud2(frequence)
#, rot.per=FALSE, colors= c("#973232", "#1E5B5B", "#6D8D2F", "#287928"))

# En sélectionnant les termes les plus fréquents
#By selecting the most common terms
inspect(removeSparseTerms(TDM, 0.8))
findFreqTerms(TDM, 9000)

# Quels termes sont les plus associés à "ethics" ?
#What terms are most associated with "ethics"?
resultatEthics <- findAssocs(TDM, "ethics", 0.01)

# Quels termes sont les plus associés à "disabled" ?
#What terms are most associated with "disabled"?
findAssocs(TDM, "disabled", 0.4)

# Avec l'approche "Recherche d'information"
#With the "Search for Information" approach
mtd4.TfIdf <- (DocumentTermMatrix(corpus, control=list(weighting=weightTfIdf)))

# Représentation des documents X termes
dim(mtd4.TfIdf)
# Inspection des 5 premiers documents et des termes 8000-8005
inspect (mtd4.TfIdf[1:5, 500:505])

mtd4.TfIdf02 <- as.matrix(removeSparseTerms(
  DocumentTermMatrix(corpus, control=list(weighting=weightTfIdf)),
  0.79))
dim(mtd4.TfIdf02)

# Matrice de distance et clustering hiérarchique
dist4 <- dist(mtd4.TfIdf, method="euclidean")
dist4
hc4 <- hclust(dist4, method="ward.D2")

plot(hc4)
# ça ne marche pas ici (pas de vrais groupes trouvés)
#it doesn't work here (no real groups found)

# Avec d'autres paramètres
dist5 <- dist(mtd4.TfIdf02, method="euclidean")
dist5
hc5 <- hclust(dist5, method="ward.D2")
plot(hc5)

kmeans4 <- kmeans (mtd4.TfIdf02, centers=6)
# affichage des clusters
kmeans4$cluster

# Avec une analyse en composantes principales
corpus.pca <- princomp(matrixDecision2Justice)
win.graph(800, 600, 10)
biplot(corpus.pca)
# graphique peu lisible -> sélection de moins de termes

library(FactoMineR)
pca <- PCA((as.matrix(matrixDecision2Justice)), scale.unit = F)


# Approche de l'Allocation de Dirichlet latente (LDA)
#Approach to the Latent Dirichlet Allowance (LDA)
library(topicmodels)

# Initialisation des paramètres pour de l'échantillonnage de Gibbs
#Initialization of parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

# Nombre de thèmes (topics)
k <- 5

# Lancement du LDA avec l'échantillonnage de Gibbs
ldaOut <-LDA(matrixDecision2Justice,k, method="Gibbs",
             control=list(nstart=nstart, 
                          seed = seed, 
                          best=best, 
                          burnin = burnin, 
                          iter = iter, 
                          thin=thin))
# Un "peu" long : le traitement met 3 minutes

# Sortie des résultats
# Documents en thèmes
ldaOut.topics <- as.matrix(topics(ldaOut))
ldaOut.topics

# write.csv(ldaOut.topics,file=paste("~/R/data/LDAGibbs",k,"DocsToTopics.csv"))

groupesDecision <- as.data.frame(ldaOut.topics)
groupesDecision$terme <- colnames(groupesDecision)

frequence
dffrequence <- as.data.frame(frequence)

dff2 <- dffrequence[order(dffrequence$word), ]

groupesDecision$word <- dff2$word
groupesDecision$frequence <- dff2$freq
groupesDecision$terme <- NULL
groupesClusters <- groupesDecision[order(groupesDecision$frequence, decreasing = TRUE), ]

groupesClustersBis <- groupesClusters[order(groupesClusters$V1), ]

groupesClustersTer <- groupesClustersBis[groupesClustersBis$frequence > 50, ]

# Mots les plus fréquents (supérieurs à 50 occurrences) 
# associés à chaque groupe de thème...
View(groupesClustersTer)

#write.csv(groupesClustersTer,file=paste0("~/R/data/MotsRepresentatifs_",k,"_Topics.csv"))

###########################################################

summary(frequence)
ldaOut.topics

topics <- as.integer(ldaOut.topics)
nomstopics <- rownames(ldaOut.topics)

nomstopics
sortedmatrix

frequence = data.frame(word = names(sortedmatrix),freq=sortedmatrix)



#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,6))
write.csv(ldaOut.terms,file=paste("~/R/data/LDAGibbs",k,"TopicsToTerms.csv"))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
write.csv(topicProbabilities,file=paste("~/R/data/LDAGibbs",k,"TopicProbabilities.csv"))


#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(matrixDecision2Justice),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])


#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(matrixDecision2Justice),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])


#write to file
write.csv(topic1ToTopic2,file=paste("~/R/data/LDAGibbs",k,"Topic1ToTopic2.csv"))
write.csv(topic2ToTopic3,file=paste("~/R/data/LDAGibbs",k,"Topic2ToTopic3.csv"))

####

library("wordcloud")
library("RColorBrewer")
par(bg="grey30")
png(file="WordCloud.png",width=1000,height=700, bg="grey30")
wordcloud(d$word, d$freq, col=terrain.colors(length(d$word), alpha=0.9), random.order=FALSE, rot.per=0.3 )
title(main = "Most Used Terms in the ICAIL papers", font.main = 1, col.main = "cornsilk3", cex.main = 1.5)

