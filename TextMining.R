#World Cloud Lab

#Installing the required packages
required_packages <- c("tm", "SnowballC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")
install.packages(required_packages, dependencies=TRUE) 

#Installing the Open Source
install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

#Create a folder named "texts" in the C Drive and copy the iliad and odyssey files into it
#Loading the text files
cname <- file.path("C:", "texts")   
cname   
dir(cname) 


#Load the R package for text mining and then load your texts into R.
library(tm)   
document <- Corpus(DirSource(cname))   
summary(document) 

#For viewing or reading the respective files
inspect(document[1])
inspect(document[2])

##PART 1: Pre-processing
#Removing numbers, capitalization, common words, punctuation for preparing your texts for analysis.

#Remove Punctuation
document <- tm_map(document, removePunctuation)

#Removing Special Characters
for(i in seq(document))   
{   
  document[[i]] <- gsub("/", " ", document[[i]])   
  document[[i]] <- gsub("@", " ", document[[i]])   
  document[[i]] <- gsub("\\|", " ", document[[i]])   
} 

#Removing numbers
document <- tm_map(document, removeNumbers) 

#Converting to lowercase, for making it similar at all cases
document <- tm_map(document, tolower)

#To find the list of all stopwords in English and its total count
stopwords("english")
length(stopwords("english")) 

#Removing Stopwords
document <- tm_map(document, removeWords, stopwords("english"))   

#Removing particular words, which are not of value towards the analysis
document <- tm_map(document, removeWords, c("department", "email"))   

#Combining certain words that should be staying together
for (i in seq(document))
{
  document[[i]] <- gsub("Peloponnesian War", "PW", document[[i]])
  document[[i]] <- gsub("World War I", "WWI", document[[i]])
  document[[i]] <- gsub("World War II", "WWII", document[[i]])
}

# Removing common word endings (e.g., "ing", "es", "s"), which are referred as stemming documents
library(SnowballC) 
document <- tm_map(document, stemDocument)   

# Stripping unnecessary whitespace from your documents
document <- tm_map(document, stripWhitespace)  

# Checking whether everything worked fine 
inspect(document[1])
inspect(document[2])

# Finally changing the preprocessed documents as a text documents
#document <- tm_map(document, PlainTextDocument)


## PART2 : STAGING THE DATA

# Create a document term matrix
dtm <- DocumentTermMatrix(document)   

# Check for the dimensions, i.e. the number of documents and terms
dim(dtm)

# Inspect the document term matrix
inspect(dtm)
# Inspecting the iliad text file for its first 10 terms
inspect(dtm[1, 1:10])
# Inspecting the two documents and its first 20 terms
inspect(dtm[1:2, 1:20])


# Creating Term Document Matrix (Transpose matrix)
tdm <- TermDocumentMatrix(document) 


## PART 3: Exploring the data

# Organize terms by their frequencies
frequencies <- colSums(as.matrix(dtm))   
length(frequencies)
ord <- order(frequencies)

# Change the working directory to C:/texts
setwd("C:/texts")

# Exporting the matrix to Excel  
m1 <- as.matrix(dtm)   
dim(m1)   
write.csv(m1, file="dtm.csv") 

# Exporting the transpose matrix
m2 <- as.matrix(tdm)   
dim(m2)   
write.csv(m2, file="tdm.csv") 


# Removing Sparse terms, which are uninteresting and less frequently used words
dtm_sparse <- removeSparseTerms(dtm, 0.99)
inspect(dtm_sparse)

# Checking the word frequency
frequencies[head(ord)]
frequencies[tail(ord)]

# Checking out the top and low 20 frequency of the frequencies
head(table(frequencies), 20)
tail(table(frequencies), 20)

# Fine-grained look at terms after sparsing
frequencies <- colSums(as.matrix(dtm_sparse))

# Finding frequency which appears more frequently more than 50 times
findFreqTerms(dtm, lowfreq = 50)
wf <- data.frame(word=names(frequencies), frequencies=frequencies) 

# Plotting Word Frequency
library(ggplot2)   
pl <- ggplot(subset(wf, frequencies>250), aes(word, frequencies))    
pl <- pl + geom_bar(stat="identity")   
pl <- pl + theme(axis.text.x=element_text(angle=45, hjust=1))   
pl


## PART 4: STATISTICS

# Finding the relationship between terms
findAssocs(dtm_sparse, c("question", "analysis"), corlimit=0.95)
findAssocs(dtm_sparse, "question", corlimit=0.90)

# Word Clouds
# Loading the word cloud package
library(wordcloud)

# Setting the starting number used to generate sequence of random numbers
set.seed(123)
wordcloud(names(frequencies), frequencies, min.freq = 25) 
wordcloud(names(frequencies), frequencies, max.words=100) 
wordcloud(names(frequencies), frequencies, min.frq = 25, max.words=100) 

##Add some color and plot words occurring at least 250 times.
set.seed(123)   
wordcloud(names(frequencies), frequencies, min.frequencies=250, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))   

##Plot the 100 most frequently occurring words.
set.seed(123)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(frequencies), frequencies, max.words=100, rot.per=0.2, colors=dark2) 

# Removing more less frequent words, matrix with 15% empty space
dtmss <- removeSparseTerms(dtm, 0.15) 
inspect(dtmss) 


# Hierarchical Clustering

# Importing the library cluster
library(cluster)

## Calculating distance between words beginning with "a..." & then cluster them according to similarity.
d <- dist(t(dtm_sparse[,1:100]), method="euclidian")   
fit <- hclust(d=d, method="ward.D")   
fit 

# Ploting the hierarchical clustered data
plot(fit, hang=-1)  
plot(fit)


# Reading a Dendogram, where "k" refers to number of clusters used and making the border color as red

dend <- rect.hclust(fit, k=5, border="red") 
dend


# K-means clustering

# Importing the "fpc" library
library(fpc)   

d1 <- dist(t(dtm_sparse), method="euclidian")   
kfit <- kmeans(d1, 2)   

# Visualizing the clusters
clusplot(as.matrix(d1), 
         kfit$cluster, 
         color=T, 
         shade=T, 
         labels=2, 
         lines=0)   





