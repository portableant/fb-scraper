#' ----
#' title: " A script for filtering Facebook reviews from the BritishMuseum page"
#' author: "Daniel Pett"
#' date: "11/01/2017"
#' output: csv_document
#' ----
#' 

# Set the working directory
setwd("/Users/danielpett/githubProjects/DH_Facebook-Analysis/") #MacOSX

# Create CSV directory if it does not exist
if (!file.exists('csv')){dir.create('csv')}

# Load up your list of required packages
list.of.packages <- c(
  "lubridate", "textcat", "plyr", "rvest", "stringr"
)

# Install packages if not there
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Read in the csv file
reviews <- read.csv('csv/britishmuseum_facebook_page_reviews.csv')

# Amend missing values to NA
reviews[ reviews == "" ] = NA

# Filter for just review text
filtered <- reviews[!is.na(reviews$review_text),] 

# Write the CSV file
write.csv(filtered, file='csv/britishmuseum_facebook_page_reviews_text.csv',row.names=FALSE, na="")

# Order the DF by rating score
ordered <- reviews[order(reviews$rating),]
orderedFilter <- ordered[!is.na(ordered$review_text),] 
# Create a histogram
hist(orderedFilter$rating,breaks=2, col="red")

# Create kernel density
d <- density(orderedFilter$rating)
plot(d, main="Kernel Density of FB ratings")
polygon(d, col="red", border="blue")

# Get mean length of reviews
meanReviews <- mean(sapply(filtered$review_text,function(x)length(unlist(gregexpr(" ",x)))+1))

# Add libraries for word clouds
library(tm)
library(SnowballC)
library(wordcloud)
ratingCorpus <- Corpus(VectorSource(orderedFilter$review_text))
ratingCorpus <- tm_map(ratingCorpus, PlainTextDocument)
ratingCorpus <- tm_map(ratingCorpus, removePunctuation)
ratingCorpus <- tm_map(ratingCorpus, removeWords, c('the', 'this', stopwords('english')))
wordcloud(ratingCorpus, max.words = 100, random.order = FALSE)

# Do same for reviews with scores of 3 or less
a <- orderedFilter[which(orderedFilter$rating <= 3), ]
ratingCorpusA <- Corpus(VectorSource(a$review_text))
ratingCorpusA <- tm_map(ratingCorpusA, PlainTextDocument)
ratingCorpusA <- tm_map(ratingCorpusA, removePunctuation)
ratingCorpusA <- tm_map(ratingCorpusA, removeWords, c('the', 'this', stopwords('english')))
wordcloud(ratingCorpusA, max.words = 100, random.order = FALSE)

# Attempt some language detection
library("textcat")
library("rvest")
library("stringr")
filtered$languages <- textcat(filtered$review_text)
write.csv(filtered, file='csv/britishmuseum_facebook_page_reviews_text_language_detect.csv',row.names=FALSE, na="")

# Do some visualisation stuff
library(lubridate)
reviews$month <- floor_date(as.POSIXct(reviews$review_published), unit = "month")

# Plot comments
data <- read.csv('csv/britishmuseum_facebook_comments.csv')
commentCorpus <- Corpus(VectorSource(data$comment_message))
commentCorpus <- tm_map(commentCorpus, PlainTextDocument)
commentCorpus <- tm_map(commentCorpus, removePunctuation)
commentCorpus <- tm_map(commentCorpus, removeWords, c('the', 'this', stopwords('english')))
wordcloud(commentCorpus, max.words = 100, random.order = FALSE)

head(data$comment_published)
data$date <- as.Date(data$comment_published)
dataSummary <- lapply(split(data, cut(data$date, "1 year")), function(x) summary(x[2]))
print(dataSummary[4])

# Calculate mean length of comments in words.
meanComments <- mean(sapply(data$comment_message,function(x)length(unlist(gregexpr(" ",x)))+1))

# Some statistical analysis

# Number of unique commentators
length(unique(data$comment_author))

# Day
for(i in data$comment_published){
  print(strsplit(as.character(i), "-")[[1]][3])
}
# Month
for(i in data$comment_published){
  print(strsplit(as.character(i), "\\s+")[[1]][2])
}

# Year 
for(i in blogtext$date){
  print(strsplit(as.character(i), "\\s+")[[1]][3])
}

# time
for(i in blogtext$date){
  print( 
    strptime(   
      paste0(strsplit(as.character(i), "\\s+")[[1]][4], ' ', strsplit(as.character(i), "\\s+")[[1]][5])
      , "%I:%M %p"), format = "%H:%M:%S") 
  ))
}


# How many posts in each year?
table(data$comment_published)

# years of operation
years <- c('2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017')

# Loop through years and get length of comments
for(a in years){
  length(unlist(lapply(data[data$year == a,]$comment_message, function(i) strsplit(i, " ")[[1]])))
}

# how many authors per year?
for(a in years) {
  length(unique(blogtext[blogtext$year == a,]$author))
}

#words per post per year
for(a in years) {
  length(unlist(lapply(blogtext[blogtext$year == a,]$text, function(i) strsplit(i, " ")[[1]])))/table(blogtext$year)[[1]]
}

# plot distribution of words
# how many words per post?
wpp <- data.frame(
  words = unlist(lapply(blogtext$text, function(i) length(strsplit(i, " ")[[1]]))),
  year = blogtext$year, stringsAsFactors = FALSE)

# only look at 2012 and 2013
wpp <- wpp[wpp$year %in% c(2010, 2011, 2012, 2013, 2014),]

# function for labels
nlabels <- table(wpp$year)

#  To create the median labels, you can use by
meds <- round(c(by(wpp$words, wpp$year, mean)),0)

# make the plot
require(ggplot2)
ggplot(wpp, aes(as.factor(year), words, label=rownames(wpp))) +
  geom_violin() +
  geom_text(data = data.frame(), aes(x = names(meds) , y = meds, 
                                     label = paste("mean =", meds))) +
  xlab("Year")

# check if a certain word is present at all 
require(tm)
# create corpus
corp <- Corpus(VectorSource(blogtext[,1]))
# if using CSV file do this instead of the line above
# corp <- Corpus(VectorSource(blogtext[,2]))
# process text 
skipWords <- function(x) removeWords(x, stopwords("english"))
funcs <- list(tolower, removePunctuation, removeNumbers, stripWhitespace, skipWords)
corp <- tm_map(corp, FUN = tm_reduce, tmFuns = funcs)
# create document term matrix
dtm <- DocumentTermMatrix(corp, control = 
                            # limit word lengths
                            list(wordLengths = c(2,10))) # , 
## A few other options for text mining
# control weighting
# weighting = weightTfIdf, 
# keep words in more than 1%
# and less than 95% of docs
# bounds = list(global = c(
#  length(corp)*0.01,length(corp)*0.95))))

# how many times does the word 'pyramid' occur in this document term matrix?
dtmdf <- data.frame(inspect(dtm))
sum(dtmdf[, names(dtmdf) == 'pyramid'])

# Indiana Jones comparison

# list words of interest - things archys use
IJ1 <- c('trowel', 'shovel', 'spade', 'gun', 'whip', 'fedora', 'computer', 'pen')
# get word counts in document term matrix
IJ2 <- dtmdf[, intersect(names(dtmdf), IJ1)]
# find words that don't occur in dtm at all
notin <- setdiff(IJ1, names(dtmdf) )
# append of cols of zeros for these words not in the dtm
IJ3 <- cbind(IJ2, replicate(length(notin), rep(0,nrow(IJ2))) )
# edit col names
names(IJ3) <- c(names(IJ2), notin)
# reshape for plotting                   
require(reshape2)
IJ4 <- melt(IJ3)
require(ggplot2)
ggplot(IJ4, aes(reorder(variable,-value), value)) + 
  geom_bar(stat="identity") +
  xlab("Things archaeologists use in the field") +
  ylab("Term Frequency") + theme(axis.text.x = element_text(colour="grey20",size=17,angle=0,hjust=.5,vjust=.5,face="plain"),
                                 axis.text.y = element_text(colour="grey20",size=15,angle=0,hjust=1,vjust=0,face="plain"),  
                                 axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
                                 axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))


# list words of interest - things archys fear
IJ1 <- c('tunnels', 'cliffs', 'heat', 'cold', 'insects', 'snakes', 'bears', 'nazis', 'aliens')
# get word counts in document term matrix
IJ2 <- dtmdf[, intersect(names(dtmdf), IJ1)]
# find words that don't occur in dtm at all
notin <- setdiff(IJ1, names(dtmdf) )
# append of cols of zeros for these words not in the dtm
IJ3 <- cbind(IJ2, replicate(length(notin), rep(0,nrow(IJ2))) )
# edit col names
names(IJ3) <- c(names(IJ2), notin)
# reshape for plotting                   
require(reshape2)
IJ4 <- melt(IJ3)
require(ggplot2)
ggplot(IJ4, aes(reorder(variable,-value), value)) + 
  geom_bar(stat="identity") +
  xlab("Dangers faced by archaeologists") +
  ylab("Term Frequency") + theme(axis.text.x = element_text(colour="grey20",size=17,angle=0,hjust=.5,vjust=.5,face="plain"),
                                 axis.text.y = element_text(colour="grey20",size=15,angle=0,hjust=1,vjust=0,face="plain"),  
                                 axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
                                 axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))

# what kinds of artefacts do archys study?

# list words of interest - things archys study
IJ1 <- c('pottery', 'bones', 'pollen', 'stones', 'bricks', 'wood', 'metal', 'treasure', 'grail')
# get word counts in document term matrix
IJ2 <- dtmdf[, intersect(names(dtmdf), IJ1)]
# find words that don't occur in dtm at all
notin <- setdiff(IJ1, names(dtmdf) )
# append of cols of zeros for these words not in the dtm
ifelse(length(notin) == 0,
       IJ3 <- IJ2,
       IJ3 <- cbind(IJ2,  replicate(length(notin), rep(0,nrow(IJ2))) )
)
# edit col names
names(IJ3) <- c(names(IJ2), notin)
# reshape for plotting                   
require(reshape2)
IJ4 <- melt(IJ3)
require(ggplot2)
ggplot(IJ4, aes(reorder(variable,-value), value)) + 
  geom_bar(stat="identity") +
  xlab("Things archaeologists study") +
  ylab("Term Frequency") + theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
                                 axis.text.y = element_text(colour="grey20",size=15,angle=0,hjust=1,vjust=0,face="plain"),  
                                 axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
                                 axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))

# what about gender?

# list words of interest - things archys study
IJ1 <- c('he', 'him', 'his', 'she', 'her', 'hers')
# get word counts in document term matrix
IJ2 <- dtmdf[, intersect(names(dtmdf), IJ1)]
# find words that don't occur in dtm at all
notin <- setdiff(IJ1, names(dtmdf) )
# append of cols of zeros for these words not in the dtm
ifelse(length(notin) == 0,
       IJ3 <- IJ2,
       IJ3 <- cbind(IJ2,  replicate(length(notin), rep(0,nrow(IJ2))) )
)
# edit col names
names(IJ3) <- c(names(IJ2), notin)
# reshape for plotting                   
require(reshape2)
IJ4 <- melt(IJ3)
require(ggplot2)
ggplot(IJ4, aes(reorder(variable,-value), value)) + 
  geom_bar(stat="identity") +
  xlab("Use of gender-specific pronouns by archaeologists") +
  ylab("Term Frequency") + theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
                                 axis.text.y = element_text(colour="grey20",size=15,angle=0,hjust=1,vjust=0,face="plain"),  
                                 axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
                                 axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain"))

# aggregate male and female pronouns
IJ5 <- data.frame(male =   rowSums(cbind(IJ3$he, IJ3$him, IJ3$his)), 
                  female = rowSums(cbind(IJ3$she, IJ3$her, IJ3$hers)))
IJ6 <- melt(IJ5)
require(ggplot2)
ggplot(IJ6, aes(reorder(variable,-value), value, fill = variable))+ 
  geom_bar(stat="identity") +
  xlab("Use of gender-specific pronouns by archaeologists") +
  ylab("Term Frequency") + theme(axis.text.x = element_text(colour="grey20",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
                                 axis.text.y = element_text(colour="grey20",size=15,angle=0,hjust=1,vjust=0,face="plain"),  
                                 axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=0,face="plain"),
                                 axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=.5,face="plain")) +
  scale_fill_manual(values=c("blue", "#FF0066"))
#' Topic modelling with MALLET using clean fulltext
#' based on http://www.cs.princeton.edu/~mimno/R/


require(mallet)
documents <- data.frame(text = blogtext$text,
                        id =   make.unique(blogtext$author),
                        class = blogtext$year, 
                        stringsAsFactors=FALSE)

mallet.instances <- mallet.import(documents$id, documents$text, "/Users/danielpett/Documents/stop.txt", token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

## Create a topic trainer object.
n.topics <- 30
topic.model <- MalletLDA(n.topics)

## Load our documents. We could also pass in the filename of a 
##  saved instance list file that we build from the command-line tools.
topic.model$loadDocuments(mallet.instances)

## Get the vocabulary, and some statistics about word frequencies.
##  These may be useful in further curating the stopword list.
vocabulary <- topic.model$getVocabulary()
word.freqs <- mallet.word.freqs(topic.model)

## Optimize hyperparameters every 20 iterations, 
##  after 50 burn-in iterations.
topic.model$setAlphaOptimization(20, 50)

## Now train a model. Note that hyperparameter optimization is on, by default.
##  We can specify the number of iterations. Here we'll use a large-ish round number.
topic.model$train(200)

## NEW: run through a few iterations where we pick the best topic for each token, 
##  rather than sampling from the posterior distribution.
topic.model$maximize(10)

## Get the probability of topics in documents and the probability of words in topics.
## By default, these functions return raw word counts. Here we want probabilities, 
##  so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)

# from http://www.cs.princeton.edu/~mimno/R/clustertrees.R
## transpose and normalize the doc topics
topic.docs <- t(doc.topics)
topic.docs <- topic.docs / rowSums(topic.docs)

## Get a vector containing short names for the topics
topics.labels <- rep("", n.topics)
for (topic in 1:n.topics) topics.labels[topic] <- paste(mallet.top.words(topic.model, topic.words[topic,], num.top.words=5)$words, collapse=" ")
# have a look at keywords for each topic
topics.labels



# create data.frame with columns as authors and rows as topics
topic_docs <- data.frame(topic.docs)
names(topic_docs) <- documents$id

# find top n topics for a certain author
df1 <- t(topic_docs[,grep("Daniel Pett", names(topic_docs))])
colnames(df1) <- topics.labels
require(reshape2)
topic.proportions.df <- melt(cbind(data.frame(df1),
                                   document=factor(1:nrow(df1))),
                             variable.name="topic",
                             id.vars = "document") 
# plot for each doc by that author
require(ggplot2)
ggplot(topic.proportions.df, aes(topic, value, fill=document)) +
  geom_bar(stat="identity") +
  ylab("proportion") +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol=5)

## cluster based on shared words
plot(hclust(dist(topic.words)), labels=topics.labels)


## How do topics differ across different years?

topic_docs_t <- data.frame(t(topic_docs))
topic_docs_t$year <- documents$class
# now we have a data frame where each row is a topic and 
# each column is a document. The cells contain topic 
# proportions. The next line computes the average proportion of
# each topic in all the posts in a given year. Note that in 
# topic_docs_t$year there is one FALSE, which dirties the data
# slightly and causes warnings
df3 <- aggregate(topic_docs_t, by=list(topic_docs_t$year), FUN=mean)
# this next line transposes the wide data frame created by the above
# line into a tall data frame where each column is a year. The 
# input data frame is subset using the %in% function 
# to omit the last row because this
# last row is the result of the anomalous FALSE value that 
# is in place of the year for one blog post. This is probably
# a result of a glitch in the blog page format. I also exclude
# the last column because it has NAs in it, a side-effect of the
# aggregate function above. Here's my original line:
# df3 <- data.frame(t(df3[-3,-length(df3)]), stringsAsFactors = FALSE)
# And below is an updated version that generalises this in case 
# you have more than two years:
years <- sort(as.character(na.omit(as.numeric(as.character(unique(topic_docs_t$year))))))
df3 <- data.frame(t(df3[(df3$Group.1 %in% years),-length(df3)]), stringsAsFactors = FALSE)
# now we put on informative column names
# names(df3) <- c("y2012", "y2013")
# Here's a more general version in case you have more than two years
# or different years to what I've got:
names(df3) <- unname(sapply(years, function(i) paste0("y",i)))
# the next line removes the first row, which is just the years
df3 <- df3[-1,]
# the next line converts all the values to numbers so we can 
# work on them
df3 <- data.frame(apply(df3, 2, as.numeric, as.character))
df3$topic <- 1:n.topics

# which topics differ the most between the years? 

# If you have 
# more than two years you will need to do things differently
# by adding in some more pairwise comparisons. Here is one 
# pairwise comparison:
df3$diff <- df3[,1] - df3[,2] 
df3[with(df3, order(-abs(diff))), ]
# # then if you had three years you might then do
# # a comparison of yrs 1 and 3
# df3$diff2 <- df3[,1] - df3[,3] 
# df3[with(df3, order(-abs(diff2))), ]
# # and the other pairwise comparison of yrs 2 and 3
# df3$diff3 <- df3[,2] - df3[,3] 
# df3[with(df3, order(-abs(diff3))), ]
## and so on


# plot
library(reshape2)
# we reshape from long to very long! and drop the 
# 'diff' column that we computed above by using a negatve 
# index, that's the -4 in the line below. You'll need to change
# that value if you have more than two years, you might find
# replacing it with -ncol(df3) will do the trick, if you just
# added one diff column. 
df3m <- melt(df3[,-4], id = 3)
ggplot(df3m, aes(fill = as.factor(topic), topic, value)) +
  geom_bar(stat="identity") +
  coord_flip()  +
  facet_wrap(~ variable)

library(cluster)
topic_df_dist <-  as.matrix(daisy(t(topic_docs), metric =  "euclidean", stand = TRUE))
# Change row values to zero if less than row minimum plus row standard deviation
# keep only closely related documents and avoid a dense spagetti diagram 
# that's difficult to interpret (hat-tip: http://stackoverflow.com/a/16047196/1036500)
topic_df_dist[ sweep(topic_df_dist, 1, (apply(topic_df_dist,1,min) + apply(topic_df_dist,1,sd) )) > 0 ] <- 0
#' Use kmeans to identify groups of similar authors

km <- kmeans(topic_df_dist, n.topics)
# get names for each cluster
allnames <- vector("list", length = n.topics)
for(i in 1:n.topics){
  allnames[[i]] <- names(km$cluster[km$cluster == i])
} 

# Here's the list of authors by group
allnames

library(igraph)
g <- as.undirected(graph.adjacency(topic_df_dist))
layout1 <- layout.fruchterman.reingold(g, niter=500)
plot(g, layout=layout1, edge.curved = TRUE, vertex.size = 1,  vertex.color= "grey", edge.arrow.size = 0, vertex.label.dist=0.5, vertex.label = NA)


# interactive in a web browser
#devtools::install_github("d3Network", "christophergandrud")
require(d3Network)
d3SimpleNetwork(get.data.frame(g),width = 1500, height = 800, 
                textColour = "orange", linkColour = "red", 
                fontsize = 10, 
                nodeClickColour = "#E34A33", 
                charge = -100, opacity = 0.9, file = "d3net.html")
# find the html file in working directory and open in a web browser

# for Gephi
# this line will export from R and make the file 'g.graphml' 
# in the working directory, ready to open with Gephi
write.graph(g, file="g.graphml", format="graphml") 