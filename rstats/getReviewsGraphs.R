#' ----
#' title: " A script for creating Facebook review graphs from the BritishMuseum page"
#' author: "Daniel Pett"
#' date: 20/01/2017"
#' output: png_document
#' ----
#' 

# Set the working directory
setwd("/Users/danielpett/githubProjects/DH_Facebook-Analysis/") #MacOSX
path <- getwd()

# Let's begin with reviews with text only
reviews <- read.csv('csv/britishmuseum_facebook_page_reviews_text.csv')

# Order the DF by rating score
ordered <- reviews[order(reviews$rating),]
orderedFilter <- ordered[!is.na(ordered$review_text),]
ratings <- orderedFilter$rating

# Create a histogram plot
file <- paste0(path, '/plots/histogramAllTimeReviews.png')
png(file = file )
hist(ratings, breaks=5, col="red", xlab = 'Facebook ratings (out of 5)', 
     main = 'A plot of Facebook ratings (all time)')
dev.off()

# Create kernel density plot
d <- density(orderedFilter$rating)
file <- paste0(path, '/plots/kernelDensityAllTimeReviews.png')
png(file = file )
plot(d, main="Kernel Density of FB ratings")
polygon(d, col="red", border="blue")
dev.off()

# Now let's plot all ratings
reviewsAll <- read.csv('csv/britishmuseum_facebook_page_reviews.csv')

# Order the DF by rating score
orderedAll <- reviewsAll[order(reviewsAll$rating),]
ratingsAll <- orderedAll$rating

# Create a histogram plot
file <- paste0(path, '/plots/histogramAllTimeReviewsTextOrNot.png')
png(file = file )
hist(ratingsAll,  breaks=5, col="red", xlab = 'Facebook ratings (out of 5)', 
     main = 'A plot of Facebook ratings (all time)')
dev.off()

# Create kernel density plot
d <- density(ratingsAll)
file <- paste0(path, '/plots/kernelDensityAllTimeReviewsTextOrNot.png')
png(file = file )
plot(d, main="Kernel Density of FB ratings")
polygon(d, col="red", border="blue")
dev.off()