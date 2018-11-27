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

# Read in the csv file
reviews <- read.csv('csv/britishmuseum_facebook_page_reviews.csv')

# Amend missing values to NA
reviews[ reviews == "" ] = NA

# Filter for just review text
filtered <- reviews[!is.na(reviews$review_text),] 

# Write the CSV file
write.csv(filtered, file='csv/britishmuseum_facebook_page_reviews_text.csv',row.names=FALSE, na="")