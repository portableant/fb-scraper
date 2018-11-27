#' ----
#' title: " A script for analysing Facebook statuses from the BritishMuseum page"
#' author: "Daniel Pett"
#' date: 20/01/2017"
#' output: png_document
#' ----

# Set the working directory
setwd("/Users/danielpett/githubProjects/DH_Facebook-Analysis/") #MacOSX
path <- getwd()

# Statuses load 
statuses <- read.csv('csv/britishmuseum_facebook_statuses.csv')

# Comments load
comments <- read.csv('csv/britishmuseum_facebook_comments.csv')

# Merge comments and statuses
noise <- merge(comments, statuses, by = 'status_id')

# Filter columns
keeps <- c(
  "status_id", "comment_id", "comment_message", "status_message", "comment_author", 
  "comment_published", "status_published", "status_type", "num_reactions", "num_comments"
  )
noiseFiltered <- noise[,(names(noise) %in% keeps)]

# Order columns
prefOrder <- c(
  "status_id", "status_message", "comment_message"," comment_author", "status_type",
  "num_reactions", "num_comments", "status_published", "comment_id", "comment_published"
  )

noiseFiltered <- noiseFiltered[ , prefOrder]
noiseFiltered <- noiseFiltered[order(noiseFiltered$status_id),]

# Write to CSV for later use - note creates quite a large file.
write.csv(noiseFiltered, file='csv/britishmuseum_facebook_page_status_comments_merged.csv',row.names=FALSE, na="")

# Set up packages for mapping
list.of.packages <- c(
  "lubridate"
)
# Install packages if not already available
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Note that reviews with text only started in 2011
require(lubridate)

dates <- as.Date(noiseFiltered$status_published)

years <- year(dates)
months <- month(dates)
day <- day(dates)

noiseFiltered$status_published_year <- years
noiseFiltered$status_published_month <- months
noiseFiltered$status_published_day <- day

filteredUnique <- noiseFiltered[!duplicated(noiseFiltered$status_id),]

averageComments <- tapply(filteredUnique$num_comments, filteredUnique$status_published_year, mean)
averageReactions <- tapply(filteredUnique$num_reactions, filteredUnique$status_published_year, mean)

# Write CSV files
write.csv(t(averageComments), file='csv/britishmuseum_facebook_page_statuses_average_comments_year.csv',row.names=FALSE, na="")
write.csv(t(averageReactions), file='csv/britishmuseum_facebook_page_statuses_average_reactions_year.csv',row.names=FALSE, na="")

averageCommentsMonth <- tapply(filteredUnique$num_comments, filteredUnique$status_published_month, mean)
averageReactionsMonth <- tapply(filteredUnique$num_reactions, filteredUnique$status_published_month, mean)

write.csv(t(averageCommentsMonth), file='csv/britishmuseum_facebook_page_statuses_average_comments_month.csv',row.names=FALSE, na="")
write.csv(t(averageReactionsMonth), file='csv/britishmuseum_facebook_page_statuses_average_reactions_month.csv',row.names=FALSE, na="")

# Set up date range sequence
annuals <- seq(2009, 2017, by=1)

# Filter reactions by year average
reactionsByYear <- NULL
for(a in annuals){
    date <- a
    meanReactions <- mean(filteredUnique[filteredUnique$status_published_year == as.character(a),]$num_reactions)
    reactionsByYear = rbind(reactionsByYear, data.frame(date, meanReactions))
}

# Filter comments by year average
commentsByYear <- NULL
for(a in annuals){
  date <- a
  meanComments <- mean(filteredUnique[filteredUnique$status_published_year == as.character(a),]$num_comments)
  commentsByYear = rbind(commentsByYear, data.frame(date, meanComments))
}

# Write CSV file
write.csv(commentsByYear, file='csv/britishmuseum_facebook_page_statuses_average_comments_year.csv',row.names=FALSE, na="")
write.csv(reactionsByYear, file='csv/britishmuseum_facebook_page_statuses_average_reactions_year.csv',row.names=FALSE, na="")

# Filter for authors
filteredAuthors <- noiseFiltered[!duplicated(noiseFiltered$comment_author),]
filteredAuthorsCut <- filteredAuthors[,c("status_message","comment_author")]
# Write CSV
write.csv(filteredAuthorsCut, file='csv/britishmuseum_facebook_page_statuses_comment_author_list.csv',row.names=FALSE, na="")

# Print the number of authors
print(nrow(filteredAuthorsCut))

# Get commentator count
commentators <- as.data.frame(table(noiseFiltered$comment_author))
names(commentators) <- c("author","frequency")
# Sort by frequency
commentators <- commentators[order(-commentators$frequency),]
# Print to CSV
write.csv(commentators, file='csv/britishmuseum_facebook_page_statuses_comment_author_table.csv',row.names=FALSE, na="")

# Get stats of statuses
uniqueStatuses <- filteredUnique[,c("status_type", "status_published_year", "status_published_month", "num_reactions", "num_comments")]
tableUniqueStatuses <- table(uniqueStatuses$status_type)
t <- as.data.frame(tableUniqueStatuses)
names(t) <- c("type","frequency")
write.csv(t, file='csv/britishmuseum_facebook_page_statuses_type.csv',row.names=FALSE, na="")
