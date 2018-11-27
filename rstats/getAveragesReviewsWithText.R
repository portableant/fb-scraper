#' ----
#' title: " A script for getting Facebook review average ratings"
#' author: "Daniel Pett"
#' date: 20/01/2017"
#' output: csv_document
#' output: png_document
#' ----
#' 
# Set up packages for mapping
list.of.packages <- c(
  "lubridate"
)

# Install packages if not already available
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Set the working directory
setwd("/Users/danielpett/githubProjects/DH_Facebook-Analysis/") #MacOSX
path <- getwd()
reviews <- read.csv('csv/britishmuseum_facebook_page_reviews_text.csv')

# Note that reviews with text only started in 2011
require(lubridate)

years <- year(as.Date(reviews$review_published, "%d-%b-%y"))
dates <- as.Date(reviews$review_published)

years <- year(dates)
months <- month(dates)
day <- day(dates)

reviews$year <- years
reviews$month <- months
reviews$day <- day

averages <- tapply(reviews$rating, years, mean)
averagesMonths <- tapply(reviews$rating, months, mean)

# Write CSV files
write.csv(t(averages), file='csv/britishmuseum_facebook_page_reviews_ratings_average.csv',row.names=FALSE, na="")
write.csv(t(averagesMonths), file='csv/britishmuseum_facebook_page_reviews_ratings_average_by_month.csv',row.names=FALSE, na="")

# Produce graph by year
file <- paste0(path, '/plots/barPlotAverageRatingByYear.png')
png(file = file )
barplot(averages, col="red", xlab = 'Facebook ratings average by year', 
     main = 'A plot of Facebook ratings average by year')
dev.off()

# Produce graph by month
file <- paste0(path, '/plots/barPlotAverageRatingByMonth.png')
png(file = file )
barplot(averagesMonths, col="red", xlab = 'Facebook ratings average by month', 
        main = 'A plot of Facebook ratings average by month')
dev.off()

# years of operation
years <- c('2011','2012','2013', '2014', '2015', '2016', '2017')

# Loop through years and get length of comments
lengthByYear <- NULL
for(i in years){
  date <- i
  averageWords <- mean(as.numeric(sapply(strsplit(as.character(reviews[reviews$year == i,]$review_text), "\\s+"), length)))
  lengthByYear = rbind(lengthByYear, data.frame(date, averageWords))
}

# Write csv file of these values
write.csv(lengthByYear, file='csv/britishmuseum_facebook_page_reviews_ratings_word_length_by_year.csv',row.names=FALSE, na="")
# Produce graph
file <- paste0(path, '/plots/reviewWordCountByYear.png')
png(file = file )
plot(lengthByYear,type = "p")
dev.off()
