# Docs for endpoint https://developers.facebook.com/docs/graph-api/reference/page/ratings/
# This script builds on work found in the facebook-page-post-scraper repo by Max Woolf. To get a page access
# access token has a few hoops. Follow instruction in the notebook to see how you do this.
# There are no parameters for this object, so you cannot pass date from and date to etc

# Import required libraries
import urllib2
import json
import datetime
import csv
import time

from ConfigParser import SafeConfigParser

# Get config from an external file
config = SafeConfigParser()
config.read('../config/facebook.cfg')
app_id = config.get('facebook', 'app_id')
page_id = config.get('facebook', 'page_id')
access_token = config.get('facebook', 'page_access_token')

def request_until_succeed(url):
    req = urllib2.Request(url)
    success = False
    while success is False:
        try: 
            response = urllib2.urlopen(req)
            if response.getcode() == 200:
                success = True
        except Exception, e:
            print e
            time.sleep(5)

            print "Error for URL %s: %s" % (url, datetime.datetime.now())
            print "Retrying."

    return response.read()

# Needed to write tricky unicode correctly to csv
def unicode_normalize(text):
    return text.translate({ 0x2018:0x27, 0x2019:0x27, 0x201C:0x22, 0x201D:0x22,
                            0xa0:0x20 }).encode('utf-8')


# Get the page review data
def getFacebookPageReviewData(page_id, access_token, num_reviews):

    # Construct the URL string; see http://stackoverflow.com/a/37239851 for
    # Reactions parameters
    base = "https://graph.facebook.com/v2.8"
    node = "/%s/ratings" % page_id
    fields = "/?fields=has_rating,has_review,review_text,created_time,rating"
    parameters = "&limit=%s&access_token=%s" % (num_reviews, access_token)
    url = base + node + fields + parameters
    print(url)
    # retrieve data
    data = json.loads(request_until_succeed(url))

    return data


def processFacebookPageFeedReview(review):
    rating = review['rating']
    if 'review_text' in review.keys():
        review_text = unicode_normalize(review['review_text'])
    else:
        review_text = ''
    review_published = datetime.datetime.strptime(
            review['created_time'],'%Y-%m-%dT%H:%M:%S+0000')
    review_published = review_published + \
            datetime.timedelta(hours=-5) # EST
    review_published = review_published.strftime(
            '%Y-%m-%d %H:%M:%S') # best time format for spreadsheet programs

    return (rating, review_text, review_published)


def scrapeFacebookPageFeedReview(page_id, access_token):
    with open('%s_facebook_page_reviews.csv' % page_id, 'wb') as file:
        w = csv.writer(file)
        w.writerow(["rating", "review_text", "review_published"])

        has_next_page = True
        num_processed = 0   # keep a count on how many we've processed
        scrape_starttime = datetime.datetime.now()

        print "Scraping %s Facebook Page reviews: %s\n" % (page_id, scrape_starttime)

        reviews = getFacebookPageReviewData(page_id, access_token, 100)

        while has_next_page:
            for review in reviews['data']:
                w.writerow(processFacebookPageFeedReview(review))
                if 'reviews' in review:
                   w.writerow(processFacebookPageFeedReview(review))

                # output progress occasionally to make sure code is not
                # stalling
                num_processed += 1
                if num_processed % 100 == 0:
                    print "%s Reviews Processed: %s" % \
                        (num_processed, datetime.datetime.now())

            # if there is no next page, we're done.
            if 'next' in reviews['paging'].keys():
                reviews = json.loads(request_until_succeed(
                                        reviews['paging']['next']))
            else:
                has_next_page = False


        print "\nDone!\n%s Reviews Processed in %s" % \
                (num_processed, datetime.datetime.now() - scrape_starttime)


if __name__ == '__main__':
    scrapeFacebookPageFeedReview(page_id, access_token)