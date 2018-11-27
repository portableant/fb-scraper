# Facebook data tools for British Museum use

This set of scripts is for the use of British Museum staff who want to analyse the Facebook MARKETING page. The majority
of the code in this has been reused under MIT license from Max Woolf's excellent Facebook scraper package, some new
scripts have been added for manipulating reviews data and for stats analysis (using R.)


## Privacy

These scripts *do not collect identifiable personal data*. You can extend this to do so if you want to do, ethics should
be considered.

### Compliance

These scripts only use officially-documented Facebook API endpoints without circumventing any rate-limits.
This uses version 2.8 of the Facebook Graph API.

# To use these scripts

* You will need to have admin status on the Facebook page for the reviews script.
* You will need to have created a [never expiring token](docs/tokenCreation.md) if you want to use this frequently with no faff.
* You will need to have created an app in Facebook developer portal.
* You will need to copy facebook.cfg.template to facebook.cfg and fill in the variables appropriately.
* Be able to run Python and R on your workstation.

# Authors

Daniel Pett ([@portableant](https://github.com/portableant))
Incorporating large amounts of Max Woolf's code ([@minimaxir](https://github.com/minimaxir/facebook-page-post-scraper))

### Scrape reviews from page

The page scraper is written in Python 2.7. To run this make sure you are in the directory with the script and have
python 2.7 installed.:

`python get fb_reviews_fb_page.py`

When this runs you will see something like:

![](/examples/fb_page_scraper_terminal.png)

This will then dump all the available reviews and ratings into a CSV file which can then be manipulated. I chose R to do
this.

### Scrape posts from a public page

The Page data scraper is implemented as a Python 2.7 script in `get_fb_posts_fb_page.py`. Enter app_id, secret and page_id
in the facebook.cfg file and then run the script by `cd` into the directory containing the script, then running:

`python get_fb_posts_fb_page.py`.

### Scrape Comments From Page/Group Posts

To scrape all the user comments from the posts, create dump CSV of statuses using above scripts, then run:

`python get_fb_comments_from_fb.py`

script, specifying the Page/Group as the 'file_id'. The output includes the original 'status_id' where the comment is
located so you can map the comment to the original Post with a 'JOIN' or 'VLOOKUP', and also a '    parent_id' if the
comment is a reply to another comment. Scraping throughput is approximately 87k comments/hour.

## License

MIT

If you do find this script useful, a link back to this repository would be appreciated. Thanks!