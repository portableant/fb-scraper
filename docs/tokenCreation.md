# Page token creation

To access some of the Graph's endpoints for data extraction, you will need an access token either as a user token or a
page token. These can have differing degrees of expiry. The below text and images will show you how to generate a token
and how to create a never expiring token.

*You must have admin rights on our Facebook page.*

### Set up an app

To collect data, you will need to set up an application on the [Facebook platform](https://developers.facebook.com/apps/).

First off click add new app and you will get an overlay with create a new appID (see below):

![Overlay to create new appID](/examples/createApp.png)

Once you have this, fill in the app name and your details and click create. You cannot use any variant of Facebook, FB etc
in the app name (so don't copy my example).

![Overlay filled in to create new appID](/examples/fillInDetails.png)

You will then get a captcha challenge screen, fill this in and away you go.

![Captcha screen](/examples/challengeScreen.png)

### Go back to graph explorer

Now you have a Facebook app, you will need to create your token with correct permissions. To do this, go to the
[Facebook Graph Explorer](https://developers.facebook.com/tools/explorer/)

You will now need to get an access token and you will need to check the scope boxes that you want. You will see this page:

![Permissions screen](/examples/permissionsCheck.png)

Check the boxes for: "read_insights, manage_pages, public_profile". (Public profile is included by default and if you have
admin rights, manage_pages will be checked.) If you do not check the read_insights box, you cannot get insights data.

You now have an access token and you need to get a page access token. To do this, you will need to choose the app that
you created from the dropdown on the top right. When you have selected the app, you can then request the page access token
for the pages you are admin on.

![Choose app from dropdown](/examples/choosePage.png)

This token expires in one hour, useful hey? No, you want to convert this to a long lived token. So click on the blue exclaimation mark
in the bar with the token string. You will see something like this:

![Access token info](/examples/accessTokenInfo.png)

At the bottom of this popup you will see a button labelled 'Open in Access Token Tool'.

Click this and you will get taken to another page and you will see a button in blue labelled extend access token. Click this!

![Extend Access token info](/examples/extendAccessToken.png)


You now have a never expiring token (until you revoke it or get removed from admin. Make a note of this and add it to your
config/facebook.cfg file so that you can use it with the scripts.








