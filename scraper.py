#!/usr/bin/env python
# encoding: utf-8


#-- Imports ---------------------------------------------------------------------
import tweepy #https://github.com/tweepy/tweepy


#-- Settings --------------------------------------------------------------------

#Twitter API credentials
consumer_key = "CONSUMER"
consumer_secret = "CONSUMER_SECRET"
access_key = "ACCESS"
access_secret = "ACCESS_SECRET"


#-- Functions -------------------------------------------------------------------

def get_all_tweets(screen_name):
    """
    Get the last 3240 tweets for a given username
    """

    #authorize twitter, initialize tweepy
    auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_key, access_secret)
    api = tweepy.API(auth)

    #initialize a list to hold all the tweepy Tweets
    alltweets = []

    #make initial request for most recent tweets (200 is the maximum allowed count)
    new_tweets = api.user_timeline(screen_name=screen_name, count=200)

    #save most recent tweets
    alltweets.extend(new_tweets)

    #save the id of the oldest tweet less one
    oldest = alltweets[-1].id - 1

    #keep grabbing tweets until there are no tweets left to grab
    while len(new_tweets) > 0:
        print(f"getting tweets before {oldest}")

        #all subsiquent requests use the max_id param to prevent duplicates
        new_tweets = api.user_timeline(screen_name=screen_name, count=200, max_id=oldest)

        #save most recent tweets
        alltweets.extend(new_tweets)

        #update the id of the oldest tweet less one
        oldest = alltweets[-1].id - 1

        print(f"...{len(alltweets)} tweets downloaded so far")

    #transform the tweepy tweets into a 2D array that will populate the csv
    outtweets = [
        [tweet.id_str, tweet.created_at, tweet.text.encode("utf-8")]
        for tweet in alltweets]
    return outtweets


def main():
    """
    This is my docstring
    There are many others like it
    But this one is mine.
    """
	#pass in the username of the account you want to download
    tweets = get_all_tweets("raisedbyfinches")
    count = len(tweets)
    for tweet in tweets:
        count -= 1
        with open(f"data/tweet{str(count).zfill(4)}.txt", 'w') as handle:
            cleaned = tweet[2].decode('utf-8')
            handle.write(cleaned)

#-- Boilerplate -----------------------------------------------------------------
if __name__ == '__main__':
    main()
