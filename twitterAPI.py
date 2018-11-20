import tweepy 
from tweepy import API
from tweepy import Cursor
from tweepy.streaming import StreamListener
from tweepy import OAuthHandler
from tweepy import Stream
import numpy as np
import pandas as pd
import sys

import creds

def connect_to_twitter():
    auth = OAuthHandler(creds.CONSUMER_KEY, creds.CONSUMER_SECRET)
    auth.set_access_token(creds.ACCESS_TOKEN, creds.ACCESS_TOKEN_SECRET)
    api = API(auth)
    return api

def tweets_to_df(tweets):
	df = pd.DataFrame(data = [tweet.text for tweet in tweets], columns = ['Tweets'])
	return df


if __name__ == "__main__":

	if len(sys.argv) == 6:
		query = sys.argv[1:4]
		geo = sys.argv[4]
		filename = sys.argv[5]
		api = connect_to_twitter()
		max_tweets = 20000

		searched_tweets = [status for status in tweepy.Cursor(api.search, q=query, geocode = geo, 
		wait_on_rate_limit = True, show_user = True).items(max_tweets)]

		df = tweets_to_df(searched_tweets)

		df.to_json(filename, orient = "records", lines = True)

		print(len(searched_tweets))
		print(df.tail())
	else:
		print("invalid command line args")







