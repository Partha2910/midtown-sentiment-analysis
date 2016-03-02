import pymongo
import tweepy
import time
from bson.json_util import dumps


CONSUMER_KEY = ''
CONSUMER_SECRET = ''
ACCESS_KEY = ''
ACCESS_SECRET = ''

auth = tweepy.OAuthHandler(CONSUMER_KEY, CONSUMER_SECRET)
auth.set_access_token(ACCESS_KEY, ACCESS_SECRET)
api = tweepy.API(auth, wait_on_rate_limit=True, wait_on_rate_limit_notify=True)



def search(query, limit=100):
	if limit > 0:
		tweets = [tweet for tweet in tweepy.Cursor(api.search,
																						 q=query, 
																						 lang="en").items(limit)]
	else:
		tweets = [tweet for tweet in tweepy.Cursor(api.search,
																						 q=query, 
																						 lang="en").items()]
	return tweets
	

def store(tweets, update=False):
	try:
		conn = pymongo.MongoClient()
		print "Connected successfully!"
	except pymongo.errors.ConnectionFailure, e:
		print "Could not connect to MongoDB: %s" % e

	db = conn.TweetSentiment
	collection = db.timelines

	for tweet in tweets:
		try:
			if update:
				collection.update_one(
									{'id_str': tweet.id_str},
									{
										'$set':	
											{
											'id_str': tweet.id_str,
											'text': tweet.text.encode('utf-8', 'ignore'),
											'user': tweet.user._json	,
											'retweet_count': tweet.retweet_count,
											'favorite_count': tweet.favorite_count,
											'geo': tweet.geo,
											'hashtags': tweet.entities['hashtags'],
											'user_mentions': tweet.entities['user_mentions'],
											'created_at': tweet.created_at
											}
									},
									upsert=True)
			else:
				collection.insert({
									'id_str': tweet.id_str,
									'text': tweet.text.encode('utf-8', 'ignore'),
									'user': tweet.user._json	,
									'retweet_count': tweet.retweet_count,
									'favorite_count': tweet.favorite_count,
									'geo': tweet.geo,
									'hashtags': tweet.entities['hashtags'],
									'user_mentions': tweet.entities['user_mentions'],
									'created_at': tweet.created_at
									})
		except pymongo.errors.DuplicateKeyError:
			print "Duplicate key, skipping: " + tweet.id_str
			continue
		except UnicodeEncodeError:
			print "Unicode Error"


def loop(hours=10):
	queries = ["bank of america", "wells fargo", "verizon",
						 "ATT", "AT&T", "lowes", "home depot"]
	print "Scraping for the next " + str(hours) + " hours"
	t0 = time.time()
	time_left = 20*60 + 1
	while time_left > 3600:
		completed = []
		while set(queries) != set(completed):
				for query in queries:
					if query not in completed:
						try:
							tweets = search(query, limit=0)
							store(tweets, update=True)
							completed.append(query)
						except:
							continue
		time_left = 3600*hours - (time.time() - t0)
		print "There are " + str(time_left/60/60) + " hours left"
		



# def test(seconds=10):
# 	queries = ['1', 'eleven', 'twent22']
# 	t0 = time.time()
# 	time_left = 1
# 	while time_left > 0:
# 		completed = []
# 		while set(queries) != set(completed):
# 			for query in queries:
# 				if query not in completed:
# 					# print "appending: " + str(query)
# 					completed.append(query)
# 		time_left = 1*seconds-(time.time()-t0)
# 		print time_left


