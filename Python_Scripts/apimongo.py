import pymongo
import tweepy
import time
from bson.json_util import dumps


CONSUMER_KEY = 'J79P30JiY90Vxv1fHdQ0Mv63M'
CONSUMER_SECRET = 'zZ87IouN9conIvzic0tv0tAJZdDtOKW9EayPFbwMpgu90jpoxz'
ACCESS_KEY = '67676031-O7u9TiKuT3ptxvUGwYgfDPK4y7HvQ7EFb37XhQDRA'
ACCESS_SECRET = 'cbWjRWTanPkLzlMQJ3QmcnLqWaspPsSCVv251kUAprGRt'

auth = tweepy.OAuthHandler(CONSUMER_KEY, CONSUMER_SECRET)
auth.set_access_token(ACCESS_KEY, ACCESS_SECRET)
api = tweepy.API(auth, wait_on_rate_limit=True, wait_on_rate_limit_notify=True)

try:
	conn = pymongo.MongoClient()
	print "Connected successfully!"
except pymongo.errors.ConnectionFailure, e:
	print "Could not connect to MongoDB: %s" % e



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
	

def store(tweets, update=False, dbname='TweetSentiment', collname='timelines'):

	db = conn[dbname]
	collection = db[collname]

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

def loop(queries, hours=10, update=False, dbname='TweetSentiment', collname='timelines', limit=1500):
	print "Scraping for the next " + str(hours) + " hours"
	t0 = time.time()
	time_left = 600000
	while time_left > 3600*hours*.2:
		completed = []
		while set(queries) != set(completed):
			for query in queries:
				if query not in completed:
					try:
						tweets = search(query, limit=limit)
						store(tweets, update=update, dbname=dbname, collname=collname)
						completed.append(query)
						print "Completed search for " + query
					except:
						continue
		time_left = 3600*hours - (time.time() - t0)
		print "There are " + str(time_left/60/60) + " hours left"
		

query = ["Home Depot", "ATT", "Verizon", "Bank of America", "Wells Fargo", "Lowes"]

loop(queries=query, hours=5, limit=-1)


# loop(queries=[":)"], hours=1, update=False,
# 		 collname='pos_emotes', limit=1500)

loop(queries=[":("], hours=1, update=False,
			collname="neg_emotes", limit=1500)




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


