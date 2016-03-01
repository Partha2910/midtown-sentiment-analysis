import pymongo
import tweepy
from bson.json_util import dumps


CONSUMER_KEY = 'PzdjDvz5rnt7tD2ORhaUoLDxL'
CONSUMER_SECRET = 'VhAXe0IsE68qC81zi2u1GTK6l8cg2cqek2IZVzcxbvfGL2Ia1O'
ACCESS_KEY = '67676031-O7u9TiKuT3ptxvUGwYgfDPK4y7HvQ7EFb37XhQDRA'
ACCESS_SECRET = 'cbWjRWTanPkLzlMQJ3QmcnLqWaspPsSCVv251kUAprGRt'

auth = tweepy.OAuthHandler(CONSUMER_KEY, CONSUMER_SECRET)
auth.set_access_token(ACCESS_KEY, ACCESS_SECRET)
api = tweepy.API(auth, wait_on_rate_limit=True, wait_on_rate_limit_notify=True)



def search(query, limit=100):
	tweets = [tweet for tweet in tweepy.Cursor(api.search,
																						 q=query, 
																						 lang="en").items(limit)]
	return tweets
	

def store(tweets):
	try:
		conn = pymongo.MongoClient()
		print "Connected successfully!"
	except pymongo.errors.ConnectionFailure, e:
		print "Could not connect to MongoDB: %s" % e

	db = conn.TweetSentiment
	collection = db.timelines

	for tweet in tweets:
		try:
			collection.insert({
								'id_str': tweet.id_str,
								'text': tweet.text.encode('utf-8', 'ignore'),
								'user': tweet.user._json	,
								'retweet_count': tweet.retweet_count,
								'favorite_count': tweet.favorite_count,
								'geo': tweet.geo,
								'hashtags': tweet.entities['hashtags'],
								'user_mentions': tweet.entities['user_mentions']
								})
		except pymongo.errors.DuplicateKeyError:
			continue
		except UnicodeEncodeError:
			print "Unicode Error"


queries = ["bank of america", "wells fargo", "verizon", "ATT", "AT&T", "lowes", "home depot"]

completed = []
done = False
while done == False:
	for query in queries:
		if query not in completed:
			try:
				tweets = search(query, limit=2000)
				store(tweets)
				completed.append(query)
				if set(queries) == set(completed):
					done = True
			except:
				continue



