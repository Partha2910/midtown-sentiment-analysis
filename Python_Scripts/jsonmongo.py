import os
import pymongo
import json
from pprint import pprint
import ast

try:
	conn = pymongo.MongoClient()
	print "Connected successfully!"
except pymongo.errors.ConnectionFailure, e:
	print "Could not connect to MongoDB: %s" % e

db = conn.TweetSentiment
collection = db.timelines


def mongo_from_file(filepath, update=False):
	with open(filepath) as open_file:
		open_file = open_file.read()
		jsonfile = ast.literal_eval(open_file)
		for f in jsonfile:
			try:
				if update:
					collection.update_one({'id_str': f['id_str']},
						{
							'$set': {
							'id_str': f['id_str'],
							'text': f['text'].decode('latin1').encode('utf-8', 'ignore'),
							'id_str': f['id_str'],
							'user': f['user'],
							'retweet_count': f['retweet_count'],
							'favorite_count': f['favorite_count'],
							'geo': f['geo'],
							'hashtags': f['entities']['hashtags'],
							'user_mentions': f['entities']['user_mentions'],
							'created_at': f['created_at']
							}
						},
						upsert=True)
				else:
					collection.insert({
											'id_str': f['id_str'],
											'text': f['text'].decode('latin1').encode('utf-8', 'ignore'),
											'id_str': f['id_str'],
											'user': f['user'],
											'retweet_count': f['retweet_count'],
											'favorite_count': f['favorite_count'],
											'geo': f['geo'],
											'hashtags': f['entities']['hashtags'],
											'user_mentions': f['entities']['user_mentions'],
											'created_at': f['created_at']
											})
					
			except pymongo.errors.DuplicateKeyError:
				continue
			except:
				print "I dunno what went wrong, sorry"

def store_all_from_file(base_dir):
	for root, dir, files in os.walk(base_dir):
		if root is not base_dir:
			tl_path = root+"/timeline.json"
			mongo_from_file(tl_path, update=True)






store_all_from_file('/Volumes/Blue Thing/agitaretech/followers')
# mongo_from_file('/Volumes/Blue Thing/agitaretech/agitaretech/timeline.json')


#id_str, 