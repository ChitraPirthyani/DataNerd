
# Chitra Pirthyani
# 08/14/2020

import json

# Twitter Developer Account Credentials

# Store Twitter account keys in a dict
access = {}
access['CONSUMER_KEY'] =  'iZasx26ZWd8Uk36Nq1kgI1obk'
access['CONSUMER_SECRET'] = 'hh7Ke3opBMSEY2ZuZu1VysT7Q4ufOg2LSRhdivLcsWgtcLNMxq'
access['ACCESS_TOKEN']= '869904060-JwRBLQ3cL7YmGF6M94PJCOwyOfvnJOwMTg0AyZ7b'
access['ACCESS_SECRET'] = 'SD8OTf7gqadHwi8uJP2Xcw8tveaUGQqbi7UP5hk1WY4Fl'

with open ("twitter_access.json", "w") as file:
    json.dump(access, file)

# Access Twitter API using Twython Library

# $ Pip install twython
import twython

from twython import Twython

with open("twitter_access.json", "r") as file:
    creds = json.load(file)

python_tweets = Twython(creds['CONSUMER_KEY'], creds['CONSUMER_SECRET'])

# Create our search query for #flooring

query = {'q': '#flooring',
        'count': 5,
        'lang': 'en',
        'result_type' : 'recent'
        }

import pandas as pd

# Search tweets
dict_df = {'Date': [], 'Tweet': []}
for status in python_tweets.search(**query)['statuses']:
    dict_df['Date'].append(status['created_at'])
    dict_df['Tweet'].append(status['text'])

# Print five tweets with their dates
df = pd.DataFrame(dict_df)
df.sort_values(by='Date', inplace=True, ascending=True)
df.head(10)
print(df)

# Storing Dataframe to CSV File
df.to_csv('/Users/chitrapirthyani/Documents/Assessment.csv', index = False, header= True)
