import snscrape.modules.twitter as sntwitter
import pandas as pd
import requests
import os
from bs4 import BeautifulSoup
from datetime import datetime, timedelta

maxTweets = 1000

instance = "http://localhost:8080" #"https://nitter.net" #"http://localhost:8456"
account = "dummyUser"

def download_tweets_for_date(date):
    until = date.date() + timedelta(days=1)
    since = until - timedelta(days=1)

    tweets_list = []

    for i, tweet in enumerate(sntwitter.TwitterSearchScraper(f'from:CercaniasMadrid since:{since} until:{until}').get_items()):
        if i > maxTweets:
            break
        tweets_list.append([tweet.date, tweet.id, tweet.rawContent, tweet.user.username, str(tweet.inReplyToTweetId), tweet.hashtags])

    tweets_df = pd.DataFrame(tweets_list, columns=['Datetime', 'Tweet_Id', 'Text', 'Username', 'Reply_to', 'Hashtags'])

    tweets_df['Tweet_Id'] = tweets_df['Tweet_Id'].astype(str)
    tweets_df['Reply_to'] = tweets_df['Reply_to'].astype(str)

    matching_tweets = []

    for tweet in tweets_df.itertuples(index=False):
        text = tweet.Text
        reply_to = tweet.Reply_to

        url = f"{instance}/{account}/status/{reply_to}"
        response = requests.get(url)
        soup = BeautifulSoup(response.text, 'html.parser')
        tweet_content = soup.find('div', class_='tweet-content media-body')
        original = tweet_content.get_text(strip=True) if tweet_content else None
        matching_tweets.append((reply_to, original))

    matching_tweets_df = pd.DataFrame(matching_tweets, columns=['Reply_to', 'original'])
    
    merged_df = pd.merge(tweets_df, matching_tweets_df, on='Reply_to', how='left')
    
    merged_df.to_csv(f'../data/raw_data/CercaniasMadrid_tweets_{date.date()}.csv', index=False)

def read_last_downloaded_day():
    try:
        script_dir = os.path.dirname(os.path.abspath(__file__))
        log_file_path = os.path.join(script_dir, 'log.txt')

        with open(log_file_path, 'r') as f:
            lines = f.readlines()
            if lines:
                last_line = lines[-1]
                date_string = last_line.strip().split(": ")[-1].split(" ")[0] 
                last_downloaded_day = datetime.strptime(date_string, "%Y-%m-%d").date()
                return last_downloaded_day
            else:
                last_downloaded_day = datetime(2023, 4, 3).date()
    except FileNotFoundError:
        return None

last_downloaded_day = read_last_downloaded_day()

if last_downloaded_day:
    start_date = last_downloaded_day + timedelta(days=1)
    end_date = datetime.now().date() - timedelta(days=1)
else:
    start_date = datetime(2023, 3, 4)
    end_date = datetime.now().date() - timedelta(days=1)


for date in pd.date_range(start=start_date, end=end_date):
    try:
        download_tweets_for_date(date)
        script_dir = os.path.dirname(os.path.abspath(__file__))
        log_file_path = os.path.join(script_dir, 'log.txt')

        with open(log_file_path, 'w') as f:
            last_downloaded_day = date
            f.write(f"Last downloaded day: {last_downloaded_day}\n")
    except Exception as e:
        with open('log.txt', 'a') as f:
            f.write(f"Error occurred on {date}: {str(e)}\n")
    download_tweets_for_date(date)