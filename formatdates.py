import pandas as pd
from googleapiclient.discovery import build
from config import api_key

youtube = build('youtube', 'v3', developerKey=api_key)

df = pd.read_csv("apify_tedtalks.csv")

df = df.drop(columns=["date"]) # remove date

def get_video_date(video_id): # function to fetch the published date using YouTube API
    try:
        video_request = youtube.videos().list(part="snippet", id=video_id)
        video_response = video_request.execute()

        # extract the published date
        published_date = video_response['items'][0]['snippet']['publishedAt']
        return published_date[:10]  # we only need date from the yyy-mm-dd format
    except Exception as e:
        print(f"Error fetching date for {video_id}: {e}")
        return None

print("Starting to fetch published dates for videos...")

for index, row in df.iterrows(): # fetch the video dates
    video_id = row['id']
    print(f"Fetching date for video ID: {video_id}...")
    published_date = get_video_date(video_id)
    if published_date:
        df.at[index, 'published_date'] = published_date
        print(f"Date fetched for {video_id}: {published_date}")
    else:
        print(f"Failed to fetch date for {video_id}")

df.to_csv("updated_dataset.csv", index=False)

print("Dataset updated with published dates. Done.")
