from googleapiclient.discovery import build
from youtube_transcript_api import YouTubeTranscriptApi, TranscriptsDisabled, NoTranscriptFound
from config import api_key

youtube = build('youtube', 'v3', developerKey=api_key)  # YouTube API client

channel_id = 'UCAuUUnT6oDeKwE6v1NGQxug'  # TED's official yt channel ID

def fetch_ted_videos(max_results=10):
    request = youtube.search().list(
        part='snippet',
        channelId=channel_id,
        maxResults=max_results,
        order='date'
    )

    response = request.execute()

    for item in response['items']:
        video_title = item['snippet']['title']

        # Check if 'videoId' exists in 'id', and if not, skip this item
        if 'videoId' in item['id']:
            video_id = item['id']['videoId']
        else:
            print(f"Skipping video: {video_title} (no videoId found)")
            continue
        
        video_url = f"https://www.youtube.com/watch?v={video_id}"

        try:
            # Fetch transcript
            transcript = YouTubeTranscriptApi.get_transcript(video_id, languages=['en'])
            transcript_text = " ".join([entry['text'] for entry in transcript])

            # Print only if transcript is available
            print(f"Title: {video_title}")
            print(f"Video URL: {video_url}")
            print(f"Transcript: {transcript_text[:500]}...")  # Print first 500 characters
            print("-" * 50)

        except (TranscriptsDisabled, NoTranscriptFound):
            pass  

fetch_ted_videos(10)

# FAIL: this ended up not working.