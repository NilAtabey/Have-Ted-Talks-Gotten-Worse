import csv
import re
from googleapiclient.discovery import build
from youtube_transcript_api import YouTubeTranscriptApi, TranscriptsDisabled, NoTranscriptFound
from config import api_key

youtube = build('youtube', 'v3', developerKey=api_key)

channel_id = 'UCAuUUnT6oDeKwE6v1NGQxug'

csv_filename = 'ted_talks.csv'

headers = ["Title", "Speaker", "Views", "Likes", "Number of Comments", "Description", "Transcript", "Published Date"]

with open(csv_filename, mode='w', newline='', encoding='utf-8') as file:
    writer = csv.writer(file)
    writer.writerow(headers)

    def fetch_ted_videos(max_results=50):
        next_page_token = None
        processed_results = 0
        
        while processed_results < max_results:
            request = youtube.search().list(
                part='snippet',
                channelId=channel_id,
                maxResults=50,
                order='date',
                pageToken=next_page_token
            )
            response = request.execute()
            next_page_token = response.get('nextPageToken') # gets token for next page, didnt work
            
            for item in response['items']:
                video_title = item['snippet']['title']
                video_id = item['id'].get('videoId')
                description = item['snippet']['description']
                published_date = item['snippet']['publishedAt']

                if not video_id:
                    print(f"Skipping {video_title} - Video ID not available.")
                    continue  # skip if no video ID (e.g., playlists)

                # filter out Shorts (titles containing hashtags)
                if '#' in video_title:
                    continue

                # filter out non-talk videos (e.g. sleeping with science series)
                if "TED" not in video_title:
                    continue

                # extract title & speaker correctly
                speaker = "Unknown"
                title = video_title

                if video_title.endswith(" | TED"):  # If it ends with " | TED", process it
                    trimmed_title = video_title[:-6]  # Remove " | TED"

                    if " | " in trimmed_title:
                        # Format: "Title | Speaker"
                        parts = trimmed_title.rsplit(" | ", 1)
                        if len(parts) == 2:
                            title, speaker = parts[0], parts[1]
                    elif ": " in trimmed_title:
                        # Format: "Speaker: Title"
                        parts = trimmed_title.split(": ", 1)
                        if len(parts) == 2:
                            speaker, title = parts[0], parts[1]

                # check if speaker is unknown
                if speaker == "Unknown":
                    # Comment explaining that this is likely a livestream, interview, or other non-TED video
                    print(f"Skipping {video_title} - Speaker unknown (likely livestream, interview, or other non-TED content).")
                    continue  # Skip this video

                # vid stats fetch
                video_stats = youtube.videos().list(
                    part='statistics',
                    id=video_id
                ).execute()

                video_views = int(video_stats['items'][0]['statistics'].get('viewCount', 0))
                video_likes = int(video_stats['items'][0]['statistics'].get('likeCount', 0))
                video_comment_count = int(video_stats['items'][0]['statistics'].get('commentCount', 0))

                try:
                    # transcript fetch
                    transcript = YouTubeTranscriptApi.get_transcript(video_id, languages=['en'])
                    transcript_text = " ".join([entry['text'] for entry in transcript])
                except (TranscriptsDisabled, NoTranscriptFound):
                    transcript_text = "No transcript available"

                # Save the data to CSV
                writer.writerow([title, speaker, video_views, video_likes, video_comment_count, description, transcript_text, published_date])
                print(f"Saved: {title} - {speaker}")
                processed_results += 1

            
            if not next_page_token:
                break

    fetch_ted_videos(50)  # fetch up to 50 videos, considering pagination

print(f"\nData saved to {csv_filename}")

# Ended up not needing this. ended up using apify to get most of it done for me.
# I will keep this here for future reference.