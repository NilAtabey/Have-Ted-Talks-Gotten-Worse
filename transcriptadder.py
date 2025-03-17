import pandas as pd
from youtube_transcript_api import YouTubeTranscriptApi, TranscriptsDisabled, NoTranscriptFound
import time

def get_transcript(video_id):
    print(f"Fetching transcript for video: {video_id}")
    try:
        transcript = YouTubeTranscriptApi.get_transcript(video_id, languages=['en'])
        transcript_text = " ".join([entry['text'] for entry in transcript])
        print(f"Successfully fetched transcript for {video_id}")
        return transcript_text
    except (TranscriptsDisabled, NoTranscriptFound):
        print(f"Transcript unavailable for {video_id}")
        return "Transcript unavailable"
    except Exception as e:
        print(f"Error fetching transcript for {video_id}: {e}")
        return "Transcript unavailable"

df = pd.read_csv("merged_data.csv")

# for testing with only 10 entries
# df_sample = df.head(10).copy()
# df_sample["transcript"] = df_sample["video_id"].apply(lambda vid: get_transcript(vid))
# df_sample.to_csv("test.csv", index=False)
# print("Test run completed. Transcripts saved in 'test.csv'")

# for normal execution
df["transcript"] = df["video_id"].apply(lambda vid: get_transcript(vid))
df.to_csv("merged_data_with_transcripts.csv", index=False)
print("Transcripts added and file saved as 'merged_data_with_transcripts.csv'")

