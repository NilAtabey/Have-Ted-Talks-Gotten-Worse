import pandas as pd
from youtube_transcript_api import YouTubeTranscriptApi, TranscriptsDisabled, VideoUnavailable

df = pd.read_csv("merged_data_with_transcripts.csv")

def get_subtitles(video_id):
    print(f"Fetching English subtitles for video: {video_id}")
    try:
        # Get available transcripts
        transcript_list = YouTubeTranscriptApi.list_transcripts(video_id)
        available_languages = [t.language_code for t in transcript_list]
        
        for lang in ['en', 'en-US', 'en-GB']: # learned from experience: gotta fetch for different language types and subtypes
            if lang in available_languages:
                transcript = transcript_list.find_transcript([lang]).fetch()
                return " ".join([entry['text'] for entry in transcript])
        
        print(f"English subtitles not found for {video_id}, available languages: {available_languages}")
        return ""
    
    except TranscriptsDisabled:
        print(f"Subtitles disabled for {video_id}")
        return ""
    except VideoUnavailable:
        print(f"Video unavailable for {video_id}")
        return ""
    except Exception as e:
        print(f"Error fetching subtitles for {video_id}: {e}")
        return ""

mask = df['transcript'].isna() | (df['transcript'] == "Subtitles available (fetch separately if needed)") # only update rows where transcript is missing or needs fetching bc this is my FIFTH TIME TRYING
df.loc[mask, 'transcript'] = df.loc[mask, 'video_id'].apply(get_subtitles)

df.to_csv("test_final.csv", index=False)
print("Updated file saved as 'test_final.csv'")

empty_transcripts = df['transcript'].isna().sum() # count the number of empty transcripts remaining