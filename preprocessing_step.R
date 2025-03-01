
# test

getwd()
setwd("C:/Users/Nil Atabey/Desktop/tedtalkscraping")

apify_tedtalks <- read.csv("apify_tedtalks.csv")

head(apify_tedtalks)
View(apify_tedtalks)

colnames(apify_tedtalks)

updated_dataset <- read.csv("updated_dataset.csv")
updated_dataset <- updated_dataset[, !colnames(updated_dataset) %in% "order"]

View(updated_dataset)
updated_dataset <- updated_dataset[updated_dataset$id != "Bzfb4ZdNQFM", ]
View(updated_dataset)
write.csv(updated_dataset, "updated_dataset.csv", row.names = FALSE)

video_info <- updated_dataset[, c("id", "title")]
video_info$speech_title <- NA
video_info$speaker_name <- NA
View(video_info)
write.csv(video_info, "video_info.csv", row.names = FALSE)
##################################################################################
# on to the title work

# Load dataset
video_info <- read.csv("video_info.csv")

# Create new columns
video_info$speaker_name <- NA
video_info$speech_title <- NA

# Function to extract speaker name & title
extract_info <- function(title) {
  speaker <- NA
  speech <- NA
  
  # Pattern 1: "Name Surname: Speech Title"
  if (grepl("^[^|]+: [^|]+$", title)) {
    parts <- unlist(strsplit(title, ": ", fixed = TRUE))
    speaker <- parts[1]
    speech <- parts[2]
  }
  
  # Pattern 2 & 3: "Speech Title | Name Surname | TED" or "Speech Title | Name Surname"
  else if (grepl(" \\| [^|]+( \\| TED)?$", title)) {
    parts <- unlist(strsplit(title, " \\| "))
    speech <- parts[1]
    speaker <- parts[2]
  }
  
  # Pattern 4 & 5: "Name Surname: Speech Title | TED" or "... | TED Countdown"
  else if (grepl(": .*\\| TED", title)) {
    parts <- unlist(strsplit(title, ": ", fixed = TRUE))
    speaker <- parts[1]
    speech <- gsub(" \\| TED.*$", "", parts[2])  # Remove " | TED" or " | TED Countdown"
  }
  
  # Pattern 6: "Name Surname on Speech Title"
  else if (grepl("^.* on .*", title)) {
    parts <- unlist(strsplit(title, " on ", fixed = TRUE))
    speaker <- parts[1]
    speech <- parts[2]
  }
  
  return(c(speaker, speech))
}

# Apply the function to each row
extracted <- t(sapply(video_info$title, extract_info))
video_info$speaker_name <- extracted[,1]
video_info$speech_title <- extracted[,2]

# Save the updated dataset
# write.csv(video_info, "video_info_updated.csv", row.names = FALSE)

# Preview result
head(video_info[, c("title", "speaker_name", "speech_title")])
View(video_info)

# Remove rows where speaker_name is exactly "TED" or contains "TED" in all caps
video_info <- subset(video_info, !(grepl("\\bTED\\b", speaker_name)))

# Save the cleaned dataset
# write.csv(video_info, "video_info_cleaned.csv", row.names = FALSE)

# Preview result
head(video_info[, c("title", "speaker_name", "speech_title")])
View(video_info)

# Remove rows where speaker_name contains "TED" in all caps
video_info <- subset(video_info, !(grepl("\\bTED\\b", speaker_name)))

# Remove rows where BOTH speech_title AND speaker_name are NA
video_info <- subset(video_info, !(is.na(speech_title) & is.na(speaker_name)))

# Save the cleaned dataset
# write.csv(video_info, "video_info_cleaned.csv", row.names = FALSE)

# Preview result
head(video_info[, c("title", "speaker_name", "speech_title")])
View(video_info)

# Find the row with the specific video ID
idx <- which(video_info$id == "EjNV6JwlV2s")

# Manually edit the speech title and speaker name for that this one specific video
video_info$speech_title[idx] <- "Part 2: How the Old Are Stealing from the Young"
video_info$speaker_name[idx] <- "Scott Galloway"

# Verify the changes
video_info[idx, c("id", "speech_title", "speaker_name")]

View(video_info)

# get ready to merge
video_info$title <- NULL
colnames(video_info)
colnames(updated_dataset)
View(video_info)

# Ensure both 'id' columns are characters (not factors)
video_info$id <- as.character(video_info$id)
updated_dataset$id <- as.character(updated_dataset$id)

# Merge the datasets using 'id' as the key
merged_data <- merge(video_info, updated_dataset, by = "id")

# Check the result
head(merged_data)
View(merged_data)

colnames(merged_data)[colnames(merged_data) == "id"] <- "video_id"
colnames(merged_data)[colnames(merged_data) == "title"] <- "video_title"
colnames(merged_data)[colnames(merged_data) == "viewCount"] <- "view_count"
merged_data <- merged_data[merged_data$video_id != "Y06CZUf3cm0", ]
merged_data <- merged_data[merged_data$video_id != "LSEuKa2cdzM", ]
merged_data <- merged_data[merged_data$video_id != "3w6Ztmpm910", ]


# Add a new column for duration in seconds
merged_data$duration_in_seconds <- sapply(merged_data$duration, function(x) {
  # Split the duration by ":" to extract hours, minutes, and seconds
  time_parts <- strsplit(x, ":")[[1]]
  
  # Handle the case when the duration includes hours
  if (length(time_parts) == 3) {
    hours <- as.numeric(time_parts[1])  # Get the hours
    minutes <- as.numeric(time_parts[2])  # Get the minutes
    seconds <- as.numeric(time_parts[3])  # Get the seconds
    return(hours * 3600 + minutes * 60 + seconds)  # Convert to seconds
  } else if (length(time_parts) == 2) {
    minutes <- as.numeric(time_parts[1])  # Get the minutes
    seconds <- as.numeric(time_parts[2])  # Get the seconds
    return(minutes * 60 + seconds)  # Convert to seconds
  }
})

# realize that some specific short videos are not ted talks, but some are

write.csv(merged_data, "merged_data.csv", row.names = FALSE)

View(merged_data)

merged_data <- merged_data[merged_data$video_id != "FawSxPJsloQ", ]
merged_data <- merged_data[merged_data$video_id != "QPmUAfuqM08", ]
merged_data <- merged_data[merged_data$video_id != "g0Q5YeZ4YOA", ]
merged_data <- merged_data[merged_data$video_id != "OtVlE9eJaDI", ]
merged_data <- merged_data[merged_data$video_id != "R4Y5wGJJ4oU", ]
merged_data <- merged_data[merged_data$video_id != "P9-fJI0iZv4", ]
merged_data <- merged_data[merged_data$video_id != "r6pItuOoGxc", ]
merged_data <- merged_data[merged_data$video_id != "YRvf00NooN8", ]
merged_data <- merged_data[merged_data$video_id != "JNG3wwLqRok", ]

colnames(merged_data)

# finish off by reordeering columns

merged_data <- merged_data[c("video_id", "url", "video_title", "speech_title", "speaker_name", "duration", "duration_in_seconds", "view_count", "published_date")]

# Check the column names after reordering
colnames(merged_data)
View(merged_data)

# we are done
write.csv(merged_data, "merged_data.csv", row.names = FALSE)
