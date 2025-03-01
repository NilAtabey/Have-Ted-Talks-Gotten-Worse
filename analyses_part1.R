# now im going to work on merged_data.csv to do some analysis of the data we have so far

library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(scales)
library(tidytext)

getwd()
setwd("C:/Users/Nil Atabey/Desktop/tedtalkscraping")

df <- read.csv("merged_data.csv")
# View(df)
colnames(df)
############################################################################################
# 1. Top Talks by Views

top_talks_by_views <- df %>%
  arrange(desc(view_count)) %>%
  head(10) %>%
  mutate(speech_title = ifelse(nchar(speech_title) > 30,
    paste0(substr(speech_title, 1, 30), "..."),
    speech_title
  )) # sorts by view count to get the top TED Talks

ggplot(top_talks_by_views, aes(x = reorder(speech_title, view_count), y = view_count)) +
  geom_bar(stat = "identity", fill = "darkolivegreen3") +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) + # format the view count
  labs(
    title = "Top 10 TED Talks by Views",
    x = "TED Talk Title",
    y = "View Count (in millions)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) # rotate x-axis labels for readability

############################################################################################
# 2. Trend Analysis of Views Over Time

df$published_date <- as.Date(df$published_date) # convert published_date to Date format

views_per_year <- df %>%
  mutate(year = year(published_date)) %>%
  group_by(year) %>%
  summarize(avg_views = mean(view_count, na.rm = TRUE)) # aggregate average views per year

# Plot the trend of views over time
ggplot(views_per_year, aes(x = year, y = avg_views)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "firebrick", size = 2) +
  scale_x_continuous(breaks = views_per_year$year) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Trend Analysis: Average TED Talk Views Per Year",
    x = "Year",
    y = "Average View Count (in millions)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) # rotate x-axis labels for readability

############################################################################################
# 3. Speaker Frequency

speaker_frequency <- df %>%
  group_by(speaker_name) %>%
  summarize(speaker_count = n()) %>%
  filter(speaker_count > 1) # only include speakers with more than 1 appearance

speaker_frequency_summary <- speaker_frequency %>%
  group_by(speaker_count) %>%
  summarize(speaker_count_frequency = n()) %>%
  arrange(desc(speaker_count)) # showing how many speakers appear a certain number of times

print(speaker_frequency_summary)


ggplot(speaker_frequency_summary, aes(x = factor(speaker_count), y = speaker_count_frequency)) +
  geom_bar(stat = "identity", fill = "#6A5ACD", color = "white", size = 0.5) +
  labs(
    title = "Speaker Frequency Analysis",
    x = "Number of Talks Given",
    y = "Number of Speakers"
  ) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(
    expand = c(0, 0),
    breaks = seq(0, 300, by = 50) # set breaks at intervals of 50 from 0 to 300
  ) +
  scale_x_discrete(breaks = 2:10)

############################################################################################
# 4. Duration Analysis

# Bin durations into categories (important!)
df <- df %>%
  mutate(duration_category = cut(
    duration_in_seconds,
    breaks = c(0, 300, 600, 900, 1200, Inf),
    labels = c("0-5 min", "5-10 min", "10-15 min", "15-20 min", "20+ min"),
    right = FALSE
  ))

duration_analysis <- df %>%
  group_by(duration_category) %>%
  summarize(avg_views = mean(view_count, na.rm = TRUE))

ggplot(duration_analysis, aes(x = duration_category, y = avg_views, fill = duration_category)) +
  geom_bar(stat = "identity", color = "white", size = 0.5) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Duration Analysis: TED Talk Length vs. Average Views",
    x = "Talk Duration",
    y = "Average Views (in millions)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none"
  ) +
  scale_fill_manual(values = c("#FF6347", "#FFD700", "#40E0D0", "#6A5ACD", "#2E8B57")) # Color palette

############################################################################################
# 5. Duration and Popularity

ggplot(df, aes(x = duration_in_seconds / 60, y = view_count)) +
  geom_point(alpha = 0.3, color = "#4682B4") +
  geom_smooth(method = "loess", color = "#FF6347", se = FALSE) + # Trend line
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Duration vs Popularity: Do Longer Talks Get More Views?",
    x = "Duration (minutes)",
    y = "Views (in millions)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#2F4F4F", hjust = 0.5),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "#2F4F4F")
  )

# alternatively prune out videos over 10m views or just work on videos that have more than 1m views bc they are "popular" and are put a limit on time such as "has to be shorter than 25 mins".
df_filtered <- df %>%
  filter(view_count > 1e6, duration_in_seconds / 60 <= 25) # keep only videos > 1M views and ≤ 25 minutes

ggplot(df_filtered, aes(x = duration_in_seconds / 60, y = view_count)) +
  geom_point(alpha = 0.3, color = "#4682B4") +
  geom_smooth(method = "loess", color = "#FF6347", se = FALSE) + # Trend line
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Duration vs Popularity (Videos > 1M Views, ≤ 25 Min)",
    x = "Duration (minutes)",
    y = "Views (in millions)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = "#2F4F4F", hjust = 0.5),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold", color = "#2F4F4F")
  )

# observing a local minimum at around 16-17 minutes. something related to the human attention span maybe? or how youtube processes views.

############################################################################################
# 6. TED Talk Trends Over Time (Yearly Analysis)

df$year <- year(df$published_date) # extract the year from the published date

talks_per_year <- df %>%
  group_by(year) %>%
  summarize(num_talks = n()) # count the number of talks per year

# Plot the number of talks per year
ggplot(talks_per_year, aes(x = year, y = num_talks)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "firebrick", size = 2) +
  labs(
    title = "TED Talk Trends Over Time: Number of Talks Published by Year",
    x = "Year",
    y = "Number of Talks Published"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# 2. (views per year) and # 6. (# of videos per year) can be plotted in a dual y-axis plot

# merge the two data frames (views_per_year and talks_per_year)
merged_trends <- left_join(views_per_year, talks_per_year, by = "year")

merged_trends <- merged_trends %>%
  mutate(views_per_videos_per_year = avg_views / num_talks)

merged_trends <- merged_trends %>%
  filter(year != 2006) # remove outlier, only 1 video for 2006

ggplot(merged_trends, aes(x = year, y = views_per_videos_per_year)) +
  geom_point(color = "darkblue", alpha = 0.6) + # Scatter plot points
  geom_smooth(method = "loess", color = "red", se = FALSE) + # Trend line
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K"),
    breaks = seq(0, 6000, by = 1000),
    limits = c(0, 6000)
  ) +
  scale_x_continuous(breaks = merged_trends$year) +
  labs(
    title = "Views/Number of Videos Published per Year",
    x = "Year",
    y = "Ratio of Average Views per Year to Videos Published per Year (in thousands)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# we can clearly see that TED talks peaked in 2014!

############################################################################################
# 7. Trending Topics

# transcripts for this could be useful because ted talk titles can be too short to analyze the topic.

############################################################################################
# 8. Publish Date Seasonality   (examining if TED Talks are published more frequently during specific months or seasons)

df$month <- month(df$published_date, label = TRUE)

talks_per_month <- df %>%
  group_by(month) %>%
  summarize(num_talks = n()) %>%
  arrange(month)

ggplot(talks_per_month, aes(x = month, y = num_talks, fill = month)) +
  geom_bar(stat = "identity", color = "white", size = 0.5) +
  labs(
    title = "TED Talks Published by Month",
    x = "Month",
    y = "Number of Talks Published"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1)
  )

############################################################################################
# 9. Views by Published Date (exploring if TED Talks published during certain times of the year (e.g., holidays, conferences) tend to attract more views)

views_per_month <- df %>%
  group_by(month) %>%
  summarize(avg_views = mean(view_count, na.rm = TRUE))

ggplot(views_per_month, aes(x = month, y = avg_views, fill = month)) +
  geom_bar(stat = "identity", color = "white", size = 0.5) +
  scale_y_continuous(labels = scales::label_number(scale = 1e-3, suffix = "k")) +
  labs(
    title = "Average Views of TED Talks by Month",
    x = "Month",
    y = "Average Views (in thousands)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 12, angle = 45, hjust = 1)
  )

# This analysis is based on the total number of views for a video published in, for example, January.
# However, the video could continue to accumulate views over the course of subsequent months and years, meaning that the views recorded in each month do not necessarily correspond to views gained only within that specific month.
# As a result, this analysis may not be entirely accurate or appropriate for examining trends in viewership over time.

############################################################################################
# 10. Word Frequency in Titles (use text analysis on video_title and speech_title to find the most common words. This could reveal popular themes or trends)

# Clean the video_title: convert to lowercase, remove punctuation, and stopwords
tidy_titles <- df %>%
  unnest_tokens(word, video_title) %>%
  filter(!word %in% stop_words$word) %>%
  filter(str_detect(word, "[a-z']")) # keep only alphabetic words

tidy_titles <- tidy_titles %>%
  filter(!word %in% c("ted", "david", "is", "ıs")) # removes "ted" and "david" because ted is in every title and apparently there are a lot of davids that they make the top 10

tidy_titles$word <- gsub("aı", "ai", tidy_titles$word) # this is oddly specific to me because my R console is in Turkish and converts a capital i to a lowercase "ı" which is a different turkish letter!
# I was going to find a more permanent solution but I couldn't find any and since I'm not using tidy_titles for any further analyses, i decided to deal with this issue manually. sometimes the solution is the simpler way.

word_count <- tidy_titles %>%
  count(word, sort = TRUE) # frequency of each word

head(word_count, 20)

word_count %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = n)) +
  geom_bar(stat = "identity", color = "white") +
  scale_fill_gradient(low = "lightblue", high = "springgreen4") +
  scale_y_continuous(
    breaks = seq(0, 150, by = 50),
    limits = c(0, 150)
  ) +
  labs(
    title = "Top 10 Most Common Words in TED Talk Titles",
    x = "Word", y = "Frequency"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1),
    legend.position = "none"
  )

############################################################################################
# 11. Keywords in Titles and Views (see if TED Talks with certain keywords in their titles (e.g., "future", "education") tend to get more views)

# define the keywords you want to look for
keywords <- c(
  "world", "future", "life", "climate", "change",
  "ai", "art", "power", "human", "people"
)

# convert video titles to lowercase and filter for rows that contain one of the keywords (case-insensitive)
filtered_df <- df %>%
  filter(str_detect(str_to_lower(video_title), paste(keywords, collapse = "|")))

# create a new column for the identified keyword in the title
filtered_df <- filtered_df %>%
  mutate(keyword = case_when(
    str_detect(str_to_lower(video_title), "world") ~ "world",
    str_detect(str_to_lower(video_title), "future") ~ "future",
    str_detect(str_to_lower(video_title), "life") ~ "life",
    str_detect(str_to_lower(video_title), "climate") ~ "climate",
    str_detect(str_to_lower(video_title), "change") ~ "change",
    str_detect(str_to_lower(video_title), "ai") ~ "ai",
    str_detect(str_to_lower(video_title), "art") ~ "art",
    str_detect(str_to_lower(video_title), "power") ~ "power",
    str_detect(str_to_lower(video_title), "human") ~ "human",
    str_detect(str_to_lower(video_title), "people") ~ "people"
  ))

avg_views_by_keyword <- filtered_df %>%
  group_by(keyword) %>%
  summarize(avg_views = mean(view_count, na.rm = TRUE)) # calculate the average view count for each keyword

ggplot(avg_views_by_keyword, aes(x = reorder(keyword, avg_views), y = avg_views, fill = avg_views)) +
  geom_bar(stat = "identity", color = "white") +
  scale_fill_gradient(low = "darkseagreen1", high = "forestgreen") +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-6, suffix = "M"),
    breaks = seq(0, 1.5e6, by = 0.5e6),
    limits = c(0, 1.5e6)
  ) +
  labs(
    title = "Average Views for TED Talks with Specific Keywords",
    x = "Keyword", y = "Average Views (in millions)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

############################################################################################
# 12. View Count & Duration (is there a positive correlation between the length of a TED Talk and the number of views?)

df_filtered_2 <- df %>%
  filter(view_count > 1e6 & view_count < 20e6, duration_in_seconds / 60 <= 25)

ggplot(df_filtered_2, aes(x = duration_in_seconds, y = view_count)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) + # regression line
  scale_y_continuous(labels = scales::label_number(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Relationship Between Duration and View Count of TED Talks",
    x = "Duration (Seconds)", y = "View Count (in millions)"
  ) +
  theme_minimal(base_size = 14)

correlation <- cor(df_filtered_2$duration_in_seconds, df_filtered_2$view_count, use = "complete.obs") # corelation coeff
correlation # comes out to be around 0.054 if calculated for df, 0.03 for df_filtered_2
# duration of the TED Talks doesn't have a strong relationship with the number of views they receive


############################################################################################
# 13. Normalize Views Based on Time Since Publication, Create a Time Decay Model and do a Comparing View Count vs. Time Since Release (unsure about results)

df <- df %>%
  mutate(
    published_date = as.Date(published_date),
    days_since_published = as.numeric(Sys.Date() - published_date)
  ) # number of days since the video was published

df <- df %>%
  mutate(views_per_day = view_count / days_since_published) # create a new column for views per day

head(df)

ggplot(df, aes(x = days_since_published, y = views_per_day)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "loess", color = "red") +
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-3, suffix = "K"),
    breaks = seq(0, 25e3, by = 5e3),
    limits = c(0, 25e3)
  ) +
  labs(
    title = "Views Per Day vs Time Since Publication",
    x = "Days Since Published", y = "Views Per Day (in millions)"
  ) +
  theme_minimal(base_size = 14)
# the videos are relatively uniform in how they accumulate views, meaning the time since publication doesn't significantly impact the daily view count.

install.packages("zoo")
library(zoo)

df <- df %>%
  arrange(published_date) %>%
  mutate(rolling_avg_views = rollmean(view_count, k = 30, fill = NA, align = "right")) # moving average of views across all videos (not grouped by month)

ggplot(df, aes(x = published_date, y = view_count)) +
  geom_line(color = "blue", alpha = 0.6) +
  geom_line(aes(y = rolling_avg_views), color = "red", linewidth = 1.2) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) + # Format y-axis in millions
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # Ensure every year appears on x-axis
  labs(
    title = "Views vs Rolling Average of Views",
    x = "Publication Year", y = "View Count (in millions)"
  ) +
  theme_minimal(base_size = 14)

# Check for NA values in the rolling average
summary(df$rolling_avg_views)
sum(is.na(df$rolling_avg_views)) # Count of NAs

# fin
