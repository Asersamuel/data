# Install packages if needed
# install.packages(c("tidyverse", "readxl", "lubridate", "scales"))

# Load libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)  # For formatting axes


# Set working directory to your specified path
setwd("/Users/anoushkagurung")

# Load data
df <- read_excel("Visualization_Project_Video_data.xlsx", sheet = "USvideos")



# Preprocessing
df <- df %>%
  mutate(
    trending_date = as.Date(trending_date, format = "%y.%d.%m"),  # Fix date format
    likes = as.numeric(likes),
    dislikes = as.numeric(dislikes),
    views = as.numeric(views)
  )

# ----------------------------------------------------------------------------------
# a) Count of videos by trending date
# ----------------------------------------------------------------------------------
trending_counts <- df %>%
  group_by(trending_date) %>%
  summarise(count = n())

ggplot(trending_counts, aes(x = trending_date, y = count)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "Count of Videos by Trending Date", x = "Trending Date", y = "Number of Videos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("a_trending_date_count.png", width = 10, height = 6)

# ----------------------------------------------------------------------------------
# b) Top channel's trending videos over time
# ----------------------------------------------------------------------------------
top_channel <- df %>%
  count(channel_title) %>%
  filter(n == max(n)) %>%
  pull(channel_title)

df_top <- df %>%
  filter(channel_title == top_channel) %>%
  group_by(trending_date) %>%
  summarise(count = n())

ggplot(df_top, aes(x = trending_date, y = count)) +
  geom_line(color = "orange") +
  geom_point(color = "orange") +
  labs(title = paste("Videos Trending Over Time for Top Channel:", top_channel),
       x = "Trending Date", y = "Number of Videos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("b_top_channel_trending.png", width = 10, height = 6)

# ----------------------------------------------------------------------------------
# c) Rank channels by likes/views ratio
# ----------------------------------------------------------------------------------
channel_stats <- df %>%
  group_by(channel_title) %>%
  summarise(
    total_likes = sum(likes, na.rm = TRUE),
    total_views = sum(views, na.rm = TRUE)
  ) %>%
  mutate(likes_views_ratio = total_likes / total_views) %>%
  arrange(desc(likes_views_ratio)) %>%
  head(10)

ggplot(channel_stats, aes(x = reorder(channel_title, likes_views_ratio), y = likes_views_ratio)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top Channels by Likes/Views Ratio", x = "Channel Title", y = "Likes/Views Ratio") +
  theme_minimal()

ggsave("c_likes_views_ratio.png", width = 10, height = 6)

# ----------------------------------------------------------------------------------
# d) Rank titles by likes/(likes + dislikes)
# ----------------------------------------------------------------------------------
df <- df %>%
  mutate(like_ratio = likes / (likes + dislikes)) %>%
  replace_na(list(like_ratio = 0))  # Handle division by zero

df_sorted <- df %>%
  arrange(desc(like_ratio)) %>%
  head(20)

ggplot(df_sorted, aes(x = reorder(title, like_ratio), y = like_ratio)) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(title = "Top Videos by Likes/(Likes + Dislikes) Ratio", x = "Video Title", y = "Ratio") +
  theme_minimal()

ggsave("d_like_dislike_ratio.png", width = 12, height = 8)

# ----------------------------------------------------------------------------------
# e) Videos with ratings disabled per channel
# ----------------------------------------------------------------------------------
ratings_disabled <- df %>%
  filter(ratings_disabled == TRUE) %>%
  group_by(channel_title) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

ggplot(ratings_disabled, aes(x = reorder(channel_title, count), y = count)) +
  geom_col(fill = "red") +
  labs(title = "Videos with Ratings Disabled by Channel", x = "Channel Title", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("e_ratings_disabled.png", width = 10, height = 6)

# ----------------------------------------------------------------------------------
# f) Likes comparison: Comments disabled vs. enabled
# ----------------------------------------------------------------------------------
comments_disabled_avg <- df %>%
  group_by(comments_disabled) %>%
  summarise(avg_likes = mean(likes, na.rm = TRUE)) %>%
  mutate(comments_disabled = factor(comments_disabled, labels = c("Enabled", "Disabled")))

ggplot(comments_disabled_avg, aes(x = comments_disabled, y = avg_likes)) +
  geom_col(fill = c("blue", "cyan")) +
  labs(title = "Average Likes: Comments Disabled vs. Enabled", x = "Comments Disabled", y = "Average Likes") +
  theme_minimal()

ggsave("f_comments_disabled_likes.png", width = 8, height = 5)

# ----------------------------------------------------------------------------------
# g) Total likes: 'Funny' tag vs. non-funny
# ----------------------------------------------------------------------------------
df <- df %>%
  mutate(has_funny_tag = str_detect(tags, regex("funny", ignore_case = TRUE)))

funny_likes <- df %>%
  group_by(has_funny_tag) %>%
  summarise(total_likes = sum(likes, na.rm = TRUE)) %>%
  mutate(label = ifelse(has_funny_tag, "With Funny Tag", "Without Funny Tag"))

ggplot(funny_likes, aes(x = label, y = total_likes)) +
  geom_col(fill = c("gold", "gray")) +
  labs(title = "Total Likes: Funny Tag vs. Non-Funny", x = "Tag Category", y = "Total Likes") +
  theme_minimal()

ggsave("g_funny_tag_likes.png", width = 8, height = 5)

# ----------------------------------------------------------------------------------
# h) Channel ranking by engagement ratios
# ----------------------------------------------------------------------------------
channel_stats <- df %>%
  group_by(channel_title) %>%
  summarise(
    total_likes = sum(likes, na.rm = TRUE),
    total_dislikes = sum(dislikes, na.rm = TRUE),
    total_views = sum(views, na.rm = TRUE)
  ) %>%
  mutate(
    likes_views_ratio = total_likes / total_views,
    dislikes_views_ratio = total_dislikes / total_views,
    net_likes_views_ratio = (total_likes - total_dislikes) / total_views
  )

# Plotting all three ratios
plot_likes <- channel_stats %>%
  arrange(desc(likes_views_ratio)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(channel_title, likes_views_ratio), y = likes_views_ratio)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Top Channels by Likes/Views Ratio", x = "Channel Title", y = "Ratio") +
  theme_minimal()

plot_dislikes <- channel_stats %>%
  arrange(desc(dislikes_views_ratio)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(channel_title, dislikes_views_ratio), y = dislikes_views_ratio)) +
  geom_col(fill = "darkred") +
  coord_flip() +
  labs(title = "Top Channels by Dislikes/Views Ratio", x = "Channel Title", y = "Ratio") +
  theme_minimal()

plot_net <- channel_stats %>%
  arrange(desc(net_likes_views_ratio)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(channel_title, net_likes_views_ratio), y = net_likes_views_ratio)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(title = "Top Channels by (Likes - Dislikes)/Views Ratio", x = "Channel Title", y = "Ratio") +
  theme_minimal()

# Combine plots
library(patchwork)  # Install if needed: install.packages("patchwork")
combined_plots <- plot_likes / plot_dislikes / plot_net
ggsave("h_engagement_ratios.png", combined_plots, width = 12, height = 18)


