library(tidytext)
library(tidyverse)
library(sentimentr)
library(ggridges)

### importing data -----

## read metadata
sofc_meta <- read_csv("https://raw.githubusercontent.com/etachov/state_of_the_city/master/sofc_metadata.csv") %>%
  # filter down to speeches that have been collected from 2017
  filter(collected == "Yes" & year == 2017)

# use the id variable to create a list of file paths
file_paths <- paste0("https://raw.githubusercontent.com/etachov/state_of_the_city/master/text/", sofc_meta$id, ".txt")

speech_txt <- function(file_path) {

  df <- data_frame(text = read_lines(file_path)) %>%
    mutate(line = row_number(),
           id = gsub("https://raw.githubusercontent.com/etachov/state_of_the_city/master/text/|\\.txt", "", file_path))

  return(df)

}


text_raw <- map_df(file_paths, speech_txt) 

# tokenize by sentence
speech_sentences <- unnest_tokens(text_raw, output = "sentences", input = "text", token = "sentences")


### sentiment scoring -----

# use sentimentr::sentiment to score the sentences using Jocker dictionary
full_sentiment <- sentiment(speech_sentences$sentences,
                            # take the full sentence into account
                            n.before = Inf, n.after = Inf) %>% 
  # element_id is the sentence id from unnest_token
  group_by(element_id) %>%
  # sum to get score and word_count for original sentences identified by unnest_tokens
  summarise(word_count = sum(word_count),
            sentiment = mean(sentiment))


# bind the sentence scores to the original data.frame
speech_sent <- bind_cols(speech_sentences, full_sentiment) %>%
  group_by(id) %>%
  # add some summary variables we'll use for the charts
  mutate(element_id = row_number(), 
         mean_sentiment = mean(sentiment, na.rm = T), 
         median_sentiment = median(sentiment, na.rm = T), 
         sd_sentiment = sd(sentiment, na.rm = T)) %>%
  # join with speech metadata
  left_join(sofc_meta) %>%
  mutate(mayor_city = paste(mayor, city, sep = ", "))


## sentiment ridges -----
sent_hist <- ggplot(speech_sent, aes(x = sentiment, y = reorder(mayor_city, median_sentiment))) +
  coord_cartesian(xlim = c(-1, 1)) +
  geom_density_ridges(scale = 2, rel_min_height = 0.01, color = "white", fill = "coral") +
  labs(x = "Sentiment score, higher = warmer sentiment", 
       y = "", 
       title = "2017 State of the City Speech Sentiment", 
       subtitle = "Columbus had the warmest sentiment\nNew York had the coolest") +
  theme_minimal(base_family = "mono", base_size = 16)

## sentimate change over speech -----
sent_change <- ggplot(speech_sent, aes(x = element_id, y = sentiment)) +
  geom_line(alpha = .05) +
  geom_line(aes(x = element_id, y = 0), color = "#696969") +
  # use local regression to find the trend in the sentence-to-sentences noise
  geom_smooth(span = .35, size = 1.8, aes(color = ..y..), fill = NA, method = "loess") +
  scale_color_gradient2(low = "black", mid = "#b4b4b4", high = "coral", midpoint = 0, guide = F) +
  coord_cartesian(ylim = c(-.5, .8)) +
  # speeches vary somewhat in length so i'm allowing the x-axis to vary
  facet_wrap(~reorder(city, -median_sentiment), scales = "free_x", ncol = 3) +
  labs(x = "Sentence Number", 
       y = "Smoothed Sentiment Score\nhigher = positive sentiment", 
       title = "Change in Sentiment over Speech", 
       subtitle = "While the speeches are generally positive\nmany have darker sections") +
  theme_minimal(base_family = "mono", base_size = 18) 

## philly focus

sent_change_philadelphia <- ggplot(speech_sent_philadelphia, aes(x = element_id, y = sentiment)) +
  # highlight high sentiment
  geom_rect(aes(xmin = 110, xmax = 115, ymin = -.7, ymax = 1), fill = "lightgrey") +
  geom_rect(aes(xmin = 138, xmax = 142, ymin = -.7, ymax = 1), fill = "lightgrey") +
  geom_line(alpha = .1) +
  geom_line(aes(x = element_id, y = 0), color = "#696969", size = 1) +
  # use local regression to find the trend in the sentence-to-sentences noise
  geom_smooth(span = .35, size = 2, aes(color = ..y..), fill = NA, method = "loess") +
  scale_color_gradient2(low = "black", mid = "#b4b4b4", high = "coral", midpoint = 0, guide = F) +
  coord_cartesian(ylim = c(-.5, .8)) +
  labs(x = "Sentence Number", 
       y = "Smoothed Sentiment Score", 
       title = "Philadelphia 2017 State of the City Sentiment") +
  theme_minimal(base_family = "mono", base_size = 16) 




