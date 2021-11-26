library(tidytext)
best_episodes_in_season <- episodes %>% 
  filter(type != "special") %>% 
  group_by(season_number) %>% 
  slice_max(order_by = rating) %>% 
  ungroup() %>% 
  select(season_number,story_number,episode_number,rating) %>% 
  inner_join(imdb,by = c("episode_number" = "ep_num", "season_number" = "season")) %>% 
  select(season_number,story_number,episode_number,uk_rating = rating.x,desc)

best_episodes_in_season %>% 
  select(season_number,desc) %>% 
  unnest_tokens(word,desc) %>% 
  anti_join(stop_words) %>% 
  group_by(season_number,word) %>% 
  summarise("wordcount" = n()) %>% 
  View()
# 
# enriched_df %>% 
#   filter(story_num =="ep166b")
# 
# best_episodes_in_season %>% 
#   filter(story_number == "166b")