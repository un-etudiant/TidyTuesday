library(tidytuesdayR)
library(tidyverse)
tuesdata <- tidytuesdayR::tt_load('2021-11-23')
writers <- tuesdata$writers
directors <- tuesdata$directors
episodes <- tuesdata$episodes
imdb <- tuesdata$imdb
rm(tuesdata)

directors %>% 
  inner_join(writers) %>% 
  distinct(director,writer)

episodes %>% 
  inner_join(directors,by = "story_number") %>% 
  inner_join(writers,by = "story_number") %>% 
  distinct(writer,director,rating) %>% 
  group_by(writer,director) %>% 
  summarise(mean_rating = mean(rating,na.rm = TRUE) , num_appearances = n()) %>% 
  arrange(-mean_rating) %>% 
  View()


  select(writer,director,season_number,story_number,episode_number,uk_viewers,rating) %>% 
  
  arrange(writer,director)
  
  episodes %>% 
    filter(type != "special") %>% 
    mutate(season_number = as.factor(season_number)) %>% 
    select(season_number,episode_number,rating) %>% 
    ggplot() +
    geom_line(mapping = aes(x = episode_number,y = rating)) + 
    facet_wrap(~season_number)
  
  episodes %>% 
    filter(type != "special") %>% 
    group_by(season_number) %>% 
    summarise("highestrated" = max(rating,na.rm = TRUE))
    
  episodes %>% 
    filter(type != "special") %>% 
    select(season_number,episode_number,rating)
  
  episodes %>% 
    filter(type != "special") %>% 
    group_by(season_number) %>% 
    slice_max(order_by = rating) %>% 
    ungroup() %>% 
    select(season_number,story_number,episode_number,rating) %>% 
    inner_join(directors,by = "story_number") %>% 
    inner_join(writers,by = "story_number") %>% 
    inner_join(imdb,by = c("episode_number" = "ep_num", "season_number" = "season")) %>% 
    select(season_number,story_number,episode_number,rating.x,desc) %>% 
    View()
  
  imdb %>% 
    select(season,ep_num,desc) %>% 
    View()