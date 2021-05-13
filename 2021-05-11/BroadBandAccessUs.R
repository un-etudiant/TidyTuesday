library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(zipcodeR)
library(maps)
library(ggthemes)
## Get data
tuesdata <- tidytuesdayR::tt_load('2021-05-11')
broadband <- tuesdata$broadband
broadband_zip <- tuesdata$broadband_zip
rm(tuesdata)
broadband <- broadband %>% janitor::clean_names() 
broadband_zip <- broadband_zip %>%
  janitor::clean_names() 

# %>% 
#   mutate(postal_code = as.character(postal_code)) %>% 
#   rename(zipcode = postal_code) %>% 
#   mutate(zipcode = normalize_zip(zipcode)) 



##
# zip_codes_info <- as_tibble(zip_code_db) %>% 
#   select(zipcode,zipcode_type,county,state,lat,lng,population,population_density,median_home_value,median_household_income,bounds_west,bounds_east,bounds_north,bounds_south)
# 
# broadband_zip <- broadband_zip %>%
#   left_join(zip_codes_info,by = c("zipcode" = "zipcode"))
# 
# broadband_zip <- broadband_zip %>%
#   filter(zipcode_type == "Standard")
  
broadband_zip_wa <- broadband_zip %>% filter(st == "WA") %>% 
  mutate(broadband_usage = broadband_usage * 100) %>% 
  group_by(county_name) %>% 
  summarise(average_bb_usage = mean(broadband_usage)) %>% 
  ungroup() %>% 
  mutate(county_name = str_to_lower(county_name))
wa_state_map <- map_data("county") %>% filter(region == "washington")

broadband_zip_wa <- broadband_zip_wa %>% 
  left_join(wa_state_map,by = c("county_name" = "subregion" ))

ggplot(data = broadband_zip_wa) +
  geom_polygon(mapping = aes(x= long , y= lat , group = group , fill = average_bb_usage ), color = "lightgrey") + 
  theme_map()

  




  
  
  
  

