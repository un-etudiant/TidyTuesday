library(tidyverse)
library(tidytuesdayR)
library(janitor)
library(zipcodeR)
library(maps)
library(ggthemes)
library(tigris)
library(ggrepel)

options(tigris_use_cache = TRUE)

## Get data
tuesdata <- tidytuesdayR::tt_load('2021-05-11')
broadband <- tuesdata$broadband
broadband_zip <- tuesdata$broadband_zip
rm(tuesdata)
broadband <- broadband %>% janitor::clean_names() 
broadband_zip <- broadband_zip %>%
  janitor::clean_names() %>% 
  mutate(postal_code = as.character(postal_code)) %>%
  rename(zipcode = postal_code) %>%
  mutate(zipcode = normalize_zip(zipcode))


zip_codes_info <- as_tibble(zip_code_db) %>% 
select(zipcode,zipcode_type,county,state,population,major_city, mc_lat = lat,mc_long = lng) %>% 
  filter(state == "WA") %>%  
  filter(str_detect(county, "King")) %>% 
  filter(!is.na(population)) %>% 
  filter(zipcode_type == "Standard")


broadband_zip_kingcounty <- broadband_zip %>% 
  filter(st == "WA", county_name == "King" )
  
broadband_zip_kingcounty <- broadband_zip_kingcounty %>% 
  inner_join(zip_codes_info, by = "zipcode")


kingcountyzips <- zipcodeR::search_county("king","wa") %>% 
  filter(!is.na(population)) %>% 
  filter(zipcode_type == "Standard") %>% 
  distinct(zipcode) %>% pull()

zipcode_shapefile <- tigris::zctas(cb = TRUE , starts_with = kingcountyzips)


zipcode_sf <- zipcode_shapefile %>% select(zipcode = ZCTA5CE10 , geometry)

zipcode_sf <- zipcode_sf %>% 
  inner_join(broadband_zip_kingcounty , by = "zipcode" )

water_kc_sf <- area_water("wa","king")

water_kc_sf <- water_kc_sf %>% 
  filter(MTFCC %in% c("H2030","H2051","H2053")) %>% 
  filter(str_detect(FULLNAME,"Washington") | str_detect(FULLNAME,"Sammamish")  | str_detect(FULLNAME , "Puget Sound")) 


(broadbandusage <- ggplot(data = zipcode_sf) +
  geom_sf( mapping = aes(fill = broadband_usage * 100), alpha = 0.6) +
  scale_fill_distiller(direction = 1 ,type = "seq" , palette = "YlGn") +
  geom_sf(data = water_kc_sf, fill = "lightblue"  ) +
  labs(fill = "Broadband Usage" , title = "Broadband Usage in King County by Zip Code, WA" , subtitle = "Does usage diminish the farther east we go ?" , caption = "Data By Microsoft Via The Verge | Visualization By @2rabbits2") +
  theme_minimal() +
  theme(legend.box = "horizontal" , legend.direction = "horizontal" , legend.position = "bottom")  +
  theme(axis.line = element_blank() , axis.text = element_blank() , panel.grid = element_blank()) +
  guides(fill = guide_colourbar(title.position = "top")) 
)

ggsave(filename = "./2021-05-11/broadbandusage.png",width = 15,
       height = 9)
  

