library(tidyverse)
library(tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2021-05-18')
survey <- tuesdata$survey
#rm(tuesdata)

## The data is terribly unclean and hence needs to be cleansed.
## We will start using currenncy codes and use that to clean a bit.

## Add Row Ids to ensure that we have some way of identifying the unique row
## Convert all country names to lower case for easier comparison
survey <- survey %>% 
  mutate(rowid = row_number(), country = str_to_lower(country)) %>% 
  relocate(rowid , .before = 1) 

survey <- survey %>% filter(!country %in% c("bulgaria" , "croatia" , "serbia" , "hungary" , "afghanistan" , "romania","congo"))

survey <- survey %>% filter(rowid != 3934)
## Lets Tackle AUD/NZ Currency code
## Later CAD
survey <- survey %>% 
  mutate(
    country = recode(country,
                     "new zealand aotearoa" = "new zealand",
                     "nz" = "new zealand",
                     "australi" = "australia",
                     "from new zealand but on projects across apac" = "new zealand" ,
                     ###--CAD-- ##
                     "canada, ottawa, ontario" = "canada",
                     "canadw" = "canada",
                     "can" = "canada",
                     "canda" = "canada",
                     "csnada" = "canada" ,
                     "canad" = "canada" ,
                     "canada and usa" = "canada",
                     "global" = "canada" ,
                     "$2,175.84/year is deducted for benefits" = "canada",
                     "catalonia" = "spain",
                     "czech republic" = "czechia",
                     "europe" = "czechia",
                     "italy (south)" = "italy",
                     "luxemburg" = "luxembourg",
                     "nederland" = "netherlands",
                     "nl" = "netherlands" ,
                     "the netherlands" = "netherlands",
                     "austria, but i work remotely for a dutch/british company" = "austria"
                     ) 
  )



survey <- survey %>%  mutate(country = if_else(
  ((
    str_detect(country,"^.*unite.*kin.*$") |
      str_detect(country,"^.*u[?.]*k.*$") |
      str_detect(country,"^englan.*$") |
      str_detect(country,"^scotlan.*$") |
      str_detect(country,"^.*wales*$") |
      str_detect(country,"britain") |
      str_detect(country,"northern ireland") |
      str_detect(country,"jersey") |
      str_detect(country,"isle of man")
  ) & currency == "GBP" )
  , "united kingdom"
  ,country
)
) 

## specific case where currency is incorrect 
## start with AUD/NZD
## USD
survey <- survey %>% 
  mutate( currency = case_when(
    rowid %in% c(17427,3492) ~ "CAD",
    rowid %in% c(25511,5842,8917,24868,3595) ~ "USD",
    rowid %in% c(17270) ~ "Other",
    rowid %in% c(21294) ~ "INR",
    rowid %in% c(5230,6175) ~ "GBP" ,
    rowid %in% c(7279) ~ "ZAR",
    rowid %in% c(16026,12034,17069) ~ "EUR",
    rowid %in% c(14073) ~ "NOK",
    rowid %in% c(11694) ~ "DKK",
    
    TRUE ~ currency
  )
  )

survey <- survey %>% 
  mutate( country = case_when(
    rowid %in% c(11070) ~ "united kingdom",
    rowid %in% c(12917) ~ "hong kong",
    
    
    TRUE ~ country
  )
  )

survey <- survey %>% 
  mutate(currency = if_else(rowid == 15796,"USD",currency),
         annual_salary = if_else(rowid == 15796,33047,annual_salary)
         )


  ## separating the AUD and NZD appropriately
survey <-  survey %>% 
    mutate(currency = case_when(
      currency == "AUD/NZD" & country == "australia" ~ "AUD",
      currency == "AUD/NZD" & country == "new zealand" ~ "NZD",
      currency == "EUR" & country == "sweden" ~ "SEK",
      currency == "EUR" & country == "denmark" ~ "DKK",
      currency == "EUR" & country == "norway" ~ "NOK",
      currency == "EUR" & country == "south africa" ~ "ZAR",
      currency == "EUR" & country == "u.k." ~ "GBP",
      currency %in% c("EUR","GBP") & country == "usa" ~ "USD",
      TRUE ~ currency
      
    )) 

survey <- survey %>% 
  mutate(currency = case_when(
    country == "argentina" ~ "ARS",
    (country == "australia" & currency == "Other") ~ "AUD",
    TRUE ~ currency
  ))

## correct countries again after correcting currencies

survey <- survey %>%  mutate(country = if_else(
  ((
    str_detect(country,"^.*unite.*kin.*$") |
      str_detect(country,"^.*u[?.]*k.*$") |
      str_detect(country,"^englan.*$") |
      str_detect(country,"^scotlan.*$") |
      str_detect(country,"^.*wales*$") |
      str_detect(country,"britain") |
      str_detect(country,"northern ireland") |
      str_detect(country,"jersey") |
      str_detect(country,"isle of man")
  ) & currency == "GBP" )
  , "united kingdom"
  ,country
)
)

## IGNORE BULGARIA , CROATIA , SERBIA , HUNGARY , AFGHANISTAN , ROMANIA ,