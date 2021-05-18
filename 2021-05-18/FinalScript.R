library(tidyverse)
library(tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load('2021-05-18')
survey <- tuesdata$survey
rm(tuesdata)

## The data is terribly unclean and hence needs to be cleansed.
## We will start using currenncy codes and use that to clean a bit.

## Add Row Ids to ensure that we have some way of identifying the unique row
## Convert all country names to lower case for easier comparison
survey <- survey %>% 
  mutate(rowid = row_number(), country = str_to_lower(country)) %>% 
  relocate(rowid , .before = 1) 

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
    
    TRUE ~ currency
  )
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
      currency == "EUR" & country == "usa" ~ "USD",
      
      
      
      
      
      TRUE ~ currency
      
    )) 





## IGNORE BULGARIA , CROATIA , SERBIA , HUNGARY