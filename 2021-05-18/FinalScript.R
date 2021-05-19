library(tidyverse)
library(tidytuesdayR)

tuesdata <- tidytuesdayR::tt_load("2021-05-18")
survey <- tuesdata$survey
rm(tuesdata)

## The data is terribly unclean and hence needs to be cleansed.
## We will start using currenncy codes and use that to clean a bit.

## Add Row Ids to ensure that we have some way of identifying the unique row
## Convert all country names to lower case for easier comparison
survey <- survey %>%
  arrange(country) %>%
  mutate(rowid = row_number(), country = str_to_lower(country)) %>%
  relocate(rowid, .before = 1)

survey <-
  survey %>% filter(!country %in% c(
      "bulgaria",
      "croatia",
      "serbia",
      "hungary",
      "afghanistan",
      "romania",
      "congo"
    )
  )
survey <-
  survey %>% mutate(country = if_else(country == "\U1F1FA\U1F1F8"
              , "united states", country))


## create a mapping to correct countries
survey_countries <- survey %>%
  distinct(country) %>%
  mutate(pass = case_when(str_length(country) <= 32 ~ "P1",
                          str_length(country) > 32 ~ "P2"))

survey_countries <- survey_countries %>%
  mutate(
    corrected_countryname = countrycode::countrycode(
      survey_countries$country,
      origin = "country.name",
      destination = "country.name"
    )
  )



surve_countries_p1 <- survey_countries %>%
  filter(pass == "P1", is.na(corrected_countryname)) %>%
  mutate(
    corrected_countryname = case_when(
      (
        str_detect(country, "^.*unite.*kin.*$") |
          str_detect(country, "^.*u[?.]*k.*$") |
          str_detect(country, "^englan.*$") |
          str_detect(country, "^scotlan.*$") |
          str_detect(country, "^.*wales*$") |
          str_detect(country, "britain") |
          str_detect(country, "northern ireland") |
          str_detect(country, "jersey")
      ) ~ "United Kingdom",
      str_detect(country, "australi") ~ "Australia",
      str_detect(country, "méxico") ~ "Mexico",
      #str_detect(country,"^.*unit.*stat.*$") ~ "United States",
      str_detect(country, "^.*uni.*sta.*$") ~ "United States",
      str_detect(country, "usa") ~ "United States",
      country %in% c(
        "virginia",
        "unted states",
        "untied states",
        "united sttes",
        "united y",
        "uxz",
        "uss",
        "usd",
        "the us",
        "united sates",
        "united sates of america",
        "ua",
        "u. s",
        "u. s.",
        "u.a.",
        "california",
        "san francisco",
        "hartford",
        "america"
      ) ~ "United States",
      country %in% c("catalonia") ~ "Spain",
      country %in% c("can", "canad", "canadw", "canda", "csnada") ~ "Canada",
      country %in% c("danmark") ~ "Denmark",
      country %in% c("brasil") ~ "Brazil",
      country %in% c("nederland", "nl") ~ "Netherlands",
      country %in% c("nz") ~ "New Zealand",
      country %in% c("panamá") ~ "Panama",
      TRUE ~ corrected_countryname
    )) %>%
  filter(!is.na(corrected_countryname))


survey_countries <- survey_countries %>%
  left_join(surve_countries_p1, by = "country") %>%
  mutate(
    corrected_countryname.x = if_else(
      pass.x == "P1" &
        is.na(corrected_countryname.x),
      corrected_countryname.y,
      corrected_countryname.x
    )
  ) %>%
  select(country, pass = pass.x, corrected_countryname = corrected_countryname.x)


survey <- survey %>%
  left_join(survey_countries, by = "country") %>%
  filter(!is.na(corrected_countryname))
