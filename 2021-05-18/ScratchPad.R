
## Step 1
survey %>% 
  filter(currency == "AUD/NZD") %>% 
  mutate(
        country = recode(country,
                          "new zealand aotearoa" = "new zealand",
                          "nz" = "new zealand",
                          "australi" = "australia",
                          "from new zealand but on projects across apac" = "new zealand"
                          ) 
        ) %>% distinct(currency,country)

survey %>% 
  group_by(country) %>% 
  summarise(count = n()) %>% View()

survey %>% 
  mutate(rowid = row_number()) %>% 
  dplyr::relocate( rowid , .before = 1) %>% 
  filter(currency == "USD") %>%
  filter(country == "\U1F1FA\U1F1F8")
#filter(country == "🇺🇸")
  
  filter(rowid == 19748) %>% 
  select(country,currency)
  
  



survey %>% 
  filter(currency == "GBP") %>% 
  distinct(currency,country) %>%  View()

%>% 
  mutate(country1 =  str_detect(country,"^.*u[?.]*k.*$") |
                      str_detect(country,"^.*unite[?.]*kingdom.*$")
         ) %>% 
  write_csv("./Data/currcoun1.csv")




survey %>% 
    distinct(currency,country) %>% 
  mutate(country1 = if_else(
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
          )  %>% 
  write_csv("./Data/currcoun1.csv")
  
  

survey %>% 
  filter(currency == "CHF") %>% 
  filter (country != "switzerland") %>% 
  View()

survey %>% 
  filter(currency == "AUD/NZD") %>% 
  distinct(currency,country)
  filter(!country %in% c("australia", "new zealand") ) 
## in this case the currency is wrong so need to change this.

survey <- survey %>% 
filter(rowid %in% c(17427)) %>% 
  mutate( currency = recode(currency ,"AUD/NZD" = "CAD"))

survey <- survey %>% 
  filter(rowid %in% c(25511)) %>% 
  mutate( currency = recode(currency ,"AUD/NZD" = "USD"))

survey <- survey %>% 
  mutate( currency = case_when(
                      rowid == 17427 ~ "CAD",
                      rowid %in% c(25511,5842,8917,24868) ~ "USD",
                      
                      TRUE ~ currency
                              )
  ) %>% 
  filter(currency == "AUD/NZD") %>% 
  filter(!country %in% c("australia", "new zealand") ) 

survey %>% 
  mutate(currency = case_when(
                        currency == "AUD/NZD" & country == "australia" ~ "AUD",
                        currency == "AUD/NZD" & country == "new zealand" ~ "NZD",
                        TRUE ~ currency
    
  )) 

          
  
  

  survey %>% 
    mutate (
      country = recode (country , c("canada, ottawa, ontario","canadw","can") = "canada")
    )




survey %>% 
  mutate(
    country = recode(country,
                     "new zealand aotearoa" = "new zealand",
                     "nz" = "new zealand",
                     "australi" = "australia",
                     "from new zealand but on projects across apac" = "new zealand" ,
                     "canada, ottawa, ontario" = "canada",
                     "canadw" = "canada",
                     "can" = "canada",
                     "canda" = "canada",
                     "csnada" = "canada" ,
                     "canad" = "canada" ,
                     "canada and usa" = "canada",
                     "global" = "canada"
    ) 
  ) %>% filter (currency == "CAD") %>% 
  distinct(currency,country)


survey %>% 
  filter(currency == "CAD") %>% 
filter(country != "canada") %>% 
  View()



survey %>% 
  filter(currency == "GBP") %>% 
  #filter (country %in% c("sweden"))  %>% 
  distinct(currency,country) %>% 
  arrange(country) %>% 
  View()



survey %>% 
  filter(currency == "GBP") %>% 
  filter(country == "usa") %>% 
  View()



survey %>% 
  filter(currency == "HKD") %>% 
  distinct(currency,country) %>% 
  arrange(country) %>% 
  View()


survey %>% 
  filter(currency == "HKD") %>%  View()


survey %>% 
  filter(currency == "JPY", country == "united states") %>%  View()



survey %>% 
  filter(currency == "SEK", country == "us") %>%  View()







