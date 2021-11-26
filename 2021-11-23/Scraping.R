library(rvest)
#baseurl <- "https://en.wikipedia.org"
mainurl <- "https://en.wikipedia.org/wiki/List_of_Doctor_Who_episodes_(2005%E2%80%93present)"
xpathsel <- "//tr[contains(concat( ' ', @class, ' '  ), concat( ' ', 'vevent', ' ' ))] "

int_page <- read_html(mainurl)

listdf <- int_page %>%
  html_nodes("table.wikitable.plainrowheaders.wikiepisodetable") %>%
  html_table() %>%
  map(.f = function(x){x %>% janitor::clean_names()}) %>% 
  map_dfr( .f = function(x){x %>%
  mutate(across(everything(), as.character))}) %>% 
  filter(!is.na(parse_integer(no_inseries))) %>% 
  mutate("title" = str_remove_all(title,pattern =  "\"")) %>%  
  select(no_story,no_inseries,title,directed_by,written_by,original_air_date,prod_code,uk_viewers_millions = uk_viewers_millions_11,ai = ai_11) 
  

allseries <- int_page %>% 
html_nodes(xpath = xpathsel) 


story_num <- allseries %>% 
  html_nodes(xpath = ".//th") %>% 
  html_attr("id") %>% 
  head(-3)

serial_num <- allseries %>% 
  html_nodes(xpath = ".//td[1]") %>% 
  html_text2() %>% 
  head(-3)

title_text <- allseries %>% 
  html_nodes(xpath = ".//td[2]") %>% 
  html_text2() %>% 
  str_remove_all(pattern =  "\"") %>% 
  head(-3)

title_url <- allseries %>% 
  html_nodes(xpath = ".//td[2]//a") %>% 
  html_attr("href") %>% 
  xml2::url_absolute(base = baseurl)


enriched_df <- tibble(story_num,serial_num,title_text,title_url)  %>% 
  filter(!is.na(parse_integer(serial_num)))

rm(
  story_num,
  serial_num,
  title_text,
  title_url
)

getalltext <- function(URL) {
  inodes <- read_html(URL) %>% 
    #html_nodes(xpath = '//span[@id="Plot"]/parent::*/following-sibling::*')
    html_nodes(xpath = "//span[contains(concat( ' ', @id, ' '  ), concat( ' ', 'Plot', ' ' )) or contains(concat( ' ', @id, ' '  ), concat( ' ', 'Synopsis', ' ' ))]/parent::*/following-sibling::*")
  l_in <- inodes %>% 
    html_name() %>% 
    detect_index(.f = ~.x !='p')
  if (l_in != 1) {
  plot_text <- inodes %>% 
    head(l_in - 1) %>% 
    html_text2() %>% 
    str_flatten()
  return(plot_text)
  } else {
    l_in_1 <- inodes %>% 
      html_name() %>% 
      detect_index(.f = ~.x =='p')
    
    inodes <- inodes %>% 
              tail(-(l_in_1 - 1))
    l_in <- inodes %>% 
      html_name() %>% 
      detect_index(.f = ~.x !='p')
    plot_text <- inodes %>% 
      head(l_in - 1) %>% 
      html_text2() %>% 
      str_flatten()
    return(plot_text)
    
  }
  
  
  
  
}



enriched_df$plot_text <- map(enriched_df$title_url,getalltext)

enriched_df <- enriched_df %>% 
  unnest(plot_text) %>% 
  filter(plot_text != "")
