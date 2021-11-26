library(rvest)

init_page <- read_html("https://en.wikipedia.org/wiki/List_of_Doctor_Who_episodes_(2005%E2%80%93present)")
xp <- '//tr[td[contains(concat( " ", @class, " " ), concat( " ", "summary", " " ))]]'
baseurl <- "https://en.wikipedia.org"

init_page %>%  
  html_nodes(xpath=xp) %>% 
  html_nodes(xpath=".//td[2]/a") %>% 
  html_attr("href") %>% 
  xml2::url_absolute(base = baseurl)
  
  html_attr("href")
  



xp <- '//tr[td[contains(concat( " ", @class, " " ), concat( " ", "summary", " " ))]]'



raw_data <-init_page %>% 
  html_node("table") %>% 
  
  html_table()  %>% 
  janitor::clean_names()

raw_data %>% 
  filter(str_detect(season_series_2,"Series") ) %>% 
  filter(season_series == "")

alltables <- init_page %>%
  html_nodes("table.wikitable.plainrowheaders.wikiepisodetable")

t1 <- alltables[1]
t1 %>% 
  html_children() %>% 
  
  html_attrs()
  html_nodes("table.wikitable:nth-child(44) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(3) > a:nth-child(1)") %>%
  
  html_attr("href") %>% xml2::url_absolute("https://en.wikipedia.org")
t1 %>% 
  html_attrs()

init_page %>%
  html_nodes("table.wikitable.plainrowheaders.wikiepisodetable") %>% 
  html_attr("href") %>%
  xml2::url_absolute("https://en.wikipedia.org")
  
html.client-js.ve-available body.mediawiki.ltr.sitedir-ltr.mw-hide-empty-elt.ns-0.ns-subject.mw-editable.page-List_of_Doctor_Who_episodes_2005–present.rootpage-List_of_Doctor_Who_episodes_2005–present.skin-vector.action-view.skin-vector-legacy div#content.mw-body div#bodyContent.vector-body div#mw-content-text.mw-body-content.mw-content-ltr div.mw-parser-output table.wikitable.plainrowheaders.wikiepisodetable tbody tr.vevent td.summary a




