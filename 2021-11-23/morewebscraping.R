spage <- read_html("https://en.wikipedia.org/wiki/The_Unquiet_Dead")

l_in <-spage %>% 
  html_nodes(xpath = '//span[@id="Plot"]/parent::*/following-sibling::*') %>% 
  html_name() %>% 
  detect_index(.f = ~.x =='p')

spage %>% 
  html_nodes(xpath = '//span[@id="Plot"]/parent::*/following-sibling::*') %>% 
  tail(-1)
spage %>% 
  html_nodes(xpath = '//span[@id="Plot"]/parent::*/following-sibling::*') %>%  head(l_in-1) %>% 
  html_text2() %>% 
  str_flatten()
  

which.min(l == 'p')
1  //p[(((count(preceding-sibling::*) + 1) = 14) and parent::*)] | //p[(((count(preceding-sibling::*) + 1) = 13) and parent::*)]//*[(@id = "Plot")]
2 //p[(((count(preceding-sibling::*) + 1) = 14) and parent::*)] | //p[(((count(preceding-sibling::*) + 1) = 13) and parent::*)]//p[(((count(preceding-sibling::*) + 1) = 12) and parent::*)]//p[(((count(preceding-sibling::*) + 1) = 11) and parent::*)]//h2[(((count(preceding-sibling::*) + 1) = 10) and parent::*)]

//p[(((count(preceding-sibling::*) + 1) = 15) and parent::*)] | //p[(((count(preceding-sibling::*) + 1) = 14) and parent::*)]//p[(((count(preceding-sibling::*) + 1) = 12) and parent::*)]//h2[(((count(preceding-sibling::*) + 1) = 10) and parent::*)]


//span[@id='Plot'] 	

html_nodes(xpath = '//span[contains(@id, "tandings")]/following::*[@title="Winning percentage" or text()="PCT"]/ancestor::table') %>% 
  html_table(fill = TRUE)
})

spage1 <- read_html("https://en.wikipedia.org/wiki/The_End_of_the_World_(Doctor_Who)")
nodes <-spage1 %>% 
  html_nodes(xpath = '//span[@id="Plot"]/parent::*/following-sibling::*')
num1 <- which.min(  nodes %>% 
  html_name() == 'p')

nodes %>% 
  head(num1-1)






obj1 <- c("A","B")
obj2 <- c(list(elt = "bar"),list(elt = "foo"))
#x <- list(obj1, obj2)
x <- tibble(obj1,obj2)
x[[2]]$elt
f_pra <- function(x) {x[[2]]}



