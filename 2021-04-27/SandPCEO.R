# We are going to to do text analysis and focus on the following reasons for a CEO's exits
# departure_code == 3 ~ "Job Performance",
# departure_code == 4 ~ "Legal Violations",
# departure_code == 5 ~ "Retired",
# departure_code == 6 ~ "Other Opportunity"
library(tidytext)
library(textstem)
library(tidyverse)
library(ggthemes)
library(ggtext)
library(wordcloud)
library(patchwork)

departures <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-27/departures.csv')
data("stop_words")
more_stop_words <- tribble(
  ~word,
  "chairman",
  "ceo",
  "company",
  "board",
  "officer",
  "director",
  "officers",
  "directors",
  "executive",
  "chief",
  "president",
  "company's"
  
)

tidy_departures <- departures %>%
  filter(!is.na(notes)) %>% 
  filter( departure_code %in% c(3,4,5,6)) %>% 
  select(dismissal_dataset_id,departure_code,notes) %>% 
  mutate(departure_reason = forcats::as_factor(
    case_when(
      departure_code == 3 ~ "Job Performance",
      departure_code == 4 ~ "Legal Violations",
      departure_code == 5 ~ "Retired",
      departure_code == 6 ~ "Other Opportunity"
      
    )
  )) %>% 
  unnest_tokens(word,notes) %>% 
  anti_join(stop_words) %>% 
  anti_join(more_stop_words) 


tidy_departures_top10  <- tidy_departures %>% 
  filter(str_length(word) > 3) %>% 
  mutate(word = lemmatize_words(word)) %>%
  group_by(departure_reason,word) %>% 
  tally() %>% 
  slice_max(order_by = n,n = 10) %>% 
  ungroup()  

bgcolour <-  "#131416"
fillcolors <- c("Retired" = '#8dd3c7',"Job Performance" = "#ffffb3","Other Opportunity" = "#bebada","Legal Violations" = "#fb8072")

theme_set(theme(
  panel.background = element_rect(
    fill = NA, 
    colour = NA),
  legend.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.background = element_rect(
    colour = bgcolour,
    fill = bgcolour),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  legend.position = "none",
  plot.margin = margin(0.5,0.5,0.5,0.5,"cm")))

plotwordfreq <- ggplot(data = tidy_departures_top10 ) +
  geom_bar(mapping = aes(x=tidytext::reorder_within(word,n,departure_reason),y = n,fill = departure_reason),stat = "identity") +
  facet_wrap(~departure_reason  ,scales = "free",dir = "v") +
  scale_x_reordered() + 
  scale_fill_manual(values = fillcolors) +
  theme_minimal() + 
  theme(legend.position = "none") +
  theme(axis.title = element_blank()) +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(family = "Oswald", face = "bold")) +
  theme(plot.background = element_rect(fill = bgcolour,colour = "white")) +
  theme(axis.text = element_text(colour = "white")) +
  theme(strip.text = element_text(colour = "white")) + 
  theme(plot.title = element_text(colour = "white") , 
        plot.subtitle = element_text(colour = "white"),
        plot.caption = element_text(colour = "white")) + 
  labs( title = "Commonly Used Words") +
  coord_flip()


tidy_departures_top10_tfidf <- tidy_departures %>% 
  select(departure_reason,word) %>% 
  count(departure_reason,word,sort = TRUE) %>% 
  bind_tf_idf(term = word,document = departure_reason,n = n) %>% 
  group_by(departure_reason) %>% 
  slice_max(tf_idf,n = 10,with_ties = FALSE) %>% 
  ungroup()

plotwordtfidf <- ggplot(data = tidy_departures_top10_tfidf ) +
  geom_bar(mapping = aes(
                        x = tidytext::reorder_within(word,tf_idf,departure_reason),
                        y = tf_idf,
                        fill = departure_reason)
           ,stat = "identity") +
  facet_wrap(~departure_reason  ,scales = "free",dir = "v") +
  scale_x_reordered() + 
  scale_fill_manual(values = fillcolors) +
  theme_minimal() + 
  theme(legend.position = "none") +
  theme(axis.text.x = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(panel.grid = element_blank()) +
  theme(text = element_text(family = "Oswald", face = "bold")) +
  theme(plot.background = element_rect(fill = bgcolour,colour = "white")) +
  theme(axis.text = element_text(colour = "white")) +
  theme(strip.text = element_text(colour = "white")) + 
  theme(plot.title = element_text(colour = "white") , 
        plot.subtitle = element_text(colour = "white"),
        plot.caption = element_text(colour = "white")) + 
  labs( title = "TF-IDF") +
  coord_flip()

ceoplot <- plotwordfreq + plotwordtfidf + plot_layout(ncol = 2,widths = c(1,1)) +
            plot_annotation(title = str_c("Text Analysis of S&P CEOs who",
                                          "<span style='color:#8dd3c7' > Retired</span>",
                                          " or left for",
                                          "<span style='color:#bebada' > Other Opportunity</span> ",
                                          "or were fired for ",
                                          "<span style='color:#ffffb3' >Job Performance</span>",
                                          " or , " ,
                                          "<span style='color:#fb8072' > Legal Violations</span>"   
            ), 
            caption =  "Visualization by 2rabbits2",theme = theme(
                plot.title = element_markdown(), 
                plot.subtitle = element_markdown(), 
                plot.caption = element_markdown(),
                text = element_text(family = "Oswald", face = "bold", colour = "white")
              ))
  ceoplot
