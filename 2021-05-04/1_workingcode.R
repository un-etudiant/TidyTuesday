library(tidyverse)
library(ggthemes)
library(ggtext)
library(patchwork)
library(lubridate)
library(maps)
library(countrycode)

water <-
  readr::read_csv(
    "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-04/water.csv"
  )
water <- water %>%
  mutate(
    report_date = mdy(report_date),
    status_id = recode(
      status_id,
      "y" = "Working",
      "n" = "Not Working",
      "u" = "Unknown"
    ),
    country_name = recode(
      country_name,
      "Congo - Brazzaville" = "Republic of Congo",
      "Congo - Kinshasa" = "Democratic Republic of the Congo"
    )
  ) %>%
  rename(
    lat = lat_deg,
    lon = lon_deg,
    reporteddate = report_date,
    country = country_name,
    condition = status_id
  ) %>%
  separate(water_tech,
           c("Technology", "Provider"),
           sep = " - ",
           remove = TRUE)

breaks <- c(30, 60, 80, 100)
color_scale <- c(
    "bin#4" = "#afeeee",
    "bin#3" = "#addcdc",
    "bin#2" = "#aacbcb",
    "bin#1" = "#a6baba",
    "binna" = "grey"
  )


african_countries <- map_data("world") %>%
  mutate(Continent = countrycode(region, "country.name", "continent")) %>%
  select(region, Continent) %>%
  distinct() %>%
  filter(Continent == "Africa") %>%
  select(region) %>%
  pull()

water_working_prpoportion <- water %>%
  filter(country %in% african_countries) %>%
  filter(condition != "Unknown") %>%
  group_by(country, condition) %>%
  summarise(Count = n(), .groups = "keep") %>%
  ungroup() %>%
  pivot_wider(
    id_cols = country,
    names_from = condition,
    values_from = Count,
    values_fill = 0
  ) %>%
  mutate(
    "Working_Proportion" = Working * 100 / (`Not Working` + Working)
    ,
    Bin = .bincode(x = Working_Proportion, breaks = breaks),
    Bin = paste0("bin#", Bin)
  ) %>%
  select(country, Working_Proportion, Bin)



africa_map <-
  map_data("world") %>% filter(region %in% african_countries)

africa_map <-  africa_map %>%
  left_join(water_working_prpoportion, by = c("region" = "country")) %>%
  replace_na(replace = list(Bin = "binna"))
bgcolour <-  "#006994"

africa_plot <- ggplot(data = africa_map) +
  geom_polygon(mapping = aes(
    x = long,
    y = lat,
    group = group,
    fill = Bin
  ),
  color = "grey") +
  coord_fixed() +
  scale_fill_manual(values = color_scale) +
  theme(legend.position = "none")

legend <-
  tibble(
    level = c("bin#1", "bin#2", "bin#3", "bin#4", "binna"),
    ymin = c(0, 30, 60, 80, 100),
    ymax = c(30, 60, 80, 100, 120)
  )
legend_txt <- legend %>%
  distinct(ymin) %>%
  mutate(offset = row_number() %% 2)

legend_plt <- legend %>%
  ggplot() +
  geom_rect(aes(
    xmin = 0,
    xmax = 1,
    ymin = ymin,
    ymax = ymax,
    fill = level
  ),
  color = bgcolour) +
  geom_text(
    data = legend_txt,
    mapping = aes(
      x = 2 + offset,
      y = ymin,
      label = glue::glue("{scales::comma(ymin)} %")
    ),
    vjust = 0,
    size = 4,
    colour = "white",
    family = "Oswald"
  ) +
  geom_segment(
    data = legend_txt,
    mapping = aes(
      x = 1,
      xend = 1.5 + offset,
      y = ymin,
      yend = ymin
    ),
    linetype = "12",
    color = "white"
  ) +
  scale_fill_manual(values = color_scale) +
  scale_x_continuous(limits = c(-1.7, 4)) +
  coord_flip() +
  guides(fill = FALSE) +  theme_void()

theme_set(
  theme(
    panel.background = element_rect(fill = NA,
                                    colour = NA),
    legend.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(colour = bgcolour,
                                   fill = bgcolour),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    legend.position = "none",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  )
)

(
  full_plot <-
    (
      africa_plot +
        inset_element(legend_plt, 0.25, -0.05, 0.9, 0.05, align_to = "panel")
    )  +
    plot_annotation(
      caption = "Visualisation: Un-Etudiant | @2rabbits2 \n Source: WPDX by Katy Sill & Adam Kariv  | #TidyTuesday \n",
      title = "Proportion of Water Sources that are working",
      theme = theme(
        plot.caption = element_text(
          family = "Courier",
          size = 10,
          color = "white",
          hjust = 1
        ),
        plot.title =  element_text(
          family = "Courier",
          size = 15,
          color = "white",
          hjust = 0.5
        )
        )
      )
)

ggsave("./2021-05-04/watersources.png",
       full_plot,
       width = 15,
       height = 9)