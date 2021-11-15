# 2021 St. Louis Cardinals Spray Charts

edman <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 669242)
edman %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Tommy Edman 2021 Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

goldy <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 502671)
goldy %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Paul Goldschmidt 2021 Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

nolan <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 571448)
nolan %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Nolan Arenado 2021 Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

bro <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 641933)
bro %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Tyler O'Neill 2021 Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

carlson <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 666185)
carlson %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Dylan Carlson 2021 Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

yadi <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 425877)
yadi %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Yadier Molina 2021 Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

bader <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 664056)
bader %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Harrison Bader 2021 Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

dejong <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 657557)
dejong %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Paul DeJong 2021 Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

sosa <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 624641)
sosa %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Edmundo Sosa 2021 Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

noot <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 663457)
noot %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Lars Nootbaar 2021 Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

carp <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 572761)
carp %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Matt Carpenter 2021 Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

kniz <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 668800)
kniz %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Andrew Knizner 2021 Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

williams <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 642211)
williams %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Justin Williams 2021 Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

rondon <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 602922)
rondon %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Jose Rondon 2021 Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

playerid_lookup(last_name = "Rondon", first_name = "Jose")




