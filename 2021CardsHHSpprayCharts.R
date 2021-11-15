# 2021 St. Louis Cardinals Hard-Hit Spray Charts

edman <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 669242)
edmanHH <- edman %>%
  filter(!is.na(launch_angle)) %>%
  filter(!is.na(launch_speed)) %>%
  filter(launch_speed >= 95) %>%
  filter(launch_angle >= 8) %>%
  filter(launch_angle <= 32) %>%
  select(pitch_type, launch_speed, launch_angle, hc_x, hc_y, events)

edman_ch <- edmanHH %>%
  tally(pitch_type == "CH")
edman_cu <- edmanHH %>%
  tally(pitch_type == "CU")
edman_fc <- edmanHH %>%
  tally(pitch_type == "FC")
edman_ff <- edmanHH %>%
  tally(pitch_type == "FF")
edman_fs <- edmanHH %>%
  tally(pitch_type == "FS")
edman_si <- edmanHH %>%
  tally(pitch_type == "SI")
edman_sl <- edmanHH %>%
  tally(pitch_type == "SL")

edmanpt <- c(edman_ch, edman_cu, edman_fc, edman_ff, edman_fs, edman_si, edman_sl) 
edmanptd <- as.data.frame(edmanpt)
names(edmanptd) <- c('Changeup', 'Curveball', 'Cutter', 'Four-Seamer', 'Splitter', 'Sinker', 'Slider')
edmanptd
edmandata <- data.frame(pt = names(edmanptd), value = c(19,10,6,30,1,24,16))

ggplot(edmandata, aes(x=pt, y=value)) + 
  geom_bar(stat = "identity", color='#F6E42D', fill='#EA2B11') + 
  geom_text(aes(label=value), vjust=1.6, color="black", size=4) +
  labs(title = "Tommy Edman", x = "Pitch Type", y = "Number of Hard-Hit Balls")

edmanHH %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Tommy Edman 2021 Hard-Hit (95+ mph EV and 8° to 32° LA) Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

goldy <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 502671)
goldyHH <- goldy %>%
  filter(!is.na(launch_angle)) %>%
  filter(!is.na(launch_speed)) %>%
  filter(launch_speed >= 95) %>%
  filter(launch_angle >= 8) %>%
  filter(launch_angle <= 32) %>%
  select(pitch_type, launch_speed, launch_angle, hc_x, hc_y, events)

goldy_ch <- goldyHH %>%
  tally(pitch_type == "CH")
goldy_cu <- goldyHH %>%
  tally(pitch_type == "CU")
goldy_cs <- goldyHH %>%
  tally(pitch_type == "CS")
goldy_fc <- goldyHH %>%
  tally(pitch_type == "FC")
goldy_ff <- goldyHH %>%
  tally(pitch_type == "FF")
goldy_fs <- goldyHH %>%
  tally(pitch_type == "FS")
goldy_kc <- goldyHH %>%
  tally(pitch_type == "KC")
goldy_si <- goldyHH %>%
  tally(pitch_type == "SI")
goldy_sl <- goldyHH %>%
  tally(pitch_type == "SL")

goldypt <- c(goldy_ch, goldy_cu, goldy_cs, goldy_fc, goldy_ff, goldy_fs, goldy_kc, goldy_si, goldy_sl) 
goldyptd <- as.data.frame(goldypt)
names(goldyptd) <- c('Changeup', 'Curveball', 'Screwball', 'Cutter', 'Four-Seamer', 'Splitter', 'Knuckle-Curve', 'Sinker', 'Slider')
goldyptd
goldydata <- data.frame(pt = names(goldyptd), value = c(14,7,1,4,56,1,4,33,19))

ggplot(goldydata, aes(x=pt, y=value)) + 
  geom_bar(stat = "identity", color='#F6E42D', fill='#EA2B11') + 
  geom_text(aes(label=value), vjust=1.6, color="black", size=4) +
  labs(title = "Paul Goldschmidt", x = "Pitch Type", y = "Number of Hard-Hit Balls")

goldyHH %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Paul Goldschmidt 2021 Hard-Hit (95+ mph EV and 8° to 32° LA) Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

nolan <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 571448)
nolanHH <- nolan %>%
  filter(!is.na(launch_angle)) %>%
  filter(!is.na(launch_speed)) %>%
  filter(launch_speed >= 95) %>%
  filter(launch_angle >= 8) %>%
  filter(launch_angle <= 32) %>%
  select(pitch_type, launch_speed, launch_angle, hc_x, hc_y, events)

nolan_ch <- nolanHH %>%
  tally(pitch_type == "CH")
nolan_cu <- nolanHH %>%
  tally(pitch_type == "CU")
nolan_fc <- nolanHH %>%
  tally(pitch_type == "FC")
nolan_ff <- nolanHH %>%
  tally(pitch_type == "FF")
nolan_fs <- nolanHH %>%
  tally(pitch_type == "FS")
nolan_kc <- nolanHH %>%
  tally(pitch_type == "KC")
nolan_si <- nolanHH %>%
  tally(pitch_type == "SI")
nolan_sl <- nolanHH %>%
  tally(pitch_type == "SL")

nolanpt <- c(nolan_ch, nolan_cu, nolan_fc, nolan_ff, nolan_fs, nolan_kc, nolan_si, nolan_sl) 
nolanptd <- as.data.frame(nolanpt)
names(nolanptd) <- c('Changeup', 'Curveball', 'Cutter', 'Four-Seamer', 'Splitter', 'Knuckle-Curve', 'Sinker', 'Slider')
nolanptd
nolandata <- data.frame(pt = names(nolanptd), value = c(18,6,4,42,1,4,18,22))

ggplot(nolandata, aes(x=pt, y=value)) + 
  geom_bar(stat = "identity", color='#F6E42D', fill='#EA2B11') + 
  geom_text(aes(label=value), vjust=1.6, color="black", size=4) +
  labs(title = "Nolan Areando", x = "Pitch Type", y = "Number of Hard-Hit Balls")

nolanHH %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Nolan Arenado 2021 Hard-Hit (95+ mph EV and 8° to 32° LA) Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

bro <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 641933)
broHH <- bro %>%
  filter(!is.na(launch_angle)) %>%
  filter(!is.na(launch_speed)) %>%
  filter(launch_speed >= 95) %>%
  filter(launch_angle >= 8) %>%
  filter(launch_angle <= 32) %>%
  select(pitch_type, launch_speed, launch_angle, hc_x, hc_y, events)

bro_ch <- broHH %>%
  tally(pitch_type == "CH")
bro_cu <- broHH %>%
  tally(pitch_type == "CU")
bro_fc <- broHH %>%
  tally(pitch_type == "FC")
bro_ff <- broHH %>%
  tally(pitch_type == "FF")
bro_kc <- broHH %>%
  tally(pitch_type == "KC")
bro_si <- broHH %>%
  tally(pitch_type == "SI")
bro_sl <- broHH %>%
  tally(pitch_type == "SL")

bropt <- c(bro_ch, bro_cu, bro_fc, bro_ff, bro_kc, bro_si, bro_sl) 
broptd <- as.data.frame(bropt)
names(broptd) <- c('Changeup', 'Curveball', 'Cutter', 'Four-Seamer', 'Knuckle-Curve', 'Sinker', 'Slider')
broptd
brodata <- data.frame(pt = names(broptd), value = c(9,12,7,33,1,17,14))

ggplot(brodata, aes(x=pt, y=value)) + 
  geom_bar(stat = "identity", color='#F6E42D', fill='#EA2B11') + 
  geom_text(aes(label=value), vjust=1.6, color="black", size=4) +
  labs(title = "Tyler O'Neill", x = "Pitch Type", y = "Number of Hard-Hit Balls")

broHH %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Tyler O'Neill 2021 Hard-Hit (95+ mph EV and 8° to 32° LA) Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

carlson <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 666185)
carlsonHH <- carlson %>%
  filter(!is.na(launch_angle)) %>%
  filter(!is.na(launch_speed)) %>%
  filter(launch_speed >= 95) %>%
  filter(launch_angle >= 8) %>%
  filter(launch_angle <= 32) %>%
  select(pitch_type, launch_speed, launch_angle, hc_x, hc_y, events)

carlson_ch <- carlsonHH %>%
  tally(pitch_type == "CH")
carlson_cu <- carlsonHH %>%
  tally(pitch_type == "CU")
carlson_fc <- carlsonHH %>%
  tally(pitch_type == "FC")
carlson_ff <- carlsonHH %>%
  tally(pitch_type == "FF")
carlson_fs <- carlsonHH %>%
  tally(pitch_type == "FS")
carlson_kc <- carlsonHH %>%
  tally(pitch_type == "KC")
carlson_si <- carlsonHH %>%
  tally(pitch_type == "SI")
carlson_sl <- carlsonHH %>%
  tally(pitch_type == "SL")

carlsonpt <- c(carlson_ch, carlson_cu, carlson_fc, carlson_ff, carlson_fs, carlson_kc, carlson_si, carlson_sl) 
carlsonptd <- as.data.frame(carlsonpt)
names(carlsonptd) <- c('Changeup', 'Curveball', 'Cutter', 'Four-Seamer', 'Splitter', 'Knuckle-Curve', 'Sinker', 'Slider')
carlsonptd
carlsondata <- data.frame(pt = names(carlsonptd), value = c(8,5,4,37,1,2,20,9))

ggplot(carlsondata, aes(x=pt, y=value)) + 
  geom_bar(stat = "identity", color='#F6E42D', fill='#EA2B11') + 
  geom_text(aes(label=value), vjust=1.6, color="black", size=4) +
  labs(title = "Dylan Carlson", x = "Pitch Type", y = "Number of Hard-Hit Balls")

carlsonHH %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Dylan Carlson 2021 Hard-Hit (95+ mph EV and 8° to 32° LA) Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

yadi <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 425877)
yadiHH <- yadi %>%
  filter(!is.na(launch_angle)) %>%
  filter(!is.na(launch_speed)) %>%
  filter(launch_speed >= 95) %>%
  filter(launch_angle >= 8) %>%
  filter(launch_angle <= 32) %>%
  select(pitch_type, launch_speed, launch_angle, hc_x, hc_y, events)

yadi_ch <- yadiHH %>%
  tally(pitch_type == "CH")
yadi_cu <- yadiHH %>%
  tally(pitch_type == "CU")
yadi_fc <- yadiHH %>%
  tally(pitch_type == "FC")
yadi_ff <- yadiHH %>%
  tally(pitch_type == "FF")
yadi_si <- yadiHH %>%
  tally(pitch_type == "SI")
yadi_sl <- yadiHH %>%
  tally(pitch_type == "SL")

yadipt <- c(yadi_ch, yadi_cu, yadi_fc, yadi_ff, yadi_si, yadi_sl) 
yadiptd <- as.data.frame(yadipt)
names(yadiptd) <- c('Changeup', 'Curveball', 'Cutter', 'Four-Seamer', 'Sinker', 'Slider')
yadiptd
yadidata <- data.frame(pt = names(yadiptd), value = c(9,12,6,20,17,13))

ggplot(yadidata, aes(x=pt, y=value)) + 
  geom_bar(stat = "identity", color='#F6E42D', fill='#EA2B11') + 
  geom_text(aes(label=value), vjust=1.6, color="black", size=4) +
  labs(title = "Yadier Molina", x = "Pitch Type", y = "Number of Hard-Hit Balls")

yadiHH %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Yadier Molina 2021 Hard-Hit (95+ mph EV and 8° to 32° LA) Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

bader <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 664056)
baderHH <- bader %>%
  filter(!is.na(launch_angle)) %>%
  filter(!is.na(launch_speed)) %>%
  filter(launch_speed >= 95) %>%
  filter(launch_angle >= 8) %>%
  filter(launch_angle <= 32) %>%
  select(pitch_type, launch_speed, launch_angle, hc_x, hc_y, events)

bader_ch <- baderHH %>%
  tally(pitch_type == "CH")
bader_cu <- baderHH %>%
  tally(pitch_type == "CU")
bader_fc <- baderHH %>%
  tally(pitch_type == "FC")
bader_ff <- baderHH %>%
  tally(pitch_type == "FF")
bader_fa <- baderHH %>%
  tally(pitch_type == "FA")
bader_kc <- baderHH %>%
  tally(pitch_type == "KC")
bader_si <- baderHH %>%
  tally(pitch_type == "SI")
bader_sl <- baderHH %>%
  tally(pitch_type == "SL")

baderpt <- c(bader_ch, bader_cu, bader_fc, bader_ff, bader_kc, bader_si, bader_sl, bader_fa) 
baderptd <- as.data.frame(baderpt)
names(baderptd) <- c('Changeup', 'Curveball', 'Cutter', 'Four-Seamer', 'Knuckle-Curve', 'Sinker', 'Slider', 'Other')
baderptd
baderdata <- data.frame(pt = names(baderptd), value = c(5,2,3,16,1,16,9,1))

ggplot(baderdata, aes(x=pt, y=value)) + 
  geom_bar(stat = "identity", color='#F6E42D', fill='#EA2B11') + 
  geom_text(aes(label=value), vjust=1.6, color="black", size=4) +
  labs(title = "Harrison Bader", x = "Pitch Type", y = "Number of Hard-Hit Balls")

baderHH %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Harrison Bader 2021 Hard-Hit (95+ mph EV and 8° to 32° LA) Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

dejong <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 657557)
dejongHH <- dejong %>%
  filter(!is.na(launch_angle)) %>%
  filter(!is.na(launch_speed)) %>%
  filter(launch_speed >= 95) %>%
  filter(launch_angle >= 8) %>%
  filter(launch_angle <= 32) %>%
  select(pitch_type, launch_speed, launch_angle, hc_x, hc_y, events)

dejong_ch <- dejongHH %>%
  tally(pitch_type == "CH")
dejong_cu <- dejongHH %>%
  tally(pitch_type == "CU")
dejong_fc <- dejongHH %>%
  tally(pitch_type == "FC")
dejong_ff <- dejongHH %>%
  tally(pitch_type == "FF")
dejong_kc <- dejongHH %>%
  tally(pitch_type == "KC")
dejong_si <- dejongHH %>%
  tally(pitch_type == "SI")
dejong_sl <- dejongHH %>%
  tally(pitch_type == "SL")

dejongpt <- c(dejong_ch, dejong_cu, dejong_fc, dejong_ff, dejong_kc, dejong_si, dejong_sl) 
dejongptd <- as.data.frame(dejongpt)
names(dejongptd) <- c('Changeup', 'Curveball', 'Cutter', 'Four-Seamer', 'Knuckle-Curve', 'Sinker', 'Slider')
dejongptd
dejongdata <- data.frame(pt = names(dejongptd), value = c(9,2,2,24,2,16,8))

ggplot(dejongdata, aes(x=pt, y=value)) + 
  geom_bar(stat = "identity", color='#F6E42D', fill='#EA2B11') + 
  geom_text(aes(label=value), vjust=1.6, color="black", size=4) +
  labs(title = "Paul DeJong", x = "Pitch Type", y = "Number of Hard-Hit Balls")

dejongHH %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Paul DeJong 2021 Hard-Hit (95+ mph EV and 8° to 32° LA) Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

sosa <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 624641)
sosaHH <- sosa %>%
  filter(!is.na(launch_angle)) %>%
  filter(!is.na(launch_speed)) %>%
  filter(launch_speed >= 95) %>%
  filter(launch_angle >= 8) %>%
  filter(launch_angle <= 32) %>%
  select(pitch_type, launch_speed, launch_angle, hc_x, hc_y, events)

sosa_ch <- sosaHH %>%
  tally(pitch_type == "CH")
sosa_cu <- sosaHH %>%
  tally(pitch_type == "CU")
sosa_fs <- sosaHH %>%
  tally(pitch_type == "FS")
sosa_ff <- sosaHH %>%
  tally(pitch_type == "FF")
sosa_si <- sosaHH %>%
  tally(pitch_type == "SI")
sosa_sl <- sosaHH %>%
  tally(pitch_type == "SL")

sosapt <- c(sosa_ch, sosa_cu, sosa_fs, sosa_ff, sosa_si, sosa_sl) 
sosaptd <- as.data.frame(sosapt)
names(sosaptd) <- c('Changeup', 'Curveball', 'Splitter', 'Four-Seamer', 'Sinker', 'Slider')
sosaptd
sosadata <- data.frame(pt = names(sosaptd), value = c(5,6,1,8,3,17))

ggplot(sosadata, aes(x=pt, y=value)) + 
  geom_bar(stat = "identity", color='#F6E42D', fill='#EA2B11') + 
  geom_text(aes(label=value), vjust=1.6, color="black", size=4) +
  labs(title = "Edmundo Sosa", x = "Pitch Type", y = "Number of Hard-Hit Balls")

sosaHH %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Edmundo Sosa 2021 Hard-Hit (95+ mph EV and 8° to 32° LA) Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

carp <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 572761)
carpHH <- carp %>%
  filter(!is.na(launch_angle)) %>%
  filter(!is.na(launch_speed)) %>%
  filter(launch_speed >= 95) %>%
  filter(launch_angle >= 8) %>%
  filter(launch_angle <= 32) %>%
  select(pitch_type, launch_speed, launch_angle, hc_x, hc_y, events)

unique(carpHH$pitch_type)

carp_ch <- carpHH %>%
  tally(pitch_type == "CH")
carp_fc <- carpHH %>%
  tally(pitch_type == "FC")
carp_kc <- carpHH %>%
  tally(pitch_type == "KC")
carp_ff <- carpHH %>%
  tally(pitch_type == "FF")
carp_si <- carpHH %>%
  tally(pitch_type == "SI")
carp_sl <- carpHH %>%
  tally(pitch_type == "SL")

carppt <- c(carp_ch, carp_fc, carp_kc, carp_ff, carp_si, carp_sl) 
carpptd <- as.data.frame(carppt)
names(carpptd) <- c('Changeup', 'Cutter', 'Knuckle-Curve', 'Four-Seamer', 'Sinker', 'Slider')
carpptd
carpdata <- data.frame(pt = names(carpptd), value = c(9,3,1,20,7,4))

ggplot(carpdata, aes(x=pt, y=value)) + 
  geom_bar(stat = "identity", color='#F6E42D', fill='#EA2B11') + 
  geom_text(aes(label=value), vjust=1.6, color="black", size=4) +
  labs(title = "Matt Carptenter", x = "Pitch Type", y = "Number of Hard-Hit Balls")

carpHH %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Matt Carpenter 2021 Hard-Hit (95+ mph EV and 8° to 32° LA) Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

kniz <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 668800)
knizHH <- kniz %>%
  filter(!is.na(launch_angle)) %>%
  filter(!is.na(launch_speed)) %>%
  filter(launch_speed >= 95) %>%
  filter(launch_angle >= 8) %>%
  filter(launch_angle <= 32) %>%
  select(pitch_type, launch_speed, launch_angle, hc_x, hc_y, events)

knizHH %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Andrew Knizner 2021 Hard-Hit (95+ mph EV and 8° to 32° LA) Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

williams <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 642211)
williamsHH <- williams %>%
  filter(!is.na(launch_angle)) %>%
  filter(!is.na(launch_speed)) %>%
  filter(launch_speed >= 95) %>%
  filter(launch_angle >= 8) %>%
  filter(launch_angle <= 32) %>%
  select(pitch_type, launch_speed, launch_angle, hc_x, hc_y, events)

williamsHH %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Justin Williams 2021 Hard-Hit (95+ mph EV and 8° to 32° LA) Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

noot <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 663457)
nootHH <- noot %>%
  filter(!is.na(launch_angle)) %>%
  filter(!is.na(launch_speed)) %>%
  filter(launch_speed >= 95) %>%
  filter(launch_angle >= 8) %>%
  filter(launch_angle <= 32) %>%
  select(pitch_type, launch_speed, launch_angle, hc_x, hc_y, events)

nootHH %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Lars Nootbaar 2021 Hard-Hit (95+ mph EV and 8° to 32° LA) Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

rondon <- scrape_statcast_savant(start_date = "2021-04-01", end_date = "2021-10-03", playerid = 602922)
rondonHH <- rondon %>%
  filter(!is.na(launch_angle)) %>%
  filter(!is.na(launch_speed)) %>%
  filter(launch_speed >= 95) %>%
  filter(launch_angle >= 8) %>%
  filter(launch_angle <= 32) %>%
  select(pitch_type, launch_speed, launch_angle, hc_x, hc_y, events)

rondonHH %>% 
  filter(events %in% c("single", "double", "triple", "home_run")) %>%
  mutate(events  = factor(events, levels = c("single", "double", "triple", "home_run"),
                          labels = c("Single", "Double", "Triple", "Home Run"))) %>%
  ggspraychart(fill_value = "events", 
               fill_palette = c("Single"="#9CA2DB", "Double"="#07117E", 
                                "Triple"="#F7F41F", "Home Run"="#ED3C3C")) + 
  labs(title = "Jose Rondon 2021 Hard-Hit (95+ mph EV and 8° to 32° LA) Hits") + 
  theme(
    plot.title = element_text(size = 12, hjust = .45),
    plot.caption = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 10, face = "bold")
  )

