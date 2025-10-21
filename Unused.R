### unused code

#survey records - what is the month/frequency? 
counts_months <- scolopacidae_df %>%
  count(survey_month)
print(counts_months)

yellowlegs_df <- scolopacidae_df %>% 
  filter(species_id == 4420 | species_id == 4450 | species_id == 4490) #4420 is Greater, 4450 is lesser, 
                                                                      #4490 is Yellowlegs sp. unsure

#figuring out how many different sites ure being used
length(unique(yellowlegs_df$SiteCode))


site13 <- yellowlegs_df %>% 
  filter(SiteCode == '13.006') %>% 
  group_by(year = survey_year) %>% 
  summarise(sum = sum(as.numeric(ObservationCount)))

site_count <- lesser_df %>% 
  group_by(site = SiteCode) %>%
  summarise(sum = sum(as.numeric(ObservationCount)))

#Originally, I tried to map and sum counts by utm_squares - more error due to double counting and NA UTM values.
utm_locations <- length(unique(scolopacidae_df$utm_square))
utm_locations_per_year <- scolopacidae_df %>% 
  group_by(year = survey_year) %>% 
  summarize(utm_zone_count = sum(n_distinct(utm_square)))

# Also plotted a line summing all Lesser Yellowlegs observations during their peak migration months.
ggplot(yellowlegs_peak_migration, aes(x = year, y = sum)) +
  geom_line()

#mapping all observations in the data set.
scolo_sf <- scolo_sd %>% 
  subset(survey_year >= 1980 & survey_year <= 2024) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(3347)

