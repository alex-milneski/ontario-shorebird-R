
# Data retrieval from NatureCounts using my own personal account.  


all <- search_species(show = "all") #Creating index to find the specific species codes for Yellowlegs sp. (Lesser and Greater) 

scolopacidae <- filter(all, family_name == 'Scolopacidae')  #Largest family, includes most shorebirds from the OSS.

scolopacidae_df <- nc_data_dl(
  username = username,
  species = scolopacidae$species_id,
  info = 'exploratory portfolio project',
  collections = 'PRISM-OSS')

counts_birds <- scolopacidae_df %>%
  count(species_id)  #Yellowlegs have a relatively high count for the scope of the project. 

scolopacidae_count <- scolopacidae_df %>% 
  group_by(year = survey_year) %>%
  summarize(sum = sum(as.numeric(ObservationCount))) #Infrequent surveys before 1980, incomplete 2025 (ongoing).

lesser_df <- scolopacidae_df %>% 
  filter(species_id == 4450) %>% 
  filter(survey_year >= 1980 & survey_year <= 2024)
  
lesser_site_specific <- lesser_df %>% 
  filter(survey_year >= 1985 & survey_year <= 2024) %>%     #1982 contains extremely counts in the 1000s (extreme outlier)
  filter(SiteCode == '42.007' | SiteCode == '52.003' | SiteCode == '42.003')

lesser_13006 <- scolopacidae_df %>% 
  filter(species_id == 4450) %>% 
  filter(survey_year >= 1980 & survey_year <= 2024) %>%
  filter(SiteCode == '13.006') #Site 13.006 has the highest survey frequency and return rate as found in site_frquency.R
  
greater_df <- scolopacidae_df %>% 
  filter(species_id == 4420) %>%
  filter(survey_year >= 1985 & survey_year <= 2024) %>%
  filter(SiteCode == '42.007' | SiteCode == '52.003' | SiteCode == '42.003')

summary_lesser <- lesser_site_specific %>% 
  group_by(site = SiteCode, year = survey_year) %>% 
  summarise(sum = sum(as.numeric(ObservationCount)), .groups = "drop")

summary_greater <- greater_df %>% 
  group_by(site = SiteCode, year = survey_year) %>% 
  summarise(sum = sum(as.numeric(ObservationCount)), .groups = "drop")

lesser_yellowlegs_count <- lesser_13006 %>% 
  group_by(year = survey_year) %>%
  summarize(sum = sum(as.numeric(ObservationCount)))

ggplot(lesser_yellowlegs_count, aes(x = year, y = sum)) +
  theme_bw() +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE,      # Regression line
              color = "red", linetype = "dashed") +
  scale_x_continuous(breaks = seq(1980, 2020, by = 5)) +
  labs(x = "Year", y = "Count", title = "Lesser Yellowleg Observations at Nonquon Sewage Lagoon")

ggplot(summary_lesser, aes(x = year, y = sum, colour = site)) +
  theme_bw() +
  geom_point() +
  geom_line(size = 0.1, linetype = 'dashed') +
  scale_x_continuous(breaks = seq(1985, 2025, by = 5)) +
  labs(x = "Year", y = "Lesser Yellowlegs Count", title = "Lesser Yellowlegs Observations at Ottawa sites")


ggplot(summary_greater, aes(x = year, y = sum, colour = site)) +
  theme_bw() +
  geom_point() +
  geom_line(size = 0.1, linetype = 'dashed') +
  scale_x_continuous(breaks = seq(1985, 2025, by = 5)) +
  labs(x = "Year", y = "Greater Yellowlegs Count", title = "Greater Yellowlegs Observations at Ottawa sites")

yellowlegs_peak_migration <- yellowlegs_df %>% 
  filter(survey_month == 7 | survey_month == 8) %>%
  group_by(year = survey_year) %>% 
  summarise(sum = sum(as.numeric(ObservationCount)))
  
ontario <- ne_states(country = "Canada", returnclass = "sf") %>% 
  filter(name == "Ontario") %>% 
  st_transform(3347)

lesser_sf <- lesser_df %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  st_transform(3347)

map <- ggplot() +
  theme_bw() +
  geom_sf(data = ontario) +
  geom_sf(data = lesser_sf)


animap1 <- map +
  transition_states(survey_year,
                    transition_length = 1,
                    state_length = 15) +
                    ease_aes('linear') +
                    ggtitle('Lesser Yellowlegs Observations',
                            subtitle = 'Survey Year: {closest_state}')

final_map <- animate(animap, fps = 10, duration = 30, end_pause = 10)


