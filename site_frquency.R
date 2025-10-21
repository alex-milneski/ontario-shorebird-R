# Data retrieval from NatureCounts using my account information.


oss <- nc_data_dl(username = username,
                  info = 'exploratory portfolio project',
                  collections = 'PRISM-OSS')

lakes <- ne_download(
  scale = 50,
  type = "lakes",
  category = "physical",
  returnclass = "sf"
)

#TO DO: Map 'Great Lakes' in the area for easier map reading.


ontario <- ne_states(country = "Canada", returnclass = "sf") %>% 
  filter(name == "Ontario") %>% 
  st_transform(3347)


dropped_na <- oss[!is.na(oss$SiteCode),] %>% 
  subset(survey_year >= 1980 & survey_year <= 2024)

site_codes <- dropped_na %>% 
   group_by(site = SiteCode, lat = latitude, long = longitude) %>%
   summarize(frequency = sum(length(SiteCode)))


sites_sf <- st_as_sf(site_codes, coords = c('long', 'lat'), crs = 4326) %>% 
  st_transform(3347)


zoom <- data.frame(lon = c(-80, -74, -80, -74),
                     lat = c(44, 44, 47, 47)) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
    st_transform(3347) %>% 
    st_bbox()

study_site1 <- sites_sf %>% 
  filter(site == '42.007' | site == '52.003' | site == '42.003')


map1 <- ggplot() +
  theme_bw() +
  ggtitle('Ontario Shorebird Survey Sites by Survey Frequency') +
  geom_sf(data = ontario) +
  geom_sf(data = sites_sf, aes(size = frequency)) +
  scale_size(
    name = "Survey Frequency (1980-2024)",
    breaks = c(500,1000,2500,7500),
    range = c(0.5,10))

map2 <- ggplot(study_site1) +
  theme_bw() +
  ggtitle('Survey Sites of Interest') +
  geom_sf(data = ontario) +
  geom_sf(data = study_site1) +
  geom_label_repel(
    data = study_site1,
    aes(label = study_site1$site, geometry = study_site1$geometry), # Map geometry
    stat = "sf_coordinates",) +
  coord_sf(xlim = zoom[c(1,3)], ylim = zoom[c(2,4)]) +
  labs(x = NULL, y = NULL)  #remove x and y-axis labels



