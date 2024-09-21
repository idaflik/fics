library(needs)
needs(tidyverse,
      glue,
      sf,
      readxl,
      rnaturalearth,
      rmapshaper
)

gctf <- read_excel("../input/GCTF initiatives.xlsx")%>%
  mutate(`Lead states (Partners)` = str_replace_all(`Lead states (Partners)`, "\\(Global Community Engagement and Resilience Fund, GCERF\\)", "Global Community Engagement and \\\nResilience Fund \\(GCERF\\)"))

countries <- gctf %>%
  mutate(id = row_number(),
         `Lead states (Partners)` = if_else(is.na(`Lead states (Partners)`), "Not specified", `Lead states (Partners)`))%>%
  group_by(`Initiative launch`)%>%
  mutate(fake_month = row_number())%>%
  ungroup()%>%
  separate(`Lead states (Partners)`, into = c("1", "2", "3", "4"), sep = ", ")%>%
  gather(c(`1`:`4`), key = key, value = country)%>%
  filter(!is.na(country))

ne <- ne_countries(scale = "medium", returnclass = "sf")%>%
  select(name, name_long)%>%
  mutate(name = case_when(
    name == "United States" ~ "USA",
    name == "United Kingdom" ~ "UK",
    !(name %in% countries$country) ~ name_long,
    T ~ name
  ))

countries_geo <- ne %>%
  right_join(countries, by = c("name" = "country"))

plot_ortho <- function(center_x, center_y, spacing = 20){
  
  # projection string used for the polygons & ocean background
  crs_string <- glue("+proj=ortho +lon_0={center_x} +lat_0={center_y}")
  
  # background for the globe - center buffered by earth radius
  ocean <- st_point(x = c(0,0)) %>%
    st_buffer(dist = 6371000) %>%
    st_sfc(crs = crs_string)
  
  ne_ortho <- ne %>% 
    st_make_valid() %>%
    st_transform(4326)%>% 
    st_intersection(ocean %>% st_transform(4326)) %>% # select visible area only
    summarize(geometry = st_union(geometry))%>%
    st_transform(crs = crs_string) # reproject to ortho
  
  countries_ortho <- countries_geo %>%
    select(name)%>%
    unique()%>% 
    st_make_valid() %>%
    st_transform(4326)%>% 
    st_intersection(ocean %>% st_transform(4326)) %>% # select visible area only
    summarize(geometry = st_union(geometry))%>%
    st_transform(crs = crs_string) # reproject to ortho
  
  ggplot()+
    geom_sf(ocean,
            mapping = aes(),
            fill = NA,
            color = "black")+
    geom_sf(st_graticule(margin = 0.01, lat = seq(-90, 90, by = spacing), lon = seq(-180, 180, by = spacing)) %>% st_make_valid(), mapping = aes(), color = "black")+
    geom_sf(ne_ortho,
            mapping = aes(),
            fill = "grey90",
            color = NA)+
    geom_sf(countries_ortho,
            mapping = aes(),
            fill = "black",
            color = NA)+
    theme_void()
  
  ggsave(glue("../plots/gctf_map_{center_x}_{center_y}_graticules.svg"))
  ggsave(glue("../plots/gctf_map_{center_x}_{center_y}_graticules.png"))
  
}

## three globes
plot_ortho(-80, 00, 40)
plot_ortho(40, 20, 40)
plot_ortho(130, 10, 40)


ggplot(countries %>%
         filter(!is.na(`Initiative launch`))%>%
         mutate(fake_month = fake_month * 2,
                date = ymd(glue("{`Initiative launch`}-{`fake_month`}-{28}")),
                country = if_else(is.na(country), "Not specified", country)))+
  geom_point(mapping = aes(
               x = date,
               y = country
             ))+
  geom_line(mapping = aes(
               x = date,
               y = country,
               group = date
             ))+
  theme_bw()

ggsave("../plots/gctf_countries_timelines.svg",
      dpi = 140,
      width = 1200/140,
      height = 400/140)
