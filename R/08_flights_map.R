library(needs)
needs(tidyverse,
      glue,
      sf,
      geosphere,
      rnaturalearth
)

## https://hub.worldpop.org/project/categories?id=13

countries <- ne_countries(returnclass = "sf", scale = "medium")%>%
  select(name, name_long, iso_a2, iso_a3, pop_est)

# flights <- read_csv("../input/Global_Flight_Data_Annual/Annual-data/prediction.csv")%>%
#   mutate(connection_nr = row_number()) %>%
#   gather(-stops, -PredMu, -connection_nr, key = key, value = airport_name)%>%
#   left_join(
#     read_csv("../input/Global_Flight_Data_Annual/Annual-data/AirportInfo.csv"),
#     by = c("airport_name" = "NodeName")
#   )%>%
#   filter(!is.na(Lon) & !is.na(Lat))

## make the lines wrap around the edge
## https://gis.stackexchange.com/questions/122494/how-to-prevent-cross-world-lines-in-ggplot-world-map
## https://stackoverflow.com/questions/25881626/plotting-lines-on-map-gcintermediate

# lines <- flights %>%
#   filter(key == "Origin")%>%
#   select(-c(airport_name:City), -key)%>%
#   left_join(
#     flights %>%
#       filter(key == "Destination")%>%
#       rename(Lat2 = Lat, Lon2 = Lon)%>%
#       select(-c(airport_name:City), -key)
#   )%>%
#   ## only direct flights
#   filter(stops == 1)
#
# lines_sphere <- c(1:nrow(lines)) %>%
#   map_dfr(function(i){
#     df <- gcIntermediate(c(lines$Lon[i],lines$Lat[i]), c(lines$Lon2[i],lines$Lat2[i]), breakAtDateLine = T)
#     if(is.list(df)){
#       df2 <- bind_rows(
#         data.frame(df[[1]])%>%
#           mutate(group = i*10 + 1,
#                  PredMu = lines$PredMu[i]),
#         data.frame(df[[2]])%>%
#           mutate(group = i*10 + 2,
#                  PredMu = lines$PredMu[i])
#       )
#     }else{
#       df2 <- data.frame(df)%>%
#         mutate(group = i*10,
#                PredMu = lines$PredMu[i])
#     }
#     df2
#   })
# 
# write_csv(lines_sphere, "../output/lines_flights_sphere.csv")

lines_sphere <- read_csv("../output/lines_flights_sphere.csv")

groups <- lines_sphere$group %>% unique()

ante_meridian <- 0 ## change as needed
offset <- ante_meridian+180

jitter = 0.01
lat <- c(seq(from=-90,to=90,by=0.25))
lon <- c(rep(offset-jitter,length(lat)),rep(offset+jitter,length(lat)))

oceans <- cbind(lon, c(lat, rev(lat))) %>% 
  st_linestring() %>% 
  st_cast("POLYGON") %>% 
  st_sfc(crs=4326)%>%
  st_as_sf()

ggplot()+
  geom_sf(countries, mapping = aes(), color = NA, fill = "grey90")+
  geom_path(
    lines_sphere,
    mapping = aes(
      x = lon,
      y = lat,
      group = group,
      linewidth = PredMu
    ),
    color = "black",
    alpha = 0.05
  )+
  scale_linewidth_continuous(range = c(0.1,5))+
  theme_void()+
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "none")

ggsave("plots/flightroutes_sphere_white_bg.png",
       dpi = 300,
       width = 5500/300,
       height = 3000/300)
