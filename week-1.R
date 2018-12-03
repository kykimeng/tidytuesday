library(readxl)
library(tidyverse)
library(leaflet)
library(raster)
library(rgeos)
library(maptools)
library(gganimate)
library(ggmap)

theme_set(theme_bw())
avg_tuition <- read_excel("Data/us_avg_tuition.xlsx")

# leaflet ----
USA <- getData("GADM", country = "usa", level = 1)
tmp2 <- subset(USA, !NAME_1 %in% c("Alaska", "District of Columbia"))
tmp <- subset(USA, NAME_1 == "Alaska")
tmp3 <- gUnaryUnion(tmp)
data_tmp <- tmp@data[1,]
rownames(data_tmp) <- row.names(tmp3)
tmp3 <- SpatialPolygonsDataFrame(tmp3, data = data_tmp)
tmp3 <- spChFIDs(tmp3, "Alaska")

USA_2 <- spRbind(tmp2, tmp3)

us_tuition <- sp::merge(USA_2, avg_tuition, by.x = "NAME_1", by.y = "State")

pal <- colorNumeric("RdYlBu", us_tuition$`2015-16`)
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = us_tuition, weight = 1, fill = ~pal(`2015-16`), 
              color = ~pal(`2015-16`), fillOpacity = 1, 
              label = ~paste("2015-16 Tuition: ", scales::comma(`2015-16`)))

# gganimate ----
us <- map_data("state")
tuition_sum <- avg_tuition %>% 
  mutate(region = tolower(State)) %>% 
  gather(year, tuition, -c(State, region)) 

tui_by_state <- inner_join(us, tuition_sum, by = "region")

p <- ggplot(data = us, aes(x=long, y = lat, group = group)) +
  geom_polygon() +
    geom_polygon(data = tui_by_state, aes(fill = tuition)) +
    theme(panel.grid = element_blank(), 
          axis.title = element_blank(), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.line = element_blank(), 
          plot.title = element_text(size = 30), 
          plot.subtitle = element_text(size = 28, face = "bold")) +
    scale_fill_gradientn(colours = c("#ffffcc", "#a1dab4", "#41b6c4", "#2c7fb8", 
                        "#253494"), name = "", labels = scales::dollar) +
    labs(title = "Average Tuition by State", 
         subtitle = "Year: {closest_state}") +
    transition_states(year, transition_length = 1, state_length = 1)
animate(p, height = 500, width = 800)
anim_save("tuition_over_year.gif")

