library(leaflet)
library(dplyr)
costcos <- readRDS('costco.rds')
# have to remove base on address afterall
costcos <- costcos %>% distinct(address, .keep_all = T)

#rating.level <- split(stores, stores$rating)

leaflet(data=costcos) %>% addTiles() %>% 
  addMarkers(~lng, 
             ~lat, 
             popup = ~as.character(paste(name,address,sep=",")), 
             label = ~as.character(rating)
             )
