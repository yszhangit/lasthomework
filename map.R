library(leaflet)
library(dplyr)
costcos <- readRDS('costco.rds')
# have to remove base on address afterall
costcos <- costcos %>% distinct(address, .keep_all = T)

# test
leaflet(data=costcos) %>% addTiles() %>% 
  addMarkers(~lng, 
             ~lat, 
             popup = ~as.character(paste(name,address,sep=",")), 
             label = ~as.character(rating)
             )

#overview rating distribution
table(costcos$rating)

  
# https://rstudio.github.io/leaflet/showhide.html
costcos <- costcos %>%
  mutate(rating.level = cut(rating,c(1, 4.4, 4.5, 5),
                                labels = c('>1 & <=4.4', '>4.4 & <=4.5', '>4.5 & <=5')))

costcos.df <- split(costcos, costcos$rating.level)

l <- leaflet() %>% addTiles()

names(costcos.df) %>%
  purrr::walk( function(df) {
    l <<- l %>%
      addMarkers(data=costcos.df[[df]],
                 lng=~lng, lat=~lat,
                 label=~as.character(rating),
                 popup=~as.character(address),
                 group = df,
                 clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
                 labelOptions = labelOptions(noHide = F,
                                             direction = 'auto'))
  })

l %>%
  addLayersControl(
    overlayGroups = names(costcos.df),
    options = layersControlOptions(collapsed = FALSE)
  )
