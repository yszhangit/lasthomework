library('googleway')
key <- readLines('google.key')

query <- "Costco VA"

cnt < 20
page_cnt <-1
stores <- data.frame()

while cnt == 20 & page_cnt <= 3 {
  res <- google_places(search_string = query, key = key)
  cnt <- dim(res$results)[1]
  page_cnt <- page_cnt + 1
  next_page <- res$next_page_token
  place_ids <- res$results$place_id
  lat <- res$results$geometry$location$lat
  lng <- res$results$geometry$location$lng
  ratings <- res$results$rating 
  addr <- res$results$formatted_address
  new_res <- data.frame(place_id = place_ids, 
                        lat = lat,
                        lng = lng, 
                        rating = ratings, 
                        address = addr)
  stores <- rbind(stores, new_res)
}

if cnt == 3 {
  print("more than 60")
}
