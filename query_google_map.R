library('googleway')

# query by state
query_state <- function(state) {
  cnt <- 0
  state_stores <-data.frame()
  
  for (i in 1:3) {
    result <- query_page(paste(query,state,sep=" "))
    if ( is.null(result$cnt)) {
      warning("error state")
      return(0)
    }
    if ( result$cnt == 0 ) {
      message("no result found")
      return(0)
    }
    # name exact match case insensitive
    # to filter out costco tire store, pharmacy etc.
    state_stores <- rbind(state_stores,result$matches[tolower(result$matches$name) == tolower(query),])
    if (is.null(next_page_token)) {
      message("last page")
      break
    }
  }
  cnt <- dim(state_stores)[1]
  if (cnt == 60) {
    warning("max 60 result from google")
  }
  # add state name and assign to global 
  state_stores <- cbind(state=rep(state, cnt), state_stores)
  stores <<- rbind(stores, state_stores)
  return(cnt)
}

# query google API, return list of (number of results, results DF)
query_page <- function(query_text) {
  # return object
  result <- list(cnt=NULL, matches=data.frame())
  
  # call google API base on pagnation
  if (is.null(next_page_token)) {
    message("first page")
    res <- google_places(search_string = query_text, key = api_key)
  }else{
    message("next page")
    # pause before search with same input, otherwise will get "invalid request" error
    Sys.sleep(3)
    res <- google_places(search_string = query_text, page_token = next_page_token, key = api_key)
  } 
  # error status
  if (res$status != "OK") {
    warning(res$status)
    next_page_token <<- NULL
    # empty result
    return(result)
  } 
  
  result$cnt <- dim(res$results)[1]
  # interesed fields
  place_ids <- res$results$place_id
  names <- res$results$name
  lat <- res$results$geometry$location$lat
  lng <- res$results$geometry$location$lng
  ratings <- res$results$rating 
  addr <- res$results$formatted_address
  result$matches <- data.frame(place_id = place_ids, 
                        name = names,
                        lat = lat,
                        lng = lng, 
                        rating = ratings, 
                        address = addr)
 
  if (is.null(res$next_page_token)) {
    message("no more pages")
    next_page_token <<- NULL
  }else{
    next_page_token <<- res$next_page_token
  }
  return(result)
}

## main
api_key <- readLines('google.key')
query <- "Costco wholesale"
stores <- data.frame()
next_page_token <- NULL

query_state('VA')
query_state('MD')
query_state('CA')



# save result for map
saveRDS(stores, 'costco.rds')