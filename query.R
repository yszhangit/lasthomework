library('googleway')

# query by state
query_state <- function(state) {
  for (i in 1:3) {
    result <- query_page(paste(query,state,sep=" "))
    if ( is.null(result$cnt)) {
      cat("error state")
      return(0)
    }
    if ( result$cnt ==0 ) {
      cat("no result found")
      return(0)
    }
    stores <<- rbind(stores,result$matches)
    if (is.null(next_page_token)) {
      cat("last page")
      break
    }
  }
}

# query google API, return list of (number of results, results DF)
query_page <- function(query_text) {
  cat(paste("called with",query_text, sep=" "))
  result <- list(cnt=NULL, matches=data.frame())
  
  if (is.null(next_page_token)) {
    cat("first page")
    res <- google_places(search_string = query_text, key = api_key)
  }else{
    cat("next page")
    res <- google_places(search_string =  query_text, page_token=next_page_token,key = api_key)
  } 
  # error status
  if (res$status != "OK") {
    cat("query error")
    print(res$status)
    next_page_token <<- NULL
    return(result)
  } 
  result$cnt <- dim(res$results)[1]
  # interesed fields
  place_ids <- res$results$place_id
  lat <- res$results$geometry$location$lat
  lng <- res$results$geometry$location$lng
  ratings <- res$results$rating 
  addr <- res$results$formatted_address
  result$matches <- data.frame(place_id = place_ids, 
                        lat = lat,
                        lng = lng, 
                        rating = ratings, 
                        address = addr)
 
  if (is.null(res$next_page_token)) {
    cat("no more pages")
    next_page_token <<- NULL
  }else{
    next_page_token <<- res$next_page_token
  }
  return(result)
}

## main
api_key <- readLines('google.key')
query <- "Costco "
stores <- data.frame()
next_page_token <- NULL

query_state('VA')
