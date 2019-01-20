library('googleway')
api_key <- readLines('google.key')
query <- "Costco "
stores <- data.frame()
next_page_token <- NULL

# query by state
query_state <- function(state) {
  for (i in 1:3) {
    result <- query_page(paste(query,state,sep=" "))
    if ( result$cnt ==0 ) {
      cat("error state")
      return(0)
    }else{
      cat(result$cnt)
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
  result <- list(cnt=0, matches=data.frame())
  
  if (is.null(next_page_token)) {
    cat("first page")
    res <- google_places(search_string = query_text, key = api_key)
  }else{
    cat("next page")
    res <- google_places(search_string =  query_text, page_token=next_page_token,key = api_key)
  } 
  # error status
  if (is.null(dim(res$results)[1])) {
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
    cat("not next page")
    next_page_token <<- NULL
  }else{
    next_page_token <<- res$next_page_token
  }
  return(result)
}

query_state('VA')

cnt <- 20
page_cnt <- 1
rm(next_page)

# max 60 result, 20 result at a time
# use page token to subsequent query
while (cnt == 20 & page_cnt < 4) {
  cat(sprintf("page %d", page_cnt))
  if (exists('next_page')) {
    cat("next page")
    res <- google_places(search_string =  query, page_token=next_page,key = api_key)
    # "INVALID_REQUEST, but sample
#    res <- google_places(page_token=next_page)
  } else {
    cat("first page")
    res <- google_places(search_string = query, key = api_key)
  }
  cnt <- dim(res$results)[1]
  # error status
  if (is.null(cnt)) {
    print(res$status)
    break
  }
 # interesed fields
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
  # append
  stores <- rbind(stores, new_res)  
  
  # no next page
  if (is.null(res$next_page_token)) {
    print('no next page')
    break
  }
  next_page <- res$next_page_token
  page_cnt <- page_cnt + 1
}

# imcomplete search, change query text if possible
if (cnt == 3) {
  print("more than 60")
}

#  note
# next page doesnt work
