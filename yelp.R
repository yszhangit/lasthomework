# https://github.com/OmaymaS/yelpr
library(yelpr)
key <-readLines("yelp.key")

stores <- business_search(api_key=key,
                          term =  'costco',
                          location='1300 Edwards Ferry Rd NE',
                          limit = 50)
print("dont use YELP, the result is random BS")