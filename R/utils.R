is_tiny_url <- function(url){

  pattern <- "tinyurl.com"
  tiny_pattern<- "tiny.one"

  # Check if any match exists
  any(grepl(pattern, url, fixed = TRUE) | grepl(tiny_pattern, url, fixed = TRUE))
}

is_tiny_domain <- function(domain){

  pattern <- "tinyurl.com"
  tiny_pattern<- "tiny.one"

  # Check if any match exists
  any(grepl(pattern, domain, fixed = TRUE) | grepl(tiny_pattern, domain, fixed = TRUE))
}

is_ishortn_url <- function(url){

  pattern <- "ishortn.ink"

  # Check if any match exists
  grepl(pattern, url, fixed = TRUE)
}

is_ishortn_domain <- function(domain){

  pattern <- "ishortn.ink"

  grepl(pattern, domain, fixed = TRUE)
}

