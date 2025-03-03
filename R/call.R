recall <- function(domain = "tinyurl.com",
                   alias = NULL
                   ){

  stopifnot(is.character(domain),
            is_tiny_domain(domain) || is_ishortn_domain(domain), # ishortn links are also supported
            (is.character(alias) & nchar(alias) >= 5)
            )

  if (is.null(alias)) {
    stop("Alias must be provided to retrieve a shortened URL.")
  }

  if (is_tiny_domain(domain)) {
    api_url <- paste0("https://api.tinyurl.com/alias/", domain, "/", alias)
    api_token <- manage_tinyurl_token()
  } else {
    api_url <- paste0("https://ishortn.ink/api/v1/links/", alias)
    api_token <- manage_ishortn_token()
  }

  # Create the request
  request <- httr2::request(api_url)

  # Dynamically apply headers based on domain
  if (is_tiny_domain(domain)) {
    request <- request |>
      httr2::req_headers(
        Authorization = paste("Bearer", api_token),
        `Content-Type` = "application/json"
      )
  } else {
    request <- request |>
      httr2::req_headers(
        `x-api-key` = api_token,
        `Content-Type` = "application/json"
      )
  }

  # Set request method to GET
  request <- request |> httr2::req_method("GET")

  # Perform the request
  response <- request |> httr2::req_perform()

  # Parse the response
  results <- response |> httr2::resp_body_json()

  # Extract the short and long URLs
  short_url <- if (is_tiny_domain(domain)) results$data$tiny_url else results$shortLink
  long_url <- if (is_tiny_domain(domain)) results$data$url else results$url

  # Return both short and long URLs as a list
  return(list(short_url = short_url, long_url = long_url))
}
