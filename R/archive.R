archive <- function(domain = "tinyurl.com",
                    alias = NULL
                   ){

  stopifnot(is.character(domain),
            is_tiny_domain(domain) || is_ishortn_domain(domain),
            (is.character(alias) & nchar(alias) >= 5)
           )

  if (is.null(alias)) {
    stop("Alias must be provided to retrieve a shortened URL.")
  }

  # Create a list with the parameters
  body <- list(
    domain = domain,
    alias = alias
  )

  if (is_tiny_domain(domain)) {
    api_url <- "https://api.tinyurl.com/archive"
    api_token <- manage_tinyurl_token()

    # Create the request
    request <- httr2::request(api_url) |>
      httr2::req_headers(
        Authorization = paste("Bearer", api_token),
        `Content-Type` = "application/json"
      ) |>
      httr2::req_body_json(body) |>
      httr2::req_method("PATCH")
  } else {
    api_url <- paste0("https://ishortn.ink/api/v1/links/", alias)
    api_token <- manage_ishortn_token()

    request <- httr2::request(api_url) |>
      httr2::req_headers(
        `x-api-key` = api_token,
        `Content-Type` = "application/json"
      ) |>
      httr2::req_body_json(body) |>
      httr2::req_method("DELETE")
  }

  # Perform the request
  response <- request |> httr2::req_perform()

  # Parse the response
  results <- response |> httr2::resp_body_json()

  archived_result <- if(is_tiny_domain(domain)) results$archived else results$archived

  return(archived_result)
}
