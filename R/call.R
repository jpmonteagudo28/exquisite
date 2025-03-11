#' Retrieve Short and Long URL from TinyURL or iShortn
#'
#' This function retrieves the **shortened URL** and the **original long URL** associated with an alias from either TinyURL or iShortn. The function works dynamically based on the provided domain (`tinyurl.com` or `ishortn.ink`).
#'
#' @param domain A character string specifying the domain to use for retrieving the shortened URL. It can either be `"tinyurl.com"` or `"ishortn.ink"`. Defaults to `"tinyurl.com"`.
#' @param alias A character string specifying the alias of the shortened URL. The alias must be at least 5 characters long.
#'
#' @details
#' The function sends a **GET request** to either the TinyURL or iShortn API, depending on the specified domain. If the domain is `"tinyurl.com"`, it constructs a URL for the TinyURL API and retrieves both the short URL (`tiny_url`) and long URL (`url`). For `"ishortn.ink"`, it retrieves the short URL (`shortLink`) and long URL (`url`).
#'
#' If the `alias` is not provided or is invalid, the function will stop with an error.
#'
#' @return A list containing two elements:
#' \itemize{
#'   \item `short_url`: The shortened URL.
#'   \item `long_url`: The original long URL.
#' }
#'
#' @examples
#' \dontrun{
#' # Retrieve the short and long URL from TinyURL
#' tinyurl_info <- recall(domain = "tinyurl.com", alias = "myalias")
#' print(tinyurl_info)
#'
#' # Retrieve the short and long URL from iShortn
#' ishortn_info <- recall(domain = "ishortn.ink", alias = "myalias")
#' print(ishortn_info)
#' }
#'
#' @export

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
