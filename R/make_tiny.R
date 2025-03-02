#' Squeeze Long URLs into Tiny URLs
#'
#' This function shortens long URLs using the TinyURL API. It requires an API token, which is stored in the `.Renviron` file, and allows you to create short URLs with optional aliases and expiration dates. The function supports two domains: `tinyurl.com` and `tiny.one`. Aliases must be at least 5 characters long and contain only alphanumeric characters. Expiration dates are only available for paid users.
#'
#' @param long_url A character string representing the URL to shorten.
#' @param domain A character string specifying the domain for the shortened URL. Can be either "tinyurl.com" or "tiny.one". Default is "tinyurl.com". Note: Custom domains are not available for free accounts.
#' @param alias A character string specifying an alias for the shortened URL. The alias must be at least 5 characters long and contain only numbers or letters. Default is `NULL`.
#' @param expires_at A character string specifying the expiration date for the shortened URL. This is available only for paid users and should be in the format "YYYY-MM-DD". Default is `NULL`, meaning no expiration date.
#'
#' @return A character string representing the shortened URL.
#'
#' @details
#' The function uses the TinyURL API to shorten the provided `long_url`. The API token is managed and retrieved automatically from the `.Renviron` file. Aliases must be at least 5 characters and contain only alphanumeric characters (no special characters). If an alias is provided, it will be used as the custom part of the shortened URL. Expiration dates are available only to paid users and are not supported for free users.
#'
#' @note The API token must be set up and stored securely in the `.Renviron` file before using this function. You can set the token with the `manage_tinyurl_token()` function.
#'
#' @examples
#' \donttest{
#' # Example usage
#' short_url <- squeeze_url("https://www.example.com", alias = "exmpl")
#' print(short_url)
#' }
#'
#' @seealso \code{\link{manage_tinyurl_token}} for managing the TinyURL API token.
#'
#' @export
squeeze_url <- function(long_url,
                        domain = "tinyurl.com",
                        alias = NULL,
                        description = NULL,    # ishortn arguments
                        expires_at = NULL,
                        disable_clicks = NULL, # ishortn arguments
                        disable_date = NULL,   # ishortn arguments
                        tags = NULL){          # ishortn arguments

  stopifnot(is.character(long_url),
            !is_tiny_url(long_url),
            is.character(domain),
            is_tiny_domain(domain) || is_shortn_domain(domain), # ishortn links are also supported
            is.null(alias) | (is.character(alias) & nchar(alias) >= 5),
            is.null(description) | (is.character(description)),
            is.null(expires_at) | (is.character(expires_at)),
            is.null(disable_clicks) | (is.numerical(disable_clicks)),
            is.null(disable_date) | (is.character(disable_date)),
            is.null(tags) | (is.character(tags))
            )


  api_url <- "https://api.tinyurl.com/create"

  api_token <- manage_tinyurl_token()

  # Create a list with the parameters
  body <- list(
    url = long_url,
    domain = domain
  )

  # Only add alias and expiration if provided
  if (!is.null(alias)) body$alias <- alias
  if (!is.null(expires_at)) body$expires_at <- expires_at
  if(!is.null(description)) body$description <- description
  if(!is.null(disable_clicks)) body$disable_clicks <- disable_clicks
  if(!is.null(disable_date)) body$disable_date <- disable_date
  if(!is.null(tags)) body$tags <- tags


  # Create the request
  response <- httr2::request(api_url) |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_token),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_body_json(body) |>
    httr2::req_perform()

  results <- response |>
    httr2::resp_body_json()

  return(results$data$tiny_url)
}
