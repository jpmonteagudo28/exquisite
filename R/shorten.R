#' Squeeze Long URLs into short URLs
#'
#' This function shortens long URLs using the TinyURL and ishortn API. It requires an API token, which is stored in the `.Renviron` file, and allows you to create short URLs with optional aliases and expiration dates. The function supports three domains: `tinyurl.com`,`tiny.one`, and `ishortn.ink`. Aliases must be at least 5 characters long and contain only alphanumeric characters. Expiration dates are only available for paid TinyURL users. ishortn free tier users are able to set expiration rules.
#'
#' @param long_url A character string representing the URL to shorten.
#' @param domain A character string specifying the domain for the shortened URL. Can be either "tinyurl.com", "tiny.one", or "ishortn.ink". Default is "tinyurl.com". Note: Custom domains are not available for free accounts in TinyURL or ishortn.ink.
#' @param alias A character string specifying an alias for the shortened URL. The alias must be at least 5 characters long and contain only numbers or letters. Default is `NULL`.
#' @param description A character string specifying a description for the shortened URL. Default is `NULL`.
#' @param expires_at A character string specifying the expiration date for the shortened URL. This is available only for paid users and should be in the format "YYYY-MM-DD". Default is `NULL`, meaning no expiration date.
#' @param disable_clicks A numeric value specifying the number of clicks after which the 'ishortn.ink' URL will be disabled. Default is `NULL`, meaning no limit on the number of clicks.
#' @param disable_date A character string specifying the date after which the 'ishortn.ink' URL will be disabled. This should be in the format "YYYY-MM-DD". Default is `NULL`, meaning no expiration date.
#' @param tags A character string specifying tags for the shortened URL. Default is `NULL`.
#' @param password A character string specifying a password to protect the ishortn link. Default is `NULL`. This feature is only available to paid users of ishortn.
#' @return A character string representing the shortened URL.
#'
#' @details
#' The function uses the TinyURL or ishortn API to shorten the provided `long_url`. The API token is managed and retrieved automatically from the `.Renviron` file. Aliases must be at least 5 characters and contain only alphanumeric characters (no special characters). If an alias is provided, it will be used as the custom part of the shortened URL. Expiration dates are available only to paid users and are not supported for free users.
#'
#' @note The API token must be set up and stored securely in the `.Renviron` file before using this function. You can set the token with the `manage_tinyurl_token()` or `manage_ishortn_token` function.
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
                        description = NULL,
                        expires_at = NULL,
                        disable_clicks = NULL, # ishortn arguments
                        disable_date = NULL,   # ishortn arguments
                        tags = NULL,           # ishortn arguments
                        password = NULL){      # ishortn arguments

  stopifnot(is.character(long_url),
            !is_tiny_url(long_url),
            is.character(domain),
            is_tiny_domain(domain) || is_ishortn_domain(domain), # ishortn links are also supported
            is.null(alias) | (is.character(alias) & nchar(alias) >= 5),
            is.null(description) | (is.character(description)),
            is.null(expires_at) | (is.character(expires_at)),
            is.null(disable_clicks) | (is.numeric(disable_clicks)),
            is.null(disable_date) | (is.character(disable_date)),
            is.null(tags) | (is.character(tags))
            )

  if(is_tiny_domain(domain)){
    api_url <- "https://api.tinyurl.com/create"
    api_token <- manage_tinyurl_token()
  } else {
    api_url <- "https://ishortn.ink/api/v1/links"
    api_token <- manage_ishortn_token()
  }



  # Create a list with the parameters
  body <- list(
    url = long_url,
    domain = domain
  )

  # Only add alias and expiration if provided
  if (!is.null(alias)) body$alias <- alias
  if (!is.null(expires_at)) body$expires_at <- expires_at
  if(!is.null(description)) body$description <- description
  if(!is.null(disable_clicks)) body$expiresAfter <- disable_clicks
  if(!is.null(disable_date)) body$expiresAt <- disable_date
  if(!is.null(tags)) body$tags <- tags


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

  # Add the JSON body
  request <- request |> httr2::req_body_json(body)

  # Set request method for iShortn (only needed for iShortn)
  if (!is_tiny_domain(domain)) {
    request <- request |> httr2::req_method("POST")
  }

  # Perform the request
  response <- request |> httr2::req_perform()

  # Parse the response
  results <- response |> httr2::resp_body_json()

  # Return the correct field
  return(if (is_tiny_url(domain)) results$data$tiny_url else results$shortLink)
}
