#' Update the Long URL of an Existing TinyURL Shortened Link
#'
#' This premium function allows you to **update the long URL** associated with an existing shortened URL (alias) on TinyURL. It requires a valid API token, which is retrieved from the `.Renviron` file. This function is **not available to free users**; a paid TinyURL account is required to use this feature.
#'
#' @param long_url A character string representing the new long URL to associate with the given alias.
#' @param domain A character string specifying the domain of the shortened URL. Default is `"tinyurl.com"`. You can also specify `"tiny.one"`.
#' @param alias A character string specifying the alias (existing shortened URL) to update. This alias must already exist and will be updated to point to the new `long_url`.
#'
#' @return A list containing the updated information, including the updated shortened URL.
#'
#' @details
#' This function sends a `PATCH` request to TinyURL’s `/change` API endpoint, which updates the long URL for an existing short link. The new long URL is associated with the existing alias. **This is a premium feature**, meaning free users **cannot access this function**.
#'
#' **Features:**
#' - Updates the long URL for an existing TinyURL.
#' - Requires a valid API token, stored in `.Renviron`.
#' - A premium TinyURL account is needed to use this function.
#' - Returns the updated shortened URL in the response.
#'
#' **Limitations:**
#' - Free users cannot access this function. A premium TinyURL account is required.
#' - The alias (`alias`) must exist in the TinyURL system.
#'
#' @note Before using this function, ensure that the API token is stored in your `.Renviron` file using the `manage_tinyurl_token()` function.
#'
#' @examples
#' \dontrun{
#' # Update the long URL for an existing TinyURL alias (premium feature)
#' updated_url <- stretch("https://newlongurl.com", domain = "tinyurl.com", alias = "existingalias")
#' print(updated_url)  # This will print the updated shortened URL
#' }
#'
#' @seealso \code{\link{manage_tinyurl_token}} for setting the TinyURL API token.
#'
#' @export
stretch <- function(long_url,
                    domain = "tinyurl.com",
                    alias = NULL){

  api_url <- "https://api.tinyurl.com/change"

  api_token <- manage_tinyurl_token()

  body <- list(
    url = long_url,
    domain = domain,
    alias = alias
  )

  # Perform the PATCH request
  response <- httr2::request(api_url) |>
    httr2::req_headers(
      Authorization = paste("Bearer", api_token),
      `Content-Type` = "application/json"
    ) |>
    httr2::req_method("PATCH") |>  # Explicitly set the request method to PATCH
    httr2::req_body_json(body) |>
    httr2::req_perform()

  results <- response |>
    httr2::resp_body_json()

  return(results)
}
