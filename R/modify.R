#' Update an Existing TinyURL Short Link
#'
#' This function modifies an existing shortened URL created with TinyURL. It allows updating the alias, domain, expiration date, description, and tags associated with the short link. The API token is automatically retrieved from the `.Renviron` file.
#'
#' @param domain A character string specifying the current domain of the TinyURL link. It must be either `"tinyurl.com"` or `"tiny.one"`.
#' @param alias A character string specifying the current alias of the TinyURL link (e.g., `"example"` for `"tinyurl.com/example"`). Default is `NULL`, but must be provided to update a specific link.
#' @param new_domain A character string specifying the new domain for the shortened URL. Changing the domain is only available for paid users. Default is `NULL`.
#' @param new_alias A character string specifying the new alias for the shortened URL. The alias must be at least 5 characters long and contain only alphanumeric characters. Default is `NULL`.
#' @param new_stats A logical value indicating whether to enable advanced analytics for the TinyURL link. Default is `NULL`.
#' @param new_tags A character vector of tags associated with the shortened URL. Tags help categorize and organize links. Default is `NULL`.
#' @param new_expires_at A character string specifying the expiration time for the TinyURL link in ISO 8601 duration format (e.g., `"P10DT4H"` for 10 days and 4 hours). This feature is available only for paid accounts. Default is `NULL`.
#' @param new_description A character string providing a new description for the TinyURL link. Default is `NULL`.
#' @param password A character string specifying a password to protect the TinyURL link. Default is `NULL`.
#' @return A character string representing the updated shortened URL.
#'
#' @details
#' This function sends a `PATCH` request to TinyURL’s `/update` API endpoint to modify an existing short link. The API token is managed automatically via the `manage_tinyurl_token()` function. To update a short link, both the `domain` and `alias` of the existing link must be specified.
#'
#' **Limitations:**
#' - Free accounts **cannot change the tags**.
#' - The `new_alias` must be at least 5 characters long and **not already in use**.
#' - If `new_alias` is already taken, the request will fail with a `422 Unprocessable Entity` error.
#' - Expiration dates (`new_expires_at`) and link statistics (`new_stats`) are available **only for paid accounts**.
#'
#' @note Ensure that the alias being updated exists before calling this function. Use `squeeze_url()` to create new TinyURLs.
#'
#' @examples
#' \donttest{
#' # Update an alias of an existing TinyURL
#' recast_tinyurl(domain = "tiny.one", alias = "parvus", new_alias = "situs")
#'
#'
#' # Change expiration date (only available for paid users)
#' recast_tinyurl(domain = "tinyurl.com", alias = "myalias", new_expires_at = "P10DT4H")
#' }
#'
#' @seealso \code{\link{squeeze_url}} for creating new TinyURLs.
#'
#' @export
recast_tinyurl <- function(domain,
                          alias = NULL,
                          new_domain = NULL,
                          new_alias = NULL,
                          new_stats = NULL,
                          new_tags = NULL,
                          new_expires_at = NULL,
                          new_description = NULL,
                          password = NULL) {

  api_url <- "https://api.tinyurl.com/update"
  api_token <- manage_tinyurl_token()

  # Construct the request body
  body <- list(
    domain = domain,
    alias = alias
  )

  # Only add optional fields if they are provided
  if (!is.null(new_domain)) body$new_domain <- new_domain
  if (!is.null(new_alias)) body$new_alias <- new_alias
  if (!is.null(new_stats)) body$new_stats <- new_stats
  if (!is.null(new_tags)) body$new_tags <- new_tags
  if (!is.null(new_expires_at)) body$new_expires_at <- new_expires_at
  if (!is.null(new_description)) body$new_description <- new_description
  if(!is.null(password)) body$password <- password

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

  return(results$data$tiny_url)

}
