#' Manage Ishortn API Token
#'
#' This function securely stores and retrieves the ishortn API token from an `.Renviron` file in the working directory. If a new token is provided, it replaces any existing token and ensures `.Renviron` is ignored in `.gitignore` to prevent accidental commits.
#'
#' @param token A character string containing the ishortn API token. If `NULL`, the function retrieves the stored token. Default is `NULL`.
#'
#' @return If a token is provided, the function stores it and returns `NULL`. If no token is provided, it returns the stored token as a character string.
#'
#' @details
#' This function manages the ishortn API token by storing it in an `.Renviron` file in the working directory. It also ensures that `.Renviron` is ignored in `.gitignore` to prevent it from being committed to version control.
#'
#' - Note that only one token can be stored at a time. If there's an already existing token, it will be replaced by the new one. So if you've used a TinyURL token before, it will be replaced by the new iShortn token, causing your requests to the TinyURL API to fail.
#' - If a **token is provided**, it is stored in `.Renviron`, and the user is prompted to restart R for the changes to take effect.
#' - If **no token is provided**, the function retrieves the stored token.
#' - If no token is found, the function stops with an error.
#'
#' **Security Measures:**
#' - Ensures `.Renviron` is automatically added to `.gitignore` to prevent exposure in public repositories.
#' - Only updates `.Renviron` if a new token is provided.
#'
#' @note Before using API functions like `squeeze_url()` or `recast_tinyurl()`, you must set up the ishortn API token using this function.
#'
#' @seealso \code{\link{squeeze_url}} for shortening URLs and \code{\link{recast_tinyurl}} for updating URLs.
#'
#' @export
manage_ishortn_token <- function(token = NULL) {

  renviron_path <- file.path(getwd(), ".Renviron")
  gitignore_path <- file.path(getwd(), ".gitignore")

  # Load existing .Renviron file from working directory
  if (file.exists(renviron_path)) {
    readRenviron(renviron_path)
  }

  current_token <- Sys.getenv("ISHORTN_API_TOKEN", unset = NA)

  # If a token is provided, store it in .Renviron
  if (!is.null(token)) {
    if (!is.na(current_token) && current_token == token) {
      message("iShortn token already stored in .Renviron.")
    } else {
      writeLines(sprintf("ISHORTN_API_TOKEN=\"%s\"", token), renviron_path)
      message("iShortn token stored successfully in the working directory. Restart R to apply changes.")
    }

    if (file.exists(gitignore_path)) {
      gitignore_lines <- readLines(gitignore_path)
      if (!any(grepl("^\\.Renviron$", gitignore_lines))) {
        writeLines("\n.Renviron", gitignore_path, append = TRUE)
        message("Added .Renviron to .gitignore to prevent accidental commits.")
      } else {
        message(".Renviron is already in .gitignore.")
      }
    } else {
      writeLines(".Renviron", gitignore_path)
      message("Created .gitignore and added .Renviron to prevent accidental commits.")
    }
  } else {
    if (is.na(current_token)) {
      stop("No iShortn token found. Use manage_ishortn_token(\"your_api_token\") to set it.")
    }
    invisible(current_token)
  }
}

