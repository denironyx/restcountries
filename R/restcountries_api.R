# TODO, user agent maybe shouldn't be bare like this

#' User agent for all interaction with restcountries_api
#' @importFrom httr user_agent
ua <- httr::user_agent("https://github.com/denironyx/countries")

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET http_error http_status http_type content
restcountries_api <- function(path){
  url <- paste0("https://restcountries.eu/rest/v2/", path = path)

  resp = GET(url, ua)

  if(http_error(resp)){

    error_status <- http_status(resp)

    stop(
      sprintf(
        "RESTCountries API request failed %s\nmessage: %s\ncategory: %s \nurl: %s",
        error_status$message,
        error_status$category,
        error_status$reason,
        url
      ),
      call. = FALSE
    )
  }

  if (http_type(resp) != "application/json") {
    stop("API call executed successfully, but did not return expected json format",
         call. = FALSE)
  } else{

    return_df <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = TRUE)
  }

  return_df
  #structure(
  #  list(
   #   content = return_df,
      #path = path,
    #  response = resp
    #),
  #  class = "restcountries_api"
  #)


}

# TODO: It isn't clear to me what these are doing, should they be here?
# Should they be documented?
# The code doesn't seem to work, so I'm commenting it out
# url_df <- jsonlite::fromJSON(content(url, "text"), simplifyVector = TRUE)




