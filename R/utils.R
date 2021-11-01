# TODO, user agent maybe shouldn't be bare like this

#' User agent for all interaction with restcountries_api
#' @importFrom httr user_agent
ua <- httr::user_agent("https://github.com/denironyx/countries")

#' Title
#' This function fetch the data from the RESTful API
#' @param path
#'
#' @return
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET http_error http_status http_type content
#' @importFrom R.cache addMemoization
restcountries_api <- addMemoization(function(path){
  url <- paste0("https://restcountries.com/v2/", path = path)

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


})





