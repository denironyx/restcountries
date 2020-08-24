
ua <- user_agent("https://github.com/denironyx/countries")

#' Title
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
restcountries_api <- function(path){
  url <- paste0("https://restcountries.eu/rest/v2/", path = path)

  resp = GET(url, ua)

  if(http_error(resp)){

    error_status <- httr::http_status(resp)

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
  }
  return_df <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = TRUE)

  #structure(
  #  list(
   #   content = return_df,
      #path = path,
    #  response = resp
    #),
  #  class = "restcountries_api"
  #)

  return_df
}

region_data <- restcountries_api("region/africa")
View(region_data$content)

View(region_data$content)



