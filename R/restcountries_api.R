
ua <- user_agent("https://github.com/denironyx/countries")

restcountries_api <- function(path){
  url <- paste0("https://restcountries.eu/rest/v2/", path = path)

  resp = GET(url, ua)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  parsed <- jsonlite::fromJSON(content(resp, "text"), simplifyVector = TRUE)

  if(status_code(resp) != 200){
    stop(
      sprintf(
        "RESTCountries API request failed"
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "restcountries_api"
  )
}

region_data <- restcountries_api("region/africa")
View(region_data$content)





