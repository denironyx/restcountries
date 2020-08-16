
get_all <- function(country = "all"){
  restcountries_api(country)
}

output <- get_all("all")
View(output$content)
