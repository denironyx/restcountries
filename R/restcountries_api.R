# INITIALISE ====

# TODO: Add roxygen2 blocks for all exported functions
# TODO: Extract repeated code, a lot looks like it can be pushed down into restcountries_api
# TODO: we generally shouldn't source like this in the package initalization
# conditionally setup environment
# if (sys.parent() == 0) try(source('R/setup_environment.R'), silent = T)
# source("R/restcountries_api.R")

#' Search by All
#' The `rc_all` function described the REST endpoint available that you can use to search for all the countries information.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rc_all()
#' x
#' }
rc_all <- function(){


  df <- restcountries_api("all") %>%
    restcountries_clean()

  df
}


#' Search by Country Language
#'
#' The `rc_by_lang` function returns the countries information for all the different language symbol. ISO 639-1 language code
#'
#' @param lang vector(character) of language symbol (string such as "en", "eng", "fr", "es")
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' ## Countries that speaks English as either an first language or second language
#' x <- rc_by_lang("en")
#' x
#' }
rc_by_lang <- function(lang){

  if(lang %in% c("en", "ng", "es", "fr")) {

    df <- restcountries_api(paste0("lang/", lang))

  } else {
    stop("country parameter has no valid values. Please check documentation for valid inputs")
  }

  df %>%
    restcountries_clean() %>%
    unnest(languages, names_sep = "_")

  df

}

#' Search by Country Full Name
#'
#' Search by country name. This function will allow you search countries by their short name, official name,native name or partial name.
#' More information can be found \url{https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_and_their_capitals_in_native_languages}
#'
#' @param name vector(character) of the name of a country of choice.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' }
rc_by_name <- function(name){

  df <- restcountries_api(paste0("name/", name, "?fullText=true")) %>%
    restcountries_clean() %>%
    unnest(languages, names_sep = "_")

  df

}


#' Search by Currency
#'
#' Currency is a system of money in general use in a particular country at a specific time. You can read more about the currencies for the various countries \url{https://www.xe.com/currency/}
#'
#' @param input Vector(character) which contains three-character alphabetic, and the three digit numeric ISO 4217 code for any country.
#'
#' @return
#' @export
#' @importFrom dplyr '%>%' all_of rename select
#' @importFrom tidyr hoist unnest
#' @examples
#' \dontrun{
#' x <- rc_by_currency('USD')
#' x
#' }
rc_by_currency <- function(input){

  df <- restcountries_api(paste0("currency/", input)) %>%
    restcountries_clean() %>%
    unnest(currencies, names_sep = "_")

  df

}


#' Search by Capital City
#'
#' `rc_by_capital()` is used to search the restcountries restful API by the name of the city or town exercising primary status in a country, state, province
#' or other administrative region, usually as its seat of government.
#'
#' @param city Vector(character) of the capital city of the country you will like to search for.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rc_by_capital('Abuja')
#' x
#' }
rc_by_capital <- function(city){

  df <- restcountries_api(paste0("capital/", city)) %>%
    restcountries_clean()

  df

}


#' Search by Region
#'
#' Query the restcountries restful api based on united nations country grouping of world regions. You can research the grouping \url{https://www.nationsonline.org/oneworld/countries_of_the_world.htm}.
#'
#'
#' @param region Vector(character) of the united nations country groupings.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' x = rc_by_region('africa')
#' x
#' }
rc_by_region <- function(region){

  df <- restcountries_api(paste0("region/", region)) %>%
    restcountries_clean()

  df

}




#' Search by Calling Code
#'
#' Country codes are a component of the international telephone numbering plan, and are necessary only when
#' dialing a telephone number to establish a call to another country. Country codes are dialed before the national telephone number.
#' By convention, international telephone numbers are represented by prefixing the country code with a plus sign +,
#' which also indicates to the subscriber that the local international call prefix must first be dialed. \url{https://en.wikipedia.org/wiki/List_of_country_calling_codes}
#'
#' @param code Vector(character) which is the country telephone number prefixes for reaching telephone subscribers in the network of the member countries or regions.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rc_by_callingcode("234")
#' x
#' }
#'
rc_by_callingcode <- function(code){

  df <- restcountries_api(paste0("callingcode/", code)) %>%
    restcountries_clean()

  df


}

#' Regional Blocs
#'
#' Search by regional blocs. A regional blocs is a union or group of countries within a specific geographical boundary. This Union or group protects its
#' member nations within that region from imports from the non-members. \url{https://en.wikipedia.org/wiki/Trade_bloc}
#'
#' @param bloc Vector(Character) which represent the abbreviation of the regional bloc of choice eg AU represents African Union
#'
#' @return
#' @export
#'
#' @examples
rc_by_blocs <- function(bloc){

  df <- restcountries_api(paste0("regionalbloc/", bloc)) %>%
    restcountries_clean()

  df


}

#' Country Code
#'
#' Search by ISO 3166-1 2-letter or 3-letter country code \url{https://www.iban.com/country-codes}
#'
#' @param code Vector(character) which contains the country code to query the restcountries API
#'
#' @importFrom purrr map_if
#' @return
#' @export
#'
#' @examples
rc_by_code <- function(code){

  df <- restcountries_api(paste0("alpha/", code))


  df_list <- map_if(df, is.data.frame, list)
  df_list$altSpellings <- NULL
  df_list$borders <- NULL
  df_list$translations <- NULL
  df_list$latlng <- NULL

  out_list <- as_tibble(df_list)


  if(length(out_list) == 0){

    warning("No data was returned for any requested country. Returning empty data frame")
    out_df <- as.data.frame(matrix(nrow = 0, ncol = length(out_list)))
    names(out_df) <- names(out_list)

  } else {

    out_list

  }


}



