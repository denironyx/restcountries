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
#' }
rc_all <- function(){


  df <- restcountries_api("all")

  ## remove the errored returns
  df_index <- sapply(df, is.data.frame)
  out_list  <- df[!df_index]

  ## defaultName" - "newName"
  out_cols <- c(
    "country_name" = "name",
    "domain" = "topLevelDomain",
    "iso2c" = "alpha2Code",
    "iso3c" = "alpha3Code",
    "calling_codes" = "callingCodes",
    "alt_spelling" = "altSpellings",
    "sub_region" = "subregion",
    "native_name" = "nativeName",
    "regional_blocs" = "regionalBlocs",
    "numeric_code" = "numericCode"
  )

  if(length(out_list) == 0){

    warning("No data was returned for any requested country. Returning empty data frame")
    out_df <- as.data.frame(matrix(nrow = 0, ncol = length(out_cols)))
    names(out_df) <- names(out_cols)

  } else {

    out_df <- out_list %>%
      rename(
        all_of(out_cols)
      ) %>%
      hoist(latlng,
            lat = list(1),
            lon = list(2)
            )
  }
  out_df
}


#' Search by Country Language
#'
#' The `rc_by_lang` function returns the countries information for all the different language symbol.
#'
#' @param lang language symbol (string such as "en", "eng", "fr", "es")
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

  if(x %in% c("en", "ng", "es", "fr")) {

    df <- restcountries_api(paste0("lang/", lang))

  } else {
    stop("country parameter has no valid values. Please check documentation for valid inputs")
  }

  ## remove the errored returns
  df_index <- sapply(df, is.data.frame)
  out_list  <- df[!df_index]

  ## defaultName" - "newName"
  out_cols <- c(
    "country_name" = "name",
    "domain" = "topLevelDomain",
    "iso2c" = "alpha2Code",
    "iso3c" = "alpha3Code",
    "calling_codes" = "callingCodes",
    "alt_spelling" = "altSpellings",
    "sub_region" = "subregion",
    "native_name" = "nativeName",
    "regional_blocs" = "regionalBlocs",
    "numeric_code" = "numericCode"
  )

  if(length(out_list) == 0){

    warning("No data was returned for any requested country. Returning empty data frame")
    out_df <- as.data.frame(matrix(nrow = 0, ncol = length(out_cols)))
    names(out_df) <- names(out_cols)

  } else {

    out_df <- out_list %>%
      rename(
        all_of(out_cols)
      ) %>%
      hoist(latlng,
            lat = list(1),
            lon = list(2)
      ) %>%
      unnest(languages, names_sep = "_")
  }
  out_df
}

#' Search by Country Full Name
#'
#' @param name -
#'
#' @return
#' @export
#'
#' @examples
rc_by_name <- function(name){

  df <- restcountries_api(paste0("name/", name, "?fullText=true"))

  ## remove the errored returns
  df_index <- sapply(df, is.data.frame)
  out_list  <- df[!df_index]

  ## defaultName" - "newName"
  out_cols <- c(
    "country_name" = "name",
    "domain" = "topLevelDomain",
    "iso2c" = "alpha2Code",
    "iso3c" = "alpha3Code",
    "calling_codes" = "callingCodes",
    "alt_spelling" = "altSpellings",
    "sub_region" = "subregion",
    "native_name" = "nativeName",
    "regional_blocs" = "regionalBlocs",
    "numeric_code" = "numericCode"
  )

  if(length(out_list) == 0){

    warning("No data was returned for any requested country. Returning empty data frame")
    out_df <- as.data.frame(matrix(nrow = 0, ncol = length(out_cols)))
    names(out_df) <- names(out_cols)

  } else {

    out_df <- out_list %>%
      rename(
        all_of(out_cols)
      ) %>%
      hoist(latlng,
            lat = list(1),
            lon = list(2)
      ) %>%
      unnest(languages, names_sep = "_")
  }
  out_df

}


#' Title
#'
#' @param input
#'
#' @return
#' @export
#' @importFrom dplyr '%>%' all_of rename select
#' @importFrom tidyr hoist unnest
rc_by_currency <- function(input){

  df <- restcountries_api(paste0("currency/", input))

  ## remove the errored returns
  df_index <- sapply(df, is.data.frame)
  out_list  <- df[!df_index]

  ## defaultName" - "newName"
  out_cols <- c(
    "country_name" = "name",
    "domain" = "topLevelDomain",
    "iso2c" = "alpha2Code",
    "iso3c" = "alpha3Code",
    "calling_codes" = "callingCodes",
    "alt_spelling" = "altSpellings",
    "sub_region" = "subregion",
    "native_name" = "nativeName",
    "regional_blocs" = "regionalBlocs",
    "numeric_code" = "numericCode"
  )

  if(length(out_list) == 0){

    warning("No data was returned for any requested country. Returning empty data frame")
    out_df <- as.data.frame(matrix(nrow = 0, ncol = length(out_cols)))
    names(out_df) <- names(out_cols)

  } else {

    out_df <- out_list %>%
      rename(
        all_of(out_cols)
      ) %>%
      hoist(latlng,
            lat = list(1),
            lon = list(2)
      ) %>%
      unnest(currencies, names_sep = "_")
  }
  out_df

}


rc_by_capital <- function(input){

  df <- restcountries_api(paste0("capital/", input))

  ## remove the errored returns
  df_index <- sapply(df, is.data.frame)
  out_list  <- df[!df_index]

  ## defaultName" - "newName"
  out_cols <- c(
    "country_name" = "name",
    "domain" = "topLevelDomain",
    "iso2c" = "alpha2Code",
    "iso3c" = "alpha3Code",
    "calling_codes" = "callingCodes",
    "alt_spelling" = "altSpellings",
    "sub_region" = "subregion",
    "native_name" = "nativeName",
    "regional_blocs" = "regionalBlocs",
    "numeric_code" = "numericCode"
  )

  if(length(out_list) == 0){

    warning("No data was returned for any requested country. Returning empty data frame")
    out_df <- as.data.frame(matrix(nrow = 0, ncol = length(out_cols)))
    names(out_df) <- names(out_cols)

  } else {

    out_df <- out_list %>%
      rename(
        all_of(out_cols)
      ) %>%
      hoist(latlng,
            lat = list(1),
            lon = list(2)
      )
  }
  out_df

}


rc_by_region <- function(input){

  df <- restcountries_api(paste0("region/", input))

  ## remove the errored returns
  df_index <- sapply(df, is.data.frame)
  out_list  <- df[!df_index]

  ## defaultName" - "newName"
  out_cols <- c(
    "country_name" = "name",
    "domain" = "topLevelDomain",
    "iso2c" = "alpha2Code",
    "iso3c" = "alpha3Code",
    "calling_codes" = "callingCodes",
    "alt_spelling" = "altSpellings",
    "sub_region" = "subregion",
    "native_name" = "nativeName",
    "regional_blocs" = "regionalBlocs",
    "numeric_code" = "numericCode"
  )

  if(length(out_list) == 0){

    warning("No data was returned for any requested country. Returning empty data frame")
    out_df <- as.data.frame(matrix(nrow = 0, ncol = length(out_cols)))
    names(out_df) <- names(out_cols)

  } else {

    out_df <- out_list %>%
      rename(
        all_of(out_cols)
      ) %>%
      hoist(latlng,
            lat = list(1),
            lon = list(2)
      )
  }
  out_df

}




#' Search by Calling Code
#'
#' Country codes are a component of the international telephone numbering plan, and are necessary only when
#' dialing a telephone number to establish a call to another country. Country codes are dialed before the national telephone number.
#' By convention, international telephone numbers are represented by prefixing the country code with a plus sign +,
#' which also indicates to the subscriber that the local international call prefix must first be dialed.
#'
#' @param code A country telephone number prefixes for reaching telephone subscribers in the network of the member countries or regions.
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

  df <- restcountries_api(paste0("callingcode/", code))

  ## remove the errored returns
  df_index <- sapply(df, is.data.frame)
  out_list  <- df[!df_index]

  ## defaultName" - "newName"
  out_cols <- c(
    "country_name" = "name",
    "domain" = "topLevelDomain",
    "iso2c" = "alpha2Code",
    "iso3c" = "alpha3Code",
    "calling_codes" = "callingCodes",
    "alt_spelling" = "altSpellings",
    "sub_region" = "subregion",
    "native_name" = "nativeName",
    "regional_blocs" = "regionalBlocs",
    "numeric_code" = "numericCode"
  )

  if(length(out_list) == 0){

    warning("No data was returned for any requested country. Returning empty data frame")
    out_df <- as.data.frame(matrix(nrow = 0, ncol = length(out_cols)))
    names(out_df) <- names(out_cols)

  } else {

    out_df <- out_list %>%
      rename(
        all_of(out_cols)
      ) %>%
      hoist(latlng,
            lat = list(1),
            lon = list(2)
      )
  }
  out_df
}

rc_by_blocs <- function(input){

  df <- restcountries_api(paste0("regionalbloc/", input))

  ## remove the errored returns
  df_index <- sapply(df, is.data.frame)
  out_list  <- df[!df_index]

  ## defaultName" - "newName"
  out_cols <- c(
    "country_name" = "name",
    "domain" = "topLevelDomain",
    "iso2c" = "alpha2Code",
    "iso3c" = "alpha3Code",
    "calling_codes" = "callingCodes",
    "alt_spelling" = "altSpellings",
    "sub_region" = "subregion",
    "native_name" = "nativeName",
    "regional_blocs" = "regionalBlocs",
    "numeric_code" = "numericCode"
  )

  if(length(out_list) == 0){

    warning("No data was returned for any requested country. Returning empty data frame")
    out_df <- as.data.frame(matrix(nrow = 0, ncol = length(out_cols)))
    names(out_df) <- names(out_cols)

  } else {

    out_df <- out_list %>%
      rename(
        all_of(out_cols)
      ) %>%
      hoist(latlng,
            lat = list(1),
            lon = list(2)
      )
  }
  out_df

}

rc_by_code <- function(input){

  df <- restcountries_api(paste0("alpha/", "co"))

  ## remove the errored returns
  df_index <- sapply(df, is.data.frame)
  out_list  <- df[!df_index]

  ## defaultName" - "newName"
  out_cols <- c(
    "country_name" = "name",
    "domain" = "topLevelDomain",
    "iso2c" = "alpha2Code",
    "iso3c" = "alpha3Code",
    "calling_codes" = "callingCodes",
    "alt_spelling" = "altSpellings",
    "sub_region" = "subregion",
    "native_name" = "nativeName",
    "regional_blocs" = "regionalBlocs",
    "numeric_code" = "numericCode"
  )

  if(length(out_list) == 0){

    warning("No data was returned for any requested country. Returning empty data frame")
    out_df <- as.data.frame(matrix(nrow = 0, ncol = length(out_cols)))
    names(out_df) <- names(out_cols)

  } else {

    out_df <- out_list %>%
      rename(
        all_of(out_cols)
      ) %>%
      hoist(latlng,
            lat = list(1),
            lon = list(2)
      )
  }

  out_df

}



