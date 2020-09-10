# INITIALISE ====

# TODO: we generally shouldn't source like this in the package initalization
# conditionally setup environment
# if (sys.parent() == 0) try(source('R/setup_environment.R'), silent = T)
# source("R/restcountries_api.R")

get_all <- function(x){

  if(x == "all") {

    df <- restcountries_api(x)

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
            )
  }
  out_df
}

get_countries_by_lang <- function(x){

  if(x %in% c("en", "ng", "es", "fr")) {

    df <- restcountries_api(paste0("lang/", x))

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

get_countries_by_name <- function(x){

  df <- restcountries_api(paste0("name/", x, "?fullText=true"))

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
get_countries_by_currency <- function(input){

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


get_countries_by_capital <- function(input){

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


get_countries_by_region <- function(input){

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

get_countries_by_callingcode <- function(input){

  df <- restcountries_api(paste0("callingcode/", input))

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

get_countries_by_blocs <- function(input){

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

get_countries_by_code <- function(input){

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



