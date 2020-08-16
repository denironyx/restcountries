
get_all <- function(country = "all"){

  if("all" %in% country) {
    restcountries_api(country)
  } else {
    stop("country parameter has no valid values. Please check documentation for valid inputs")
  }

  ## remove the errored returns
  df_index <- sapply(return_df, is.data.frame)
  out_list  <- return_df[df_index]

  ## defaultName" - "newName"
  out_cols <- c(
    "name" = "country_name",
    "topLevelDomain" = "domain",
    "alpha2Code" = "iso2c",
    "alpha3Code" = "iso3c",
    "callingCodes" = "phone_code",
    "capital" = "capital",
    "altSpellings" = "alt_spelling",
    "region" = "region",
    "subregion" = "sub_region",
    "population" = "population",
    "latlng" = "lat_lon",
    "demonym" = "demonym",
    "area"  = "area",
    "gini" = "gini",
    "timezones" = "timezones",
    "borders" = "borders",
    "nativeName" = "native_name",
    "numericCode" = "numeric_code",
    "currencies" = "currencies",
    "languages" = "languages",
    "flag" = "flags",
    "regionalBlocs" = "regional_blocs",
    "cioc" = "cioc"
  )

  if(length(out_list) == 0){

    warning("No data was returned for any requested country. Returning empty data frame")
    out_df <- as.data.frame(matrix(nrow = 0, ncol = length(out_cols)))
    names(out_df) <- names(out_cols)

  } else {

    out_df <- row.names(do.call('rbind', out_list))
    row.names(out_df) <- NULL

  }
}

output <- get_all("country")

