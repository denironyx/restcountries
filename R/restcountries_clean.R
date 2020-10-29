#' Title
#'
#' @param df
#'
#' @return
#' @importFrom dplyr '%>%' all_of rename select
#' @importFrom tidyr unnest hoist
#'
#' @examples
restcountries_clean <- function(df){

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

