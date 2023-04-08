# restcountriesr
A simple wrapper of the rest countries restful API - http://restcountries.eu/

# Installing `restcountries`

The restcountries package can be downloaded from Github using the remotes package which allows easy installation of R packages from remote repositories such as Github. 
```
install.packages("remotes")
library(remotes)
remotes::install_github("denironyx/restcountries")
```

# Rest Countries endpoints
The restcountriesr datapackage can be filter by the following.

- Search by All
- Search by Name or Full Name
- Country's Code
- Country's Currency
- Search by Capital City
- Search by Calling Code
- Search by Region

```
library(restcountriesr)
library(dplyr)

df <- rc_all()

##List of columns
parameter <- c("country_name", "capital", "iso2c", "iso3c", "calling_codes", "lat", "lon", "region", "currencies", "population")

# The first 6 rows of the result queried
df %>% 
  select(all_of(parameter)) %>% 
  head()
```
