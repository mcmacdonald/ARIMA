


# construct denominators to calculate homicide rates from census data 

# ISTAT page to download age-specific population data from 1971 - 2011:
# http://dati-censimentopopolazione.istat.it/Index.aspx?lang=en#

# ISTAT page to download age-specific population data from 2012-2019
# https://esploradati.istat.it/databrowser/#/en/dw/dashboards
setwd("~/Desktop/Projects/apc_mafia/ita_census/region/") # set working directory
census_1982.1991 <- read.csv("ita_1982-1991.csv")
census_1992.2001 <- read.csv("ita_1992-2001.csv")
# don't run... these files don't contain all necessary age groups
# census_2002.2011 <- read.csv("ita_2002-2011.csv")
# census_2012.2019 <- read.csv("ita_2012-2019.csv")
census_2002.2019_calabria <- read.csv("ita_2002-2019_calabria.csv")
census_2002.2019_campania <- read.csv("ita_2002-2019_campania.csv")
census_2002.2019_lazio    <- read.csv("ita_2002-2019_lazio.csv")
census_2002.2019_lombardia<- read.csv("ita_2002-2019_lombardia.csv")
census_2002.2019_puglia   <- read.csv("ita_2002-2019_puglia.csv")
census_2002.2019_sicilia  <- read.csv("ita_2002-2019_sicilia.csv")
# add region to dataframe
census_2002.2019_calabria <- dplyr::mutate(census_2002.2019_calabria, region = "CALABRIA")
census_2002.2019_campania <- dplyr::mutate(census_2002.2019_campania, region = "CAMPANIA")
census_2002.2019_lazio    <- dplyr::mutate(census_2002.2019_lazio, region = "LAZIO")
census_2002.2019_lombardia<- dplyr::mutate(census_2002.2019_lombardia, region = "LOMBARDIA")
census_2002.2019_puglia   <- dplyr::mutate(census_2002.2019_puglia, region = "PUGLIA")
census_2002.2019_sicilia  <- dplyr::mutate(census_2002.2019_sicilia, region = "SICILIA")
# join
census_2002.2019 <- rbind(
  census_2002.2019_campania,
  census_2002.2019_calabria,
  census_2002.2019_lazio,
  census_2002.2019_lombardia,
  census_2002.2019_puglia,
  census_2002.2019_sicilia
  )
rm( # drop from workspace
  census_2002.2019_campania,
  census_2002.2019_calabria,
  census_2002.2019_lazio,
  census_2002.2019_lombardia,
  census_2002.2019_puglia,
  census_2002.2019_sicilia
  )

# drop columns
census_1982.1991 <- dplyr::select(census_1982.1991, Territory, ETA1, Gender, TIME, Value)
census_1992.2001 <- dplyr::select(census_1992.2001, Territory, ETA1, Gender, TIME, Value)
census_2002.2019 <- dplyr::select(census_2002.2019, region, AGE, TIME_PERIOD, OBS_VALUE)

# filter data by sex/gender
census_1982.1991 <- dplyr::filter(census_1982.1991, Gender == "males") # almost all of the victims are men
census_1982.1991 <- dplyr::select(census_1982.1991, -Gender) # drop
census_1992.2001 <- dplyr::filter(census_1992.2001, Gender == "males") # almost all of the victims are men
census_1992.2001 <- dplyr::select(census_1992.2001, -Gender) # drop

# relable columns
label <- function(x){
  colnames(x) <- c("region", "eta", "anno", "pop") # column names
  x <- dplyr::select(x, region, anno, eta, pop) # reshuffle columns
  return(x) # return
}
census_1982.1991 <- label(census_1982.1991)
census_1992.2001 <- label(census_1992.2001)
census_2002.2019 <- label(census_2002.2019)

# join
census <- rbind(
  census_1982.1991,
  census_1992.2001,
  census_2002.2019
)
rm( # remove from workspace
  census_1982.1991,
  census_1992.2001,
  census_2002.2019
)

# filter data by year
census <- dplyr::filter(census, anno <= 2017)

# filter out total population
census <- dplyr::filter(census, eta != "TOTAL")

# transform years of age to numeric
census$eta <- stringr::str_replace_all(census$eta, "Y_GE", "")
census$eta <- stringr::str_replace_all(census$eta, "Y", "")
census$eta <- as.numeric(census$eta)

# sort the dataset
census <- census[with(census, order(region, anno, eta)),]

# filter census data by region
census$region <- toupper(census$region) # region names to upper case
census <- dplyr::filter(
  census, 
  region == "CAMPANIA" |
    region == "CALABRIA" | 
    region == "PUGLIA"   |
    region == "SICILIA"
    )


# drop anyone younger than 17 years of age
census <- dplyr::filter(census, eta >= 17)

# average life expectancy in Italy, by year # source: https://data.worldbank.org/indicator/SP.DYN.LE00.IN?locations=IT
ale <- data.frame( 
  anno = c(
    1982,
    1983,
    1984,
    1985,
    1986,
    1987,
    1988,
    1989,
    1990,
    1991,
    1992,
    1993,
    1994,
    1995,
    1996,
    1997,
    1998,
    1999,
    2000,
    2001,
    2002,
    2003,
    2004,
    2005,
    2006,
    2007,
    2008,
    2009,
    2010,
    2011,
    2012,
    2013,
    2014,
    2015,
    2016,
    2017
    ),
  ale = c(
    74.61,
    74.88,
    75.18,
    75.48,
    75.78,
    76.08,
    76.38,
    76.60,
    76.82,
    77.03,
    77.25,
    77.47,
    77.74,
    78.00,
    78.27,
    78.53,
    78.80,
    79.10,
    79.40,
    79.69,  
    79.99,
    80.29,
    80.53,
    80.78,
    81.02,
    81.27,
    81.49,
    81.64,
    82.04,
    82.19,
    82.24,
    82.69,
    83.09,
    82.54,
    83.24,
    82.95
    )
)

# join
census <- merge(census, ale, by = "anno"); rm(ale)

# round average life expectancy to nearest whole number
census$ale <- round(census$ale, digits = 0)

# filter by average life expectancy
census <- dplyr::filter(census, eta <= ale)

# collapse by region
`%>%` <- magrittr::`%>%` # call pipe locally
census <- census %>% 
  dplyr::select(anno, region, pop) %>%
  dplyr::group_by(anno, region) %>%
  dplyr::summarise(pop = sum(pop))


