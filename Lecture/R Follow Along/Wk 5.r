# Load packages
library(data.table)
library(dtplyr)
library(dplyr)
library(ggplot2)

## Loading data
# Where are we getting the data from
met_url <- "https://github.com/USCbiostats/data-science-data/raw/master/02_met/met_all.gz"

# Downloading the data to a tempfile (so it is destroyed afterwards)
# you can replace this with, for example, your own data:
tmp <- tempfile(pattern = "met", fileext = ".gz")
# tmp <- "met.gz"

# We should be downloading this, ONLY IF this was not downloaded already.
# otherwise is just a waste of time.
if (!file.exists(tmp)) {
  download.file(
    url      = met_url,
    destfile = tmp,
    # method   = "libcurl", timeout = 1000 (you may need this option)
  )
}

# Reading the data
dat <- read.csv(tmp)
head(dat)

# Selecting columns
dat[, c('USAFID', 'lat', 'lon')]

# select only the relevant variables
dat <- dat |> 
  select(USAFID, WBAN, year, month, day, 
         hour, min, lat, lon, elev, 
         wind.sp, temp, atm.press)

# Q1: How many ways can you write an XOR operator
myxor <- function(x, y) {
  res <- logical(length(x))
  for (i in 1:length(x)) {
    res[i] <- # do something with x[i] and y[i]
  }
  return(res)
}

## Or if vectorized
myxor <- function(x, y) {
  # INSERT YOUR CODE HERE
}

# Result
myxor1 <- function(x,y) {(x & !y) | (!x & y)}
myxor2 <- function(x,y) {!((!x | y) & (x | !y))}
myxor3 <- function(x,y) {(x | y) & (!x | !y)}
myxor4 <- function(x,y) {!((!x & !y) | (x & y))}
cbind(
  ifelse(xor(test[,1], test[,2]), "true", "false"),
  ifelse(myxor1(test[,1], test[,2]), "true", "false"),
  ifelse(myxor2(test[,1], test[,2]), "true", "false"),
  ifelse(myxor3(test[,1], test[,2]), "true", "false"),
  ifelse(myxor4(test[,1], test[,2]), "true", "false")
)


# Filtering (subsetting) data
## In base R
dat[dat$day == 1 &
      dat$lat > 40 &
      (dat$elev < 500 | dat$elev > 1000), ]

## W/ dplyr
dat |>
  filter(day == 1, lat > 40, (elev < 500) | (elev > 1000)) |>
  collect() |> # Notice this line!
  nrow() 

# Q2.1: Records w/ temp 18-25
nrow(dat[dat$temp < 25 & dat$temp > 18, ])
# dat[temp %between% c(18, 25), .N] 
# dat |> filter(between(temp, 18, 25)) |> collect() |> nrow()

# Q2.2: Missing values
nrow(dat[is.na(dat$temp), ])
# more succinct: sum(is.na(dat$temp))

# Q2.3: Plot sample of 1k pairs (lat, long)
## Drawing a sample
set.seed(123)
idx1 <- sample(which(is.na(dat$temp)), 1000)
idx2 <- sample(which(!is.na(dat$temp)), 1000)

## Visualizing the data
### make a map of the US, as we did last class
ggplot(map_data("state"), aes(x = long, y = lat)) +
  geom_map(aes(map_id = region), map = map_data("state"), col = "lightgrey", fill = "gray") +
  geom_jitter(
    data    = dat[c(idx1, idx2), ],
    mapping = aes(x = lon, y = lat, col = is.na(temp)),
    inherit.aes = FALSE, alpha = .5, cex = 2
  )














