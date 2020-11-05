library(shiny)
library(flexdashboard) # devtools::install_github('rstudio/flexdashboard')

# ----------- NZ Energy Forecasting --------------#

library(data.table)
library(prophet)
library(plotly) # devtools::install_github('ropensci/plotly')
library(ggplot2) # devtools::install_github('hadley/ggplot2')



# pricing_features <- fread("../data/pricing_features.csv")
# pricing_actuals <- fread("../data/pricing_actuals_features.csv")
# pricing_futures <- fread("../data/pricing_future_features.csv")
# 
# m <- prophet()
# 
# for (f in colnames(pricing_features)[3:12]) {
#     m <- add_regressor(m, f)
# }
# 
# m <- fit.prophet(m, pricing_features)
# 
# forecast <- predict(m, future)
# 
# g <- plot(m, forecast)


library(shinySignals)
library(dplyr)
library(bubbles)


# ---------------- KPI CARD --------------------



# An empty prototype of the data frame we want to create
prototype <- data.frame(date = character(), time = character(),
                        size = numeric(), r_version = character(), r_arch = character(),
                        r_os = character(), package = character(), version = character(),
                        country = character(), ip_id = character(), received = numeric())

# Connects to streaming log data for cran.rstudio.com and
# returns a reactive expression that serves up the cumulative
# results as a data frame
packageStream <- function(session = getDefaultReactiveDomain()) {
    # Connect to data source
    sock <- socketConnection("cransim.rstudio.com", 6789, blocking = FALSE, open = "r")
    # Clean up when session is over
    session$onSessionEnded(function() {
        close(sock)
    })
    
    # Returns new lines
    newLines <- reactive({
        invalidateLater(1000, session)
        readLines(sock)
    })
    
    # Parses newLines() into data frame
    reactive({
        if (length(newLines()) == 0)
            return()
        read.csv(textConnection(newLines()), header=FALSE, stringsAsFactors=FALSE,
                 col.names = names(prototype)
        ) %>% mutate(received = as.numeric(Sys.time()))
    })
}

# Accumulates pkgStream rows over time; throws out any older than timeWindow
# (assuming the presence of a "received" field)
packageData <- function(pkgStream, timeWindow) {
    shinySignals::reducePast(pkgStream, function(memo, value) {
        rbind(memo, value) %>%
            filter(received > as.numeric(Sys.time()) - timeWindow)
    }, prototype)
}

# Count the total nrows of pkgStream
downloadCount <- function(pkgStream) {
    shinySignals::reducePast(pkgStream, function(memo, df) {
        if (is.null(df))
            return(memo)
        memo + nrow(df)
    }, 0)
}

# Use a bloom filter to probabilistically track the number of unique
# users we have seen; using bloom filter means we will not have a
# perfectly accurate count, but the memory usage will be bounded.
userCount <- function(pkgStream) {
    # These parameters estimate that with 5000 unique users added to
    # the filter, we'll have a 1% chance of false positive on the next
    # user to be queried.
    bloomFilter <- BloomFilter$new(5000, 0.01)
    total <- 0
    reactive({
        df <- pkgStream()
        if (!is.null(df) && nrow(df) > 0) {
            # ip_id is only unique on a per-day basis. To make them unique
            # across days, include the date. And call unique() to make sure
            # we don't double-count dupes in the current data frame.
            ids <- paste(df$date, df$ip_id) %>% unique()
            # Get indices of IDs we haven't seen before
            newIds <- !sapply(ids, bloomFilter$has)
            # Add the count of new IDs
            total <<- total + length(newIds)
            # Add the new IDs so we know for next time
            sapply(ids[newIds], bloomFilter$set)
        }
        total
    })
}

# Quick and dirty bloom filter. The hashing "functions" are based on choosing
# random sets of bytes out of a single MD5 hash. Seems to work well for normal
# values, but has not been extensively tested for weird situations like very
# small n or very large p.

library(digest)
library(bit)

BloomFilter <- setRefClass("BloomFilter",
                           fields = list(
                               .m = "integer",
                               .bits = "ANY",
                               .k = "integer",
                               .bytesNeeded = "integer",
                               .bytesToTake = "matrix"
                           ),
                           methods = list(
                               # @param n - Set size
                               # @param p - Desired false positive probability (e.g. 0.01 for 1%)
                               initialize = function(n = 10000, p = 0.001) {
                                   m = (as.numeric(n) * log(1 / p)) / (log(2)^2)
                                   
                                   .m <<- as.integer(m)
                                   .bits <<- bit(.m)
                                   .k <<- max(1L, as.integer(round((as.numeric(.m)/n) * log(2))))
                                   
                                   # This is how many *bytes* of data we need for *each* of the k indices we need to
                                   # generate
                                   .bytesNeeded <<- as.integer(ceiling(log2(.m) / 8))
                                   .bytesToTake <<- sapply(rep_len(.bytesNeeded, .k), function(byteCount) {
                                       # 16 is number of bytes an md5 hash has
                                       sample.int(16, byteCount, replace = FALSE)
                                   })
                               },
                               .hash = function(x) {
                                   hash <- digest(x, "md5", serialize = FALSE, raw = TRUE)
                                   sapply(1:.k, function(i) {
                                       val <- rawToInt(hash[.bytesToTake[,i]])
                                       # Scale down to fit into the desired range
                                       as.integer(val * (as.numeric(.m) / 2^(.bytesNeeded*8)))
                                   })
                               },
                               has = function(x) {
                                   all(.bits[.hash(x)])
                               },
                               set = function(x) {
                                   .bits[.hash(x)] <<- TRUE
                               }
                           )
)

rawToInt <- function(bytes) {
    Reduce(function(left, right) {
        bitwShiftL(left, 8) + right
    }, as.integer(bytes), 0L)
}


#--------------- CHARTS ----------------------#

library(leaflet) # devtools::install_github('rstudio/leaflet')
library(highcharter) # devtools::install_github('jbkunst/highcharter')
library(sp)
library(dplyr)
library(rgeos)
library(mapproj)
library(maptools)
library(readr)
library(ggthemes)


# Define the list of available metros
lookup <- structure(c(12060L, 12420L, 12580L, 13820L, 14460L, 15380L, 16740L, 
                      16980L, 17140L, 17460L, 18140L, 19100L, 19740L, 19820L, 25540L, 
                      26420L, 26900L, 27260L, 28140L, 29820L, 31100L, 31140L, 32820L, 
                      33100L, 33340L, 33460L, 34980L, 35620L, 36420L, 36740L, 37980L, 
                      38060L, 38300L, 38900L, 39300L, 40060L, 40380L, 40900L, 
                      41180L, 41620L, 41700L, 41740L, 41860L, 42660L, 45300L,  
                      47900L), .Names = c("Atlanta", "Austin", "Baltimore", "Birmingham", 
                                          "Boston", "Buffalo", "Charlotte", "Chicago", "Cincinnati", "Cleveland", 
                                          "Columbus", "Dallas-Fort Worth", "Denver", "Detroit", "Hartford", "Houston", 
                                          "Indianapolis", "Jacksonville", "Kansas City", "Las Vegas", "Los Angeles", 
                                          "Louisville", "Memphis", "Miami", "Milwaukee", "Minneapolis-St. Paul", 
                                          "Nashville", "New York", "Oklahoma City", "Orlando", "Philadelphia", 
                                          "Phoenix", "Pittsburgh", "Portland", "Providence", 
                                          "Richmond", "Rochester", "Sacramento", "St. Louis", "Salt Lake City", 
                                          "San Antonio", "San Diego", "San Francisco-Oakland", "Seattle", "Tampa-St. Petersburg", 
                                          "Washington"))

# Read in data, and subset for the selected metro
full_tracts <- readRDS('../data/full_simp2.rds')
metro <- reactive({
    
    m <- full_tracts[full_tracts$metroid == input$metro_name, ]
    m$Distance <- m$distmiles
    
    m$Score <- m$entropy
    
    return(m)
    
})
# Generate data for the second tab
full_compare <- readRDS('../data/comparative.rds')
compare_metro <- reactive({
    
    out <- full_compare %>%
        filter(metroid == input$metro_name) %>%
        mutate(Distance = distance, Year = as.factor(year), Score = entropy)
    
    return(out)
    
})