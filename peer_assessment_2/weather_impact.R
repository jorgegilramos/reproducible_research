library(R.utils)  # bunzip2
library(plyr)     # aggregate
library(reshape2) # melt function for plotting
library(ggplot2)  # plotting
library(knitr)    # Rmd file to html

# Turn off scientific notations for numbers
options(scipen = 1)

# Set working directory
setwd("/home/user/reproducible_research/peer_assessment_2")

# Download data
rdata_file <- "./data/StormData.RData"

create_rdata_file <- TRUE
# Check if file StormData.RData already exists
if (file.exists(rdata_file)) {
  load(rdata_file)
  create_rdata_file <- FALSE
}

if (create_rdata_file) {
  # Create a data dir if it doesn't exist
  if (!file.exists("./data")) {
    dir.create("./data")
  }
  
  # Download file if it is not in the directory 
  url <- "https://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2"
  file_name <- "./data/StormData.csv"
  bz2_file <- "./data/StormData.csv.bz2"
  if (!file.exists(bz2_file)) {
    cat("Downloading", bz2_file, "at", getwd(), "...\n")
    download.file(url, bz2_file, method = "curl")
  }
  
  # Unzip bz2 file to extract csv
  if (!file.exists(file_name)){
    bunzip2(bz2_file, file_name, overwrite = TRUE, remove = FALSE)
  }

  # Load data and store them in stormdata data frame
  df_stormdata <- read.csv(file_name, header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE)
  dim(df_stormdata)
  head(df_stormdata)
  
  # Format date and extract year column
  df_stormdata$BGN_DATE <- as.POSIXct(df_stormdata$BGN_DATE, format = "%m/%d/%Y %H:%M:%S")
  df_stormdata$year <- as.numeric(format(df_stormdata$BGN_DATE, "%Y"))
  
  # Histogram of data
  ggplot(df_stormdata, aes(x = year)) +
    geom_histogram(breaks = seq(1950, 2015, by = 5), colour = "#a6bddb", fill = "#2b8cbe") +
    geom_vline(aes(xintercept = 1995), color = "red", linetype = "dashed", size = 1) +
    xlab("5-Year Terms") +
    ylab("Number of Events") +
    ggtitle("Registered NOAA Weather Events in US (1950-2011)")
  
  # Subset variables related to health and economic analysis
  storm <- df_stormdata[df_stormdata$year >= 1995, 
                        c("year", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG",
                          "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
  # Free memory
  #rm(df_stormdata)

  dim(storm)
  
  # Lookup exponents
  unique(append(unique(storm$PROPDMGEXP), unique(storm$CROPDMGEXP)))
  
  # Apply to a number a power of 10
  apply_power_to_number <- function(num, exp) {
    pow <- 0
    if (is.numeric(exp)) {
      pow <- exp
    } else { 
      if (exp == "") {
        pow <- 0
      } else {
        exp <- toupper(exp)
        if (exp == "H") pow <- 2
        else if (exp == "K") pow <- 3
        else if (exp == "M") pow <- 6
        else if (exp == "B") pow <- 9
      }
    }
    # Return result
    result <- num * 10^pow
  }
  
  # Create calculated property and crop damages
  storm$property <- mapply(apply_power_to_number, storm$PROPDMG, storm$PROPDMGEXP)
  storm$crop <- mapply(apply_power_to_number, storm$CROPDMG, storm$CROPDMGEXP)
  
  # Aggregate personal and economic damage per damage_type
  summarydata <- ddply(storm, .(EVTYPE), summarize,
                       property = sum(property),
                       crop = sum(crop),
                       economic_damage = sum(property) + sum(crop),
                       injuries= sum(INJURIES),
                       fatalities = sum(FATALITIES),
                       personal_damage = sum(INJURIES) + sum(FATALITIES))

  # Save RData file
  save(df_stormdata, storm, summarydata, file = rdata_file)
}

# Personal dataframe
df_personal_damage = head(summarydata[order(summarydata$personal_damage, decreasing = TRUE), 
                                      c("EVTYPE", "injuries", "fatalities")], 10)
df_personal_damage <- melt(df_personal_damage, id.vars = "EVTYPE", 
                           measures.vars = c("injuries", "fatalities"), 
                           variable.name = "damage_type",
                           value.name = "personal_damage")

# Plot personal damage per event type
ggplot(df_personal_damage, aes(x = EVTYPE, y = personal_damage / 10^6, fill = damage_type)) + 
  geom_bar(stat = "identity", aes(x = reorder(EVTYPE, -personal_damage))) + 
  scale_fill_manual(values = c("#a6bddb", "#2b8cbe")) +
  xlab("Event Type") +
  theme(axis.text.x = element_text(angle = 30, size = 10, hjust = 1, vjust = 1)) +
  ylab("Total Damage (millions of USD)") +
  ggtitle("Top 10 most dangerous Weather Events in US (1995-2011)")


# Economic dataframe
df_economic_damage = head(summarydata[order(summarydata$economic_damage, decreasing = TRUE), 
                                      c("EVTYPE", "property", "crop")], 10)
df_economic_damage <- melt(df_economic_damage, id.vars = "EVTYPE", 
                           measures.vars = c("property", "crop"), 
                           variable.name = "damage_type",
                           value.name = "economic_damage")

# Plot economic damage per event type
ggplot(df_economic_damage, aes(x = EVTYPE, y = economic_damage / 10^6, fill = damage_type)) + 
  geom_bar(stat = "identity", aes(x = reorder(EVTYPE, -economic_damage))) + 
  scale_fill_manual(values = c("#a6bddb", "#2b8cbe")) +
  xlab("Event Type") +
  theme(axis.text.x = element_text(angle = 30, size = 10, hjust = 1, vjust = 1)) +
  ylab("Total Damage (millions of USD)") +
  ggtitle("Top 10 most economic harmful Weather Events in US (1995-2011)")

