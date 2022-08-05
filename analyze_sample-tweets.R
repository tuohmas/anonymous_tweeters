# Tuomas Heikkilä
# tuomas.k.heikkila@helsinki.fi
# University of Helsinki
# 
# Version 1.3: 2022-08-01
# 
# Sample tweets from full tweet archive based on a datetime, cluster and
# accounts pooled average eigencentrality value across Retweet and Mentions
# Networks. Calculates and applies a stratified sampling scheme reprentative 
# to a cluster's proportion of COVID-19 tweet counts per time interval.

# PREPARATIONS #################################################################
# clean up environment
rm(list = ls())

# Digit display options: penalize scientific notation
options(digits = 22, scipen = 999)

# TEMP
setwd("./R/anonymous_tweeters")
getwd()

# Load packages to memory
pacman::p_load(tidyverse,
               lubridate,
               academictwitteR,
               checkpoint,
               webshot2,
               devtools)

# devtools::install_github("gadenbuie/tweetrmd")
library(tweetrmd)

# Use packages as they existed on the date of creation
create_checkpoint("2022-07-05")

# Load pseudoanonymous account descriptive statistics
fname_names <- "active-accounts-profile-statistics"
suffix_names <- 1

while (
  file.exists(paste0(fname_names, "_v", sprintf("%02d", suffix_names), ".csv"))
) { suffix_names <- suffix_names + 1 }

# When loop breaks, subtract one from the suffix (latest version)
suffix_names <- suffix_names -1
print(suffix_names)

names <- 
  read.csv(paste0(fname_names, "_v", sprintf("%02d", suffix_names), ".csv"),
           sep = ";", header = TRUE, numerals = "no.loss")

glimpse(names)

# HEADING FIX ME ###############################################################

# Orange clusters: retweet network modularity class 3, mentions mod.class 7 
orange.cluster <- names %>% 
  filter(retweets_mod_class == 3 & mentions_mod_class == 7) %>% 
  mutate(avg.eigencentrality = (retweets_eigencentrality + 
                                  mentions_eigencentrality) / 2) %>% 
  arrange(desc(avg.eigencentrality))

orange.cluster.psanon <- orange.cluster %>% filter(group == "pseudoanon")
orange.cluster.psanon

# Cyan clusters: retweet network modularity class 0, mentions mod.class 3 
cyan.cluster <- names %>% 
  filter(retweets_mod_class == 0 & mentions_mod_class == 3) %>% 
  mutate(avg.eigencentrality = (retweets_eigencentrality + 
                                  mentions_eigencentrality) / 2) %>% 
  arrange(desc(avg.eigencentrality))

cyan.cluster.psanon <- cyan.cluster %>% filter(group == "pseudoanon")
cyan.cluster.psanon

# Magenta clusters: retweet network modularity class 4, mentions mod.class 1 
magenta.cluster <- names %>% 
  filter(retweets_mod_class == 4 & mentions_mod_class == 1) %>% 
  mutate(avg.eigencentrality = (retweets_eigencentrality + 
                                  mentions_eigencentrality) / 2) %>% 
  arrange(desc(avg.eigencentrality))

magenta.cluster.psanon <- magenta.cluster %>% filter(group == "pseudoanon")
magenta.cluster.psanon

# Combine clusters into a list object
clusters <- list(orange.cluster.psanon,
                 cyan.cluster.psanon,
                 magenta.cluster.psanon)

# TIME INTERVALS ###############################################################

# From Mon 9 Mar 2020 until Sun 24 May 2020 (weeks 11 through 21/2020)
first.interval <- c("2020-03-09", "2020-05-24") %>% as_date()

# From Mon 1 Mar 2021 until 2 Apr 2021 (weeks 9 through 17/2021)
second.interval <- c("2021-03-01", "2021-04-02") %>% as_date()

# From Mon 26 Aug 2021 until 31 Oct 2021 (weeks 30 through 43/2021)
third.interval <- c("2021-08-26", "2021-10-31") %>% as_date()

# Combine intervals into a list object
intervals <- list(first.interval, second.interval, third.interval)

# SAMPLING SCHEME ##############################################################

# Read Psanon Covid-19 tweet counts
tweet_counts_cases <- 
  read.csv("analysis-psanon-covid-twcnts-cases_v02.csv",
           sep = ";", header = TRUE, numerals = "no.loss") %>% 
  mutate(date = as_date(date))

# Glimpse tweet counts
glimpse(tweet_counts_cases)

# Add variable for cluster colours
attach(tweet_counts_cases)

tweet_counts_cases$cluster <- NA

tweet_counts_cases$cluster <- 
  ifelse(retweets_mod_class == 3 | mentions_mod_class == 7, "orange",
         tweet_counts_cases$cluster)

tweet_counts_cases$cluster <- 
  ifelse(retweets_mod_class == 0 | mentions_mod_class == 3, "cyan",
         tweet_counts_cases$cluster)

tweet_counts_cases$cluster <- 
  ifelse(retweets_mod_class == 4 | mentions_mod_class == 1, "magenta",
         tweet_counts_cases$cluster)

tweet_counts_cases$cluster <- 
  ifelse(retweets_mod_class == 2 | mentions_mod_class == 4, "grey",
         tweet_counts_cases$cluster)

tweet_counts_cases$cluster <- 
  ifelse(retweets_mod_class == "#korona", "#korona",
         tweet_counts_cases$cluster)

detach(tweet_counts_cases)

# Glimpse tweet counts
glimpse(tweet_counts_cases)

tweet_counts_cases$cluster %>% table()

# Record main clusters as a vector
colours <- c("orange", "cyan", "magenta")

# Build sampling scheme using stratification sampling method (sampling 
# represents data's distribution, in this case covid-19 tweet counts)
 
sampling.scheme <- as.data.frame(matrix(ncol = 0, nrow = 3))

for (interval in 1:length(intervals)) {

  values <- tweet_counts_cases %>%
    filter(cluster %in% colours & 
             between(date, intervals[[interval]][1], 
                     intervals[[interval]][2])) %>% 
    group_by(cluster) %>%
    summarize(col = n()) %>% as.data.frame()
  
  colnames(values) <- c("cluster",interval)
  
  sampling.scheme <- cbind(sampling.scheme, values)
}

# Name rows with clusters and drop clusters variable
rownames(sampling.scheme) <- sampling.scheme[,"cluster"]
sampling.scheme <- select(sampling.scheme, -cluster)

# Turn frequency table into a prop table (transforming into matrix and back)
sampling.scheme <- sampling.scheme %>% 
  as.matrix() %>% 
  prop.table(margin = 1) %>% # Divide each individual value by the row sums
  as.data.frame() %>% 
  mutate(total = rowSums(.)) # Add row sums

# Multiply values by a desired number and round to the nearest integer
sample.size <- 15 # with 10 consequtive tweeets OR 30 with 5 consequtive tweets
consequtive.tweets <- 10 # OR 5

sampling.scheme <- sampling.scheme * sample.size
sampling.scheme <- round(sampling.scheme, digits = 0)

# Print sampling scheme
sampling.scheme

# Empty dataframe for tweets using custom output format function 
# ("simplify_tweet") which separates metadata into 35 variables + 3 additional
# varibales (...)
sample.tidy <- as.data.frame(matrix(ncol = 44, nrow = 0))
samples.tidy <- list(sample.tidy, sample.tidy, sample.tidy)
  
# Import simplify_tweets function from simplified-tweets script
source("func_simplify-tweets.R")

# Iterate through clusters (change between output formats "tidy" and "raw")
# WARNING: The code is currently extremely time intensive.
for (cluster in 1:length(clusters)) { # Loop through the main clusters
  
  # Loop through the 10 most influential accounts in a cluster
  for (account in 1:10) {
    
    tweets <- bind_tweets(paste0("./Twitter API data/",
                          clusters[[cluster]][account,"screen_name"],
                          "-all-tweets"), output_format = "raw")
    
    # Apply custom function to "raw" format
    tweets <- simplify_tweets(tweets)
    
    glimpse(tweets)
    
    tweets$created_at_ymd_hms <- tweets$created_at %>% ymd_hms(tz="EET")
    
    for (interval in 1:length(intervals)) { # Loop through the timeintervals
      
      # Create sequence of dates spanning intervals
      set.seed(10 * cluster + account)
      interval.random <- seq(as.POSIXct(intervals[[interval]][1]),
                             as.POSIXct(intervals[[interval]][2]),
                             by = "1 mins") %>%
        
        # Sample N random timestamps from the first interval period determined 
        # by the strat.scheme dataframe
        sample(size = sampling.scheme[colours[cluster],interval], replace=T) %>% 
        sort()
      
      # Loop through the random date-times within intervals
      for (time in 1:sampling.scheme[colours[cluster],interval]) {  
        
        # Keep the first N consecutive tweets published after sample date-time
        sample.tweets <- tweets %>% 
          filter(created_at_ymd_hms >= interval.random[time]) %>% 
          arrange(created_at)
        
        sample.tweets <- sample.tweets[1:consequtive.tweets,] %>% 
          as.data.frame() %>% mutate(timeperiod = time,
                                     cluster = colours[cluster])
        
        glimpse(sample.tweets)
        
        # Bind sample tweets
        samples.tidy[[cluster]] <- 
          rbind(samples.tidy[[cluster]], sample.tweets)
      }
    }
  }
}


# Tidy formatting (replace NA amongs <NULL> in list variables)
for (i in 1:length(samples.tidy)) {

  samples.tidy[[i]]$url_expanded_url <- 
    ifelse(is.na(samples.tidy[[i]]$url_expanded_url) |
                   grepl("\\bNA\\b", samples.tidy[[i]]$url_expanded_url), 
           vector(mode = "list", length = 1),
           samples.tidy[[i]]$url_expanded_url)
  
  samples.tidy[[i]]$url_images <- 
    ifelse(is.na(samples.tidy[[i]]$url_images) |
             grepl("\\bNA\\b", samples.tidy[[i]]$url_images), 
           vector(mode = "list", length = 1),
           samples.tidy[[i]]$url_images)
  
  samples.tidy[[i]]$url_title <- 
    ifelse(is.na(samples.tidy[[i]]$url_title) | 
             grepl("\\bNA\\b", samples.tidy[[i]]$url_title) | 
             is.na(samples.tidy[[i]]$url_title), 
           vector(mode = "list", length = 1),
           samples.tidy[[i]]$url_title)
  
  samples.tidy[[i]]$url_description <- 
    ifelse(is.na(samples.tidy[[i]]$url_description) | 
             grepl("\\bNA\\b", samples.tidy[[i]]$url_description), 
           vector(mode = "list", length = 1),
           samples.tidy[[i]]$url_description)
  
  samples.tidy[[i]]$referenced_url_title <- 
    ifelse(is.na(samples.tidy[[i]]$referenced_url_title) | 
             grepl("\\bNA\\b", samples.tidy[[i]]$referenced_url_title),
           vector(mode = "list", length = 1),
           samples.tidy[[i]]$referenced_url_title)
  
  samples.tidy[[i]]$referenced_url_description <- 
    ifelse(is.na(samples.tidy[[i]]$referenced_url_description) | 
             grepl("\\bNA\\b", samples.tidy[[i]]$referenced_url_description), 
           vector(mode = "list", length = 1),
           samples.tidy[[i]]$referenced_url_description)
  
  samples.tidy[[i]]$referenced_url_images <- 
    ifelse(is.na(samples.tidy[[i]]$referenced_url_images) | 
             grepl("\\bNA\\b", samples.tidy[[i]]$referenced_url_images), 
           vector(mode = "list", length = 1),
           samples.tidy[[i]]$referenced_url_images)
}

# Partition samples tidy list
orange.samples.tidy <- samples.tidy[[1]]
glimpse(orange.samples.tidy)

cyan.samples.tidy <- samples.tidy[[2]]
glimpse(cyan.samples.tidy)

magenta.samples.tidy <- samples.tidy[[3]]
glimpse(magenta.samples.tidy)

# Flatten lists and write data to csv files
orange.samples.tidy %>%
  rowwise() %>%
  mutate_if(is.list, ~paste(unlist(., recursive = TRUE, 
                                   use.names = TRUE), collapse = '¤')) %>%
  write_delim("psanon-orange-sample-tidy_v01.csv", delim = ";")

cyan.samples.tidy %>%
  rowwise() %>%
  mutate_if(is.list, ~paste(unlist(., recursive = TRUE, 
                                   use.names = TRUE), collapse = '¤')) %>%
  write_delim("psanon-cyan-sample-tidy_v01.csv", delim = ";")

magenta.samples.tidy %>%
  rowwise() %>%
  mutate_if(is.list, ~paste(unlist(., recursive = TRUE, 
                                   use.names = TRUE), collapse = '¤')) %>%
  write_delim("psanon-magenta-sample-tidy_v01.csv", delim = ";")

# Check results
orange.samples <- 
  read_delim("psanon-orange-sample-tidy_v01.csv", delim = ";",
             show_col_types = TRUE, lazy = FALSE,
             # stringify id intergers 
             col_types = "ccTccccccciiiicccccccTiiiiccccccTllccccccTic") %>% 
  as.data.frame()

magenta.samples <- 
  read_delim("psanon-magenta-sample-tidy_v01.csv", delim = ";",
             show_col_types = TRUE, lazy = FALSE,
             # stringify id intergers 
             col_types = "ccTccccccciiiicccccccTiiiiccccccTllccccccTic") %>% 
  as.data.frame()

cyan.samples <- 
  read_delim("psanon-cyan-sample-tidy_v01.csv", delim = ";",
             show_col_types = TRUE, lazy = FALSE,
             # stringify id intergers 
             col_types = "ccTccccccciiiicccccccTiiiiccccccTllccccccTic") %>% 
  as.data.frame()


# Return list objects, delimited by '¤'
read$url_expanded_url <- read$url_expanded_url %>% strsplit("¤") %>% as.list()
read$url_images <- read$url_images %>% strsplit("¤") %>% as.list()
read$url_title <- read$url_title %>% strsplit("¤") %>% as.list()
read$url_description <- read$url_description %>% strsplit("¤") %>% as.list()
read$referenced_url_title <- 
  read$referenced_url_title %>% strsplit("¤") %>% as.list()
read$referenced_url_description <- 
  read$referenced_url_description %>% strsplit("¤") %>% as.list()
read$referenced_url_images <- 
  read$referenced_url_images %>% strsplit("¤") %>% as.list()

# read$url_expanded_url[i][[1]] %>% length()

View(read)

glimpse(orange.samples)
glimpse(cyan.samples)
glimpse(magenta.samples)

# END OF SCRIPT
 
# url_expanded_url
# url_images
# url_title
# url_description
# referenced_url_title
# referenced_url_description
# referenced_url_images

# orange.samples.tidy %>% 
#   rowwise() %>%
#   mutate_if(is.list, ~paste(unlist(., recursive = TRUE, use.names = TRUE), collapse = '¤')) %>%
#   write.table("psanon-orange-sample-tidy.csv", 
#               sep = ";", col.names = TRUE, row.names = FALSE, 
#               # eol = "\r\n"
#               )
# 
# cyan.samples.tidy %>% 
#   rowwise() %>%
#   mutate_if(is.list, ~paste(unlist(., recursive = TRUE, 
#                                    use.names = TRUE), collapse = '¤')) %>%
#   # write.table("psanon-cyan-sample-tidy.csv", 
#   #             sep = ";", col.names = TRUE, row.names = FALSE, 
#   #             # eol = "\r\n"
#   #             )
#   write_delim("psanon-cyan-sample-tidy.csv", delim = ";")
# 
# 
# magenta.samples.tidy %>% 
#   rowwise() %>%
#   mutate_if(is.list, ~paste(unlist(., recursive = TRUE, use.names = TRUE), collapse = '¤')) %>%
#   write.table("psanon-magenta-sample-tidy.csv", 
#               sep = ";", col.names = TRUE, row.names = FALSE, 
#               # eol = "\r\n"
#               )
# 
# library(tidyverse)
# 
# test1 <- read.csv("psanon-cyan-sample-tidy.csv", 
#                   sep = ";", header = TRUE, numerals = "no.loss", encoding = "UTF-16",
#                   quote = "\"")
# 
# test2 <- read_delim("psanon-cyan-sample-tidy.csv", delim = ";")
# 
# orange.samples.read <- readr::read_delim("psanon-orange-sample-tidy.csv",
#                            delim = ";", 
#                            escape_backslash = TRUE,
#                            show_col_types = TRUE,
#                            lazy = FALSE,
#                            col_types = "ccTciiiicccccccTiiiiccccccTllcccccic"
#                            )
# 
# View(orange.samples.read)
# 
# library(stringi)
# 
# ?locale()
# 
# cyan.samples.read <- readr::read_delim("psanon-cyan-sample-tidy.csv",
#                            delim = ";",
#                            locale = locale(encoding = "Windows-1252"),
#                            escape_backslash = TRUE,
#                            show_col_types = TRUE,
#                            lazy = FALSE,
#                            col_types = "ccTciiiicccccccTiiiiccccccTllcccccic"
#                            )
# 
# View(cyan.samples.read)
# 
# cyan.samples.read <- read.csv("psanon-cyan-sample-tidy.csv",
#                                        sep = ";", encoding = "UTF-16")
# 
# magenta.samples.read <- readr::read_delim("psanon-magenta-sample-tidy.csv",
#                            delim = ";",
#                            locale = locale(encoding = "Windows-1252"),
#                            escape_backslash = TRUE,
#                            show_col_types = TRUE,
#                            lazy = FALSE,
#                            col_types = "ccTciiiicccccccTiiiiccccccTllcccccic"
#                            )
# 
# View(magenta.samples.read)
# 
# 
# # TEMP: retrieve author information and conversation id
# 
# keep <- data.frame(tweet_id = character(),
#                    text = character(),
#                    conversation_id = character(),
#                    author_id = character(),
#                    user_username = character(),
#                    user_name = character(),
#                    user_profile_image_url = character())
# 
# keep.orange <- keep
# keep.cyan <- keep
# keep.magenta <- keep
# 
#   # Loop through the 10 most influential accounts in a cluster
#   # SOMETHING WRONG! DOES NOT FIND ALL CASES: 1260 instead of 1500
#   for (account in 1:10) {
# 
#     
#     tweets.append <- bind_tweets(paste0("./Twitter API data/",
#                                  clusters[[1]][account,"screen_name"],
#                                  "-all-tweets"), output_format = "tidy")
#     
#     glimpse(tweets.append)
#     
#     keep.tweets <- 
#       tweets.append[tweets.append$tweet_id %in% orange.samples.read$tweet_id,
#                     names(keep.orange)] %>% as.data.frame()
#     
#     keep.orange <- rbind(keep.orange, keep.tweets)
#     
#   }
# 
# # Loop through the 10 most influential accounts in a cluster
# # SOMETHING WRONG! DOES NOT FIND ALL CASES: 1442 instead of 1500
# for (account in 1:10) {
#   
#   
#   tweets.append <- bind_tweets(paste0("./Twitter API data/",
#                                       clusters[[2]][account,"screen_name"],
#                                       "-all-tweets"), output_format = "tidy")
#   
#   glimpse(tweets.append)
#   
#   keep.tweets <- 
#     tweets.append[tweets.append$tweet_id %in% cyan.samples.read$tweet_id,
#                   names(keep.cyan)] %>% as.data.frame()
#   
#   keep.cyan <- rbind(keep.cyan, keep.tweets)
#   
# }
# 
# # Loop through the 10 most influential accounts in a cluster
# # SOMETHING WRONG! DOES NOT FIND ALL CASES: 1260 instead of 1500
# for (account in 1:10) {
#   
#   
#   tweets.append <- bind_tweets(paste0("./Twitter API data/",
#                                       clusters[[3]][account,"screen_name"],
#                                       "-all-tweets"), output_format = "tidy")
#   
#   glimpse(tweets.append)
#   
#   keep.tweets <- 
#     tweets.append[tweets.append$tweet_id %in% magenta.samples.read$tweet_id,
#                   names(keep.magenta)] %>% as.data.frame()
#   
#   keep.magenta <- rbind(keep.magenta, keep.tweets)
#   
# }
# 
# # TEMP: order rows by key variable tweet_id
# keep.orange <- keep.orange[order(match(keep.orange[,1],
#                                         pull(orange.samples.read[,1]))),]
# 
# keep.cyan <- keep.cyan[order(match(keep.cyan[,1],
#                                         pull(cyan.samples.read[,1]))),]
# 
# keep.magenta <- keep.magenta[order(match(keep.magenta[,1],
#                                         pull(magenta.samples.read[,1]))),]
# 
# 
# # TEMP: Merge dataframe columns
# orange.samples.read_ <- 
#   cbind(orange.samples.read, keep.orange[,3:ncol(keep.orange)])
# rownames(orange.samples.read_) <- 1:nrow(orange.samples.read_)
# View(orange.samples.read_)
# 
# View(keep.orange)
# 
# cyan.samples.read_ <- 
#   cbind(cyan.samples.read, keep.cyan[,3:ncol(keep.cyan)])
# rownames(cyan.samples.read_) <- 1:nrow(cyan.samples.read_)
# View(cyan.samples.read_)
# 
# magenta.samples.read_ <- 
#   cbind(magenta.samples.read, keep.magenta[,3:ncol(keep.magenta)])
# rownames(magenta.samples.read_) <- 1:nrow(magenta.samples.read_)
# View(magenta.samples.read_)
# 
# # TEMP write new csvs:
# 
# orange.samples.read_ %>%
#   write.table("psanon-orange-sample-tidy_v02.csv", 
#               sep = ";", col.names = TRUE, row.names = FALSE, eol = "\r\n",
#               fileEncoding = "UTF-8")
# 
# orange.samples.read_ %>%
#   write_delim("psanon-orange-sample-tidy_v03.csv", 
#               delim = ";", escape = "none")
# 
# ###
# cyan.samples.read_ %>%
#   write.table("psanon-cyan-sample-tidy_v02.csv", 
#               sep = ";", col.names = TRUE, row.names = FALSE, eol = "\r\n",
#               fileEncoding = "UTF-8")
# 
# cyan.samples.read_ %>%
#   write_delim("psanon-cyan-sample-tidy_v03.csv", 
#               delim = ";", escape = "none")
# 
# ###
# magenta.samples.read_ %>%
#   write.table("psanon-magenta-sample-tidy_v02.csv", 
#               sep = ";", col.names = TRUE, row.names = FALSE, eol = "\r\n",
#               fileEncoding = "UTF-8")
# 
# magenta.samples.read_ %>%
#   write_delim("psanon-magenta-sample-tidy_v03.csv", 
#               delim = ";", escape = "none")

# test$url_images
# 
# orange.samples.tidy$url_images <- 
#   ifelse(is.na(orange.samples.tidy$url_images), vector(mode = "list", length = 1),
#          orange.samples.tidy$url_images)
# 
# 
# View(orange.samples.tidy)
# 
# is.na(orange.samples.tidy[,9]),9] <- vector(mode = "list", length = 1)
# 
# orange.samples.tidy[,list.objects] <- orange.samples.tidy[,list.objects] %>% as.character() 
# 
# orange.samples.tidy <- samples.tidy[[1]] %>% as.data.frame()
# View(orange.samples.tidy)
# 
# orange.samples.tidy <- samples.tidy[[1]] %>% as.data.frame()
# View(orange.samples.tidy)
# 
# orange.samples.tidy[,9] %>% head(10)
# 
# for (i in 1:ncol(orange.samples.tidy)) {
# 
# orange.samples.tidy[,i] %>% typeof() %>% print()
#   
# }
# 
# ?lapply
# 
# orange.samples.tidy <- samples.tidy[[1]]
# 
# # Turn list columns into character columns
# lists.orange <- 
#   orange.samples.tidy[,sapply(orange.samples.tidy, typeof) == "list"] %>% 
#   names()
# orange.samples.tidy[,lists.orange] <- orange.samples.tidy[,lists.orange] %>% 
#   as.character()
# 
# cyan.samples.tidy <- samples.tidy[[2]] %>% as.data.frame()
# 
# # Turn list columns into character columns
# 
# View(cyan.samples.tidy)
# 
# magenta.samples.tidy <- samples.tidy[[3]] %>% as.data.frame()
# View(magenta.samples.tidy)
# 
# # Write cluster samples into csv
# ?write.csv
# 
# write.table(orange.samples.tidy[,8:9], "psanon-orange-sample-tidy_test.csv",
#             row.names = FALSE, col.names = TRUE, sep = ";")
# 
# test.orange <- read.table("psanon-orange-sample-tidy.csv", header = T,
#                           sep = ";")
# 
# write.table(cyan.samples.tidy, "psanon-cyan-sample-tidy.csv",
#             row.names = FALSE, col.names = TRUE, sep = ";")
# 
# write.table(magenta.samples.tidy, "psanon-magenta-sample-tidy_1.csv",
#             row.names = FALSE, col.names = TRUE, sep = ";")
# 
# # OPTIONAL: READ DATAFRAMES FROM CSV
# orange.samples <- read.csv("psanon-orange-sample.csv", header = TRUE,
#                            sep = ";", numerals = "no.loss")

# THIS ONE WORKS: GSUB <U+000000> Unicode Emojis to actual Unicode 
# orange.samples$text <-as.character(
#   parse(text=shQuote(gsub("<U\\+([A-Z0-9]+)>",
#                           "\\\\U\\1", orange.samples$text))))

# cyan.samples <- read.csv("psanon-cyan-sample.csv", header = TRUE,
#                            sep = ";", numerals = "no.loss")
# 
# magenta.samples <- read.csv("psanon-magenta-sample.csv", header = TRUE,
#                            sep = ";", numerals = "no.loss")

# Take screenshots of tweets; retry 10 times on a error:
screenshot_tweet <- function(id, path) {
  tweet_screenshot(
    tweet_url = paste0("https://twitter.com/i/status/",id),
    hide_media = FALSE, hide_thread = FALSE,
    file = paste0(getwd(),"/graphs/tweet screenshots/",path,"/",id,".png"))
}

# Loop through ORANGE cluster sample
for(id in orange.samples[,"tweet_id"]) {

  r <- NULL
  path <- "orange"
  attempt <- 1
  while( is.null(r) && attempt <= 10 ) {
    attempt <- attempt + 1
    try(
      r <- screenshot_tweet(id, path)
    )
  }
}

# Loop through CYAN cluster sample
for(id in cyan.samples[,"tweet_id"]) {

  r <- NULL
  path <- "cyan"
  attempt <- 1
  while( is.null(r) && attempt <= 10 ) {
    attempt <- attempt + 1
    try(
      r <- screenshot_tweet(id, path)
    )
  }
}

# Loop through MAGENTA cluster sample
for(id in magenta.samples[,"tweet_id"]) {

  r <- NULL
  path <- "magenta"
  attempt <- 1
  while( is.null(r) && attempt <= 10 ) {
    attempt <- attempt + 1
    try(
      r <- screenshot_tweet(id, path)
    )
  }
}

# END SCRIPT ###################################################################
# clean up environment
# rm(list = ls())