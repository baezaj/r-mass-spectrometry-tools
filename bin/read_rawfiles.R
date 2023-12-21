

### Libraries ###

library(rawDiag)
library(tidyverse)
library(rio)
library(lubridate)


### Data import ###

# Directory path
raw_file_dir <- "../data/raw/"

# Reading the files in the directory
raw_files <- list(filename = dir(raw_file_dir)[grepl(".raw", dir(raw_file_dir))],
                  creation_date = file.info(dir(raw_file_dir, full.names = TRUE)[grepl(".raw", dir(raw_file_dir))])$mtime)

# converting the files into dataframe
annot <- bind_rows(raw_files)

# Formatting annot table
annot <- annot %>%
  mutate(filename = str_remove_all(filename, ".raw"),
         creation_date = ymd_hms(creation_date)) %>%
  separate(filename, into = c("date", "user", "nls", "instrument", "ms_type", "condition_1",
                              "time_point", "batch", "replicate", "techrep", "run_order"),
           sep = "_", remove = FALSE, convert = TRUE)

### Reading raw files using RawDiag ###

# Reading list of raw files from the directory
raw_file_names <- dir(raw_file_dir, full.names = TRUE)[grep(".raw$", list.files(raw_file_dir))]

# Reading raw files using rawDiag and saving as a list
RAW <- lapply(raw_file_names, read.raw)

# Concatenating list into a single dataframe
data <- bind_rows(RAW)

### Formatting ###

# cleanup of raw file names
data <- data %>%
  mutate(filename = str_remove_all(filename, ".raw"))

# adding annotation to dataframe
data <- full_join(annot, data)

