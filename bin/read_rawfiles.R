

# Libraries ---------------------------------------------------------------


library(rawDiag)
library(tidyverse)
library(lubridate)



# Directory path
raw_file_dir <- "../path/to/raw/files"


# Annotation File ---------------------------------------------------------


# Reading the files in the directory
raw_files <- list(filename = dir(raw_file_dir)[grepl(".raw", dir(raw_file_dir))],
                  creation_date = file.info(dir(raw_file_dir, full.names = TRUE)[grepl(".raw", dir(raw_file_dir))])$mtime,
                  file_size = file.info(dir(raw_file_dir, full.names = TRUE)[grepl(".raw", dir(raw_file_dir))])$size)

# converting the files into dataframe
annot <- bind_rows(raw_files)

# Formatting annot table
annot <- annot %>%
  mutate(filename = str_remove_all(filename, ".raw"),
         creation_date = ymd_hms(creation_date))


# RawDiag -----------------------------------------------------------------


# Reading list of raw files from the directory
raw_file_names <- dir(raw_file_dir, full.names = TRUE)[grep(".raw$", list.files(raw_file_dir))]

# Reading raw files using rawDiag and saving as a list
RAW <- lapply(raw_file_names, read.raw)

# Concatenating list into a single dataframe
data <- bind_rows(RAW)


# cleanup of raw file names
data <- data %>%
  mutate(filename = str_remove_all(filename, ".raw")) %>% 
  full_join(annot, .)


