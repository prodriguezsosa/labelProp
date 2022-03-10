# to download the full data set use the following code
url <- 'https://www.dropbox.com/s/1u54o5zx7ot9dcc/anes2016.zip?dl=1' # link to raw data
destfile <- 'DESTINATION FILE HERE' # e.g. '~/Dropbox/GitHub/large_data/labelProp/anes2016.zip'
download.file(url, destfile)

#---------------------------------
# setup
#---------------------------------
library(dplyr)
library(quanteda)
library(readxl)
library(stringr)
library(tidyr)
#library(foreign)

# paths
path_to_data <- "~/Dropbox/GitHub/large_data/labelProp/anes2016/"

#-----------------
# load open-ends
#-----------------
# sheets of interest: 20, 21, 22
open_ends <- lapply(20:22, function(sheet){
  sheet_i <- read_excel(paste0(path_to_data, "anes_timeseries_2016_redacted_openends.xlsx"), sheet = sheet)
  colnames(sheet_i) <- c("respondent_id", "response")
  sheet_i <- sheet_i %>% mutate(mention_rank = sheet - 19)
  return(sheet_i)
  })
open_ends <- do.call(rbind, open_ends)
open_ends <- open_ends[!is.na(open_ends$response),] # drop NAs

#-----------------
# load contraction data
#-----------------
contraction_list <- read_excel(paste0(path_to_data, "contractions_list.xlsx"))
contraction_list <- contraction_list %>% mutate(nchar = nchar(pattern)) %>% arrange(-nchar)
contraction_list <- contraction_list %>% filter(pattern!="'s")  # too blunt

#------------------
# check which documents were redacted
#------------------
redacted <- grepl("REDACTED", open_ends$response, ignore.case = FALSE)
open_ends$response <- str_remove(open_ends$response, "\\s*\\[[^\\)]+\\]")  # remove redacted message
open_ends$redacted <- redacted
cat(table(redacted)[["TRUE"]], "documents with redactions", "\n")
open_ends <- open_ends %>% filter(!redacted)  # drop documents with redactions

#------------------
# find misspelled words
#------------------
library(spelling)
library(hunspell)
library(progress)
misspellings <- spell_check_text(open_ends$response) # check spelling
cat(length(unique(unlist(misspellings$found))), "documents with misspellings", "\n")
word_corrections <- hunspell_suggest(misspellings$word) # find suggested corrections for misspellings
word_corrections <- lapply(word_corrections, `[`, 1) %>% unlist() # choose first suggestion
spelling_tibble <- tibble(pattern = misspellings$word, replacement = word_corrections) # build replacement tibble

# correct spelling using first suggestion
pb <- progress_bar$new(total = nrow(spelling_tibble))
for(i in 1:nrow(spelling_tibble)){
  open_ends$response <- gsub(paste0("\\<", spelling_tibble$pattern[i],"\\>"), spelling_tibble$replacement[i], open_ends$response)
  pb$tick()
}

open_ends$misspellings <- FALSE
open_ends$misspellings[unique(unlist(misspellings$found))] <- TRUE
#open_ends <- open_ends %>% filter(misspellings == TRUE)  # drop documents with spelling mistakes

#------------------
# pre-process
#------------------
open_ends <- open_ends %>% mutate(response = tolower(response))  # lowercase
open_ends <- open_ends %>% mutate(response = gsub("[^[:alnum:][:space:]']", " ", response)) # remove all non-alpha characters
open_ends <- open_ends %>% mutate(response = str_replace_all(response, "^ +| +$|( ) +", "\\1"))  # remove excess white space
open_ends <- open_ends[open_ends$response!="",] # drop documents with an empty text field
open_ends <- open_ends %>% mutate(num_char = nchar(open_ends$response)) %>% filter(num_char >=3) # drop entries with two characters or less

# keep unique documents and add ID
anes2016 <- open_ends %>% distinct(response, .keep_all = TRUE) %>% mutate(docid = paste0("text", 1:nrow(.))) %>% select(docid, response)

# save
usethis::use_data(anes2016, compress = 'xz', overwrite = TRUE)

