#---------------------------------
# setup
#---------------------------------
library(dplyr)
# to download the full data set use the following code
url <- 'https://www.dropbox.com/s/8xqtqwv1j5rljq6/cr_glove.rds?dl=1' # link to raw data
destfile <- 'DESTINATION FILE HERE' # e.g. '~/Dropbox/GitHub/large_data/conText/data/local_glove.rds'
download.file(url, destfile)

#---------------------------------
# libraries
#---------------------------------
library(dplyr)
library(quanteda)
library(quanteda.textstats)


# load glove
glove <- readRDS('~/Dropbox/GitHub/large_data/conText/data/glove.rds')

# identify top features in data_char_ukimmig2010
feats <- tokens(data_char_ukimmig2010,
                  remove_punct = T,
                  remove_symbols = T,
                  remove_numbers = T,
                  remove_url = T,
                  remove_separators = T) %>%
  dfm %>%
  dfm_trim(min_termfreq = 2) %>%
  featnames

# identify overlap between top features and features in glove
common_features <- intersect(feats, rownames(glove))

# subset glove
glove_subset <- glove[common_features,]
usethis::use_data(glove_subset, compress = 'xz', overwrite = TRUE)
