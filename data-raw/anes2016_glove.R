# run anes2016.R first

#---------------------------------
# setup
#---------------------------------
library(dplyr)
# to download the full data set use the following code
url <- 'https://www.dropbox.com/s/2eak4ua08n4yi66/glove.rds?dl=1' # link to raw data
destfile <- 'DESTINATION FILE HERE' # e.g. '~/Dropbox/GitHub/large_data/conText/data/glove.rds'
download.file(url, destfile)

#---------------------------------
# libraries
#---------------------------------
library(dplyr)
library(quanteda)

# load glove
glove <- readRDS('~/Dropbox/GitHub/large_data/conText/data/glove.rds')
load("~/Dropbox/GitHub/repositories/labelProp/data/anes2016.rda")

# identify features in anes2016
feats <- anes2016$response %>% tokens() %>% dfm() %>% dfm_trim(min_termfreq = 5) %>% featfreq() %>% names()

# identify overlap between top features and features in glove
common_features <- intersect(feats, rownames(glove))

# subset glove
anes2016_glove <- glove[common_features,]
usethis::use_data(anes2016_glove, compress = 'xz', overwrite = TRUE)
