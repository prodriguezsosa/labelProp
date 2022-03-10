#---------------------------------
# setup
#---------------------------------
library(dplyr)
# to download the full data set use the following code
url <- 'https://www.dropbox.com/s/54sjstszpw33i9b/khodakA.rds?dl=1' # link to raw data
destfile <- 'DESTINATION FILE HERE' # e.g. '~/Dropbox/GitHub/large_data/conText/data/khodakA.rds'
download.file(url, destfile)

# load data
khodakA <- readRDS("~/Dropbox/GitHub/large_data/conText/data/khodakA.rds")

# save
usethis::use_data(khodakA, compress = 'xz', overwrite = TRUE)
