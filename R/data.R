#' GloVe subset
#'
#' A subset of the Stanford GloVe (300 dimensional) embeddings, consisting of
#' all words found in the ANES 2016 dataset with at least 5 occurrences.
#'
#' @format A matrix with 1441 rows and 300 columns:
#' \describe{
#'   \item{row}{each row corresponds to a word}
#'   \item{column}{each column corresponds to a dimension in the embedding space}
#'   ...
#' }
#' @source \url{https://nlp.stanford.edu/projects/glove/}
"anes2016_glove"


#' Khodak et al's transformation matrix for Stanford GloVe (300 dimensions)
#'
#' A square matrix corresponding to the transformation matrix computed by Khodak et al.
#'
#' @format A 300 by 300 matrix.
#' @source \url{https://github.com/NLPrinceton/ALaCarte}
"khodakA"

#' ANES 2016 data
#'
#' A data.frame with 8135 observations and 2 variables, containing
#' unique responses to the open-ended question in the ANES (2016):
#' "what are the most important issues facing the country?"
#' See /data-raw for pre-processing file.
#'
#' @format A data.frame with 8135 observations and 2 variables:
#' \describe{
#'   \item{docid}{a unique document ID}
#'   \item{response}{text of open-ended response}
#'   ...
#' }
#' @source \url{https://electionstudies.org/data-center/2016-time-series-study/}
"anes2016"
