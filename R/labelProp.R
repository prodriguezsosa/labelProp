#==============================================
# function: label_propag
# objective: score every word according to probability of being sampled by a random walk
# inputs:
#   seeds = list of length 2 with two sets of seeds, each characterizing one end of the scale (e.g. positive/negative)
#   vocab = vector of all tokens mapped on the semantic space (i.e. for which we have embeddings)
#   distance_matrix = square distance matrix (need not be symmetrics)
#   alpha = weights used when computing score (opposite seeds vs. everything else).
#   alpha = 1, only account for distance to the opposite set of seeds when computing distance ratio
#   alpha = 0, only account for distance to everything else when computing distance ratio
#   weigths = weight each seed in a seed set carries when computing score, use if you have varying confidence on the seed set
# output: data.table with 5 columns: token, score for set 1, score for set 2, weighted score for set 1, weighted score for set 2
#==============================================

# TO DO:
# ADD BOOTSTRAPPING + PERMUTATION
# SCALE OR NOT TO SCALE
# polarityScore(seeds = list(2)...)
# boostrapping
# permutation options
# visualization
# EXAMPLES

#' Compute matrix of transition probabilities
#'
#' Computes a matrix of transition probabilities.
#' see https://nlp.stanford.edu/pubs/hamilton2016inducing.pdf for details.
#'
#' @inheritParams parallelDist
#'
#' @return a V x V matrix of transition probabilities, with V = number of rows in x.
#'
#' @export
#' @rdname build_transition_matrix
#' @keywords build_transition_matrix
#' @examples
#'
labelProp <- function(seeds, x, method = "rw", metric = "cosine", beta = 0.5, verbose = TRUE){

  # check seeds are in x, if not, report and remove
  in_x <- seeds %in% rownames(x)

  # remove from seed set
  seeds <- seeds[in_x]

  # if no seeds are present in x, stop
  if(length(seeds)==0) stop("none of the seeds are present in the x")

  # report seeds not in x
  if(!all(in_x)) cat("words not in the vocabulary: ", paste(seeds[!in_x], collapse = ', '))

  if(method == "rw"){

    # check x is a transition matrix
    if(!(nrow(x) == ncol(x) && all(round(rowSums(x), 0) == 1) && all(diag(x) == 0)))stop('if method = "rw", x must be a symmetric transtition matrix with rows summing to 1 and diagonal set to 0.')

    if(bootstrap)

    # initialize vector of scores
    score <- matrix(rep(1/nrow(x),nrow(x)), nrow = nrow(x), ncol = 1)

    # anchor vector for seeds
    seed_anchor <- matrix(0, nrow = nrow(x), ncol = 1)
    seed_anchor[which(colnames(x) == seeds)] <- 1/length(seed_anchor)

    # iterative updating solution
    #epsilon = 0.00000001
    #conv_dist <- 1
    #while(conv_dist > epsilon){
    #  score_lag <- score
    #  score <- beta*x%*%score_lag + (1 - beta)*seed_anchor
    #  conv_dist <- sqrt(sum((score - score_lag)^2))
    #  if(verbose) print(conv_dist)
    #}

    # closed-form solution
    score <- solve(diag(nrow(x)) - beta*x)%*%seed_anchor/length(seeds)

    # result
    # TO DO: SCALE OR NOT TO SCALE
    result <- data.frame("instance" = rownames(x), "score" = scale(score)[,1])
  } else if(method == "nns"){

    # take column average of seed embeddings
    seed_vec = matrix(Matrix::colMeans(x[seeds,]), nrow = 1)

    # compute similarity
    sim_vec <- text2vec::sim2(x = seed_vec, y = x, method = "cosine", norm = "l2")[1,]

    # result
    result <- data.frame("instance" = names(sim_vec), "score" = scale(unname(sim_vec))[,1])

  } else stop('method must be either "rw" or "nns"')

  # output
  return(result)
}

compute_score <- function(x){
  # initialize vector of scores
  score <- matrix(rep(1/nrow(x),nrow(x)), nrow = nrow(x), ncol = 1)

  # anchor vector for seeds
  seed_anchor <- matrix(0, nrow = nrow(x), ncol = 1)
  seed_anchor[which(colnames(x) == seeds)] <- 1/length(seed_anchor)

  # iterative updating solution
  #epsilon = 0.00000001
  #conv_dist <- 1
  #while(conv_dist > epsilon){
  #  score_lag <- score
  #  score <- beta*x%*%score_lag + (1 - beta)*seed_anchor
  #  conv_dist <- sqrt(sum((score - score_lag)^2))
  #  if(verbose) print(conv_dist)
  #}

  # closed-form solution
  score <- solve(diag(nrow(x)) - beta*x)%*%seed_anchor/length(seeds)

  # result
  # TO DO: SCALE OR NOT TO SCALE
  result <- data.frame("instance" = rownames(x), "score" = scale(score)[,1])
  return(result)
}
