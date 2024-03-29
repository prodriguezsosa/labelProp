#' Computes a matrix of transition probabilities.
#'
#' @param x a numeric matrix (each row is one series) or list of numeric matrices for
#' multidimensional series (each matrix is one series, a row is a dimension of a series)
#' @param method the distance measure to be used.
#' A list of all available distance methods can be found in `?parallelDist::parDist`
#' @param threads number of cpu threads for calculating a distance matrix.
#' Default is 4.
#'
#' @return a V x V matrix of transition probabilities, with V = number of rows in x.
#'
#' @export
#' @rdname build_transition_matrix
#' @keywords build_transition_matrix
#' @examples
#'
#' transition_matrix <- build_transition_matrix(anes2016_glove, threads = 2)
#'
build_transition_matrix <- function(x, method = "cosine", threads = 4L){

  # compute distance vector
  dist_vec <- parallelDist::parDist(x = x, method = method, diag = FALSE, upper = FALSE, threads = threads)

  # create similarity (full) matrix
  sim_mat <- as.matrix(1 - dist_vec)
  sim_mat[sim_mat > 1] <- 1
  sim_mat[sim_mat < -1] <- -1
  sim_mat <- acos(-sim_mat)/pi
  diag(sim_mat) <- 0

  # create transition matrix: each row sums to 1
  result <- sweep(sim_mat, 1, rowSums(sim_mat), FUN = "/")

  # add names
  colnames(result) <- rownames(result) <- rownames(x)

  # result
  return(result)
}

