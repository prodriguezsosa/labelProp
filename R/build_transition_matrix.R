# require(parallelDist) # DELETE THIS
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
build_transition_matrix <- function(x, metric = "cosine", threads = 4L){

  # compute distance vector
  dist_vec <- parallelDist::parDist(x = x, method = metric, diag = FALSE, upper = FALSE, threads = threads)

  # create similarity (full) matrix
  sim_mat <- matrix(0, nrow = attr(dist_vec, "Size"), ncol = attr(dist_vec, "Size"))
  sim_mat[upper.tri(sim_mat, diag = FALSE)] <- 1 - dist_vec
  sim_mat[lower.tri(sim_mat, diag = FALSE)] <- t(sim_mat[upper.tri(sim_mat, diag = FALSE)])
  sim_mat[sim_mat > 1] <- 1 # fix rounding errors
  sim_mat <- acos(-sim_mat)/pi
  diag(sim_mat) <- 0

  # create transition matrix: each row sums to 1
  result <- sweep(sim_mat, 1, rowSums(sim_mat), FUN = "/")

  # add names
  colnames(result) <- rownames(result) <- rownames(x)

  # result
  return(result)
}

