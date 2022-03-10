#' Computes a matrix of transition probabilities.
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

