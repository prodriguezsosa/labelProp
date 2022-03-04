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
# INSTANCE?

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
labelProp <- function(x, seeds, metric = "cosine", beta = 0.5, bootstrap = FALSE, num_bootstraps = 50, prop_seeds = 0.5, permute = FALSE, num_permutations = 100, num_seeds = 5, verbose = TRUE){

  # check seeds are in x, if not, report and remove
  orig_seeds <- seeds
  seeds <- lapply(seeds, function(s) s[s %in% rownames(x)])

  # if no seeds are present in x, stop
  if( length(seeds)==0 ) stop("none of the seeds are present in the x")

  # report seeds not in x
  not_in_x <- setdiff(unlist(orig_seeds), unlist(seeds))
  if( length(not_in_x)!=0  ) cat("seeds not in x:", paste(not_in_x, collapse = ', '))


  # check x is a transition matrix
  if(!(nrow(x) == ncol(x) && all(round(rowSums(x), 0) == 1) && all(diag(x) == 0)))stop('if method = "rw", x must be a symmetric transtition matrix with rows summing to 1 and diagonal set to 0.')

  if (bootstrap) {

    result <- replicate(num_bootstraps, compute_rw_score(x = x, seeds = lapply(seeds, function(s) sample(s, size = ceiling(prop_seeds*length(s)), replace = FALSE)), beta = beta, as_list = FALSE), simplify = FALSE)
    result <- dplyr::bind_rows(result) %>% dplyr::group_by(class, node) %>% dplyr::summarize(std.error = sd(score), score = mean(score), .groups = "drop_last") %>% dplyr::select(node, class, score, std.error) %>% dplyr::ungroup()

  } else {

    result <- compute_rw_score(x = x, seeds = seeds, beta = beta)

  }

  if ( permute ){

    # reshape result
    result <- result %>% group_by(class) %>% group_split()


    perm_result <- permute_rw_score(x = x, seeds = seeds, beta = beta, as_list = TRUE)

    for(i in 1:length(result)){
      temp <- result[[1]]$score > perm_result[[1]]$score
    }


  }

  # output
  return(result)
}

compute_rw_score <- function(x, seeds, beta = 0.5, as_list = FALSE){

  # anchor vector for seeds
  Y <- lapply(seeds, function(s){
    seed_anchor <- matrix(0, nrow = nrow(x), ncol = 1)
    seed_anchor[which(colnames(x) %in% s)] <- 1
    return(seed_anchor)
  }) %>% do.call(cbind, .)

  # closed-form solution
  score <- solve(diag(nrow(x)) - beta*x)%*%Y %>% data.frame(., row.names = NULL)
  if(is.null(names(seeds))) names(score) <- paste0("class", 1:length(seeds))
  else names(score) <- names(seeds)

  # result
  result <- cbind("node" = rownames(x), score) %>% tidyr::pivot_longer(cols = colnames(score), names_to = "class", values_to = "score")

  # if as_list
  if(as_list) result <- result %>% group_by(class) %>% group_split()

  # out
  return(result)
}

permute_rw_score <- function(x, seeds, beta = 0.5, as_list = FALSE){

  # anchor vector for seeds
  Y <- lapply(seeds, function(s){
    sample_s <- sample(rownames(x), length(s), replace = FALSE)
    seed_anchor <- matrix(0, nrow = nrow(x), ncol = 1)
    seed_anchor[which(colnames(x) %in% sample_s)] <- 1
    return(seed_anchor)
  }) %>% do.call(cbind, .)

  # closed-form solution
  score <- solve(diag(nrow(x)) - beta*x)%*%Y %>% data.frame(., row.names = NULL)
  if(is.null(names(seeds))) names(score) <- paste0("class", 1:length(seeds))
  else names(score) <- names(seeds)

  # result
  result <- cbind("node" = rownames(x), score) %>% tidyr::pivot_longer(cols = colnames(score), names_to = "class", values_to = "score")

  # if as_list
  if(as_list) result <- result %>% group_by(class) %>% group_split()

  # out
  return(result)
}







# sample seeds
init_seeds <- as.list(sample(rownames(x), num_permutations, replace = FALSE))

# find neighborhoods
sample_seeds <- compute_rw_score(x, seeds = init_seeds, beta = beta) %>%
  dplyr::group_by(class) %>%
  dplyr::slice_max(order_by = score, n = num_seeds) %>%
  select(node, class) %>%
  group_by(class) %>% group_split() %>%
  lapply(., function(i) i %>% pull(node))

# permuted scores
perm_score <- compute_rw_score(x, seeds = sample_seeds, beta = beta) %>%
  tidyr::pivot_wider(names_from = "class", values_from = "score") %>% select(-node) %>% as.matrix()

# add p.value
result <- lapply(result, function(i){
  p.value <- sweep(perm_score, 1, i$score, FUN = "-")
  p.value <- p.value < 0
  i$p.value <- 1 - rowSums(p.value)/ncol(perm_score)
  return(i)}) %>% bind_rows()

}




