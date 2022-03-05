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


# require(pbapply)
#' Compute matrix of transition probabilities
#'
#' Computes a matrix of transition probabilities.
#' see https://nlp.stanford.edu/pubs/hamilton2016inducing.pdf for details.
#'
#' @param x (numeric) a symmetric matrix of transition probabilities
#' @param seeds
#' @param beta
#' @param bootstrap (logical) if TRUE, use bootstrapping -- sample from texts with replacement and
#' re-estimate cosine similarities for each sample. Required to get std. errors.
#' If `groups` defined, sampling is automatically stratified.
#' @param num_bootstraps (integer) number of bootstraps to use.
#' @param prop_seeds (numeric) proportion of seeds to sample
#' @param as_list (logical) if FALSE all results are combined into a single data.frame
#' If TRUE, a list of data.frames is returned with one data.frame per target.
#' @param verbose (logical) progress bar
#'
#' @return a `data.frame` or list of data.frames (one for each target)
#' with the following columns:
#' \describe{
#'  \item{`node`}{ (character) rownames of `x`, excluding seed nodes
#'  \item{`class`}{(character) name of class. If none provided, then
#'  classes will be labeled `class1`, `class2` etc.}
#'  \item{`score`}{(numeric) score assigned to node.}
#'  \item{`std.error`}{(numeric) std. error of score.
#'  Column is dropped if `bootstrap = FALSE`.}
#'  }
#'
#' @return a V x V matrix of transition probabilities, with V = number of rows in x.
#'
#' @export
#' @rdname build_transition_matrix
#' @keywords build_transition_matrix
#' @examples
#'
labelProp <- function(x, seeds, method = 'rw', beta = 0.5, bootstrap = FALSE, num_bootstraps = 50, prop_seeds = 0.5, as_list = FALSE, verbose = TRUE){

  # check seeds are in x, if not, report and remove
  orig_seeds <- seeds
  seeds <- lapply(seeds, function(s) s[s %in% rownames(x)])

  # if no seeds are present in x, stop
  if( length(seeds)==0 ) stop("none of the seeds are present in the x")

  # report seeds not in x
  not_in_x <- setdiff(unlist(orig_seeds), unlist(seeds))
  if( length(not_in_x)!=0  ) cat("seeds not in x:", paste(not_in_x, collapse = ', '))


  # check x is a transition matrix
  if(!(nrow(x) == ncol(x) && all(round(rowSums(x), 0) == 1) && all(diag(x) == 0)))stop('x must be a symmetric transtition matrix with rows summing to 1 and diagonal set to 0.')

  if ( method == "rw" ) {

    if (bootstrap) {

      if(verbose) result <- pbreplicate(num_bootstraps, compute_rw_score(x = x, seeds = lapply(seeds, function(s) sample(s, size = ceiling(prop_seeds*length(s)), replace = FALSE)), beta = beta), simplify = FALSE)
      else result <- replicate(num_bootstraps, compute_rw_score(x = x, seeds = lapply(seeds, function(s) sample(s, size = ceiling(prop_seeds*length(s)), replace = FALSE)), beta = beta), simplify = FALSE)
      result <- dplyr::bind_rows(result) %>% dplyr::group_by(class, node) %>% dplyr::summarize(std.error = sd(score), score = mean(score), .groups = "drop_last") %>% dplyr::select(node, class, score, std.error) %>% dplyr::ungroup()

    } else {

      result <- compute_rw_score(x = x, seeds = seeds, beta = beta)

    }

  } else if ( method == "nns" ) {

    if (bootstrap) {

      if(verbose) result <- pbreplicate(num_bootstraps, compute_nns_score(x = x, seeds = lapply(seeds, function(s) sample(s, size = ceiling(prop_seeds*length(s)), replace = FALSE))), simplify = FALSE)
      else result <- replicate(num_bootstraps, compute_nns_score(x = x, seeds = lapply(seeds, function(s) sample(s, size = ceiling(prop_seeds*length(s)), replace = FALSE))), simplify = FALSE)
      result <- dplyr::bind_rows(result) %>% dplyr::group_by(class, node) %>% dplyr::summarize(std.error = sd(score), score = mean(score), .groups = "drop_last") %>% dplyr::select(node, class, score, std.error) %>% dplyr::ungroup()

    } else {

      result <- compute_nns_score(x = x, seeds = seeds)

    }

  } else stop('method must be either "rw" or "nns"')

  # if as_list
  if(as_list){
    result <- result %>% group_by(class) %>% group_split() %>% as.list()
    if(is.null(names(seeds))) names(result) <- paste0("class", 1:length(seeds))
    else names(result) <- names(seeds)
  }

  # output
  return(result)
}

compute_rw_score <- function(x, seeds, beta = 0.5){

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
  result <- cbind("node" = rownames(x), score) %>% tidyr::pivot_longer(cols = colnames(score), names_to = "class", values_to = "score") %>% filter(node!=unlist(seeds))

  # out
  return(result)
}

compute_nns_score <- function(x, seeds, metric){

  # take column average of seed embeddings
  seeds_vec = lapply(seeds, function(s) matrix(Matrix::colMeans(matrix(x[s,], nrow = length(s), ncol = ncol(x))), nrow = 1))

  # compute similarity
  result <- lapply(1:length(seeds_vec), function(s){
    sim_vec <- text2vec::sim2(x = seeds_vec[[s]], y = x, method = "cosine", norm = "l2")[1,]
    return(data.frame(node = names(sim_vec), class = ifelse(!is.null(names(seeds_vec)), names(seeds_vec)[[s]], paste0("class", s)), score = unname(sim_vec)))
    })

  # result
  result <- bind_rows(result) %>% filter(node!=unlist(seeds))

  # out
  return(result)
}
