#' Compute relevance scores for a collection of nodes based on a set of seed nodes.
#'
#' See: https://proceedings.neurips.cc/paper/2003/file/87682805257e619d49b8e0dfdc14affa-Paper.pdf
#'
#' @param x (numeric) if `method = rw`, then `x` is a symmetric matrix of transition probabilities.
#' if `method = nns`, then `x` is a matrix of vector representations.
#' @param seeds (list) a list of character vectors defining the classes of interest.
#' If named, then `names(seeds)` will be used to define classes,
#' otherwise classes will be labeled `class1`, `class2` etc.
#' @param method (character) either `nns` or `rw`. If `nns`, then values will be computed
#' using cosine similarity, if not values are computed using spreading activation.
#' @param beta (numeric) in (0,1), specifies the extent to which the algorithm favors local (similar labels for neighbors)
#' vs. global (correct labels on seed words) consistency.
#' @param bootstrap (logical) if TRUE, use bootstrapping -- sample a proportion (defined by `prop_seeds`)
#' of seeds and re-run algorithm. Required to get std. errors.
#' @param num_bootstraps (integer) number of bootstraps to use.
#' @param prop_seeds (numeric) proportion of seeds to sample when bootstrapping.
#' @param as_list (logical) if FALSE all results are combined into a single data.frame
#' If TRUE, a list of data.frames is returned with one data.frame per class.
#' @param verbose (logical) if TRUE show progress bar.
#'
#' @return a `data.frame` or list of data.frames (one for each class)
#' with the following columns:
#' \describe{
#'  \item{`node`}{ (character) rownames of `x`.}
#'  \item{`class`}{(character) name of class. If none provided, then
#'  classes will be labeled `class1`, `class2` etc.}
#'  \item{`score`}{(numeric) score assigned to node.}
#'  \item{`std.error`}{(numeric) std. error of score.
#'  Column is dropped if `bootstrap = FALSE`.}
#'  }
#'
#' @export
#' @rdname labelProp
#' @keywords labelProp
#' @examples
labelProp <- function(x, seeds, N = 10, method = "rw", beta = 0.5, bootstrap = FALSE, num_bootstraps = 50, prop_seeds = 0.5, softmax = FALSE, as_list = FALSE, verbose = TRUE){

  # check object type
  if(!is.list(seeds) & !is.character(seeds)) warning('"seeds" must be a character vector or list of character vectors. \n Each characeter vector is understood to represent a single class. \n', call. = FALSE)
  if(is.character(seeds)) seeds <- list(seeds)

  # softmax requires two classes
  if(softmax){
    if(length(seeds) < 2){
      softmax <- FALSE
      warning('to use softmax you must provide at least 2 classes of seeds. Proceeding without softmax. \n', call. = FALSE)
    }}

  # check seeds are in x, if not, report and remove
  orig_seeds <- seeds
  seeds <- lapply(seeds, function(s) s[s %in% rownames(x)])

  # if no seeds are present in x, stop
  if( length(seeds)==0 ) stop("none of the seeds are present in the x \n")

  # report seeds not in x
  not_in_x <- setdiff(unlist(orig_seeds), unlist(seeds))
  if( length(not_in_x)!=0  ) cat("seeds not in x:", paste(not_in_x, collapse = ', '), "\n")

  if ( method == "rw" ) {

    # check x is a transition matrix
    if(!(nrow(x) == ncol(x) && all(round(rowSums(x), 0) == 1) && all(diag(x) == 0)))stop('if method = "rw", x must be a symmetric transtition matrix with rows summing to 1 and diagonal set to 0.')

    if (bootstrap) {

      if(verbose) result <- pbreplicate(num_bootstraps, compute_rw_score(x = x, seeds = lapply(seeds, function(s) sample(s, size = ceiling(prop_seeds*length(s)), replace = FALSE)), beta = beta, softmax = TRUE), simplify = FALSE)
      else result <- replicate(num_bootstraps, compute_rw_score(x = x, seeds = lapply(seeds, function(s) sample(s, size = ceiling(prop_seeds*length(s)), replace = FALSE)), beta = beta, softmax = TRUE), simplify = FALSE)
      result <- dplyr::bind_rows(result) %>% dplyr::group_by(class, node) %>% dplyr::summarize(std.error = sd(score), score = mean(score), .groups = "drop_last") %>% dplyr::select(node, class, score, std.error) %>% dplyr::ungroup()

    } else {

      result <- compute_rw_score(x = x, seeds = seeds, beta = beta, softmax = softmax)

    }

  } else if ( method == "nns" ) {

    # check x is a transition matrix
    if((nrow(x) == ncol(x) && all(round(rowSums(x), 0) == 1) && all(diag(x) == 0)))warning('this looks like a transition matrix, are you sure you want to set method = "nns"?', call. = FALSE)

    if (bootstrap) {

      if(verbose) result <- pbreplicate(num_bootstraps, compute_nns_score(x = x, seeds = lapply(seeds, function(s) sample(s, size = ceiling(prop_seeds*length(s)), replace = FALSE)), softmax = softmax), simplify = FALSE)
      else result <- replicate(num_bootstraps, compute_nns_score(x = x, seeds = lapply(seeds, function(s) sample(s, size = ceiling(prop_seeds*length(s)), replace = FALSE)), softmax = softmax), simplify = FALSE)
      result <- dplyr::bind_rows(result) %>% dplyr::group_by(class, node) %>% dplyr::summarize(std.error = sd(score), score = mean(score), .groups = "drop_last") %>% dplyr::select(node, class, score, std.error) %>% dplyr::ungroup()

    } else {

      result <- compute_nns_score(x = x, seeds = seeds, softmax = softmax)

    }

  } else stop('method must be either "rw" or "nns"')

  # limit output to top N
  if(is.numeric(N) || is.integer(N)) result <- result %>% group_by(class) %>% top_n(N, wt = score) %>% ungroup %>% arrange(-score)

  # if as_list
  if(as_list){
    result <- result %>% group_by(class) %>% group_split() %>% as.list()
    if(is.null(names(seeds))) names(result) <- paste0("class", 1:length(seeds))
    else names(result) <- rev(names(seeds))
  }

  # output
  return(result)
}

# sub-functions
compute_rw_score <- function(x, seeds, beta = 0.5, softmax = TRUE){

  # initialize vector of scores
  score <- matrix(rep(1/nrow(x),nrow(x)), nrow = nrow(x), ncol = length(seeds))

  # anchor vector for seeds
  Y <- lapply(seeds, function(s){
    seed_anchor <- matrix(0, nrow = nrow(x), ncol = 1)
    seed_anchor[which(colnames(x) %in% s)] <- 1/length(s)
    return(seed_anchor)
  }) %>% do.call(cbind, .)

  # iterative updating solution
  epsilon = 0.00000001
  conv_dist <- 1
  while(conv_dist > epsilon){
    score_lag <- score
    score <- beta*x%*%score_lag + (1 - beta)*Y
    conv_dist <- sqrt(sum((score - score_lag)^2))
    #if(verbose) print(conv_dist)
  }

  # closed-form solution (matrix inversion is VERY slow for large matrices)
  #score <- (1-beta)*solve(diag(nrow(x)) - beta*x)%*%Y

  # apply softmax if specified
  if(softmax) score <- sweep(exp(score), 1, rowSums(exp(score)), '/')
  score <- data.frame(score, row.names = NULL)

  # class names
  if(is.null(names(seeds))) names(score) <- paste0("class", 1:length(seeds))
  else names(score) <- names(seeds)

  # result
  result <- cbind("node" = rownames(x), score)
  result <- result %>% tidyr::pivot_longer(cols = colnames(score), names_to = "class", values_to = "score") %>% filter(!(node %in% unlist(seeds)))

  # scale
  result <- result %>% group_by(class) %>% mutate(score = scale(score)[,1]) %>% ungroup()

  # out
  return(result)
}

# sub-functions
compute_nns_score <- function(x, seeds, softmax = TRUE){

  # take column average of seed embeddings
  seeds_vec = lapply(seeds, function(s) matrix(Matrix::colMeans(matrix(x[s,], nrow = length(s), ncol = ncol(x))), nrow = 1))

  # compute similarity
  result <- lapply(1:length(seeds_vec), function(s){
    sim_vec <- text2vec::sim2(x = seeds_vec[[s]], y = x, method = "cosine", norm = "l2")[1,]
    return(data.frame(node = names(sim_vec), class = ifelse(!is.null(names(seeds_vec)), names(seeds_vec)[[s]], paste0("class", s)), score = unname(sim_vec)))
    })

  # result
  result <- bind_rows(result)

  # softmax: shift cosine similarity scale from -1 to 1 to to 0 - 2
  if(softmax) result <- result %>% mutate(score = score + 1) %>% group_by(node) %>% mutate(score = score/sum(score)) %>% ungroup()

  # scale
  result <- result %>% filter(!(node %in% unlist(seeds))) %>% group_by(class) %>% mutate(score = scale(score)[,1]) %>% ungroup()

  # out
  return(result)
}
