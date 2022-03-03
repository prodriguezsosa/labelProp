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
labelProp <- function(seeds, x, method = "rw", metric = "cosine", beta = 0.5, bootstrap = FALSE, num_bootstraps = 50, num_seeds = 1, permute = FALSE, num_permutations = 100, verbose = TRUE){
  
  # check seeds are in x, if not, report and remove
  in_x <- seeds %in% rownames(x)
  
  # remove from seed set
  seeds <- seeds[in_x]
  
  # if no seeds are present in x, stop
  if( length(seeds)==0 ) stop("none of the seeds are present in the x")
  
  # report seeds not in x
  if( !all(in_x) ) cat("words not in the vocabulary: ", paste(seeds[!in_x], collapse = ', '))
  
  if ( method == "rw" ) {
    
    # check x is a transition matrix
    if(!(nrow(x) == ncol(x) && all(round(rowSums(x), 0) == 1) && all(diag(x) == 0)))stop('if method = "rw", x must be a symmetric transtition matrix with rows summing to 1 and diagonal set to 0.')
    
    if (bootstrap) {
      
      result <- replicate(num_bootstraps, compute_rw_score(x = x, sample(seeds, size = num_seeds, replace = FALSE)), simplify = FALSE)
      result <- bind_rows(result) %>% group_by(instance) %>% summarize(value = mean(score), std.error = sd(score)) %>% ungroup()
    
      } else {
        
      result <- compute_score(x = x, seeds = seeds) %>% select(instance, value = score)
   
      }
    
    if ( permute ){
      
      perm_result <- replicate(num_permutations, compute_rw_score(x = x, sample(rownames(x), size = length(seeds), replace = FALSE)), simplify = FALSE)
      perm_result <- lapply(perm_result, function(i) result$value > i$score)
      result$p.value <- 1 - rowSums(do.call(cbind, perm_result))/length(perm_result)
      
    }
    
  } else if ( method == "nns" ) {
    
    if (bootstrap) {
      
      result <- replicate(num_bootstraps, compute_nns_score(x = x, sample(seeds, size = num_seeds, replace = FALSE)), simplify = FALSE)
      result <- bind_rows(result) %>% group_by(instance) %>% summarize(value = mean(score), std.error = sd(score)) %>% ungroup()
      
    } else {
      
      result <- compute_nns_score(x = x, seeds = seeds) %>% select(instance, value = score)
      
    }
    
    if ( permute ){
      
      perm_result <- replicate(num_permutations, compute_nns_score(x = x, sample(rownames(x), size = length(seeds), replace = FALSE)), simplify = FALSE)
      perm_result <- lapply(perm_result, function(i) result$value > i$score)
      result$p.value <- 1 - rowSums(do.call(cbind, perm_result))/length(perm_result)
      
    }
    
  } else stop('method must be either "rw" or "nns"')
  
  # output
  return(result)
}

compute_rw_score <- function(x, seeds){
  
  # initialize vector of scores
  score <- matrix(rep(1/nrow(x),nrow(x)), nrow = nrow(x), ncol = 1)
  
  # anchor vector for seeds
  seed_anchor <- matrix(0, nrow = nrow(x), ncol = 1)
  seed_anchor[which(colnames(x) %in% seeds)] <- 1/length(seed_anchor)

  # closed-form solution
  score <- solve(diag(nrow(x)) - beta*x)%*%seed_anchor/length(seeds)
  score <- scale(score)[,1]
  
  # result
  result <- data.frame("instance" = names(score), "score" = unname(score))
  
  # out
  return(result)
}

compute_nns_score <- function(x, seeds){
  
# take column average of seed embeddings
seed_vec = matrix(Matrix::colMeans(matrix(x[seeds,], nrow = length(seeds), ncol = ncol(x))), nrow = 1)

# compute similarity
sim_vec <- text2vec::sim2(x = seed_vec, y = x, method = "cosine", norm = "l2")[1,]

# result
result <- data.frame("instance" = names(sim_vec), "score" = scale(unname(sim_vec))[,1])

# out
return(result)
}
