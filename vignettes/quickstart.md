Quick Start Guide
================

## Installing the package

``` r
devtools::install_github("prodriguezsosa/labelProp")
```

## Load package

``` r
library(labelProp)

# other libraries used in this guide
library(dplyr)
library(quanteda)
library(conText)
```

# Preliminaries

`labelProp` offers a semi-supervised approach to labeling text data,
leveraging vector representations of text and network methods. The
general problem `labelProp` solves is the following: given a collection
of nodes –these can be words, short texts etc.– for which we have vector
representations –importantly, on the same space–, including a small
subset for which we have labels –which we call seeds–, how do we
determine the labels of the remaining set of nodes? Currently
`labelProp` offers two methods:

1.  **Nearest neighbors (nns)**: each unlabeled node is assigned a score
    based on its mean cosine similarity to seed nodes. This algorithm
    requires vector representations for the full set of nodes, labeled
    and unlabeled.
2.  **Random walk (rw)**: each unlabeled node is assigned a score based
    on the probability of a random walk from the seed set hitting that
    node. This algorithm is adapted from [Zhou et al.
    (2004)](https://proceedings.neurips.cc/paper/2003/file/87682805257e619d49b8e0dfdc14affa-Paper.pdf)
    and [Hamilton et al.](2016). This algorithm requires a transition
    matrix be estimated from the vector representations for the full set
    of nodes, labeled and unlabeled. For very large node sets
    (\(10^6\)), this can be computationally intensive in which case
    users should use the `nns` algorithm. The advantage of `rw` over
    `nns` is that…

# Data

`labelProp` comes with three datasets used in the examples below:

1.  A (data.frame) **anes2016** with open-ended responses to the “most
    important issues facing the country” question in ANES 2016.
2.  A set of (GloVe) **pre-trained embeddings** corresponding to the
    full set of features (with min. freq. of 5) appearing in the
    open-ended responses.
3.  A **transformation matrix** estimated by [Khodak et
    al.](https://github.com/NLPrinceton/ALaCarte) for the Stanford GloVe
    300 embeddings.

# Labeling words using seed words

Dictionary-based approaches are popular in the social sciences. They are
transparent and easy to implement. However, while there are plenty of
off-the-shelf dictionaries available –e.g.
[LIWIC](https://lit.eecs.umich.edu/geoliwc/liwc_dictionary.html)–, they
are often ill-suited to the corpus and/or concept of interest. On the
other hand, building a dictionary from scratch however labor-intensive
task. Given a small set of seed dictionary terms, `labelProp` scores
unlabeled terms, giving users a principled way of “expanding” their
locally-tailored dictionaries.

To use the `rw` algorithm we first build a transition matrix using our
set of pre-trained vector-representations as input. This can be done
using the `labelProp::build_transition_matrix()` function which is a
wrapper around `parallelDist`, allowing for parallelization. `beta` is
the only other function parameter in `labelProp` specific to the `rw`
algorithm, it specifies the extent to which the algorithm favors local
(similar labels for neighbors) vs. global (correct labels on seed nodes)
consistency. In practice we’ve found results to be fairly robust to the
specification of `beta`. To use the `nns` algorithm, users must provide
a set of vector representations, one for each node. The user must then
provide a set of labeled nodes –`seeds`– in the form of a named list.

Users can futher choose to compute standard errors for the assigned
scores using a bootstrapping procedure –the algorithm is replicated
`num_bootstrap` times using a sample proportion –`prop_seeds`– of the
seeds. The users can also choose to normalize scores by the sum of the
scores across the various classes – this is useful if for examle we are
building a sentiment dictionary and are looking to rank words according
to their polarity score. This can be done using the `softmax` parameter.
Finally, the user can specify whether to limit the number of nodes that
are output. If `N` is defined, then the top `N` scoring nodes for each
class are output, otherwise the output will include the full set of
nodes with their respective scores. Note, the unlabeled nodes with the
highest scores will likely be different depending on the algorithm. It
will often make sense to use both – for example when it is combined
human selection of words.

``` r
# to use the random-walkd algorithm we first build a transition matrix
transition_matrix <- build_transition_matrix(x = anes2016_glove, threads = 6L)

# define seeds (labeled nodes), if list is unlabeled, 'class1', 'class2' etc.
# will be used as labels
seeds = list(immigration = c("immigration", "immigrants", "immigrant"), economy = c("jobs", 
    "unemployment", "wages"))

# propagate label using rw
rw_labels <- labelProp(x = transition_matrix, seeds = seeds, method = "rw", N = 10, 
    beta = 0.5, as_list = TRUE)

# propagate label using nns, notice the main input, x, are the vector
# representations
nns_labels <- labelProp(x = anes2016_glove, seeds = seeds, method = "nns", N = 10, 
    as_list = TRUE)

# check output for economy
rw_labels[["economy"]]
```

    ## # A tibble: 10 × 3
    ##    node       class   score
    ##    <chr>      <chr>   <dbl>
    ##  1 wage       economy  5.50
    ##  2 employment economy  5.38
    ##  3 incomes    economy  5.08
    ##  4 salaries   economy  5.06
    ##  5 unemployed economy  4.99
    ##  6 workforce  economy  4.81
    ##  7 inflation  economy  3.97
    ##  8 workers    economy  3.91
    ##  9 stagnant   economy  3.79
    ## 10 benefits   economy  3.73

``` r
nns_labels[["economy"]]
```

    ## # A tibble: 10 × 3
    ##    node       class   score
    ##    <chr>      <chr>   <dbl>
    ##  1 employment economy  4.36
    ##  2 wage       economy  4.16
    ##  3 benefits   economy  3.59
    ##  4 salaries   economy  3.59
    ##  5 incomes    economy  3.52
    ##  6 workers    economy  3.51
    ##  7 job        economy  3.46
    ##  8 unemployed economy  3.25
    ##  9 labor      economy  3.17
    ## 10 economy    economy  3.02

``` r
# check output for immigration
rw_labels[["immigration"]]
```

    ## # A tibble: 10 × 3
    ##    node         class       score
    ##    <chr>        <chr>       <dbl>
    ##  1 undocumented immigration  7.08
    ##  2 migrants     immigration  6.70
    ##  3 illegals     immigration  6.07
    ##  4 deported     immigration  5.25
    ##  5 deportation  immigration  5.17
    ##  6 citizenship  immigration  4.91
    ##  7 latinos      immigration  4.48
    ##  8 illegal      immigration  4.38
    ##  9 aliens       immigration  4.06
    ## 10 migration    immigration  4.05

``` r
nns_labels[["immigration"]]
```

    ## # A tibble: 10 × 3
    ##    node         class       score
    ##    <chr>        <chr>       <dbl>
    ##  1 undocumented immigration  5.12
    ##  2 migrants     immigration  5.10
    ##  3 illegal      immigration  4.34
    ##  4 citizenship  immigration  3.91
    ##  5 deportation  immigration  3.41
    ##  6 citizens     immigration  3.40
    ##  7 illegals     immigration  3.34
    ##  8 deported     immigration  3.33
    ##  9 latinos      immigration  3.07
    ## 10 welfare      immigration  3.06

# Document scoring

`labelProp` is not limited to words, indeed it can be applied to any set
of inputs for which we have or can estimate vector representations. In
the two examples that follow we use
[`conText`](https://github.com/prodriguezsosa/conText) together with a
subset of [Stanford GloVe](https://nlp.stanford.edu/projects/glove/)
(300 dimensional) embeddings and their corresponding transformation
matrix –estimated by [Khodak et
al.](https://github.com/NLPrinceton/ALaCarte)– to embed our short ANES
open-ended responses.

## Labeling documents using seed words

Since these document embeddings lie on the same space as the our set of
pre-trained word embeddings, we can use our seed terms to score
documents according to their relevance to each class.

``` r
# tokenize documents
anes_toks <- unique(anes2016$response) %>% tokens()

# embed documents using conText
doc_vs <- anes_toks %>% dfm %>% dem(pre_trained = anes2016_glove, transform = TRUE, 
    transform_matrix = khodakA, verbose = FALSE) %>% as.matrix()

# seed word vectors (from pre-trained embeddings)
word_vs <- anes2016_glove[unlist(seeds), ]

# combine word and document vectors into a single matrix of node vectors
node_vs <- rbind(doc_vs, word_vs)

# transition matrix
transition_matrix <- build_transition_matrix(x = node_vs, threads = 6L)

# label propagate
rw_labels <- labelProp(x = transition_matrix, seeds = seeds, method = "rw", N = 10, 
    beta = 0.5, as_list = TRUE)

# output
rw_labels[["economy"]]
```

    ## # A tibble: 12 × 3
    ##    node     class   score
    ##    <chr>    <chr>   <dbl>
    ##  1 text6220 economy  4.75
    ##  2 text1999 economy  4.74
    ##  3 text6295 economy  4.58
    ##  4 text1499 economy  4.56
    ##  5 text4185 economy  4.36
    ##  6 text5907 economy  4.30
    ##  7 text5819 economy  4.24
    ##  8 text4437 economy  4.20
    ##  9 text488  economy  4.14
    ## 10 text145  economy  4.11
    ## 11 text1117 economy  4.11
    ## 12 text7133 economy  4.11

``` r
rw_labels[["immigration"]]
```

    ## # A tibble: 10 × 3
    ##    node     class       score
    ##    <chr>    <chr>       <dbl>
    ##  1 text681  immigration  6.27
    ##  2 text4024 immigration  6.27
    ##  3 text4771 immigration  6.27
    ##  4 text4937 immigration  6.27
    ##  5 text7523 immigration  6.27
    ##  6 text2855 immigration  6.21
    ##  7 text474  immigration  6.15
    ##  8 text4350 immigration  5.95
    ##  9 text5143 immigration  5.52
    ## 10 text7738 immigration  5.42

``` r
# combine with texts
tokens_subset(anes_toks, docid(anes_toks) %in% rw_labels[["economy"]]$node) %>% sapply(function(x) paste(x, 
    collapse = " "))
```

    ##                         text145                         text488 
    ##                  "unemployment" "economy job rate unemployment" 
    ##                        text1117                        text1499 
    ##       "nanosecond unemployment"        "unemployment low wages" 
    ##                        text1999                        text4185 
    ##              "employment wages"        "economy wages and jobs" 
    ##                        text4437                        text5819 
    ##   "jobs unemployment rate down"     "livable wage jobs economy" 
    ##                        text5907                        text6220 
    ##         "unemployment and jobs"                         "wages" 
    ##                        text6295                        text7133 
    ##        "jobs unemployment rate"       "widespread unemployment"

``` r
tokens_subset(anes_toks, docid(anes_toks) %in% rw_labels[["immigration"]]$node) %>% 
    sapply(function(x) paste(x, collapse = " "))
```

    ##                               text474                               text681 
    ##                           "immigrant"                    "eagle immigrants" 
    ##                              text2855                              text4024 
    ## "immigration undocumented immigrants"                 "unneeded immigrants" 
    ##                              text4350                              text4771 
    ##      "immigration illegal immigrants"   "immigrants resisting assimilation" 
    ##                              text4937                              text5143 
    ##                          "immigrants"     "decriminalization to immigrants" 
    ##                              text7523                              text7738 
    ##                   "vetted immigrants"         "immigrants not assimilating"

## Labeling documents using seed words

We can also use embedded documents –for which we know their label– as
seeds.

``` r
# documents as seeds
doc_seeds <- list(immigration = c("text4771", "text4024"), economy = c("text2010", 
    "text5925"))

# transition matrix
transition_matrix <- build_transition_matrix(x = doc_vs, threads = 6L)

# label propagate
rw_labels <- labelProp(x = transition_matrix, seeds = doc_seeds, method = "rw", N = 10, 
    beta = 0.5, as_list = TRUE)

# output
rw_labels[["economy"]]
```

    ## # A tibble: 12 × 3
    ##    node     class   score
    ##    <chr>    <chr>   <dbl>
    ##  1 text4075 economy  3.89
    ##  2 text23   economy  3.88
    ##  3 text6673 economy  3.71
    ##  4 text2535 economy  3.64
    ##  5 text4175 economy  3.62
    ##  6 text761  economy  3.55
    ##  7 text2161 economy  3.51
    ##  8 text2495 economy  3.51
    ##  9 text5917 economy  3.47
    ## 10 text1095 economy  3.44
    ## 11 text1612 economy  3.44
    ## 12 text2469 economy  3.44

``` r
rw_labels[["immigration"]]
```

    ## # A tibble: 10 × 3
    ##    node     class       score
    ##    <chr>    <chr>       <dbl>
    ##  1 text681  immigration 10.2 
    ##  2 text4937 immigration 10.2 
    ##  3 text7523 immigration 10.2 
    ##  4 text2855 immigration  7.17
    ##  5 text7738 immigration  6.83
    ##  6 text6872 immigration  6.79
    ##  7 text5143 immigration  6.68
    ##  8 text586  immigration  6.67
    ##  9 text4350 immigration  6.49
    ## 10 text4188 immigration  6.16

``` r
# merge with texts
tokens_subset(anes_toks, docid(anes_toks) %in% rw_labels[["economy"]]$node) %>% sapply(function(x) paste(x, 
    collapse = " "))
```

    ##                                                                                                                                     text23 
    ##                                                                                                   "jobs keeping jobs in the united states" 
    ##                                                                                                                                    text761 
    ##                                                                                                                        "economy more jobs" 
    ##                                                                                                                                   text1095 
    ##                                                                                                                             "economy jobs" 
    ##                                                                                                                                   text1612 
    ##                                                                                                                 "economy jobs instability" 
    ##                                                                                                                                   text2161 
    ## "the economy is unstable small business is paralyzed and afraid to invest unemployment is much worse than the federal statistics reported" 
    ##                                                                                                                                   text2469 
    ##                                                                                                                             "jobs economy" 
    ##                                                                                                                                   text2495 
    ##                                                                                                     "economy jobs are leaving the country" 
    ##                                                                                                                                   text2535 
    ##                                                                                "many problems first the economy not enough jobs in the us" 
    ##                                                                                                                                   text4075 
    ##                                                                                                                "jobs in the united states" 
    ##                                                                                                                                   text4175 
    ##                                                                                                              "economy jobs going overseas" 
    ##                                                                                                                                   text5917 
    ##                                                                                                             "economy too much bureaucracy" 
    ##                                                                                                                                   text6673 
    ##                                                                                                                  "economy keep jobs in us"

``` r
tokens_subset(anes_toks, docid(anes_toks) %in% rw_labels[["immigration"]]$node) %>% 
    sapply(function(x) paste(x, collapse = " "))
```

    ##                                          text586 
    ##                             "illegal immigrants" 
    ##                                          text681 
    ##                               "eagle immigrants" 
    ##                                         text2855 
    ##            "immigration undocumented immigrants" 
    ##                                         text4188 
    ##           "undocumented immigrants and the wall" 
    ##                                         text4350 
    ##                 "immigration illegal immigrants" 
    ##                                         text4937 
    ##                                     "immigrants" 
    ##                                         text5143 
    ##                "decriminalization to immigrants" 
    ##                                         text6872 
    ## "illegal immigrants and immigrants already here" 
    ##                                         text7523 
    ##                              "vetted immigrants" 
    ##                                         text7738 
    ##                    "immigrants not assimilating"
