#' Simulate pre-post transition data from known LCA parameters
#'
#' Generates data from the latent class model with known parameters
#' for use in econometric validation tests.
#'
#' @param n Number of observations
#' @param lambdas Vector c(lgg, lgk, lkk) summing to 1
#' @param gamma Guessing probability
#' @param n_items Number of items (default 2 for compatibility with lca_se)
#' @return List with pre and post data frames
simulate_prepost_data <- function(n, lambdas, gamma, n_items = 2) {
  pre_list <- list()
  post_list <- list()

  for (item in seq_len(n_items)) {
    classes <- sample(1:3, n, replace = TRUE, prob = lambdas)

    pre <- numeric(n)
    post <- numeric(n)

    for (i in seq_len(n)) {
      if (classes[i] == 1) {
        pre[i] <- rbinom(1, 1, gamma)
        post[i] <- rbinom(1, 1, gamma)
      } else if (classes[i] == 2) {
        pre[i] <- rbinom(1, 1, gamma)
        post[i] <- 1
      } else {
        pre[i] <- 1
        post[i] <- 1
      }
    }

    pre_list[[paste0("item", item)]] <- pre
    post_list[[paste0("item", item)]] <- post
  }

  list(
    pre = as.data.frame(pre_list),
    post = as.data.frame(post_list)
  )
}

#' Simulate data with specific learning fraction
#'
#' @param n Number of observations
#' @param learning_frac Proportion who learn (lgk)
#' @param gamma Guessing probability
#' @param lkk Proportion who know both times (default 0.3)
#' @param n_items Number of items (default 2)
#' @return List with pre and post data frames
simulate_with_learning <- function(n, learning_frac, gamma, lkk = 0.3, n_items = 2) {
  lgk <- learning_frac
  lgg <- 1 - lkk - lgk

  if (lgg < 0) {
    stop("learning_frac + lkk must be <= 1")
  }

  simulate_prepost_data(n, c(lgg, lgk, lkk), gamma, n_items = n_items)
}

#' Simulate pre-post data with Don't Know responses
#'
#' @param n Number of observations
#' @param lambdas Vector of 7 parameters for DK model
#' @param gamma Guessing probability
#' @return List with pre and post data frames (character type with "d")
simulate_dk_prepost_data <- function(n, lambdas = NULL, gamma = 0.25) {
  if (is.null(lambdas)) {
    lambdas <- c(0.25, 0.15, 0.10, 0.10, 0.15, 0.10, 0.15)
  }

  classes <- sample(1:7, n, replace = TRUE, prob = lambdas)

  pre <- character(n)
  post <- character(n)

  for (i in seq_len(n)) {
    cl <- classes[i]
    if (cl == 1) {
      pre[i] <- ifelse(rbinom(1, 1, gamma) == 1, "1", "0")
      post[i] <- ifelse(rbinom(1, 1, gamma) == 1, "1", "0")
    } else if (cl == 2) {
      pre[i] <- ifelse(rbinom(1, 1, gamma) == 1, "1", "0")
      post[i] <- "1"
    } else if (cl == 3) {
      pre[i] <- ifelse(rbinom(1, 1, gamma) == 1, "1", "0")
      post[i] <- "d"
    } else if (cl == 4) {
      pre[i] <- "1"
      post[i] <- ifelse(rbinom(1, 1, gamma) == 1, "1", "0")
    } else if (cl == 5) {
      pre[i] <- "1"
      post[i] <- "1"
    } else if (cl == 6) {
      pre[i] <- "1"
      post[i] <- "d"
    } else {
      pre[i] <- "d"
      post[i] <- "d"
    }
  }

  list(
    pre = data.frame(item1 = pre, stringsAsFactors = FALSE),
    post = data.frame(item1 = post, stringsAsFactors = FALSE)
  )
}

#' Generate transition counts from known parameters
#'
#' @param n Number of observations
#' @param lambdas Vector c(lgg, lgk, lkk)
#' @param gamma Guessing probability
#' @return Named vector of transition counts (x00, x01, x10, x11)
generate_transition_counts <- function(n, lambdas, gamma) {
  lgg <- lambdas[1]
  lgk <- lambdas[2]
  lkk <- lambdas[3]
  g <- gamma

  probs <- numeric(4)
  probs[1] <- (1 - g) * (1 - g) * lgg
  probs[2] <- (1 - g) * g * lgg + (1 - g) * lgk
  probs[3] <- (1 - g) * g * lgg
  probs[4] <- g * g * lgg + g * lgk + lkk

  counts <- as.vector(rmultinom(1, n, probs))
  names(counts) <- c("x00", "x01", "x10", "x11")
  counts
}
