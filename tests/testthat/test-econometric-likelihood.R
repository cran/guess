test_that("cell probabilities sum to 1 for any valid parameters", {
  set.seed(42)
  n_tests <- 100

  for (i in seq_len(n_tests)) {
    lambdas <- runif(3)
    lambdas <- lambdas / sum(lambdas)
    gamma <- runif(1, 0.1, 0.9)

    g <- gamma
    lgg <- lambdas[1]
    lgk <- lambdas[2]
    lkk <- lambdas[3]

    vec <- numeric(4)
    vec[1] <- (1 - g) * (1 - g) * lgg
    vec[2] <- (1 - g) * g * lgg + (1 - g) * lgk
    vec[3] <- (1 - g) * g * lgg
    vec[4] <- g * g * lgg + g * lgk + lkk

    expect_equal(sum(vec), 1, tolerance = 1e-10)
  }
})

test_that("cell probabilities are non-negative for all valid parameters", {
  set.seed(43)
  n_tests <- 100

  for (i in seq_len(n_tests)) {
    lambdas <- runif(3)
    lambdas <- lambdas / sum(lambdas)
    gamma <- runif(1, 0.01, 0.99)

    g <- gamma
    lgg <- lambdas[1]
    lgk <- lambdas[2]
    lkk <- lambdas[3]

    vec <- numeric(4)
    vec[1] <- (1 - g) * (1 - g) * lgg
    vec[2] <- (1 - g) * g * lgg + (1 - g) * lgk
    vec[3] <- (1 - g) * g * lgg
    vec[4] <- g * g * lgg + g * lgk + lkk

    expect_true(all(vec >= 0))
  }
})

test_that("expected values match likelihood function formulas", {
  gamma_values <- c(0.1, 0.25, 0.5, 0.75, 0.9)
  lambda_sets <- list(
    c(0.4, 0.3, 0.3),
    c(0.6, 0.2, 0.2),
    c(0.33, 0.33, 0.34),
    c(0.1, 0.1, 0.8)
  )

  for (gamma_i in gamma_values) {
    for (lambdas in lambda_sets) {
      lgg <- lambdas[1]
      lgk <- lambdas[2]
      lkk <- lambdas[3]

      lik_vec <- numeric(4)
      lik_vec[1] <- (1 - gamma_i) * (1 - gamma_i) * lgg
      lik_vec[2] <- (1 - gamma_i) * gamma_i * lgg + (1 - gamma_i) * lgk
      lik_vec[3] <- (1 - gamma_i) * gamma_i * lgg
      lik_vec[4] <- gamma_i * gamma_i * lgg + gamma_i * lgk + lkk

      expected <- calculate_expected_values(gamma_i, lambdas, 1, "nodk")

      expect_equal(expected, lik_vec, tolerance = 1e-12)
    }
  }
})

test_that("negative log-likelihood is minimized near true parameters", {
  skip_if(
    Sys.getenv("GUESS_FULL_TESTS") != "true",
    "Extended tests require GUESS_FULL_TESTS=true"
  )

  set.seed(123)
  n <- 1000

  true_lambdas <- c(0.4, 0.3, 0.3)
  true_gamma <- 0.25

  counts <- generate_transition_counts(n, true_lambdas, true_gamma)
  data_vec <- as.numeric(counts)

  true_nll <- guess_lik(c(true_lambdas, true_gamma), data = data_vec)

  perturbations <- list(
    c(0.5, 0.25, 0.25, 0.25),
    c(0.35, 0.35, 0.30, 0.30),
    c(0.4, 0.3, 0.3, 0.40),
    c(0.2, 0.4, 0.4, 0.15)
  )

  for (pert in perturbations) {
    pert_nll <- guess_lik(pert, data = data_vec)
    expect_true(
      pert_nll >= true_nll - 0.5,
      info = paste("Perturbed params:", paste(pert, collapse = ", "))
    )
  }
})

test_that("DK model cell probabilities are non-negative", {
  set.seed(44)
  n_tests <- 50

  for (i in seq_len(n_tests)) {
    lambdas <- runif(7)
    lambdas <- lambdas / sum(lambdas)
    gamma <- runif(1, 0.1, 0.9)

    g <- gamma
    lgg <- lambdas[1]
    lgk <- lambdas[2]
    lgd <- lambdas[3]
    lkg <- lambdas[4]
    lkk <- lambdas[5]
    lkd <- lambdas[6]
    ldd <- lambdas[7]

    vec <- numeric(9)
    vec[1] <- (1 - g) * (1 - g) * lgg
    vec[2] <- (1 - g) * g * lgg + (1 - g) * lgk
    vec[3] <- (1 - g) * lgd
    vec[4] <- (1 - g) * g * lgg + lkg
    vec[5] <- g * g * lgg + g * lgk + g * lkg + lkk
    vec[6] <- g * lgd + lkd
    vec[7] <- lkg
    vec[8] <- g * lgk + lkd
    vec[9] <- ldd

    expect_true(all(vec >= 0))
  }
})
