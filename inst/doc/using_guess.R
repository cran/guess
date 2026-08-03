## ----eval = FALSE, cran_install-----------------------------------------------
# install.packages("guess")

## ----eval = FALSE, install----------------------------------------------------
# # install.packages("devtools")
# library(devtools)
# # devtools::install_github("soodoku/guess")

## ----eval = FALSE, stndcor----------------------------------------------------
# # Load library
# library(guess)
# 
# # Generate some data without DK
# pre_test <- data.frame(item1 = c(1, 0, 0, 1, 0), item2 = c(1, NA, 0, 1, 0))
# pst_test <- pre_test + cbind(c(0, 1, 1, 0, 0), c(0, 1, 0, 0, 1))
# lucky <- rep(.25, 2)
# 
# # Unadjusted Effect
# # Treating Don't Know as ignorance
# colMeans(nona(pst_test) - nona(pre_test))
# 
# # MCAR
# colMeans(pst_test - pre_test, na.rm = T)
# 
# # Adjusted Effect
# stnd_cor(pre_test, pst_test, lucky)

## ----eval = FALSE, transmat---------------------------------------------------
# # Without Don't Know
# pre_test_var <- c(1, 0, 0, 1, 0, 1, 0)
# pst_test_var <- c(1, 0, 1, 1, 0, 1, 1)
# print(transmat(pre_test_var, pst_test_var))
# 
# # With Don't Know
# pre_test_var <- c(1, 0, NA, 1, "d", "d", 0, 1, 0)
# pst_test_var <- c(1, 0, 1, "d", 1, 0, 1, 1, "d")
# print(transmat(pre_test_var, pst_test_var))

## ----eval = FALSE, guesstimate------------------------------------------------
# # Create example data for demonstration
# set.seed(123)
# nitems <- 10 # Number of knowledge questions
# npeople <- 100 # Number of respondents
# 
# # Generate simulated pre-test and post-test responses (0=incorrect, 1=correct)
# pre_test <- replicate(nitems, rbinom(npeople, 1, 0.4)) # 40% correct pre-test
# post_test <- replicate(nitems, rbinom(npeople, 1, 0.6)) # 60% correct post-test
# 
# transmatrix <- multi_transmat(pre_test, post_test)
# 
# res <- lca_cor(transmatrix)
# 
# round(res$params[, 1:4], 3)
# 
# round(res$learning[1:4], 3)
# 
# # LCA Correction with Don't Know responses
# # Generate data with Don't Know responses (coded as 2)
# pre_test_dk <- replicate(nitems, sample(c(0, 1, 2), npeople, replace = TRUE, prob = c(0.5, 0.35, 0.15)))
# post_test_dk <- replicate(nitems, sample(c(0, 1, 2), npeople, replace = TRUE, prob = c(0.4, 0.45, 0.15)))
# 
# transmatrix <- multi_transmat(pre_test_dk, post_test_dk, force9 = TRUE)
# res_dk <- lca_cor(transmatrix)
# 
# round(res_dk$params[, 1:4], 3)
# 
# round(res_dk$learning[1:4], 3)

## ----eval = FALSE, grp_adjust-------------------------------------------------
# pre_test_var <- data.frame(pre = c(1, 0, 1, 1, 0, "d", "d", 0, 1, NA, 0, 1, 1, 1, 1, 0, 0, "d", 0, 0))
# pst_test_var <- data.frame(pst = c(1, 0, NA, 1, "d", 1, 0, 1, 1, "d", 1, 1, 1, 0, 1, 1, 0, 1, 1, 0))
# grp <- c(rep(1, 10), rep(0, 10))
# 
# group_adj(pre_test_var, pst_test_var, gamma = 0, dk = 0)$learn
# group_adj(pre_test_var, pst_test_var, gamma = .25, dk = 0)$learn
# stnd_cor(pre_test_var, pst_test_var, lucky = .25)$learn
# 
# grp0_raw <- group_adj(subset(pre_test_var, grp == 0), subset(pst_test_var, grp == 0), gamma = 0, dk = 0)$learn
# grp1_raw <- group_adj(subset(pre_test_var, grp == 1), subset(pst_test_var, grp == 1), gamma = 0, dk = 0)$learn
# 
# grp0_adj <- group_adj(subset(pre_test_var, grp == 0), subset(pst_test_var, grp == 0), gamma = .25, dk = 0)$learn
# grp1_adj <- group_adj(subset(pre_test_var, grp == 1), subset(pst_test_var, grp == 1), gamma = .25, dk = 0)$learn
# 
# grp0_raw - grp1_raw
# grp0_adj - grp1_adj

## ----eval = FALSE, lca_err----------------------------------------------------
# # Raw
# # Generate some data without DK
# pre_test <- data.frame(item1 = c(1, 0, 0, 1, 0), item2 = c(1, NA, 0, 1, 0))
# pst_test <- pre_test + cbind(c(0, 1, 1, 0, 0), c(0, 1, 0, 0, 1))
# diff <- pst_test - pre_test
# stnd_err <- sapply(diff, function(x) sqrt(var(x, na.rm = T) / length(x)))
# 
# # Bootstrapped s.e.
# 
# # LCA model
# lca_stnd_err <- lca_se(pre_test, post_test, 10)
# sapply(lca_stnd_err, function(x) round(head(x, 1), 3))
# 
# lca_dk_stnd_err <- lca_se(pre_test_dk, post_test_dk, 10)
# sapply(lca_dk_stnd_err, function(x) round(head(x, 1), 3))

## ----eval = FALSE, fit_lca----------------------------------------------------
# fit <- fit_nodk(pre_test, post_test, res$params["gamma", ], res$params[c("gg", "gk", "kk"), ])
# 
# print(fit[, 1:4])
# 
# fit <- fit_dk(pre_test_dk, post_test_dk, res_dk$params["gamma", ], res_dk$params[c("gg", "gk", "gd", "kk", "dg", "dk", "dd"), ], force9 = TRUE)
# 
# print(fit[, 1:4])

## ----eval = FALSE, model_criticism--------------------------------------------
# # Generate data
# set.seed(42)
# nitems <- 10
# npeople <- 200
# 
# pre_test <- as.data.frame(replicate(nitems, rbinom(npeople, 1, 0.4)))
# post_test <- as.data.frame(replicate(nitems, pmin(1, pre_test +
#   replicate(nitems, rbinom(npeople, 1, 0.25)))))
# names(pre_test) <- names(post_test) <- paste0("item", 1:nitems)
# 
# # Fit model using convenience wrapper
# fit <- item_lca_fit(pre_test, post_test)
# 
# # Or equivalently:
# # transmatrix <- multi_transmat(pre_test, post_test)
# # fit <- lca_cor(transmatrix)

## ----eval = FALSE, perplexity_items-------------------------------------------
# transmatrix <- multi_transmat(pre_test, post_test)
# 
# # Overall perplexity
# perplexity_items(fit, transmatrix)
# 
# # Per-item perplexity
# sapply(1:nitems, function(i) perplexity_items(fit, transmatrix, item = i))

## ----eval = FALSE, perplexity_individuals-------------------------------------
# # Overall perplexity (averaged across individuals)
# perplexity_individuals(fit, pre_test, post_test)
# 
# # Per-individual perplexity (identify poorly-fit respondents)
# ind_ppl <- perplexity_individuals(fit, pre_test, post_test, per_individual = TRUE)
# hist(ind_ppl, main = "Distribution of Individual Perplexity")

## ----eval = FALSE, cv_items---------------------------------------------------
# transmatrix <- multi_transmat(pre_test, post_test)
# cv_result <- cv_items(transmatrix, k = 5, seed = 123)
# 
# cv_result$perplexity # Held-out perplexity
# cv_result$se # Standard error across folds
# cv_result$fold_results # Per-fold details

## ----eval = FALSE, cv_individuals---------------------------------------------
# cv_result <- cv_individuals(pre_test, post_test, k = 5, seed = 123)
# 
# cv_result$perplexity # Held-out perplexity
# cv_result$se # Standard error across folds
# cv_result$fold_results # Per-fold details

## ----eval = FALSE, model_comparison-------------------------------------------
# # True parameters (if known from simulation)
# true_params <- c(0.4, 0.3, 0.3, 0.25)
# 
# # Misspecified parameters
# bad_params <- c(0.1, 0.1, 0.8, 0.5)
# 
# transmatrix <- multi_transmat(pre_test, post_test)
# 
# # Compare: lower perplexity = better fit
# perplexity_items(true_params, transmatrix)
# perplexity_items(bad_params, transmatrix)
# perplexity_items(fit, transmatrix) # Fitted model should be close to true

## ----eval = FALSE, simulate_lca-----------------------------------------------
# # Simulate data with known learning rate of 30%
# sim <- simulate_lca(
#   n = 500, # 500 individuals
#   n_items = 2, # 2 test items
#   gg = 0.35, # 35% in guess->guess (stable ignorance)
#   gk = 0.30, # 30% in guess->know (LEARNED)
#   kk = 0.35, # 35% in know->know (stable knowledge)
#   gamma = 0.25, # 25% chance of guessing correctly
#   seed = 123
# )
# 
# # Fit model and check recovery
# fit <- item_lca_fit(sim$pre, sim$post)
# fit$params["gk", ] # Should be close to 0.30 (true learning rate)

## ----eval = FALSE, simulate_dk------------------------------------------------
# # Simulate DK data with known parameters
# sim_dk <- simulate_lca_dk(
#   n = 500,
#   n_items = 1,
#   gg = 0.25, gk = 0.15, gd = 0.10,
#   kk = 0.20, dg = 0.10, dk = 0.10,
#   dd = 0.10, gamma = 0.25,
#   seed = 456
# )
# 
# # Fit DK model
# fit_dk <- item_lca_fit(sim_dk$pre, sim_dk$post)
# fit_dk$params["gk", ] # Should be close to 0.15

## ----eval = FALSE, validate_recovery------------------------------------------
# # Run 100 simulations to assess parameter recovery
# results <- validate_recovery(
#   true_params = c(gg = 0.35, gk = 0.30, kk = 0.35, gamma = 0.25),
#   n = 500, # Sample size per simulation
#   n_items = 2, # Number of items
#   n_sims = 100, # Number of simulations
#   seed = 789
# )
# 
# # View results
# print(results)
# # Shows: bias, RMSE, standard error, and coverage for each parameter

## ----eval = FALSE, sample_size_effect-----------------------------------------
# # Compare parameter recovery across sample sizes
# sample_sizes <- c(100, 250, 500, 1000)
# true_params <- c(gg = 0.35, gk = 0.30, kk = 0.35, gamma = 0.25)
# 
# results_by_n <- lapply(sample_sizes, function(n) {
#   validate_recovery(true_params, n = n, n_items = 2, n_sims = 100, seed = 123)
# })
# 
# # Combine into summary table
# efficiency_table <- do.call(rbind, lapply(seq_along(sample_sizes), function(i) {
#   r <- results_by_n[[i]]
#   data.frame(
#     n = sample_sizes[i],
#     parameter = r$parameter,
#     bias = round(r$bias, 4),
#     rmse = round(r$rmse, 4),
#     coverage = round(r$coverage_95, 2)
#   )
# }))
# 
# # Display results for the learning parameter (gk)
# gk_results <- efficiency_table[efficiency_table$parameter == "gk", ]
# print(gk_results)

## ----eval = FALSE, items_effect-----------------------------------------------
# # Compare recovery with varying number of items
# n_items_values <- c(1, 2, 5, 10)
# true_params <- c(gg = 0.35, gk = 0.30, kk = 0.35, gamma = 0.25)
# 
# results_by_items <- lapply(n_items_values, function(ni) {
#   validate_recovery(true_params, n = 500, n_items = ni, n_sims = 50, seed = 456)
# })
# 
# # Combine results
# items_table <- do.call(rbind, lapply(seq_along(n_items_values), function(i) {
#   r <- results_by_items[[i]]
#   data.frame(
#     n_items = n_items_values[i],
#     parameter = r$parameter,
#     rmse = round(r$rmse, 4),
#     coverage = round(r$coverage_95, 2)
#   )
# }))
# 
# # Show gamma results (most affected by number of items)
# gamma_results <- items_table[items_table$parameter == "gamma", ]
# print(gamma_results)

## ----eval = FALSE, parameter_scenarios----------------------------------------
# # Test different true parameter configurations
# scenarios <- list(
#   balanced = c(gg = 0.35, gk = 0.30, kk = 0.35, gamma = 0.25),
#   high_learning = c(gg = 0.20, gk = 0.50, kk = 0.30, gamma = 0.25),
#   low_learning = c(gg = 0.50, gk = 0.10, kk = 0.40, gamma = 0.25),
#   high_guessing = c(gg = 0.35, gk = 0.30, kk = 0.35, gamma = 0.40)
# )
# 
# scenario_results <- lapply(scenarios, function(params) {
#   validate_recovery(params, n = 500, n_items = 2, n_sims = 50, seed = 789)
# })
# 
# # Compare learning (gk) recovery across scenarios
# gk_comparison <- data.frame(
#   scenario = names(scenarios),
#   true_gk = sapply(scenarios, function(p) p["gk"]),
#   bias = sapply(scenario_results, function(r) round(r$bias[r$parameter == "gk"], 4)),
#   rmse = sapply(scenario_results, function(r) round(r$rmse[r$parameter == "gk"], 4)),
#   coverage = sapply(scenario_results, function(r) round(r$coverage_95[r$parameter == "gk"], 2))
# )
# print(gk_comparison)

