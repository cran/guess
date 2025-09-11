## ----eval = FALSE, cran_install-----------------------------------------------
# install.packages("guess")

## ----eval = FALSE, install----------------------------------------------------
# # install.packages("devtools")
# library(devtools)
# #devtools::install_github("soodoku/guess")

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
# nitems <- 10  # Number of knowledge questions
# npeople <- 100  # Number of respondents
# 
# # Generate simulated pre-test and post-test responses (0=incorrect, 1=correct)
# pre_test <- replicate(nitems, rbinom(npeople, 1, 0.4))  # 40% correct pre-test
# post_test <- replicate(nitems, rbinom(npeople, 1, 0.6))  # 60% correct post-test
# 
# transmatrix <- multi_transmat(pre_test, post_test)
# 
# res <- lca_cor(transmatrix)
# 
# round(res$param.lca[,1:4], 3)
# 
# round(res$est.learning[1:4], 3)
# 
# # LCA Correction with Don't Know responses
# # Generate data with Don't Know responses (coded as 2)
# pre_test_dk <- replicate(nitems, sample(c(0,1,2), npeople, replace=TRUE, prob=c(0.5, 0.35, 0.15)))
# post_test_dk <- replicate(nitems, sample(c(0,1,2), npeople, replace=TRUE, prob=c(0.4, 0.45, 0.15)))
# 
# transmatrix <- multi_transmat(pre_test_dk, post_test_dk, force9 = TRUE)
# res_dk <- lca_cor(transmatrix)
# 
# round(res_dk$param.lca[,1:4], 3)
# 
# round(res_dk$est.learning[1:4], 3)

## ----eval = FALSE, grp_adjust-------------------------------------------------
# pre_test_var <- data.frame(pre=c(1, 0, 1, 1, 0, "d", "d", 0, 1, NA, 0, 1, 1, 1, 1, 0, 0, 'd', 0, 0))
# pst_test_var <- data.frame(pst=c(1, 0, NA, 1, "d", 1, 0, 1, 1, "d", 1, 1, 1, 0, 1, 1, 0, 1, 1, 0))
# grp = c(rep(1, 10), rep(0, 10))
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
# 
# # Raw
# # Generate some data without DK
# pre_test <- data.frame(item1 = c(1, 0, 0, 1, 0), item2 = c(1, NA, 0, 1, 0))
# pst_test <- pre_test + cbind(c(0, 1, 1, 0, 0), c(0, 1, 0, 0, 1))
# diff <- pst_test - pre_test
# stnd_err <-  sapply(diff, function(x) sqrt(var(x, na.rm = T)/length(x)))
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
# fit <- fit_nodk(pre_test, post_test, res$param.lca[4, ], res$param.lca[1:3, ])
# 
# print(fit[, 1:4])
# 
# fit <- fit_dk(pre_test_dk, post_test_dk, res_dk$param.lca[8, ], res_dk$param.lca[1:7, ], force9 = TRUE)
# 
# print(fit[, 1:4])

