## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----bfi-----------------------------------------------------------------
library(FactorAssumptions, quietly = T, verbose = F)
bfi_data <- bfi
#Remove rows with missing values and keep only complete cases
bfi_data <- bfi_data[complete.cases(bfi_data),]
head(bfi_data)

## ----KMO-----------------------------------------------------------------
kmo_bfi <- kmo_optimal_solution(bfi_data)

## ----removed_kmo---------------------------------------------------------
kmo_bfi$removed

## ----how_many_factors----------------------------------------------------
how_many_factors(bfi_data)

## ----communalities-------------------------------------------------------
comm_bfi <- communalities_optimal_solution(kmo_bfi$df, nfactors = 7)

## ----removed_comm--------------------------------------------------------
comm_bfi$removed

## ----final_solution------------------------------------------------------
comm_bfi$pca_results

