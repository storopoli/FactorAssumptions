## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----bfi, message=FALSE--------------------------------------------------
library(FactorAssumptions, quietly = T, verbose = F)
bfi_data <- bfi
#Remove rows with missing values and keep only complete cases
bfi_data <- bfi_data[complete.cases(bfi_data),]
head(bfi_data)

## ----KMO-----------------------------------------------------------------
kmo_bfi <- kmo_optimal_solution(bfi_data, squared = FALSE)

## ----removed_kmo---------------------------------------------------------
kmo_bfi$removed

## ----communalities-------------------------------------------------------
comm_bfi <- communalities_optimal_solution(kmo_bfi$df, type = "principal", nfactors = 7, squared = FALSE)

## ----removed_comm--------------------------------------------------------
comm_bfi$removed

## ----final_solution------------------------------------------------------
comm_bfi$results

