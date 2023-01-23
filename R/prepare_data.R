
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(testthat)


source("R/read_eurostat.R")
zz <- list.files("raw/eurostat_xlsx/", full.names = TRUE)
res <- invisible(lapply(zz, read_eurostat))
pp1 <- do.call("rbind", res)
pp2 <- pp1 %>% filter(str_length(code) != 4)

# Test that collected data have all the input codes--------

ess <- read.csv("raw/input_codes.csv")
ic <- as.integer(unique(pp1$code))

test_that("No codes are missing", {
  a <- setdiff(ic, ess$code)
  b <- setdiff(ess$code, ic)
  expect_true((length(a) == 0) & (length(b) == 0))
})

test_that("No duplication", {
  stt <- pp1 %>%
    count(code) %>%
    count(n)
  expect_true(nrow(stt) == 1)
})

# Test that 4 digit codes are not really necessary ---

pp1 %>%
  count(code, product) %>%
  select(-n) %>%
  arrange(code) %>%
  write.csv("meta/export_sanctions_codes.csv", row.names = FALSE)

cds <- read.csv("meta/export_sanctions_codes.csv", colClasses = c("character", "character"))

cds1 <- cds %>% mutate(header = substr(code, 1, 4))
cds1 %>% filter(code == header)

cd4 <- cds1 %>%
  filter(str_length(code) == 4) %>%
  .$code %>%
  unique()

hd4 <- cds1$header %>% unique()

test_that("All 4 digit codes have 8 digit codes counterparts", {
  ints <- intersect(cd4, hd4)
  a <- setdiff(ints, cd4)
  b <- setdiff(cd4, ints)

  expect_true((length(a) == 0) & (length(b) == 0))
})

test_that("8 digit codes cover more industries than 4 digit code present", {
  a <- hd4 %>% setdiff(cd4)
  expect_true(length(a) > 0)
})

## Write out the data

pp2 %>% arrow::write_parquet("raw/eu_export_sanctions_data.parquet")
