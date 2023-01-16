library(dplyr)
library(lubridate)
library(readxl)

fix_na <- function(x) {
  x[is.na(x)] <- 0
  x
}

read_eurostat1 <- function(xlsx_name, sheet) {
  meta <- read_xlsx(xlsx_name, sheet = sheet, n_max = 8)
  product_name <- pull(meta[which(pull(meta, 1) == "PRODUCT [PRODUCT]"), 3], 1)
  value_name <- "value"
  texp <- read_xlsx(xlsx_name, sheet = 3, skip = 8)
  texp1 <- texp %>%
    filter(!(TIME...1 %in% c("FREQ (Labels)", "PARTNER (Labels)", "Special value", ":"))) %>%
    filter(!is.na(TIME...1)) %>%
    rename(reporter = TIME...1, partner = TIME...2) %>%
    pivot_longer(cols = -(partner:reporter), names_to = "month", values_to = value_name) %>%
    filter(grepl("-", month)) %>%
    mutate(month = ymd(paste(month, "01", sep = "-"))) %>%
    arrange(partner, reporter, month) %>%
    mutate(across(.cols = all_of(value_name), .fns = as.numeric)) %>%
    mutate(across(.cols = all_of(value_name), .fns = fix_na)) %>%
    mutate(product = product_name)

  texp1
}

read_sanctions <- function(xlsx_name) {
  contents <- read_xlsx(xlsx_name, sheet = 1)
  no_sheets <- length(grep("Sheet", unlist(contents[, 2]), value = TRUE))
  res <- lapply(2 + 1:no_sheets, function(no) try(read_eurostat1(xlsx_name, sheet = no)))
  do.call(rbind, res)
}
