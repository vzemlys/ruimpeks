library(dplyr)
library(lubridate)
library(readxl)

fix_na <- function(x) {
  x[is.na(x)] <- 0
  x
}

read_eurostat_sheet <- function(xlsx_name, sheet, flow = FALSE) {
  meta <- read_xlsx(xlsx_name, sheet = sheet, n_max = 8)
  product_name <- pull(meta[which(pull(meta, 1) == "PRODUCT [PRODUCT]"), 3], 1)
  value_name <- "value"

  texp <- read_xlsx(xlsx_name, sheet = sheet, skip = 8)
  texp1 <- texp %>%
    filter(!(TIME...1 %in% c("FREQ (Labels)", "REPORTER (Labels)", "Special value", ":"))) %>%
    filter(!is.na(TIME...1)) %>%
    rename(reporter = TIME...1, partner = TIME...2) %>%
    pivot_longer(cols = -(partner:reporter), names_to = "month", values_to = value_name) %>%
    filter(grepl("-", month)) %>%
    mutate(month = ymd(paste(month, "01", sep = "-"))) %>%
    arrange(partner, reporter, month) %>%
    mutate(across(.cols = all_of(value_name), .fns = as.numeric)) %>%
    mutate(across(.cols = all_of(value_name), .fns = fix_na)) %>%
    mutate(product = product_name)

  if (flow) {
    flow_name <- tolower(pull(meta[which(pull(meta, 1) == "FLOW"), 3], 1))
    texp1 <- texp1 %>% mutate(flow = flow_name)
  }
  texp1
}

read_eurostat <- function(xlsx_name, flow = FALSE) {
  contents <- read_xlsx(xlsx_name, sheet = 1)
  no_sheets <- length(grep("Sheet", unlist(contents[, 2]), value = TRUE))
  res <- lapply(2 + 1:no_sheets, function(no) try(read_eurostat_sheet(xlsx_name, sheet = no, flow = flow)))
  pp <- do.call(rbind, res)
  oo <- str_split_fixed(pp$product, fixed("["), 2)
  pp1 <- pp %>%
    select(-product) %>%
    mutate(product = str_trim(oo[, 1]), code = gsub("]", "", oo[, 2]))
  pp1
}
