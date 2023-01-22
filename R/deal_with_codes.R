cds <- read.csv("meta/export_sanctions_codes.csv", colClasses = c("character", "character"))

cds1 <- cds %>% mutate(header = substr(code, 1, 4))
cds1 %>% filter(code == header)

cd4 <- cds1 %>%
  filter(str_length(code) == 4) %>%
  .$code %>%
  unique()
hd4 <- cds1$header %>% unique()
ints <- intersect(cd4, hd4)
setdiff(ints, cd4)
setdiff(cd4, ints)
