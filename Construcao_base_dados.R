
base = read_parquet("caminho")
base$CAT_var_275 <- as.factor(ifelse(is.na(base$var_275), "C0",
                                   ifelse(base$var_275 == 1,"C1","C2")))


base <- base[, !names(base) %in% c("var_275", "var_309", "var_304")]
base$var_1 <- as.factor(base$var_1)
base$var_339=as.factor(base$var_339)
base$var_325=as.factor(base$var_325)
base$var_365 <- as.factor(base$var_365)

base[is.na(base)] <- 0

