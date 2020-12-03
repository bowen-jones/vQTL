library(qtl2)

# load data and convert to appropriate type
stress <- qtl::read.cross(file = "fullvqtldata#3.csv", crosstype = "riself") # crosstype argument IMPERATIVE
stress <- convert2cross2(stress)

# impute w/ viterbi
out <- viterbi(stress, error_prob = 0)
write.csv(out, "viterbiout.csv")

# impute w/ sim_geno*
simout <- sim_geno(stress, error_prob = 0)
write.csv(simout, "simout.csv")

# impute w/ maxmarg*
max <- calc_genoprob(stress, error_prob = 0)
max <- maxmarg(max, minprob = 0)
write.csv(max, "maxmarg.csv")

#### Methods with asterisks preferred by Broman
### viterbi and sim_geno impute completely, but will edit original data unless `error_prob = 0`
## maxmarg can be forced to impute completely with `minprob = 0`



sum(is.na(prob$geno$`1`$data=="NA"))
prob$geno$`1`$data[is.na(prob$geno$`1`$data)] <- 2


prob$geno$`2`$data[is.na(prob$geno$`2`$data)] <- 2
prob$geno$`3`$data[is.na(prob$geno$`3`$data)] <- 2
prob$geno$`4`$data[is.na(prob$geno$`4`$data)] <- 2
prob$geno$`5`$data[is.na(prob$geno$`5`$data)] <- 2
prob$geno$`6`$data[is.na(prob$geno$`6`$data)] <- 2
prob$geno$`7`$data[is.na(prob$geno$`7`$data)] <- 2
prob$geno$`8`$data[is.na(prob$geno$`8`$data)] <- 2
prob$geno$`9`$data[is.na(prob$geno$`9`$data)] <- 2
prob$geno$`10`$data[is.na(prob$geno$`10`$data)] <- 2