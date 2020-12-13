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

#### Methods with asterisks preferred by Broman; check ?sim_geno(), ?maxmarg(), ?viterbi() for more about underlying processes
### viterbi and sim_geno impute completely, but will edit original data unless `error_prob = 0`
## maxmarg can be forced to impute completely with `minprob = 0`, otherwise does not