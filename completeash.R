install.packages("dglm"); 
install.packages("ashr"); 
library(dglm);
library(ashr);

setwd("/work/07644/tg869432/stampede2/vQTL/")

df.breed <- read.csv(file = "simdata3.csv")

mod = dglm(stress ~ ., dformula = ~., data = df.breed)

mod.test<-summary(mod[[1]])$coefficients;
t1<-ash(mod.test[,1],mod.test[,2])$result;
results <- cbind(t1[,1:4],mod.test[,4]);

write.csv(mod.test, "dglmcoeffs.csv")
write.csv(results, "completeash.csv")
