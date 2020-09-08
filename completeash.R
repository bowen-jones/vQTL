install.packages("dglm"); 
install.packages("ashr"); 
library(dglm);
library(ashr);

setwd("/work/07644/tg869432/stampede2/vQTL/")

df.breed <- read.csv(file = "simdata3.csv")
df.breed <- df.breed[-c(1,2),]

mod = dglm(stress ~ ., dformula = ~., data = df.breed)

dglm.out<-summary(mod)$coefficients
t1<-ash(dglm.out[,1],dglm.out[,2])$result
results <- cbind(t1[,1:4],dglm.out[,4]);
names(results)[5] <- "dglm.p.value"

write.csv(results, "completeash.csv")