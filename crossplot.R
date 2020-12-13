library(ggplot2)

# Create a dataset similar to linear regression summary
viz <- data.frame(mean = rnorm(3, mean = 15, sd = 3), var = rnorm(3, mean = 5, sd = .6), 
                  se_mean = rnorm(3, 1, .5), se_var = rnorm(3, .5, .2))

viz

ggplot(data = viz, aes(x = mean, y = var)) +  
  geom_pointrange(aes(ymin = var - se_var, ymax = var + se_var)) +
  geom_errorbarh(aes(xmax = mean + se_mean, xmin = mean - se_mean, height = 0))
