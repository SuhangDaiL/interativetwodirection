library(dplyr)

rm(list = ls())
# generate Dataset
# --------------------------------------------------------

v1 <- c("a", "b", "c")  # v1 has to be a factor level
v2 <- c("d", "e")  # v2 has to be a factor level

if (is.null(v2)) {
    v2 <- v1
}

mydata <- data.frame(f1 = sample(v1, 
    100, replace = TRUE), f2 = sample(v2, 
    100, replace = TRUE), value = rnorm(100))
mydata$f1 <- factor(mydata$f1, levels = v1, 
    ordered = TRUE)
mydata$f2 <- factor(mydata$f2, levels = v2, 
    ordered = TRUE)



fun <- mean  # a function of numeric!

mytable1 <- mydata %>% group_by(f1 = f1) %>% 
    summarise(value = fun(value))
mytable2 <- mydata %>% group_by(f2 = f2) %>% 
    summarise(value = fun(value))

# Basic Analysis
# --------------------------------------------


