library(dplyr)

rm(list = ls())
# generate Dataset --------------------------------------------------------

v1 <- c("a","b","c") # v1 has to be a factor level
v2 <- c("d","e") # v2 has to be a factor level

if (is.null(v2)){
  v2 <- v1
}

mydata <- data.frame(f1 = sample(v1,100, replace = TRUE),
           f2 = sample(v2,100, replace = TRUE),
           value = rnorm(100))
mydata$f1 <- factor(mydata$f1, levels = v1, ordered = TRUE)
mydata$f2 <- factor(mydata$f2, levels = v2, ordered = TRUE)


# generate Table ----------------------------------------------------------

genTable <- function(data, v1, v2, value = NULL, fun = sum, na.rm = TRUE){
  
  # Check! ------------------------
  # check v1, v2 length
  if (length(v1) != 1 | length(v2) != 1){
    stop("check v1 and v2  length = 1?")
  }
  # v1 and v2 are two column's of data
  if (!v1 %in% names(data) | !v2 %in% names(data) | v1 == v2){
    stop("check your names")
  }
  # data has to be data.frame
  if (!is.data.frame(data)){
    warning("data is not dataframe and we change it")
    data <- data.frame(data)
  }
  # v1, v2 has to be factor
  if (!is.factor(data[,v1])){
    warning("v1 is not a factor and we change it to the factor")
    data[,v1] <- factor(data[,v2])
  }
  if (!is.factor(data[,v2])){
    warning("v2 is not a factor and we change it to the factor")
    data[,v2] <- factor(data[,v2])
  }
  v1.levels <- levels(data[,v1])
  v2.levels <- levels(data[,v2])
  
  # Default value! -----------------------------------------
  # if na.rm is true, function will remove the rows value is null
  if (na.rm){
    data <- na.omit(data) 
  }
  # value = null
  if (is.null(value)){
    value = "count"
    data[,value] <- 1
  }
  
  # Outputs --------------------------------------------------
  eval(parse(text = paste0("table <- data %>%",
    "group_by(", v1, "=", v1, ",", v2, "=", v2, ") %>%",
    "summarise(", value, "=", "fun(", value,"))", sep = "")))
  output <- data.frame(rep(v1.levels, each = length(v2.levels)),
                      rep(v2.levels, length(v1.levels)))
  names(output) <- c(v1,v2)
  output <- dplyr::left_join(output, data.frame(table), by = c(v1,v2))
  levels(output[,v1]) = v1.levels
  levels(output[,v2]) = v2.levels
  return(output)
}
fun <- mean # a function of numeric!

mytable1 <- mydata %>%
  group_by(f1 = f1) %>%
  summarise(value = fun(value))
mytable2 <- mydata %>%
  group_by(f2 = f2) %>%
  summarise(value = fun(value))

# Basic Analysis --------------------------------------------


