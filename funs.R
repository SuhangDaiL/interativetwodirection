genTable <- function(data, v1, v2, value = NULL, fun = sum, na.rm = TRUE) {
    
    # Check! ------------------------
    
    # check v1, v2 length
    if (length(v1) != 1 | length(v2) != 1) {
        stop("check v1 and v2  length = 1?")
    }
    # v1 and v2 are two column's of data
    if (!v1 %in% names(data) | !v2 %in% names(data) | v1 == v2) {
        stop("check your names")
    }
    # data has to be data.frame
    if (!is.data.frame(data)) {
        warning("data is not dataframe and we change it")
    }
    data <- data.frame(data)
    # v1, v2 has to be factor
    if (!is.factor(data[, v1])) {
        warning("v1 is not a factor and we change it to the factor")
        data[, v1] <- factor(data[, v2])
    }
    if (!is.factor(data[, v2])) {
        warning("v2 is not a factor and we change it to the factor")
        data[, v2] <- factor(data[, v2])
    }
    v1.levels <- levels(data[, v1])
    v2.levels <- levels(data[, v2])
    
    # Default value! -----------------------------------------
    
    # if na.rm is true, function will remove the rows value is null
    if (na.rm) {
        data <- na.omit(data)
    }
    # value = null
    if (is.null(value)) {
        value <- "count"
        data[, value] <- 1
    }
    
    # Outputs --------------------------------------------------
    eval(parse(text = paste0("table <- data %>%", "group_by(", v1, "=", v1, ",", v2, "=", v2, ") %>%", 
        "summarise(", value, "=", "fun(", value, "))", sep = "")))
    output <- data.frame(rep(v1.levels, each = length(v2.levels)), rep(v2.levels, length(v1.levels)))
    names(output) <- c(v1, v2)
    output <- dplyr::left_join(output, data.frame(table), by = c(v1, v2))
    levels(output[, v1]) <- v1.levels
    levels(output[, v2]) <- v2.levels
    output[is.na(output[,value]), value] = 0
    return(output)
}

combineTable <- function(data, v1, v2, value = NULL,
                         name = c("V1", "V2", "value", "G1", "G2")) {
  data = data.frame(data)
  output <- data.frame()
  for (i in 1:length(v1)){
    for (j in 1:length(v2)){
      x <- data.frame(genTable(data, v1[i], v2[j], value), v1[i],v2[j]) %>%
        mutate_if(.predicate = is.factor, .funs = as.character)
      output <- bind_rows(output, setNames(x, name))
    }
  }
  return(output)
}

# Test --------------------------------------------------------------------

library(dplyr)
v1 = "tv1"
v2 = "tv2"
v3 = "tv3"
v4 = "tv4"
fun = sum
v1.levels = c("a", "b", "c")
v2.levels = c("b", "d")
v3.levels = c("e", "a")
v4.levels = c("b", "n")
n = 100
value = "tvalue"
mydata = data.frame(tv1 = factor(sample(v1.levels, n, replace = TRUE), levels = v1.levels),
           tv2 = factor(sample(v2.levels, n, replace = TRUE), levels = v2.levels),
           tv3 = factor(sample(v3.levels, n, replace = TRUE), levels = v3.levels),
           tv4 = factor(sample(v4.levels, n, replace = TRUE), levels = v4.levels))
mydata[mydata$tv1 == "a",] = NA
p <- combineTable(mydata, v1 = c(v1, v2), v2 = c(v3, v4))

