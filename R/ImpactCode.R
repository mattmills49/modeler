smoother_binomial <- function(x, y){
  group_names <- sort(unique(x))
  group_counts <- vapply(group_names, function(gr) sum(x == gr), numeric(1))
  group_yes <- vapply(group_names, function(gr) sum(y[x == gr]), numeric(1))
  group_probs <- group_yes / group_counts
  
  params <- MASS::fitdistr(group_probs, densfun = "beta", list(shape1 = 1, shape2 = 1))
  a <- params$estimate[1]
  b <- params$estimate[2]
  
  posterior <- (group_yes + a) / (a + b + group_counts)
  names(posterior) <- group_names
  return(posterior)
}

smoother_gaussian <- function(x, y){
  group_names <- sort(unique(x))
  group_counts <- vapply(group_names, function(gr) sum(x == gr), numeric(1))
  
  group_probs <- vapply(group_names, function(gr) mean(y[x == gr]), numeric(1))
  
  y_var <- var(y)
  y_prob <- mean(y)
  
  group_var <- vapply(group_names, function(gr) var(y[x == gr]), numeric(1))
  smoother <- rep(0, length(group_names))
  names(smoother) <- group_names
  for(gr in group_names){
    smoother[gr] <- y_var / (group_var[gr] / group_counts[gr] +  y_var)
  }
  posterior <- smoother * group_probs + (1 - smoother) * y_prob
}

smoother_weights <- function(x, y, m){
  group_names <- sort(unique(x))
  group_counts <- vapply(group_names, function(gr) sum(x == gr), numeric(1))
  group_yes <- vapply(group_names, function(gr) sum(y[x == gr]), numeric(1))
  group_probs <- group_yes / group_counts
  n <- length(x)
  
  smoother <- group_counts / (m + group_counts) 
  posterior <- smoother * group_probs + (1 - smoother) * sum(y == 1) / n
  names(posterior) <- group_names
}


categorical_transformation <- function(x, y, smoother_fun, output = "vector", ...){
  # Emperical Bayes Method taken from here: http://helios.mm.di.uoa.gr/~rouvas/ssi/sigkdd/sigkdd.vol3.1/barreca.pdf
  stopifnot(class(x) %in% c("character", "numeric"))
  stopifnot(length(unique(y)) == 2)
  if(class(y) == "character"){
    y <- 1 * (y == y[1])
  } else if(class(y) == "logical"){
    y <- 1 * y
  } else if(class(y) != "numeric") stop("y must be a character, numeric, or logical vector")
  stopifnot(all(!is.na(x)))
  stopifnot(all(!is.na(y)))
  stopifnot(output %in% c("vector", "data.frame"))
  
  posterior <- smoother_fun(x, y, ...)

  if(output == "vector") return(posterior[as.character(x)])
  if(output == "data.frame") return(data.frame(Group = names(posterior), Probabilities = unname(posterior), stringsAsFactors = F))
}

hierarchical_transformation <- function(x, y, higher_group){
  
}

sections <- sample(c(101:120, 201:220, 301:320), size = 100, replace = T)
levels <- stringr::str_sub(sections, 1, 1)
y <- rbinom(n = 100, size = 1, prob = sections / 500)

smoother_binomial(levels, y)
