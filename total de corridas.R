
library(gtools)
library(png)
library(grid)
library(ggplot2)
y <- 8
subsets <- permutations(n= 2, r= y, v= c(1,0), repeats.allowed = TRUE)


corridas <- function(x) {
  n <- length(x)
  runs <- vector(mode = "numeric", length = y)
  k <- 1
  for (i in 1:n){
  for (i in 1:(n-k+1)) {
    if (all(x[i:(i+k-1)]==1)) runs[k] <- runs[k] + 1
  }
  k <- k+1
  }
  return(runs)
}


totalruns <- function(a) {
  total <- vector(mode = "numeric", length = y)
  i <- 1
  for (i in 1:nrow(a)) {
    total <- total + corridas(c(a[i, 1:y]))
  }
  return(total)
}
cada_corrida <- totalruns(subsets)
sum(cada_corrida)

corridas_total <- function(z) {
  total <- NULL
  i <- 1
  for (i in 1:length(z)) {
    total <- c(total, rep(i,z[i]))
  }
  return(total)
}

corridas_total(cada_corrida)
mean(corridas_total(cada_corrida))
mean(sample(corridas_total(cada_corrida), replace = TRUE))

simulacion <- function(n) {
  btrp <- vector(length = length(n))
  for (i in 1:n) {
  btrp[i] <- mean(sample(corridas_total(cada_corrida), replace = TRUE))
  }
  return(btrp)
}

dts <- round(simulacion(10000), digits = 2)

data <- as.data.frame(dts, rep("duración",10000))
ggplot(data, aes(x = dts)) +
geom_bar()  

result <- vector()
for(i in 1:10000) {
  result <- c(result, duración(sample(c(1,0), 30, replace = TRUE)))
}
mean(result)
