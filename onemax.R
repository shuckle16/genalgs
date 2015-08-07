blend <- function(x) {
  blnd <- numeric(ncol(x))
  for (i in 1:ncol(x)) {
    blnd[i] <- sample(x[,i],size=1) 
  }
  return(blnd)
}

mutate <- function(x) {
  mt <- x
  mt_gene <- sample(1:length(x),size=1)
  if (mt[mt_gene] == 1) {
    mt[mt_gene] <- 0 
  } else {
    mt[mt_gene] <- 1
  }
  return(mt)
}

mat <- matrix(nrow=10,ncol=100)

for (i in 1:nrow(mat)) { 
  mat[i,] <- sample(c(0,1),ncol(mat),replace=TRUE,prob=c(.99,.01))  
}
original <- mat
hist(rowSums(mat))

#new_mat<- mat[order(rowSums(mat),decreasing=TRUE),]

nkids <- (nrow(mat)*(nrow(mat)-1))/2
kids <- matrix(nrow=nkids,ncol=100)
ngen <- 1000
historical_means <- numeric(ngen)

for (g in 1:ngen) {
  historical_means[g] <- mean(rowMeans(mat))
  for (j in 1:nkids) {
    parents <- mat[combn(1:10,2)[,j],]
    kids[j,] <- blend(parents)
    if (runif(1) < .05) {
      kids[j,] <- mutate(kids[j,])
    }
  }

  num_best <- nrow(mat)/2
  kids <- kids[order(rowSums(kids),decreasing=TRUE),]
  best_of_breed <- kids[1:num_best,]
  others <- kids[sample((num_best+1):nrow(kids),size = num_best),]

  next_gen <- rbind(best_of_breed,others)
  mat <- next_gen
}
plot(historical_means,type="l")
