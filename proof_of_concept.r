x <- seq(-10,20,length=10000)

y <- function(x) {
  #output <- .5*x* sin(1/(.001*x)) + 8
  #output[(x <= 0)] <- 5
  #output <- -(x-5)^2
  #output <- -abs(x-6)
  #output <- tan(x)
  #output <- numeric(length(x)) + 2
  #output[(x < .4 & x > .3)] <- 20
  
  output <- numeric(length(x))
  output[(x < 1.5)] <- 3 
  output[(x < 1.452 & x > 1.4515)] <- 8.01
  output[(x < 1.6 & x >= 1.5)] <- 4
  output[(x < 1.56 & x > 1.55)] <- 8
  output[(x < 1.755 & x >= 1.75)] <- 7.9
  output[(x < 1.652 & x >= 1.65)] <- 8.009
  output[(x < 1.81 & x >= 1.7)] <- 7
  output[(x < 1.755 & x >= 1.75)] <- 7.99
  output[(x < 4 & x >= 2.5)] <- 6
  output[(x < 14 & x >= 10)] <- 4.5
  return(output)
}

plot(x,y(x),type="l")


max(y(x))


samp <- sample(x,100)

points(samp,y(samp),col=1,pch=20)

samp

ngen <- 10

combos <- (combn(samp,m = 2))
best_model <- numeric(ngen)
best_model[1] <- max(y(samp))
historical_generations <- as.data.frame(cbind("first_kid_x"=samp,y(samp),"gen"=1))
for (i in 2:ngen) {
first_kid_x <- numeric(ncol(combos))
for (colz in 1:ncol(combos)) {
  first_kid_x[colz] <- y(combos[1,colz]) / sum(y(combos[,colz])) * combos[1,colz] + y(combos[2,colz])/sum(y(combos[,colz])) * combos[2,colz] 
}

temp_df <- as.data.frame(cbind(first_kid_x,y(first_kid_x)))
best_of_breed <- head(temp_df[order(temp_df$V2,decreasing=TRUE),],length(samp)/2)
others <- tail(temp_df[order(temp_df$V2,decreasing=TRUE),],nrow(temp_df)-nrow(best_of_breed))[sample(1:(nrow(temp_df)-nrow(best_of_breed)),length(samp)/2),]

next_gen <- rbind(best_of_breed,others)

historical_generations <- rbind(historical_generations,cbind(next_gen,"gen"=i))

plot(x,y(x),type="l")
points(next_gen,col=i,pch=20)

best_model[i] <- max(next_gen$V2,na.rm=TRUE)

combos <- combn(next_gen[,1],m=2)
}

#plot(max(y(x))-best_model,type="l")

#best_model

#aggregate(historical_generations$V2~historical_generations$gen,FUN=median)

