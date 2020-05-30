
#1 
divide <- function(x){
  if ( x %% 3 == 0 & x %% 5 == 0 ){return("Divisible")}
  else if (x %% 5 == 0){ return("Divisible5")}
  else if (x %% 3 == 0){ return("Divisible3")}
  else {return(x)} }

divide(9) # 被3整除

divide(20) # 被5整除

divide(15) # 被3、5整除

divide(16) # 不被3、5整除

results <- c()
for (i in 1:100){
  results[i] <- divide(i)
}

#2

X <- runif(40, min = 0, max = 100)
ε <- runif(40, min = 0, max = 2)
Y = X + ε 
Y <- ifelse(round(Y) > 100 , 100 , round(Y))

score <- function(x) {
  CI_1 <- mean(x) - qnorm(0.025)*sd(x)/sqrt(length(x))
  CI_2 <- mean(x) + qnorm(0.025)*sd(x)/sqrt(length(x))
  avg <- mean(x)
  return( data.frame( 
          "CI_95%" = paste0("[" , round(CI_1,3) , "," , round(CI_2,3) , "]"),
          "average" = avg ))
}
score(Y)

sd(x)