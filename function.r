# For regular expression
regEx <- function(data,exp){
  logic <- grepl(data, pattern = exp)
  match <- gregexpr(pattern = exp, text = data[logic])
  temp <- regmatches(data[logic], match)
  return(temp)
}

# Drop last n terms of a string
# eg. subStrRight("ABCDE",2) => "ABC"
subStrRight <- function(str, n){
  temp<-substr(str,1, nchar(str)-n)
  return(temp)
}

# Drop first m and last n terms of a string
# eg. subStrMid("ABCDE",2,1) => "CD"
subStrMid <- function(str,m,n){
  temp<-substr(str,m,nchar(str)-n)
}



#KNN
library(ISLR)
KNN.decision <-function(Lag1.new, Lag2.new, K = 5,Lag1 = Smarket$Lag1,Lag2 = Smarket$Lag2,Dir = Smarket$Direction) {
  n <-length(Lag1)
  stopifnot(length(Lag2)==n,length(Lag1.new)==1,length(Lag2.new)==1, K<=n)
  dists <-sqrt((Lag1-Lag1.new)^2+(Lag2-Lag2.new)^2)
  neighbors <-order(dists)[1:K]
  neighb.dir <- Dir[neighbors]
  choice <- names(which.max(table(neighb.dir)))
  return(choice)}

KNN.decision(Lag1.new=2,Lag2.new=4.25)


KNN.regression<-function(Year.new,K = 5,Year.train = Congress_train$Year,Congress.train = Congress_train$Rating) {
  n <-length(Year.train)
  stopifnot(length(Year.train)==n,length(Congress.train)==n,length(Year.new)==1, K<=n)
  dists <-sqrt((Year.train-Year.new)^2)
  neighbors <-order(dists)[1:K]
  neighb.Congress <- Congress.train[neighbors]
  return(mean(neighb.Congress ))}

KNN.regression(Year.new=2000)

```
