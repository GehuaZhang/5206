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
