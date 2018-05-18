data <- read.csv("Rui Qin", header = TRUE)
total = dim(data)[1]
cat("coverage", (1 - sum(is.na(data$PRC))/total),"\n")
data.1 <- data[which(data$PRC!=is.na(data$PRC)),]
data.new <- data.1[which(data.1$PRC>0),]
permno <- c(unique(data.new$PERMNO))
med <- c()
for (i in 1:length(permno)){
  permno.price <- c(data.new[which(data.new$PERMNO==unique(data.new$PERMNO)[i]),7])
  data.ratio1 <- c()
  for (j in 1:length(permno.price)){
    data.ratio1[j] <- permno.price[j+1]/permno.price[j]
  }
  data.ratio <- data.ratio1[1:(length(data.ratio1)-1)]
  med[i] = median(data.ratio)
}
A <- as.data.frame(cbind(permno,med))
A$rank <- rank(-A$med)
X <- A[which(A$rank<=20),]
a <- c(X$permno)
R <- c()
for (i in 1:20){
  return <- data.new[which(data.new$PERMNO==a[i]),]
  R[i] <- (return$PRC[length(return$PRC)]/return$PRC[i])/20
}
cat("for 20 is", (sum(R)-1), "\n")
A <- as.data.frame(cbind(permno,med))
A$rank <- rank(-A$med)
X <- A[which(A$rank<=10),]
a <- c(X$permno)
R <- c()
for (i in 1:10){
  return <- data.new[which(data.new$PERMNO==a[i]),]
  R[i] <- (return$PRC[length(return$PRC)]/return$PRC[i])/10
}
cat("for 10 is", (sum(R)-1), "\n")
A <- as.data.frame(cbind(permno,med))
A$rank <- rank(-A$med)
X <- A[which(A$rank<=30),]
a <- c(X$permno)
R <- c()
for (i in 1:30){
  return <- data.new[which(data.new$PERMNO==a[i]),]
  R[i] <- (return$PRC[length(return$PRC)]/return$PRC[i])/30
}
cat("for 30 is", (sum(R)-1), "\n")