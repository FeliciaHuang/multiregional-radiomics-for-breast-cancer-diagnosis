library('lpSolve')
library('irr')

# inter-observer agreement ------------------------------------------------
icc12 <- matrix(nrow = 455,ncol = 1)
for (i in (1 : 455)){
  icc12[i]<- icc(data.frame(seg1[,i], seg2[,i]), "twoway")$value
}
mean(icc12[,1], na.rm = TRUE)
l1 <- length(which(icc12[,1] >= 0.8)) ;l1 
s1 <- length(which(icc12[,1] < 0.8)); s1


# inter-observer agreement ------------------------------------------------
icc13 <- matrix(nrow = 455,ncol = 1)
for (i in (1 : 455)){
  iccs13[i]<- icc(data.frame(seg1[,i], seg3[,i]), "twoway")$value
}
mean(icc13[,1], na.rm = TRUE)
l2 <- length(which(icc13[,1] >= 0.8)) ;l2
s2 <- length(which(icc13[,1] < 0.8)); s2


# histogram ---------------------------------------------------------------
hist12 <- hist(icc12$value, xlab = "ICC", main= "Histogram of intra-class correlation coefficients", col="lightblue", border="red",xlim = c(0, 1.0),ylim=c(0,460))
hist13 <- hist(icc13$value, xlab = "ICC", main= "Histogram of inter-class correlation coefficients", col="lightblue",border="red",xlim = c(0, 1.0),ylim=c(0,460))
