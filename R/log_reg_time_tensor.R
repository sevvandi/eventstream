log_reg_time_tensor <- function(data, b) {

    # data = cbind(t,X,Z) X is a nxm matrix and Z is a nx1 vector n is the number of observations m is the number of
    # attributes in X / or features b needs to be a vector of (T)x(m+1) +1 vector last entry of b is the penalty T
    # determined from the data you have dat.1 <- data[1:(dim(data)[1]-1),] lambda <- data[dim(data)[1],1] t <- dat.1[,1] X
    # <- dat.1[,2:(dim(dat.1)[2]-1)] Z <- dat.1[,dim(dat.1)[2]] dat = list(t,X,Z,lambda) dat = list(t,X3D,Z_allt,lambda,N)
    t <- unlist(data[1])
    X3D = data[[2]]
    Z_allt = data[[3]]
    lambda = unlist(data[4])
    N = unlist(data[5])

    mplus1 = dim(X3D)[3]
    len <- length(b)
    a <- b
    a <- matrix(a, ncol = mplus1)
    a3D = array(0, dim = c(dim(a)[1], dim(a)[1], mplus1))
    for (kk in 1:mplus1) {
        a3D[, , kk] <- diag(a[, kk])
    }

    X.Tensor <- tensorA::as.tensor(X3D)  # ,c(A=max.t.len,B=t.len,C=(m+1)))
    a.Tensor <- tensorA::as.tensor(a3D)
    Z_allt <- tensorA::as.tensor(Z_allt)  #,c(A=t.len,B=t.len,C=(m+1)))
    int.step.1 <- tensorA::mul.tensor(X.Tensor, c("I2", "I3"), a.Tensor, c("I1", "I3"))
    # max.fun.2 <- sum(Z_allt*int.step.1- log(1+exp(int.step.1)))*(-1/N) +
    # abs(lambda)*sum((diff(a)^2/(diff(sort(unique(t)))^2)))
    max.fun.2 <- sum(Z_allt * int.step.1 - log(1 + exp(int.step.1))) * (-1/N) + abs(lambda) * sum((diff(a)^2))
    return(max.fun.2)
}
