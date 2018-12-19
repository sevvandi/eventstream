grad <- function(data, b) {

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


    grad.p1 <- matrix(0, dim(a)[1], ncol = mplus1)
    grad.p2 <- matrix(0, dim(a)[1], ncol = mplus1)

    beta <- a  ## paras in matrix form
    t.len <- length(unique(t))

    ## grad.p1 is Gradient part 1 which comes from the penalty term
    grad.p1[1, ] <- 2 * lambda * (beta[1, ] - beta[2, ])
    grad.p1[t.len, ] <- 2 * lambda * (beta[t.len, ] - beta[(t.len - 1), ])
    beta_plus_1 <- beta[3:t.len, ]
    beta_minus_1 <- beta[1:(t.len - 2), ]
    grad.p1[2:(t.len - 1), ] <- 2 * lambda * (2 * beta[2:(t.len - 1), ] - beta_minus_1 - beta_plus_1)

    ## grad.p2 is Gradient part 2 which comes from the logistic regression term
    X.Tensor <- tensorA::as.tensor(X3D)  # ,c(A=max.t.len,B=t.len,C=(m+1)))
    a.Tensor <- tensorA::as.tensor(a3D)
    int.step.1 <- tensorA::mul.tensor(X.Tensor, c("I2", "I3"), a.Tensor, c("I1", "I3"))
    int.step.2 <- (tensorA::as.tensor(Z_allt) - exp(int.step.1)/(1 + exp(int.step.1))) * (-1/N)
    int.step.2 <- tensorA::as.tensor(int.step.2)
    # grad.p2[1,] <- apply(int.step.2,2,sum)


    int.step.3 <- int.step.2 * X.Tensor
    grad.p2 <- apply(int.step.3, c(2, 3), sum)
    # mul.tensor(int.step.2,c('I1'), X.Tensor,c('I1'))

    grad <- grad.p1 + grad.p2
    return(grad)
}
