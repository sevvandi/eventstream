predict_td_logistic <- function(model, dat, pr) {
    # b, XX, Z, t dat = cbind(t,X)

    t <- dat[, 1]
    ord.t <- order(t)
    t <- t[ord.t]
    X <- dat[ord.t, 2:dim(dat)[2]]
    if (is.null(dim(X))) {
        m = 1
    } else {
        m = dim(X)[2]
    }


    mod.scale <- model$scale
    mod.mean <- mod.scale[, 1]
    mod.sd <- mod.scale[, 2]
    mod.t <- model$t
    X <- scale(X, center = mod.mean, scale = mod.sd)

    t.new <- rep(0, length(t))
    for (km in 1:length(model$t)) {
        if (km == 1) {
            t.new[t <= mod.t[km]] <- mod.t[km]
        } else {
            t.new[(t <= mod.t[km]) & (t > mod.t[(km - 1)])] <- mod.t[km]
        }
    }
    big.inds <- which(t > max(model$t))
    if (length(big.inds) > 0) {
        t.new[big.inds] <- max(model$t)
    }
    t <- t.new

    b <- model$par
    len <- length(b)
    a.ori <- b
    a.ori <- matrix(a.ori, ncol = m + 1)

    a <- a.ori

    t.len <- length(unique(t))
    t.each.len <- table(t)
    max.t.len = max(t.each.len)
    # X_allt = matrix(0,nrow=max.t.len, ncol=t.len)
    preds.count = 0
    # a0.rm <- t(repmat(a0, 1, max.t.len) )


    # X_allt = matrix(0,nrow=max.t.len, ncol=t.len)
    X3D = array(0, dim = c(max.t.len, t.len, (m + 1)))
    a3D = array(0, dim = c(t.len, t.len, (m + 1)))

    for (kk in 1:(m + 1)) {
        if (kk == 1) {
            for (j in 1:t.len) {
                X3D[1:t.each.len[j], j, 1] <- 1
            }
            ## Inserted to check if all time stamps are present in test model
            if (dim(a3D)[1] == dim(a)[1]) {
                a3D[, , 1] <- diag(a[, 1])
            } else {
                ## You need to match the times
                inds <- which(model$t %in% unique(t))
                a3D[, , 1] <- diag(a[inds, 1])
            }

        } else {
            for (j in 1:t.len) {
                ## edited previously it was X[t==unique(t)[j],(kk-1)]
                X3D[1:t.each.len[j], j, kk] <- X[t == sort(unique(t))[j], (kk - 1)]
            }
            if (dim(a3D)[1] == dim(a)[1]) {
                a3D[, , kk] <- diag(a[, kk])
            } else {
                ## You need to match the times
                inds <- which(model$t %in% unique(t))
                a3D[, , kk] <- diag(a[inds, kk])
            }

            # a3D[,,kk] <- diag(a[,kk])
        }
    }

    # X_allt = matrix(0,nrow=max.t.len, ncol=t.len) int.step <- a0.rm for(kk in 1:m){ for(j in 1:t.len ){
    # X_allt[1:t.each.len[j],j] <- X[t==unique(t)[j],kk] } ## Prediction step int.step <- int.step +
    # X_allt%*%diag(a[,(kk+1)]) }
    int.step <- array(0, dim = c(max.t.len, t.len, (m + 1)))
    int.step[] <- abind::abind(lapply(1:dim(X3D)[3], function(i) X3D[, , i] %*% a3D[, , i]), along = 3)
    int.step.1 <- apply(int.step, c(1, 2), function(x)sum(x, na.rm=TRUE))

    probs <- (exp(int.step.1))/(1 + exp(int.step.1))
    rr <- which(int.step.1>500)
    probs[rr] <- 1

    preds.temp <- round(probs)
    preds <- rep(0, length(t))
    pred.probs <- rep(0, length(t))
    start <- 1
    for (jj in 1:t.len) {
        end <- start + t.each.len[jj] - 1
        preds[start:end] <- preds.temp[1:t.each.len[jj], jj]
        pred.probs[start:end] <- probs[1:t.each.len[jj], jj]
        start <- end + 1
    }
    preds.out <- rep(0, length(t))
    probs.out <- rep(0, length(t))
    preds.out[ord.t] <- preds
    probs.out[ord.t] <- pred.probs
    # preds.out <- preds
    if (pr) {
        return(probs.out)
    } else {
        return(preds.out)
    }
}
