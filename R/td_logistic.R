#' Classification with incomplete-event-classifier
#'
#' This function does classification of incomplete events. The events grow with time. The input vector \code{t} denotes the age of the event. The classifier takes the growing event features, \code{X} and combines with a \code{L2} penalty for smoothness.
#' @param t The age of events.
#' @param X The event features.
#' @param Y The class labels. \code{Y} needs to be binary output.
#' @param lambda The penalty coefficient. Default is 1.
#' @param scale If \code{TRUE}, each column of \code{X} is scaled to zero mean and standard deviation 1.
#' @param num_bins The number of time slots to use.
#' @param quad If \code{TRUE}, the squared attributes \code{X^2} are included.
#' @param interact if \code{TRUE}, the most relevant interactions are included.
#' @param logg If \code{TRUE} logarithms  of positive attributes will be computed.
#' @return A list with following components:
#'   \item{\code{par}}{The parameters of the incomplete-event-classifier, after its fitted.}
#'   \item{\code{convergence}}{The difference between the final two output values.}
#'   \item{\code{scale}}{If \code{scale=TRUE}, contains the mean and the standard deviation of each column of \code{X}.}
#'   \item{\code{t}}{The age of events \code{t} is split into bins. This list element contains the   boundary values of the bins.}
#'   \item{\code{quad}}{The value of \code{quad} in arguments.}
#'   \item{\code{interact}}{The value of \code{interact} in arguments.}
#'
#'
#' @examples
#' # Generate data
#' N <- 1000
#' t <- sort(rep(1:10, N))
#' set.seed(821)
#' for(kk in 1:10){
#'   if(kk==1){
#'      X <- seq(-11,9,length=N)
#'   }else{
#'      temp <- seq((-11-kk+1),(9-kk+1),length=N)
#'      X <- c(X,temp)
#'   }
#' }
#' real.a.0 <- seq(2,20, by=2)
#' real.a.1 <- rep(2,10)
#' Zstar <-real.a.0[t] + real.a.1[t]*X + rlogis(N, scale=0.5)
#' Z <- 1*(Zstar > 0)
#'
#' # Plot data for t=1 and t=8
#' oldpar <- par(mfrow=c(1,2))
#' plot(X[t==1],Z[t==1], main="t=1 data")
#' abline(v=-1, lty=2)
#' plot(X[t==8],Z[t==8],main="t=8 data")
#' abline(v=-8, lty=2)
#' par(oldpar) 
#'
#' # Fit model
#' model_td <- td_logistic(t,X,Z)
#'
#' @seealso \code{\link{predict_tdl}} for prediction.
#' @export



td_logistic <- function(t, X, Y, lambda = 1, scale = TRUE, num_bins=4, quad = TRUE, interact = FALSE, logg=TRUE) {

    X <- as.data.frame(X)

    if(logg){
      min_x <- apply(X,2,min)
      pos_cols_1 <- which(min_x > 0)
      sd_x <- apply(X,2,sd)
      sd_r <- sd_x/median(sd_x)
      sd_cols <- which(sd_r>100)
      pos_cols <- intersect(pos_cols_1,sd_cols)
      X[,pos_cols] <- log(X[,pos_cols])
    }else{
      pos_cols <- NULL
    }


    ord.t <- order(t)
    t <- t[ord.t]
    X <- X[ord.t, ]
    Z <- Y[ord.t]
    X1 <- X

    if (quad) {
        Xsquared <- X1^2
        if(!is.null(dim(X))){
          colnames(Xsquared) <- paste("Sq_", colnames(X1), sep="")
        }
        X <- cbind(X1, Xsquared)
    }


    max_cols <- 5
    if (interact) {
      # --- Inserted stepAIC part --- Begin
      if(dim(X1)[2]>max_cols){
        mm <-  model.matrix(~.^2-1, X1)
        colnames(mm) <- gsub("`", "", colnames(mm), fixed = TRUE)
        cor_vals <- cor(mm,Z)
        ord <- order(abs(cor_vals), decreasing=TRUE)
        # get the 5 most correlated variables
        sel_vars <- rep("A", max_cols)
        pp <- 1
        qq <- 1
        while(qq  <= max_cols){
          col_name <- rownames(cor_vals)[pp]
          if( grepl( ":", col_name) ){
            xx <- strsplit(col_name, ":")
            col1 <- xx[[1]][1]
            col2 <- xx[[1]][2]
            if(sum(sel_vars %in%  col1)==0 ){
              sel_vars[qq] <- col1
              qq <- qq +1
            }
            if(qq <= max_cols){
              if(sum(sel_vars %in%  col2)==0 ){
                sel_vars[qq] <- col2
                qq <- qq +1
              }
            }
          }
          pp <- pp + 1
        }




        X3 <- X1[,sel_vars]
        X2 <- cbind.data.frame(X3,Z)

      }else{
        X2 <- cbind.data.frame(X1,Z)
      }

      fit_1  <- suppressWarnings( glm(Z~.^2, family="binomial", data=X2) )
      best_fit <- suppressWarnings( MASS::stepAIC(fit_1, trace=FALSE) )
      coefs_names <- names(best_fit$coefficients)
      coefs_names <- gsub("`", "", coefs_names, fixed = TRUE)
      inds <- which(grepl( ":", coefs_names))
      combos <- matrix(0,nrow=2, ncol=1)
      if(length(inds)>0){
        for(gg in 1:length(inds)){
          xx <- strsplit(coefs_names[inds[gg]], ":")
          col1 <- xx[[1]][1]
          col2 <- xx[[1]][2]
          c1 <- which(colnames(X)==col1)
          c2 <- which(colnames(X)==col2)
          if(gg==1){
            combos[,1]<- c(c1,c2)
          }else{
            combos <- cbind(combos, c(c1,c2))
          }
        }
      }
      # --- Inserted stepAIC wise part --- End

      if(dim(combos)[2]>1){ # there are actually some combinations
        for (i in 1:dim(combos)[2]) {
          prod <- apply(X[, combos[, i]], 1, function(x) x[1] * x[2])
          X <- cbind(X, prod)
        }
      }

    }else{
      combos<-NULL
    }

    pp <- hist(t,breaks=num_bins, plot = FALSE)  # hist(t,breaks=5,plot=FALSE) #
    ## changed - do not want t=0
    t.unique <- pp$breaks[2:length(pp$breaks)]  # pp$breaks[1:length(pp$breaks)]
    t.new <- rep(0, length(t))
    for (km in 1:length(t.unique)) {
        if (km == 1) {
            t.new[t <= t.unique[km]] <- t.unique[km]
        } else {
            t.new[(t <= t.unique[km]) & (t > t.unique[(km - 1)])] <- t.unique[km]
        }
    }
    t <- t.new

    t.len <- length(unique(t))
    if (is.null(dim(X))) {
        m = 1
        if (scale) {
            mean.x <- mean(X)
            sd.x <- sd(X)
            X <- unitize(X)
        } else {
            mean.x <- 0
            sd.x <- 1
        }
    } else {
        m <- dim(X)[2]
        if (scale) {
            mean.x <- apply(X, 2, mean)
            sd.x <- apply(X, 2, sd)
            X <- apply(X, 2, unitize)
        } else {
            mean.x <- rep(0, dim(X)[2])
            sd.x <- rep(1, dim(X)[2])
        }
    }



    ### Put the glm in a try catch
    cof.1 <- tryCatch({
        mod.1 <- glm(as.factor(Z) ~ ., family = binomial, data = data.frame(X))
        cof.1 <- mod.1$coefficients
        # return(cof.1)
    }, warning = function(w) {
        #message(w)
        msg <- conditionMessage(w)
        #print("\n")
        # if (grepl("converge", msg)) {
        #     print("Proceeding with glmnet")
        #     mod.1 <- glmnet::cv.glmnet(as.matrix(X), as.factor(Z), family = "binomial", alpha = 0)
        #     cof.1 <- coef(mod.1, s = 0.01)
        #     return(cof.1)
        # } else {
        #    mod.1 <- glm(as.factor(Z) ~ ., family = binomial, data = data.frame(X))
        #     cof.1 <- mod.1$coefficients
        #     return(cof.1)
        # }
        print("Proceeding with glmnet")
        mod.1 <- glmnet::cv.glmnet(as.matrix(X), as.factor(Z), family = "binomial", alpha = 0)
        cof.1 <- coef(mod.1, s = 0.01)
        return(cof.1)
    }, error = function(e) {
        #message(e)
        print("Proceeding with glmnet")
        mod.1 <- glmnet::cv.glmnet(as.matrix(X), as.factor(Z), family = "binomial", alpha = 0)
        cof.1 <- coef(mod.1, s = 0.01)  #'lambda.min')
        return(cof.1)
    })



    # cof.1 <- tryCatch({ coef(mod.1,s='lambda.min') },warning=function(w){ mod.1$coefficients },error=function(e){
    # mod.1$coefficients })
    cof <- apply(t(matrix(cof.1)), 2, function(x) rep(x, t.len))

    # mod.1 <- glm(as.factor(Z)~.,family=binomial, data=data.frame(X)) cof <-
    # apply(t(matrix(mod.1$coefficients)),2,function(x) rep(x,t.len))
    paras <- c(as.vector(cof))  # initial parameters from normal logistic regression

    b <- paras
    # m = dim(X)[2]
    len <- length(b)
    a <- b
    a <- matrix(a, ncol = (m + 1))

    t.len <- length(unique(t))
    t.each.len <- table(t)
    max.t.len = max(t.each.len)
    Z_allt = matrix(0, nrow = max.t.len, ncol = t.len)

    for (j in 1:t.len) {
        Z_allt[1:t.each.len[j], j] <- Z[t == unique(t)[j]]
    }

    N = length(Z)
    X3D = array(0, dim = c(max.t.len, t.len, (m + 1)))
    a3D = array(0, dim = c(t.len, t.len, (m + 1)))

    if (m > 1) {
        for (kk in 1:(m + 1)) {
            a3D[, , kk] <- diag(a[1, kk], t.len)  # diag(a[,kk]) changed to diag(a[1,kk],t.len)
            if (kk == 1) {
                for (j in 1:t.len) {
                  X3D[1:t.each.len[j], j, 1] <- 1
                }
            } else {
                for (j in 1:t.len) {
                  X3D[1:t.each.len[j], j, kk] <- X[t == unique(t)[j], (kk - 1)]
                }
            }
        }
    } else {
        for (kk in 1:(m + 1)) {
            a3D[, , kk] <- diag(a[1, kk], t.len)  # diag(a[,kk]) changed to diag(a[1,kk],t.len)
            if (kk == 1) {
                for (j in 1:t.len) {
                  X3D[1:t.each.len[j], j, 1] <- 1
                }
            } else {
                for (j in 1:t.len) {
                  X3D[1:t.each.len[j], j, kk] <- X[t == unique(t)[j]]
                }
            }
        }
    }

    # dat = rbind(cbind.data.frame(t,X,Z), lambda)
    dat = list(t, X3D, Z_allt, lambda, N)

    optim.out <- optim(par = paras, log_reg_time_tensor, gr = grad, method = "BFGS", control = list(maxit=500), data = dat)

    paras <- optim.out$par
    out1 <- log_reg_time_tensor(dat, paras)


    # for (i in 1:num.t) {
    #     optim.out <- optim(paras, log_reg_time_tensor, gr = grad, method = "BFGS", data = dat)
    #     paras <- optim.out$par
    #     out2 <- log_reg_time_tensor(dat, paras)
    #     print(out2)
    #     if (i != num.t) {
    #         out1 <- out2
    #     }
    # }

    model <- list()
    model$par <- paras
    model$convergence <- optim.out$convergence #abs(out2 - out1)
    model$scale <- cbind(mean.x, sd.x)
    model$t <- sort(unique(t))
    model$quad <- quad
    model$interact <- interact
    model$combos <- combos
    model$logcols <- pos_cols
    return(model)
}
