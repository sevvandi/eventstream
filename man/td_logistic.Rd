% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/td_logistic.R
\name{td_logistic}
\alias{td_logistic}
\title{Classification with incomplete-event-classifier}
\usage{
td_logistic(
  t,
  X,
  Y,
  lambda = 1,
  scale = TRUE,
  num_bins = 4,
  quad = TRUE,
  interact = FALSE,
  logg = TRUE
)
}
\arguments{
\item{t}{The age of events.}

\item{X}{The event features.}

\item{Y}{The class labels. \code{Y} needs to be binary output.}

\item{lambda}{The penalty coefficient. Default is 1.}

\item{scale}{If \code{TRUE}, each column of \code{X} is scaled to zero mean and standard deviation 1.}

\item{num_bins}{The number of time slots to use.}

\item{quad}{If \code{TRUE}, the squared attributes \code{X^2} are included.}

\item{interact}{if \code{TRUE}, the most relevant interactions are included.}

\item{logg}{If \code{TRUE} logarithms  of positive attributes will be computed.}
}
\value{
A list with following components:
  \item{\code{par}}{The parameters of the incomplete-event-classifier, after its fitted.}
  \item{\code{convergence}}{The difference between the final two output values.}
  \item{\code{scale}}{If \code{scale=TRUE}, contains the mean and the standard deviation of each column of \code{X}.}
  \item{\code{t}}{The age of events \code{t} is split into bins. This list element contains the   boundary values of the bins.}
  \item{\code{quad}}{The value of \code{quad} in arguments.}
  \item{\code{interact}}{The value of \code{interact} in arguments.}
}
\description{
This function does classification of incomplete events. The events grow with time. The input vector \code{t} denotes the age of the event. The classifier takes the growing event features, \code{X} and combines with a \code{L2} penalty for smoothness.
}
\examples{
# Generate data
N <- 1000
t <- sort(rep(1:10, N))
set.seed(821)
for(kk in 1:10){
  if(kk==1){
     X <- seq(-11,9,length=N)
  }else{
     temp <- seq((-11-kk+1),(9-kk+1),length=N)
     X <- c(X,temp)
  }
}
real.a.0 <- seq(2,20, by=2)
real.a.1 <- rep(2,10)
Zstar <-real.a.0[t] + real.a.1[t]*X + rlogis(N, scale=0.5)
Z <- 1*(Zstar > 0)

# Plot data for t=1 and t=8
oldpar <- par(mfrow=c(1,2))
plot(X[t==1],Z[t==1], main="t=1 data")
abline(v=-1, lty=2)
plot(X[t==8],Z[t==8],main="t=8 data")
abline(v=-8, lty=2)
par(oldpar) 

# Fit model
model_td <- td_logistic(t,X,Z)

}
\seealso{
\code{\link{predict_tdl}} for prediction.
}
