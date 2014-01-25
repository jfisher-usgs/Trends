# These functions are from the WRS package (v0.15).
# > install.packages("WRS", repos="http://R-Forge.R-project.org")
# Code format has been slightly altered for readability purposes.
# see http://wrs.r-forge.r-project.org/

RunTheilSen <- function(x, y, alpha=0.05, pr=FALSE, xout=FALSE) {

  # R.R. Wilcox' robust statistics functions:

  # Compute a .95 confidence interval for each of the parameters of
  # a linear regression equation. The default regression method is
  # the Theil-Sen estimator.
  # When using the least squares estimator, and when n < 250, use
  # lsfitci instead.
  # The predictor values are assumed to be in the n by p matrix x.
  # The default number of bootstrap samples is nboot=599
  # regfun can be any R function that returns the coefficients in
  # the vector regfun$coef, the first element of which contains the
  # estimated intercept, the second element contains the estimated of
  # the first predictor, etc.
  regci <- function(x, y, regfun=tsreg, nboot=599, alpha=.05, SEED=TRUE,
                    pr=TRUE, xout=FALSE, outfun=out, ...) {
    x <- as.matrix(x)
    p1 <- ncol(x) + 1
    p <- ncol(x)
    xy <- cbind(x, y)
    xy <- elimna(xy)
    x <- xy[, 1:p]
    y <- xy[, p1]
    if (xout) {
      m <- cbind(x, y)
      flag <- outfun(x, plotit=FALSE)$keep
      m <- m[flag, ]
      x <- m[, 1:p]
      y <- m[, p1]
    }
    x <- as.matrix(x)
    # Set seed of random number generator so that results can be duplicated.
    if (SEED)
      set.seed(2)
    if (pr)
      print("Taking bootstrap samples. Please wait.")
    data <- matrix(sample(length(y), size=length(y) * nboot, replace=TRUE),
                   nrow=nboot)
    bvec <- apply(data, 1, regboot, x, y, regfun, ...)
    # bvec is a p + 1 by nboot matrix. The first row contains the bootstrap
    # intercepts, the second row contains the bootstrap values for first
    # predictor, etc.
    regci <- matrix(0, p1, 5)
    VAL <- c("intercept", rep("X", ncol(x)))
    dimnames(regci) <- list(VAL, c("ci.low", "ci.up", "Estimate",
                                   "S.E.", "p-value"))
    ilow <- round((alpha / 2) * nboot)
    ihi <- nboot - ilow
    ilow <- ilow + 1
    se <- NA
    pvec <- NA
    for(i in 1:p1) {
      bsort <- sort(bvec[i, ])
      pvec[i] <- (sum(bvec[i, ] < 0) + .5 * sum(bvec[i, ] == 0)) / nboot
      if (pvec[i] > .5)
        pvec[i] <- 1 - pvec[i]
      regci[i, 1] <- bsort[ilow]
      regci[i, 2] <- bsort[ihi]
      se[i] <- sqrt(var(bvec[i, ]))
    }
    estit <- regfun(x, y)$coef
    regci[, 3] <- estit
    pvec <- 2 * pvec
    regci[, 4] <- se
    regci[, 5] <- pvec
    if (pr){
      print("First row of regci is the confidence interval for the intercept,")
      print("the second row is the confidence interval for the first slope.")
    }
    list(regci=regci)
  }

  # Remove any rows of data having missing values
  elimna <- function(m) {
    if (is.null(dim(m)))
      m <- as.matrix(m)
    ikeep <- c(1:nrow(m))
    for(i in 1:nrow(m)) {
      if (sum(is.na(m[i, ]) >= 1))
        ikeep[i] <- 0
    }
    elimna <- m[ikeep[ikeep >= 1], ]
    elimna
  }

  # Perform regression using x[isub] to predict y[isub]
  # isub is a vector of length n, a bootstrap sample from the sequence of
  # integers 1, 2, 3, ..., n
  # This function is used by other functions when computing bootstrap estimates.
  # regfun is some regression method already stored in R
  # It is assumed that regfun$coef contains the intercept and slope estimates
  # produced by regfun.  The regression methods written for this book, plus
  # regression functions in R, have this property.
  # x is assumed to be a matrix containing values of the predictors.
  regboot <- function(isub, x, y, regfun, ...) {
    xmat <- matrix(x[isub, ], nrow(x), ncol(x))
    vals <- regfun(xmat, y[isub], ...)$coef
    vals
  }

  # Compute the percentage bend midvariance. beta is the bending constant for
  # omega sub N.
  pbvar <- function(x, beta=.2) {
    pbvar <- 0
    w <- abs(x - median(x))
    w <- sort(w)
    m <- floor((1 - beta) * length(x) + .5)
    omega <- w[m]
    if (omega > 0) {
      y <- (x - median(x)) / omega
      z <- ifelse(y > 1, 1, y)
      z <- ifelse(z < (-1), -1, z)
      pbvar <- length(x) * omega^2 * sum(z^2) / (length(x[abs(y) < 1]))^2
    }
    pbvar
  }

  # Compute the percentage bend correlation between x and y.
  # beta is the bending constant for omega sub N.
  pbcor <- function(x, y, beta=.2) {
    if (length(x) != length(y))
      stop("The vectors do not have equal lengths")
    if (sum(is.na(c(x, y))) != 0) {
      m1 <- matrix(c(x, y),length(x), 2)
      m1 <- elimna(m1)
      x <- m1[, 1]
      y <- m1[, 2]
    }
    temp <- sort(abs(x - median(x)))
    omhatx <- temp[floor((1 - beta) * length(x))]
    temp <- sort(abs(y - median(y)))
    omhaty <- temp[floor((1 - beta) * length(y))]
    a <- (x - pbos(x, beta)) / omhatx
    b <- (y - pbos(y, beta)) / omhaty
    a <- ifelse(a <= -1, -1, a)
    a <- ifelse(a >= 1, 1, a)
    b <- ifelse(b <= -1, -1, b)
    b <- ifelse(b >= 1, 1, b)
    pbcor <- sum(a * b) / sqrt(sum(a^2) * sum(b^2))
    test <- pbcor * sqrt((length(x) - 2) / (1 - pbcor^2))
    sig <- 2 * (1 - pt(abs(test), length(x) - 2))
    list(cor=pbcor, test=test, siglevel=sig)
  }

  # Compute Theil-Sen regression estimator. Use Gauss-Seidel algorithm when
  # there is more than one predictor.
  tsreg <- function(x, y, xout=FALSE, outfun=out, iter=10, varfun=pbvar,
                    corfun=pbcor, plotit=FALSE, ...) {
    x <- as.matrix(x)
    xx <- cbind(x, y)
    xx <- elimna(xx)
    x <- xx[, 1:ncol(x)]
    x <- as.matrix(x)
    y <- xx[, ncol(x) + 1]
    temp <- NA
    x <- as.matrix(x)
    if (xout) {
      x <- as.matrix(x)
      flag <- outfun(x, plotit=plotit, ...)$keep
      x <- x[flag, ]
      y <- y[flag]
      x <- as.matrix(x)
    }
    if (ncol(x) == 1) {
      temp1 <- tsp1reg(x, y)
      coef <- temp1$coef
      res <- temp1$res
    }
    if (ncol(x) > 1) {
      for(p in 1:ncol(x)) {
        temp[p] <- tsp1reg(x[, p], y)$coef[2]
      }
      res <- y - x %*% temp
      alpha <- median(res)
      r <- matrix(NA, ncol=ncol(x), nrow=nrow(x))
      tempold <- temp
      for(it in 1:iter) {
        for(p in 1:ncol(x)) {
          r[, p] <- y - x %*% temp - alpha + temp[p] * x[, p]
          temp[p] <- tsp1reg(x[, p], r[, p], plotit=FALSE)$coef[2]
        }
        alpha <- median(y - x %*% temp)
        tempold <- temp
      }
      coef <- c(alpha, temp)
      res <- y - x %*% temp - alpha
    }
    yhat <- y - res
    stre <- NULL
    e.pow <- varfun(yhat) / varfun(y)
    if (!is.na(e.pow)) {
      if (e.pow >= 1)
        e.pow <- corfun(yhat, y)$cor^2
      stre <- sqrt(e.pow)
    }
    list(coef=coef, residuals=res, Strength.Assoc=stre, Explanatory.Power=e.pow)
  }

  # Compute the Theil-Sen regression estimator. Only a single predictor is
  # allowed in this version
  tsp1reg <- function(x, y, plotit=FALSE) {
    temp <- matrix(c(x, y),ncol = 2)
    temp <- elimna(temp)
    x <- temp[, 1]
    y <- temp[, 2]
    ord <- order(x)
    xs <- x[ord]
    ys <- y[ord]
    vec1 <- outer(ys, ys, "-")
    vec2 <- outer(xs, xs, "-")
    v1 <- vec1[vec2 > 0]
    v2 <- vec2[vec2 > 0]
    slope <- median(v1 / v2)
    coef <- median(y) - slope * median(x)
    names(coef) <- "Intercept"
    coef <- c(coef, slope)
    if (plotit) {
      plot(x, y, xlab="X", ylab="Y")
      abline(coef)
    }
    res <- y - slope * x - coef[1]
    list(coef=coef, residuals=res)
  }

  # Search for outliers using robust measures of location and scatter,
  # which are used to compute robust analogs of Mahalanobis distance.
  # x is an n by p matrix or a vector of data.
  # The function returns the values flagged as an outlier plus
  # the (row) number where the data point is stored.
  # If x is a vector, out.id=4 indicates that the fourth observation
  # is an outlier and outval=123 indicates that 123 is the value.
  # If x is a matrix, out.id=4 indicates that the fourth row of
  # the matrix is an outlier and outval reports the corresponding
  # values.
  # The function also returns the distance of the
  # points identified as outliers
  # in the variable dis.
  # For bivariate data, if plotit=TRUE, plot points and circle outliers.
  # cov.fun determines how the measure of scatter is estimated.
  # Possible hoices are
  # cov.mve (the MVE estimate)
  # cov.mcd (the MCD estimate)
  # covmba2 (the MBA or median ball algorithm)
  # rmba  (an adjustment of MBA suggested by D. Olive)
  # cov.roc (Rocke's TBS estimator)
  out <- function(x, cov.fun=cov.mve, plotit=TRUE, SEED=TRUE,
                  xlab="X", ylab="Y", qval=.975, crit=NULL, ...) {
    if (SEED)
      set.seed(12)
    if (is.data.frame(x))
      x <- as.matrix(x)
    if (is.list(x))
      stop("Data cannot be stored in list mode")
    nrem <- nrow(as.matrix(x))
    if (!is.matrix(x)){
      dis <- (x - median(x, na.rm=T))^2 / mad(x, na.rm=T)^2
      if (is.null(crit))
        crit <- sqrt(qchisq(.975, 1))
      vec <- c(1:length(x))
    }
    if (is.matrix(x)) {
      mve <- cov.fun(elimna(x))
      dis <- mahalanobis(x, mve$center, mve$cov)
      if (is.null(crit))
        crit <- sqrt(qchisq(.975, ncol(x)))
      vec <- c(1:nrow(x))
    }
    dis[is.na(dis)] <- 0
    dis <- sqrt(dis)
    chk <- ifelse(dis > crit, 1, 0)
    id <- vec[chk == 1]
    keep <- vec[chk == 0]
    if (is.matrix(x)){
      if (ncol(x) == 2 && plotit){
        plot(x[, 1], x[, 2], xlab=xlab, ylab=ylab, type="n")
        flag <- rep(TRUE, nrow(x))
        flag[id] <- FALSE
        points(x[flag, 1], x[flag, 2])
        if (sum(!flag) > 0)
          points(x[!flag, 1], x[!flag, 2], pch="*")
      }
    }
    if (!is.matrix(x))
      outval <- x[id]
    if (is.matrix(x))
      outval <- x[id, ]
    list(out.val=outval, out.id=id, keep=keep, dis=dis, crit=crit)
  }

  # Compute the one-step percentage bend measure of location
  pbos <- function(x, beta=.2) {
    temp <- sort(abs(x - median(x)))
    omhatx <- temp[floor((1 - beta) * length(x))]
    psi <- (x - median(x)) / omhatx
    i1 <- length(psi[psi < (-1)])
    i2 <- length(psi[psi > 1])
    sx <- ifelse(psi < (-1), 0, x)
    sx <- ifelse(psi > 1, 0, sx)
    pbos <- (sum(sx) + omhatx * (i2 - i1)) / (length(x) - i1 - i2)
    pbos
  }


  # Main program:

  regci(x=x, y=y, alpha=alpha, pr=pr, xout=xout)
}
