#' 
#' @title Internal med.fun function
#' @description This is an internal function copied from the mediation package
#' (mediation:::med.fun)
#' @details Not required
#' @param y.data not info needed
#' @param index not info needed
#' @param m.data not info needed
#' @keywords internal
#' @return not info needed
#'
med.fun <- function (y.data, index, m.data) 
{
  if (isSurvreg.m) {
    mname <- names(m.data)[1]
    nc <- nchar(mediator)
    eventname <- substr(mname, 5 + nc + 3, nchar(mname) - 
                          1)
    if (nchar(eventname) == 0) {
      m.data.tmp <- data.frame(m.data, as.numeric(m.data[, 
                                                         1L][, 1L]))
      names(m.data.tmp)[c(1L, ncol(m.data) + 1)] <- c(mname, 
                                                      mediator)
    }
    else {
      m.data.tmp <- data.frame(m.data, as.numeric(m.data[, 
                                                         1L][, 1L]), as.numeric(model.m$y[, 2]))
      names(m.data.tmp)[c(1L, ncol(m.data) + (1:2))] <- c(mname, 
                                                          mediator, eventname)
    }
    Call.M$data <- m.data.tmp[index, ]
  }
  else {
    Call.M$data <- m.data[index, ]
  }
  if (isSurvreg.y) {
    yname <- names(y.data)[1]
    nc <- nchar(outcome)
    eventname <- substr(yname, 5 + nc + 3, nchar(yname) - 
                          1)
    if (nchar(eventname) == 0) {
      y.data.tmp <- data.frame(y.data, as.numeric(y.data[, 
                                                         1L][, 1L]))
      names(y.data.tmp)[c(1L, ncol(y.data) + 1)] <- c(yname, 
                                                      outcome)
    }
    else {
      y.data.tmp <- data.frame(y.data, as.numeric(y.data[, 
                                                         1L][, 1L]), as.numeric(model.y$y[, 2]))
      names(y.data.tmp)[c(1L, ncol(y.data) + (1:2))] <- c(yname, 
                                                          outcome, eventname)
    }
    Call.Y$data <- y.data.tmp[index, ]
  }
  else {
    Call.Y$data <- y.data[index, ]
  }
  Call.M$weights <- m.data[index, "(weights)"]
  Call.Y$weights <- y.data[index, "(weights)"]
  if (isOrdered.m && length(unique(y.data[index, mediator])) != 
      m) {
    stop("insufficient variation on mediator")
  }
  new.fit.M <- NULL
  new.fit.Y <- NULL
  if (use_speed) {
    if (isGlm.m) 
      new.fit.M <- speedglm::speedglm(formula = Call.M$formula,
                                        data = Call.M$data,
                                        family = eval(Call.M$family),
                                        weights = Call.M$weights)
    else if (isLm.m) {
      formula <- Call.M$formula
      new.fit.M <- speedglm::speedlm(formula = formula, 
                                     data = Call.M$data, weights = Call.M$weights)
    }
    if (isGlm.y) 
      new.fit.Y <- speedglm::speedglm(formula = Call.Y$formula,
                                      data = Call.Y$data,
                                      family = eval(Call.Y$family),
                                      weights = Call.Y$weights)
    else if (isLm.y) {
      formula <- Call.Y$formula
      new.fit.Y <- speedglm::speedlm(formula = formula, 
                                     data = Call.Y$data, weights = Call.Y$weights)
    }
  }
  if (is.null(new.fit.M)) 
    new.fit.M <- eval.parent(Call.M)
  if (is.null(new.fit.Y)) 
    new.fit.Y <- eval.parent(Call.Y)
  pred.data.t <- pred.data.c <- m.data
  if (isFactorT) {
    pred.data.t[, treat] <- factor(cat.1, levels = t.levels)
    pred.data.c[, treat] <- factor(cat.0, levels = t.levels)
  }
  else {
    pred.data.t[, treat] <- cat.1
    pred.data.c[, treat] <- cat.0
  }
  if (!is.null(covariates)) {
    for (p in 1:length(covariates)) {
      vl <- names(covariates[p])
      if (is.factor(pred.data.t[, vl])) {
        pred.data.t[, vl] <- pred.data.c[, vl] <- factor(covariates[[p]], 
                                                         levels = levels(m.data[, vl]))
      }
      else {
        pred.data.t[, vl] <- pred.data.c[, vl] <- covariates[[p]]
      }
    }
  }
  if (isGlm.m) {
    muM1 <- stats::predict(new.fit.M, type = "response", newdata = pred.data.t)
    muM0 <- stats::predict(new.fit.M, type = "response", newdata = pred.data.c)
    if (FamilyM == "poisson") {
      PredictM1 <- stats::rpois(n, lambda = muM1)
      PredictM0 <- stats::rpois(n, lambda = muM0)
    }
    else if (FamilyM == "Gamma") {
      shape <- MASS::gamma.shape(new.fit.M)$alpha
      PredictM1 <- stats::rgamma(n, shape = shape, scale = muM1/shape)
      PredictM0 <- stats::rgamma(n, shape = shape, scale = muM0/shape)
    }
    else if (FamilyM == "binomial") {
      PredictM1 <- stats::rbinom(n, size = 1, prob = muM1)
      PredictM0 <- stats::rbinom(n, size = 1, prob = muM0)
    }
    else if (FamilyM == "gaussian") {
      sigma <- sqrt(summary(new.fit.M)$dispersion)
      error <- stats::rnorm(n, mean = 0, sd = sigma)
      PredictM1 <- muM1 + error
      PredictM0 <- muM0 + error
    }
    else if (FamilyM == "inverse.gaussian") {
      disp <- summary(new.fit.M)$dispersion
      PredictM1 <- SuppDists::rinvGauss(n, nu = muM1, lambda = 1/disp)
      PredictM0 <- SuppDists::rinvGauss(n, nu = muM0, lambda = 1/disp)
    }
    else {
      stop("unsupported glm family")
    }
  }
  else if (isOrdered.m) {
    probs_m1 <- stats::predict(new.fit.M, newdata = pred.data.t, 
                        type = "probs")
    probs_m0 <- stats::predict(new.fit.M, newdata = pred.data.c, 
                        type = "probs")
    draws_m1 <- matrix(NA, n, m)
    draws_m0 <- matrix(NA, n, m)
    for (ii in 1:n) {
      draws_m1[ii, ] <- t(stats::rmultinom(1, 1, prob = probs_m1[ii, 
      ]))
      draws_m0[ii, ] <- t(stats::rmultinom(1, 1, prob = probs_m0[ii, 
      ]))
    }
    PredictM1 <- apply(draws_m1, 1, which.max)
    PredictM0 <- apply(draws_m0, 1, which.max)
  }
  else if (isRq.m) {
    call.new <- new.fit.M$call
    call.new$tau <- stats::runif(n)
    newfits <- eval.parent(call.new)
    tt <- stats::delete.response(terms(new.fit.M))
    m.t <- stats::model.frame(tt, pred.data.t, xlev = new.fit.M$xlevels)
    m.c <- stats::model.frame(tt, pred.data.c, xlev = new.fit.M$xlevels)
    X.t <- stats::model.matrix(tt, m.t, contrasts = new.fit.M$contrasts)
    X.c <- stats::model.matrix(tt, m.c, contrasts = new.fit.M$contrasts)
    rm(tt, m.t, m.c)
    PredictM1 <- rowSums(X.t * t(newfits$coefficients))
    PredictM0 <- rowSums(X.c * t(newfits$coefficients))
    rm(newfits, X.t, X.c)
  }
  else if (isLm.m) {
    if (class(new.fit.M) == "speedlm") 
      sigma <- sqrt(summary(new.fit.M)$var.res)
    else sigma <- summary(new.fit.M)$sigma
    error <- stats::rnorm(n, mean = 0, sd = sigma)
    PredictM1 <- stats::predict(new.fit.M, type = "response", newdata = pred.data.t) + 
      error
    PredictM0 <- stats::predict(new.fit.M, type = "response", newdata = pred.data.c) + 
      error
    rm(error)
  }
  else if (isSurvreg.m) {
    dd <- survival::survreg.distributions[[new.fit.M$dist]]
    if (is.null(dd$itrans)) {
      itrans <- function(x) x
    }
    else {
      itrans <- dd$itrans
    }
    dname <- dd$dist
    if (is.null(dname)) {
      dname <- new.fit.M$dist
    }
    scale <- new.fit.M$scale
    lpM1 <- stats::predict(new.fit.M, newdata = pred.data.t, type = "linear")
    lpM0 <- stats::predict(new.fit.M, newdata = pred.data.c, type = "linear")
    error <- switch(dname, extreme = log(rweibull(n, shape = 1, 
                                                  scale = 1)), gaussian = stats::rnorm(n), logistic = stats::rlogis(n), 
                    t = stats::rt(n, df = dd$parms))
    PredictM1 <- as.numeric(itrans(lpM1 + scale * error))
    PredictM0 <- as.numeric(itrans(lpM0 + scale * error))
    rm(error)
  }
  else {
    stop("mediator model is not yet implemented")
  }
  effects.tmp <- matrix(NA, nrow = n, ncol = 4)
  for (e in 1:4) {
    tt <- switch(e, c(1, 1, 1, 0), c(0, 0, 1, 0), c(1, 0, 
                                                    1, 1), c(1, 0, 0, 0))
    pred.data.t <- pred.data.c <- y.data
    if (!is.null(covariates)) {
      for (p in 1:length(covariates)) {
        vl <- names(covariates[p])
        if (is.factor(pred.data.t[, vl])) {
          pred.data.t[, vl] <- pred.data.c[, vl] <- factor(covariates[[p]], 
                                                           levels = levels(y.data[, vl]))
        }
        else {
          pred.data.t[, vl] <- pred.data.c[, vl] <- covariates[[p]]
        }
      }
    }
    cat.t <- ifelse(tt[1], cat.1, cat.0)
    cat.c <- ifelse(tt[2], cat.1, cat.0)
    cat.t.ctrl <- ifelse(tt[1], cat.0, cat.1)
    cat.c.ctrl <- ifelse(tt[2], cat.0, cat.1)
    if (isFactorT) {
      pred.data.t[, treat] <- factor(cat.t, levels = t.levels)
      pred.data.c[, treat] <- factor(cat.c, levels = t.levels)
      if (!is.null(control)) {
        pred.data.t[, control] <- factor(cat.t.ctrl, 
                                         levels = t.levels)
        pred.data.c[, control] <- factor(cat.c.ctrl, 
                                         levels = t.levels)
      }
    }
    else {
      pred.data.t[, treat] <- cat.t
      pred.data.c[, treat] <- cat.c
      if (!is.null(control)) {
        pred.data.t[, control] <- cat.t.ctrl
        pred.data.c[, control] <- cat.c.ctrl
      }
    }
    PredictM1.tmp <- PredictM1
    PredictM0.tmp <- PredictM0
    PredictMt <- PredictM1 * tt[3] + PredictM0 * (1 - tt[3])
    PredictMc <- PredictM1 * tt[4] + PredictM0 * (1 - tt[4])
    if (isFactorM) {
      pred.data.t[, mediator] <- factor(PredictMt, levels = 1:m, 
                                        labels = m.levels)
      pred.data.c[, mediator] <- factor(PredictMc, levels = 1:m, 
                                        labels = m.levels)
    }
    else {
      pred.data.t[, mediator] <- PredictMt
      pred.data.c[, mediator] <- PredictMc
    }
    if (isRq.y) {
      pr.1 <- stats::predict(new.fit.Y, type = "response", newdata = pred.data.t, 
                      interval = "none")
      pr.0 <- stats::predict(new.fit.Y, type = "response", newdata = pred.data.c, 
                      interval = "none")
    }
    else {
      pr.1 <- stats::predict(new.fit.Y, type = "response", newdata = pred.data.t)
      pr.0 <- stats::predict(new.fit.Y, type = "response", newdata = pred.data.c)
    }
    pr.mat <- as.matrix(cbind(pr.1, pr.0))
    effects.tmp[, e] <- pr.mat[, 1] - pr.mat[, 2]
    rm(pred.data.t, pred.data.c, pr.1, pr.0, pr.mat)
  }
  d1 <- stats::weighted.mean(effects.tmp[, 1], weights)
  d0 <- stats::weighted.mean(effects.tmp[, 2], weights)
  z1 <- stats::weighted.mean(effects.tmp[, 3], weights)
  z0 <- stats::weighted.mean(effects.tmp[, 4], weights)
  c(d1 = d1, d0 = d0, z1 = z1, z0 = z0)
}
