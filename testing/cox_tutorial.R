library(survival)
url <- "http://socserv.mcmaster.ca/jfox/Books/Companion/data/Rossi.txt"
Rossi <- read.table(url, header=TRUE)
Rossi[1:5, 1:10]

#initial cox model
mod.allison <- coxph(Surv(week, arrest) ~ + fin + age + race + wexp + mar + paro + prio, data=Rossi)
mod.allison
summary(mod.allison)


plot(survfit(mod.allison), ylim=c(0.7, 1), xlab="Weeks", ylab="Proportion Not Rearrested")

#display effect of financial aid
Rossi.fin <- with(Rossi, data.frame(fin=c(0, 1),age=rep(mean(age), 2), race=rep(mean(race == "other"), 2),wexp=rep(mean(wexp == "yes"), 2), 
                                    mar=rep(mean(mar == "not married"), 2),
                                    paro=rep(mean(paro == "yes"), 2), prio=rep(mean(prio), 2)))
plot(survfit(mod.allison, newdata=Rossi.fin), conf.int=TRUE,
     lty=c(1, 2), ylim=c(0.6, 1), xlab="Weeks",
     ylab="Proportion Not Rearrested")
legend("bottomleft", legend=c("fin = no", "fin = yes"), lty=c(1 ,2), inset=0.02)

#time-dependent covariates
Rossi.2 <- unfold(Rossi, time="week", event="arrest", cov=11:62, cov.names="employed")

#fit cox model with time dependent covariate "employed"
mod.allison.2 <- coxph(Surv(start, stop, arrest.time) ~fin + age + race + wexp + mar + paro + prio + employed,data=Rossi.2)
summary(mod.allison.2)

#diagnostics
mod.allison.4 <- coxph(Surv(week, arrest) ~ fin + age + prio,data=Rossi)
mod.allison.4
cox.zph(mod.allison.4)
par(mfrow=c(2, 2))
plot(cox.zph(mod.allison.4))

#integrating an interaction term because age does not fulfill the proport. hazard assumption
mod.allison.5 <- coxph(Surv(start, stop, arrest.time) ~fin + age + age:stop + prio,data=Rossi.2)
mod.allison.5

#or use some strata function
library(car)
 Rossi$age.cat <- recode(Rossi$age, " lo:19=1; 20:25=2; 26:30=3; 31:hi=4 ")
 xtabs(~ age.cat, data=Rossi)

mod.allison.6 <- coxph(Surv(week, arrest) ~ fin + prio + strata(age.cat), data=Rossi)
mod.allison.6
cox.zph(mod.allison.6)

#check for influential observations
  dfbeta <- residuals(mod.allison.4, type="dfbeta")
  par(mfrow=c(2, 2))
  for (j in 1:3) {
   plot(dfbeta[, j], ylab=names(coef(mod.allison.4))[j])
   abline(h=0, lty=2)
   }

#check for nonlinearity
par(mfrow=c(2, 2))
res <- residuals(mod.allison.4, type="martingale")
X <- as.matrix(Rossi[, c("age", "prio")]) # matrix of covariates
par(mfrow=c(2, 2))
for (j in 1:2) { # residual plots
   plot(X[, j], res, xlab=c("age", "prio")[j], ylab="residuals")
  abline(h=0, lty=2)
  lines(lowess(X[, j], res, iter=0))
  }
b <- coef(mod.allison.4)[c(2,3)] # regression coefficients
 for (j in 1:2) { # component-plus-residual plots
   plot(X[, j], b[j]*X[, j] + res, xlab=c("age", "prio")[j],
          ylab="component+residual")
   abline(lm(b[j]*X[, j] + res ~ X[, j]), lty=2)
   lines(lowess(X[, j], b[j]*X[, j] + res, iter=0))
   }


#----------------------
#unfold function
unfold <- function (data, time, event, cov, cov.names = paste("covariate",
                                                              ".", 1:ncovs, sep = ""), suffix = ".time", cov.times = 0:ncov,
                    common.times = TRUE, lag = 0, ...) {
  vlag <- function(x, lag) c(rep(NA, lag), x[1:(length(x) -
                                                  lag)])
  xlag <- function(x, lag) apply(as.matrix(x), 2, vlag, lag = lag)
  all.cov <- unlist(cov)
  if (!is.numeric(all.cov))
    all.cov <- which(is.element(names(data), all.cov))
  if (!is.list(cov))
    cov <- list(cov)
  ncovs <- length(cov)
  nrow <- nrow(data)
  ncol <- ncol(data)
  ncov <- length(cov[[1]])
  nobs <- nrow * ncov
  if (length(unique(c(sapply(cov, length), length(cov.times) -
                        1))) > 1)
    stop(paste("all elements of cov must be of the same length and \n",
               "cov.times must have one more entry than each element of cov."))
  var.names <- names(data)
  subjects <- rownames(data)
  omit.cols <- if (!common.times)
    c(all.cov, cov.times)
  else all.cov
  keep.cols <- (1:ncol)[-omit.cols]
  factors <- names(data)[keep.cols][sapply(data[keep.cols],
                                           is.factor)]
  levels <- lapply(data[factors], levels)
  first.covs <- sapply(cov, function(x) x[1])
  factors.covs <- which(sapply(data[first.covs], is.factor))
  levels.covs <- lapply(data[names(factors.covs)], levels)
  nkeep <- length(keep.cols)
  if (is.numeric(event))
    event <- var.names[event]
  events <- sort(unique(data[[event]]))
  if (length(events) > 2 || (!is.numeric(events) && !is.logical(events)))
    stop("event indicator must have values {0, 1}, {1, 2} or {FALSE, TRUE}")
  if (!(all(events == 0:1) || all(events == c(FALSE, TRUE)))) {
    if (all(events = 1:2))
      data[[event]] <- data[[event]] - 1
    else stop("event indicator must have values {0, 1}, {1, 2} or {FALSE, TRUE}")
  }
  times <- if (common.times)
    matrix(cov.times, nrow, ncov + 1, byrow = TRUE)
  else data[, cov.times]
  new.data <- matrix(Inf, nobs, 3 + ncovs + nkeep)
  rownames <- rep("", nobs)
  colnames(new.data) <- c("start", "stop", paste(event, suffix,
                                                 sep = ""), var.names[-omit.cols], cov.names)
  end.row <- 0
  data <- as.matrix(as.data.frame(lapply(data, as.numeric)))
  for (i in 1:nrow) {
    start.row <- end.row + 1
    end.row <- end.row + ncov
    start <- times[i, 1:ncov]
    stop <- times[i, 2:(ncov + 1)]
    event.time <- ifelse(stop == data[i, time] & data[i,
                                                      event] == 1, 1, 0)
    keep <- matrix(data[i, -omit.cols], ncov, nkeep, byrow = TRUE)
    select <- apply(matrix(!is.na(data[i, all.cov]), ncol = ncovs),
                    1, all)
    rows <- start.row:end.row
    cov.mat <- xlag(matrix(data[i, all.cov], nrow = length(rows)),
                    lag)
    new.data[rows[select], ] <- cbind(start, stop, event.time,
                                      keep, cov.mat)[select, ]
    rownames[rows] <- paste(subjects[i], ".", seq(along = rows),
                            sep = "")
  }
  row.names(new.data) <- rownames
  new.data <- as.data.frame(new.data[new.data[, 1] != Inf &
                                       apply(as.matrix(!is.na(new.data[, cov.names])), 1, all),
                                     ])
  for (fac in factors) {
    new.data[[fac]] <- factor(levels[[fac]][new.data[[fac]]])
  }
  fcv <- 0
  for (cv in factors.covs) {
    fcv <- fcv + 1
    new.data[[cov.names[cv]]] <- factor(levels.covs[[fcv]][new.data[[cov.names[cv]]]])
  }
  new.data
}