conf_int <- function(sigma, X, Y, alpha, t){
  cat("Trying uniroot with sigma =", sigma, "\n")
  
  optim <- optimize(function(d) EL.statistic('pp', Y*sigma, X, d, t), interval = c(0, 1))$minimum
  lower <- uniroot(function(d) EL.statistic('pp', Y*sigma, X, d, t) - qchisq(alpha, df=1), interval=c(1e-6, optim))$root
  cat("f(lower) =", lower, "\n")
  upper <- uniroot(function(d) EL.statistic('pp', Y*sigma, X, d, t) - qchisq(alpha, df=1), interval=c(optim, 1 - 1e-6))$root
  cat("f(upper) =", upper, "\n")
  data.frame(sigma = sigma, lower = lower, upper = upper, estimate = optim)
}

get_scale_parameter_confint <- function(method='pp',X, Y, interval, t, alpha=0.95, bw=bw.nrd0){
  cat("Trying uniroot with t =", t, "\n")
  optim <- optimize(function(sigma) EL.statistic(method, Y*sigma, X, t, t, bw=bw), interval = interval)$minimum
  lower <- uniroot(function(sigma) EL.statistic(method, Y*sigma, X, t, t, bw=bw) - qchisq(alpha, df=1), interval=c(interval[1], optim))$root
  upper <- uniroot(function(sigma) EL.statistic(method, Y*sigma, X, t, t, bw=bw) - qchisq(alpha, df=1), interval = c(optim, interval[2]))$root
  data.frame(t = t, lower = lower, upper = upper, estimate = optim)
}


simulate <- function(method='pp', sigma, interval, t, n, bw=bw.nrd0){
  cat('sigma:', sigma, 't', t, 'n', n, '\n')
  prec <- 0
  for (i in 1:1000) {
    X <- rexp(n, rate=1)
    Y <- rexp(n, rate=sigma)
    intervals <- get_scale_parameter_confint(method=method, X, Y, interval=interval, t=t, bw=bw)
    intervals_str <- paste0("(", intervals$lower, ", ", intervals$upper, ")")
    cat('i:', i, 'intervals: ', intervals_str, '\n')
    if((sigma > intervals$lower) && (sigma < intervals$upper)){
      prec <- prec + 1
    }
  }
  prec <- prec/1000
  data.frame(sigma=sigma, t=t, method=method, bw=deparse(substitute(bw)), prec=prec)
}

sigma_vertibas <- seq(1e-6, 10, by=0.1)
plot(sigma_vertibas, sapply(sigma_vertibas, function(sigma) EL.statistic('pp', Y*sigma, X, 0.5, 0.5, bw=bw.ucv)), type='l', ylim=c(0,6))
abline(h=qchisq(0.95, df = 1))
par(mfrow=c(1,1))
     