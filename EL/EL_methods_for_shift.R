delta_conf_int <- function(mu, X, Y, alpha=0.95, t){
  optim <- optimize(function(d) EL.statistic('pp', Y + mu, X, d, t), interval = c(0, 1))$minimum
  lower <- uniroot(function(d) EL.statistic('pp', Y + mu, X, d, t) - qchisq(alpha, df=1), interval=c(1e-6, optim))$root
  upper <- uniroot(function(d) EL.statistic('pp', Y + mu, X, d, t) - qchisq(alpha, df=1), interval=c(optim, 1 - 1e-6))$root
  data.frame(mu = mu, lower = lower, upper = upper, estimate = optim)
}

get_location_parameter_confint <- function(method='pp', X, Y, interval, t, alpha=0.95, bw=bw.nrd0){
  optim <- optimize(function(mu) EL.statistic(method, Y + mu, X, t, t, bw=bw), interval = interval)$minimum
  lower <- uniroot(function(mu) EL.statistic(method, Y + mu, X, t, t, bw=bw) - qchisq(alpha, df=1), interval=c(interval[1], optim))$root
  upper <- uniroot(function(mu) EL.statistic(method, Y + mu, X, t, t, bw=bw) - qchisq(alpha, df=1), interval = c(optim, interval[2]))$root
  data.frame(t = t, lower = lower, upper = upper, estimate = optim)
}

simulate <- function(method='pp', mu, interval, t, n, bw=bw.nrd0){
  cat('mu:', mu, 't', t, 'n', n, '\n')
  prec <- 0
  for (i in 1:10000) {
    X <- rnorm(n, mean=mu, sd=1)
    Y <- rnorm(n, mean=0, sd=1)
    intervals <- get_location_parameter_confint(method, X, Y, interval, t, bw=bw)
    intervals_str <- paste0("(", intervals$lower, ", ", intervals$upper, ")")
    cat('i:', i, 'intervals: ', intervals_str, '\n')
    if((mu > intervals$lower) && (mu < intervals$upper)){
      prec <- prec + 1
    }
  }
  prec <- prec/10000
  data.frame(mu=mu, t=t, method=method, bw=deparse(substitute(bw)), prec=prec)
}
