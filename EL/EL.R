n <- 100

X <- rnorm(n, mean = 0, sd = 1)
X <- sort(X)

a1 <- round(X[1], 2)
b1 <- round(X[n], 2)

if (a1 < X[1]) {
  a1 <- a1 + 0.01
}
#if (b1 < X[n]) {
#  b1 <- b1 + 0.01
#}


# EL
EL <- function(mu) 2 * sum( log( 1 + uniroot(function (l) sum((X - mu)/(1 + l *(X - mu))), 
                                             c((1 - 1/n)/(mu - X[n]), (1 - 1/n)/(mu - X[1])),
                                             lower = min(c((1 - 1/n)/(mu - X[n]), (1 - 1/n)/(mu - X[1]))), 
                                             upper = max(c((1 - 1/n)/(mu - X[n]), (1 - 1/n)/(mu - X[1]))))$root
                                 * (X - mu)) )

##paņēmu -0.5 0.5 lai skaistāk ir

mumu <- seq(-0.5, 0.5, by = 0.01)
EL_values <- sapply(mumu, EL)

## zīmējam likelihood funkciju priekš mu
plot(mumu, EL_values, type='l', col='green')
### kritiska vērtība
abline(h=3.84)

EL_tic <- function(mu) 2 * sum( log( 1 + uniroot(function (l) sum((X - mu)/(1 + l *(X - mu))), 
                                                 c((1 - 1/n)/(mu - X[n]), (1 - 1/n)/(mu - X[1])),
                                                 lower = min(c((1 - 1/n)/(mu - X[n]), (1 - 1/n)/(mu - X[1]))), 
                                                 upper = max(c((1 - 1/n)/(mu - X[n]), (1 - 1/n)/(mu - X[1]))))$root
                                     * (X - mu)) ) - 3.84

mu_nov <- optimise(EL, lower=a1, upper=b1, maximum=FALSE)$minimum
### atradīsim tic intervālu:

uniroot(EL_tic, lower = a1, upper = mu_nov)$root
uniroot(EL_tic, lower = mu_nov, upper = b1)$root




### jāatord tic intervāli (uniroot)
### jāsalīdzina ar iebūvēto funkciju, pārbaudīt, vai pareizi



# pārklājuma precizitāte

confidence_level <- 0.95

el_metode <- function(X) {
  
  a1 <- round(X[1], 2)
  b1 <- round(X[n], 2)
  
  if (a1 < X[1]) {
    a1 <- a1 + 0.01
  }

  
  EL <- function(mu) 2 * sum(log(1 + uniroot(function(l) sum((X - mu) / (1 + l * (X - mu))), 
                                            c((1 - 1/n) / (mu - X[n]), (1 - 1/n) / (mu - X[1])),
                                            lower = min(c((1 - 1/n) / (mu - X[n]), (1 - 1/n) / (mu - X[1]))),
                                            upper = max(c((1 - 1/n) / (mu - X[n]), (1 - 1/n) / (mu - X[1])))
  )$root * (X - mu)))
  
  mu_nov <- optimize(EL, interval = c(a1, b1), maximum = FALSE)$minimum
  return(mu_nov)
}

el_metode_tic <- function(X, mu_nov, alpha) {
  # kā ar malām? ja ņemt X[n], tad unifoot un optimise nestrādā(NaNs)
  a1 <- round(X[1], 2)
  b1 <- round(X[n-1], 2) 
  
  if (a1 < X[1]) {
    a1 <- a1 + 0.01
  }
 
  
  EL_tic <- function(mu) 2 * sum(log(1 + uniroot(function(l) sum((X - mu) / (1 + l * (X - mu))), 
                                             c((1 - 1/n) / (mu - X[n]), (1 - 1/n) / (mu - X[1])),
                                             lower = min(c((1 - 1/n) / (mu - X[n]), (1 - 1/n) / (mu - X[1]))),
                                             upper = max(c((1 - 1/n) / (mu - X[n]), (1 - 1/n) / (mu - X[1])))
  )$root * (X - mu))) - qchisq(1-alpha, 1)

  ### atradīsim tic intervālu:
  
  return(c(uniroot(EL_tic, lower = a1, upper = mu_nov)$root, +
  uniroot(EL_tic, lower = mu_nov, upper = b1)$root))
  
}


gener_norm_el <- function(n, alpha) {
  mu = 0
  X <- sort(rnorm(n, mean = mu, sd = 1))
  mu_nov <- el_metode(X)
  confidence_interval <- el_metode_tic(X, mu_nov, alpha)
  is_inside_interval <- mu >= confidence_interval[1] & mu <= confidence_interval[2]
  return(is_inside_interval)
}


vector <- seq(1, 10000)

for (i in 1:10000) {
  rez <- gener_norm_el(n = 100, alpha = 0.05)
  vector[i] <- rez
}

sum(vector)/10000


gener_norm_t <- function(n, alpha) {
  mu = 0
  X <- sort(rnorm(n, mean = mu, sd = 1))
  confidence_interval <- t.test(X)$conf.int
  is_inside_interval <- mu >= confidence_interval[1] & mu <= confidence_interval[2]
  return(is_inside_interval)
}

vector <- seq(1, 10000)

for (i in 1:10000) {
  rez <- gener_norm_t(n = 100, alpha = 0.05)
  vector[i] <- rez
}

sum(vector)/10000

gener_chisq_el <- function(n, confidence_level) {
  mu = 1
  X <- sort(rchisq(n, df = 1))
  mu_nov <- el_metode(X)
  confidence_interval <- el_metode_tic(X, mu_nov, alpha)
  is_inside_interval <- mu >= confidence_interval[1] & mu <= confidence_interval[2]
  return(is_inside_interval)
}


vector <- seq(1, 10000)

for (i in 1:10000) {
  rez <- gener_chisq_el(n = 100, confidence_level = 0.95)
  vector[i] <- rez
}

sum(vector)/10000

gener_chisq_t <- function(n, confidence_level) {
  mu = 1
  X <- sort(rchisq(n, df = 1))
  confidence_interval <- t.test(X)$conf.int
  is_inside_interval <- mu >= confidence_interval[1] & mu <= confidence_interval[2]
  return(is_inside_interval)
}


vector <- seq(1, 10000)

for (i in 1:10000) {
  rez <- gener_chisq_t(n = 100, confidence_level = 0.95)
  vector[i] <- rez
}

sum(vector)/10000

