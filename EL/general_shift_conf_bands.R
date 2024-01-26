# atkartojam piemeru no doksum/sievers

set.seed(5)


## palīgfunkcijas

get_y <- function(Y, i){
  
  if (i <= 0){
    return(-Inf)
  }
  
  if (i > length(Y)){
    return(Inf)
  }
  
  return(Y[i])
}



get_i <- function(smpl, value){
  n <- length(smpl)
  for (i in 1:n){
    if (smpl[i] > value){
      return(i-1)
    }
    if (smpl[i] == value){
      return(i)
    }
  }
  if (value < smpl[1]){
    return(0)
  }
  return(n + 1)
}


## S-band
# K statistika - 1.36

s_band <- function(x, X, Y){
  m <- length(X)
  n <- length(Y)
  N <- m + n
  M <- m * n / N
  alpha = 0.95
  K <- 1.36
  

  s_band_helper <- function(x_val, X, Y) {
    i <- get_i(X, x_val)
    lower_bound <- get_y(Y, ceiling(n * (i / m - K / sqrt(M))) ) - x_val
    ### jāmeklē kļūda
    upper_bound <- get_y(Y, floor(n * (i / m + K / sqrt(M)) + 1) ) - x_val
    return(data.frame(x_val = x_val, lower_bound = lower_bound, upper_bound = upper_bound))
  }
  
  result_df <- do.call(rbind, lapply(x, function(x_val) s_band_helper(x_val, X, Y)))
  return(result_df)
}




### W band
### K = 3.02



w_band <- function(x, X, Y){
  m <- length(X)
  n <- length(Y)
  N <- m + n
  M <- m * n / N
  K <- 3.02
  c <- K^2/M
  lambda <- m/N
  
  h_lower <-function(i){
    (i/m + 1/2*c*(1 - lambda)*(1 - 2*lambda*i/m) - 1/2*sqrt(c^2*(1-lambda)^2 + 4*c*i/m*(1-i/m)))/(1 + c*(1-lambda)^2)
    
  }
  h_upper <-function(i){
    (i/m + 1/2*c*(1 - lambda)*(1 - 2*lambda*i/m) + 1/2*sqrt(c^2*(1-lambda)^2 + 4*c*i/m*(1-i/m)))/(1 + c*(1-lambda)^2)
    
  }
  
  
  w_band_helper <- function(x_val, X, Y) {
    i <- get_i(X, x_val)
    lower_bound <- get_y(Y, ceiling(n * h_lower(i))) - x_val
    
    upper_bound <- get_y(Y, floor(n * h_upper(i)) + 1) - x_val
    return(data.frame(x_val = x_val, lower_bound = lower_bound, upper_bound = upper_bound))
  }
  
  result_df <- do.call(rbind, lapply(x, function(x_val) w_band_helper(x_val, X, Y)))
  return(result_df)
}

## Estimate
estimate <- function(x, X, Y){
  m <- length(X)
  n <- length(Y)
  
  estimate_helper <- function(x_val, X, Y) {
    i <- get_i(X, x_val)
    return(data.frame(x_val = x_val, estimate = get_y(Y, floor(n*i/m)) - x_val))
  }
  result <- do.call(rbind, lapply(x, function(x_val) estimate_helper(x_val, X, Y)))
  return(result)
}

# palaiž visu kopā
work <- function(){
  library(ggplot2)
  x = seq(X[1], X[length(X)], by = 0.01)
  s_band_res <- s_band(x, X, Y)
  w_band_res <- w_band(x, X, Y)
  estimate_res <- estimate(x, X, Y)
  # Create a data frame with the values of x, lower and upper bounds for S band, W band, and estimate
  data <- data.frame(
    x = x,
    s_lower = s_band_res$lower_bound,
    s_upper = s_band_res$upper_bound,
    w_lower = w_band_res$lower_bound,
    w_upper = w_band_res$upper_bound,
    estimate = estimate_res$estimate
  )
  
  

  ggplot(data, aes(x = x)) +
    geom_line(aes(y = s_lower, color = "s band"), linetype = "solid") +
    geom_line(aes(y = s_upper, color = "s band"), linetype = "solid") +
    geom_line(aes(y = w_lower, color = "w band"), linetype = "solid") +
    geom_line(aes(y = w_upper, color = "w band"), linetype = "solid") +
    geom_line(aes(y = estimate, color = "estimate"), linetype = "solid") +
    labs(x = "t", y = "∆") +
    theme_minimal() +
    scale_color_manual(name='Joslas pēc Doksuma',
                       breaks=c('s band', 'w band', 'estimate', 'teor'),
                       values=c('s band'='black', 'w band'='green3', 'estimate'='steelblue', 'teor'='red'))
}


m <- 100
n <- 120


# lokācijas-mēroga modelis
X <- sort(rnorm(m, 0, 1))
Y <- sort(rnorm(n, 1, 4))


## izveidosim attēlus
png("Doksum.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)
work()
dev.off()



# lokācijas modelis
X <- sort(rnorm(m, 0, 1))
Y <- sort(rnorm(n, 3, 1))

png("Doksum_lok.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)
work()
dev.off()


# mēroga modelis
X <- sort(rexp(m, rate=1))
Y <- sort(rexp(n, rate=2))

png("Doksum_mer.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)
work()
dev.off()



