n <- 100
X <- rexp(n, rate=1)
Y <- rexp(n, rate=2)

alpha <- 0.95


library(EL)
library(ggplot2)
source("EL_methods_for_scale.R")
source("general_shift_conf_bands.R")

png("EL_stat_scale.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)
## t = 0.5
d <- seq(1e-6, 1 - 1e-6, by=0.01)
data_df <- data.frame(
  d = rep(d, times = 2),
  sigma = rep(c(1.8, 2.2), each = length(d)),
  EL_statistic = c(
    sapply(d, function(d) EL.statistic('pp', Y*1.8, X, d, 0.5)),
    sapply(d, function(d) EL.statistic('pp', Y*2.2, X, d, 0.5))
  )
)

ggplot(data_df, aes(x = d, y = EL_statistic, color = as.factor(sigma))) +
  geom_line() +
  ylim(0, 16) +
  labs(x = "∆", y = "-2 log R") +
  geom_hline(yintercept = qchisq(0.95, df = 1), linetype = "dashed", color = "red") +
  scale_color_manual(values = c("1.8" = "blue", "2.2" = "black"), name = "sigma") +
  theme_minimal()
dev.off()
####################################################################################
####################################################################################

#konstruēsim tic. intervālu priekš sigma
png("sigma_conf_int.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)
sigma_vertibas <- seq(0.5, 5, by=0.025)

conf_int_list <- lapply(sigma_vertibas, function(sigma) conf_int(sigma, X, Y, alpha, 0.3))
conf_int_vertibas <- do.call(rbind, conf_int_list)

sigma_conf_int <- get_scale_parameter_confint(X, Y, c(1e-6,5), 0.3, alpha)
sigma_conf_int <- c(sigma_conf_int$lower, sigma_conf_int$upper)

ggplot(conf_int_vertibas, aes(x = sigma)) +
  geom_rect(xmin = sigma_conf_int[1], xmax = sigma_conf_int[2],
            ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.01, inherit.aes = FALSE) +
  geom_linerange(aes(ymin = lower, ymax = upper)) +
  geom_point(aes(y = estimate)) +
  geom_hline(yintercept = 0.3, linetype = "dashed", color = "blue") + 
  theme_minimal() +
  labs(x = "sigma", y = "∆")
dev.off()

####################################################################################
####################################################################################

#tagad uzzīmēsim tic intervālus priekš mu dažādām t vērtībām
png("sigma_conf_int_visiem_t.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)
t_vertibas <- seq(0.2, 0.9, by=0.1)
conf_int_vertibas <- lapply(t_vertibas, function(t) get_scale_parameter_confint(method='pp', X, Y, interval=c(1e-6,5), t))
conf_int_vertibas <- do.call(rbind, conf_int_vertibas)


ggplot(conf_int_vertibas, aes(x = t)) +
  geom_linerange(aes(ymin = lower, ymax = upper)) +
  geom_point(aes(y = estimate)) +
  theme_minimal() +
  labs(x = "t", y = "sigma")
dev.off()

####################################################################################
####################################################################################
# parādīsim, ka pie t=0.1 viss slikti
#konstruēsim tic. intervālu priekš sigma
sigma_vertibas <- seq(0.2, 100, by=0.5)

conf_int_list <- lapply(sigma_vertibas, function(sigma) conf_int(sigma, X, Y, alpha, 0.1))
conf_int_vertibas <- do.call(rbind, conf_int_list)


#šeit izmantoju faktu, ka uz grafika redzams, ka funkcijai ir parabolas veids arī
#pēc mainīgā mu, pie fiksētiem pārējiem parametriem:
# pie t = 0.1 nesanāk((
png("sigma_pie_01.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)
par(mfrow=c(2,3))
plot(sigma_vertibas, sapply(sigma_vertibas, function(sigma) EL.statistic('pp', Y*sigma, X, 0.1, 0.1)), type='l', ylim=c(0,4), ylab='∆', xlab='sigma')
abline(h=qchisq(0.95, df=1), col='red')

plot(sigma_vertibas, sapply(sigma_vertibas, function(sigma) EL.statistic('pp', Y*sigma, X, 0.1, 0.1, bw=bw.ucv)), type='l', ylim=c(0,4), ylab='∆', xlab='sigma')
abline(h=qchisq(0.95, df=1), col='red')

plot(sigma_vertibas, sapply(sigma_vertibas, function(sigma) EL.statistic('pp', Y*sigma, X, 0.1, 0.1, bw=bw.SJ)), type='l', ylim=c(0,4), ylab='∆', xlab='sigma')
abline(h=qchisq(0.95, df=1), col='red')


plot(sigma_vertibas, sapply(sigma_vertibas, function(sigma) EL.statistic('pp', Y*sigma, X, 0.1, 0.1, bw=bw.nrd)), type='l', ylim=c(0,4), ylab='∆', xlab='sigma')
abline(h=qchisq(0.95, df=1), col='red')

plot(sigma_vertibas, sapply(sigma_vertibas, function(sigma) EL.statistic('pp', Y*sigma, X, 0.1, 0.1, bw=bw.bcv)), type='l', ylim=c(0,4), ylab='∆', xlab='sigma')
abline(h=qchisq(0.95, df=1), col='red')
dev.off()

####################################################################################
####################################################################################
#visiem t ar gludināšanu ucv kopā ar Doksuma tic joslu
png("sigma_conf_int_visiem_t_ucv.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)
t_vertibas <- seq(0.1, 0.9, by=0.1)
conf_int_vertibas <- lapply(t_vertibas, function(t) get_scale_parameter_confint(X, Y, c(1e-6,11), t, alpha, bw=bw.ucv))
conf_int_vertibas <- do.call(rbind, conf_int_vertibas)
tt <- seq(0.01, 1, by=0.01)
w_band_res <- w_band(tt,X,Y)



ggplot(conf_int_vertibas, aes(x = t)) +
  geom_linerange(aes(ymin = lower, ymax = upper)) +
  geom_point(aes(y = estimate)) +
  geom_line(data = w_band_res, aes(x = tt, y = lower_bound)) +
  geom_line(data = w_band_res, aes(x = tt, y = upper_bound)) +
  theme_minimal() +
  labs(x = "t", y = "sigma")
dev.off()
