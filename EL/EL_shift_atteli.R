set.seed(123)
n <- 100
X <- rnorm(n, mean=1, sd=1)
Y <- rnorm(n, mean=0, sd=1)
alpha <- 0.95


library(EL)
library(ggplot2)
source("EL_methods_for_shift.R")


####################################################################################
####################################################################################
png("EL_stat.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)
## t = 0.5
d <- seq(1e-6, 1 - 1e-6, by=0.01)
data_df <- data.frame(
  d = rep(d, times = 2),
  mu = rep(c(0.8, 1.2), each = length(d)),
  EL_statistic = c(
    sapply(d, function(d) EL.statistic('pp', Y +0.8, X, d, 0.5)),
    sapply(d, function(d) EL.statistic('pp', Y + 1.2, X, d, 0.5))
  )
)
ggplot(data_df, aes(x = d, y = EL_statistic, color = as.factor(mu))) +
  geom_line() +
  ylim(0, 16) +
  labs(x = "∆", y = "-2 log R") +
  geom_hline(yintercept = qchisq(0.95, df = 1), linetype = "dashed", color = "red") +
  scale_color_manual(values = c("0.8" = "blue", "1.2" = "black")) +
  theme_minimal() +
  guides(color = guide_legend(title = "mu"))
dev.off()
####################################################################################
####################################################################################


png("mu_conf_int.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)

#konstruēsim tic. intervālu priekš mu
mu_vertibas <- seq(0, 2, by=0.025)
conf_int_list <- lapply(mu_vertibas, function(mu) delta_conf_int(mu, X, Y, alpha, 0.5))
conf_int_vertibas <- do.call(rbind, conf_int_list)

mu_conf_int <- location_parameter_confint(X, Y, c(0,2), 0.5, alpha)
mu_conf_int <- c(mu_conf_int$lower, mu_conf_int$upper)

ggplot(conf_int_vertibas, aes(x = mu)) +
  geom_rect(xmin = mu_conf_int[1], xmax = mu_conf_int[2],
            ymin = -Inf, ymax = Inf, fill = "grey", alpha = 0.01, inherit.aes = FALSE) +
  geom_linerange(aes(ymin = lower, ymax = upper)) +
  geom_point(aes(y = estimate)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "blue") + # Добавляем прерывистую линию
  theme_minimal() +
  labs(x = "mu", y = "∆ vērtības", title = "ticamības intrervāli priekš ∆")
dev.off()

####################################################################################
####################################################################################


png("mu_conf_int_visas_metodes.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)

t_vertibas <- seq(0.1, 0.9, by=0.1)
conf_int_vertibas_pp <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='pp',X, Y, interval=c(-0.5,3), t, alpha=0.95))
conf_int_vertibas_qq <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='qq',X, Y, interval=c(-0.5,3), t, alpha=0.95))
conf_int_vertibas_roc <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='roc',X, Y, interval=c(-0.5,3), t, alpha=0.95))
conf_int_vertibas_qdiff <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='qdiff',X, Y, interval=c(-0.5,3), t, alpha=0.95))

conf_int_vertibas_pp <- do.call(rbind, conf_int_vertibas_pp)
conf_int_vertibas_qq <- do.call(rbind, conf_int_vertibas_qq)
conf_int_vertibas_roc <- do.call(rbind, conf_int_vertibas_roc)
conf_int_vertibas_qdiff <- do.call(rbind, conf_int_vertibas_qdiff)


ggplot() +
  geom_ribbon(data = conf_int_vertibas_pp, aes(x = t, ymin = lower, ymax = upper, fill = "pp"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_qq, aes(x = t, ymin = lower, ymax = upper, fill = "qq"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_roc, aes(x = t, ymin = lower, ymax = upper, fill = "roc"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_qdiff, aes(x = t, ymin = lower, ymax = upper, fill = "qdiff"), alpha = 0.3, color = NA) +
  
  geom_line(data = conf_int_vertibas_pp, aes(x = t, y = estimate, color = "pp")) +
  geom_line(data = conf_int_vertibas_qq, aes(x = t, y = estimate, color = "qq")) +
  geom_line(data = conf_int_vertibas_roc, aes(x = t, y = estimate, color = "roc")) +
  geom_line(data = conf_int_vertibas_qdiff, aes(x = t, y = estimate, color = "qdiff")) +
  
  labs(x = "t", y = "mu") +
  scale_color_manual(name = "metodes", values = c("pp" = "blue", "qq" = "red", "roc" = "green", "qdiff" = "yellow3")) +
  scale_fill_manual(name = "metodes", values = c("pp" = "blue", "qq" = "red", "roc" = "green", "qdiff" = "yellow")) +
  theme_minimal()

png("mu_conf_int_dažādi_glud_qq.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)

t_vertibas <- seq(0.1, 0.9, by=0.025)
conf_int_vertibas_1 <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='qq',X, Y, interval=c(-0.5,3), t, alpha=0.95, bw=c(n^(-1), n^(-1))))
conf_int_vertibas_12 <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='qq',X, Y, interval=c(-0.5,3), t, alpha=0.95, bw=c(n^(-1/2), n^(-1/2))))
conf_int_vertibas_14 <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='qq',X, Y, interval=c(-0.5,3), t, alpha=0.95, bw=c(n^(-1/4), n^(-1/4))))
conf_int_vertibas_16 <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='qq',X, Y, interval=c(-0.5,3), t, alpha=0.95, bw=c(n^(-1/6), n^(-1/6))))


conf_int_vertibas_1 <- do.call(rbind, conf_int_vertibas_1)
conf_int_vertibas_12 <- do.call(rbind, conf_int_vertibas_12)
conf_int_vertibas_14 <- do.call(rbind, conf_int_vertibas_14)
conf_int_vertibas_16 <- do.call(rbind, conf_int_vertibas_16)


ggplot() +
  geom_ribbon(data = conf_int_vertibas_1, aes(x = t, ymin = lower, ymax = upper, fill = "n^(-1)"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_12, aes(x = t, ymin = lower, ymax = upper, fill = "n^(-1/2)"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_14, aes(x = t, ymin = lower, ymax = upper, fill = "n^(-1/4)"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_16, aes(x = t, ymin = lower, ymax = upper, fill = "n^(-1/6)"), alpha = 0.3, color = NA) +
  
  geom_line(data = conf_int_vertibas_1, aes(x = t, y = estimate, color = "n^(-1)")) +
  geom_line(data = conf_int_vertibas_12, aes(x = t, y = estimate, color = "n^(-1/2)")) +
  geom_line(data = conf_int_vertibas_14, aes(x = t, y = estimate, color = "n^(-1/4)")) +
  geom_line(data = conf_int_vertibas_16, aes(x = t, y = estimate, color = "n^(-1/6)")) +
  
  labs(x = "t", y = "mu") +
  scale_color_manual(name = "loga platums", values = c("n^(-1)" = "blue", "n^(-1/2)" = "red", "n^(-1/4)" = "green", "n^(-1/6)" = "yellow3")) +
  scale_fill_manual(name = "loga platums", values = c("n^(-1)" = "blue", "n^(-1/2)" = "red", "n^(-1/4)" = "green", "n^(-1/6)" = "yellow")) +
  theme_minimal()
dev.off()

png("mu_conf_int_dažādi_glud_roc.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)

t_vertibas <- seq(0.1, 0.9, by=0.025)
conf_int_vertibas_1 <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='roc',X, Y, interval=c(-0.5,3), t, alpha=0.95, bw=c(n^(-1), n^(-1))))
conf_int_vertibas_12 <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='roc',X, Y, interval=c(-0.5,3), t, alpha=0.95, bw=c(n^(-1/2), n^(-1/2))))
conf_int_vertibas_14 <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='roc',X, Y, interval=c(-0.5,3), t, alpha=0.95, bw=c(n^(-1/4), n^(-1/4))))
conf_int_vertibas_16 <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='roc',X, Y, interval=c(-0.5,3), t, alpha=0.95, bw=c(n^(-1/6), n^(-1/6))))


conf_int_vertibas_1 <- do.call(rbind, conf_int_vertibas_1)
conf_int_vertibas_12 <- do.call(rbind, conf_int_vertibas_12)
conf_int_vertibas_14 <- do.call(rbind, conf_int_vertibas_14)
conf_int_vertibas_16 <- do.call(rbind, conf_int_vertibas_16)


ggplot() +
  geom_ribbon(data = conf_int_vertibas_1, aes(x = t, ymin = lower, ymax = upper, fill = "n^(-1)"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_12, aes(x = t, ymin = lower, ymax = upper, fill = "n^(-1/2)"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_14, aes(x = t, ymin = lower, ymax = upper, fill = "n^(-1/4)"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_16, aes(x = t, ymin = lower, ymax = upper, fill = "n^(-1/6)"), alpha = 0.3, color = NA) +
  
  geom_line(data = conf_int_vertibas_1, aes(x = t, y = estimate, color = "n^(-1)")) +
  geom_line(data = conf_int_vertibas_12, aes(x = t, y = estimate, color = "n^(-1/2)")) +
  geom_line(data = conf_int_vertibas_14, aes(x = t, y = estimate, color = "n^(-1/4)")) +
  geom_line(data = conf_int_vertibas_16, aes(x = t, y = estimate, color = "n^(-1/6)")) +
  
  labs(x = "t", y = "mu") +
  scale_color_manual(name = "loga platums", values = c("n^(-1)" = "blue", "n^(-1/2)" = "red", "n^(-1/4)" = "green", "n^(-1/6)" = "yellow3")) +
  scale_fill_manual(name = "loga platums", values = c("n^(-1)" = "blue", "n^(-1/2)" = "red", "n^(-1/4)" = "green", "n^(-1/6)" = "yellow")) +
  theme_minimal()
dev.off()

png("mu_conf_int_dažādi_glud_qdiff.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)

t_vertibas <- seq(0.1, 0.9, by=0.025)
conf_int_vertibas_1 <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='qdiff',X, Y, interval=c(-0.5,3), t, alpha=0.95, bw=c(n^(-1), n^(-1))))
conf_int_vertibas_12 <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='qdiff',X, Y, interval=c(-0.5,3), t, alpha=0.95, bw=c(n^(-1/2), n^(-1/2))))
conf_int_vertibas_14 <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='qdiff',X, Y, interval=c(-0.5,3), t, alpha=0.95, bw=c(n^(-1/4), n^(-1/4))))
conf_int_vertibas_16 <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='qdiff',X, Y, interval=c(-0.5,3), t, alpha=0.95, bw=c(n^(-1/6), n^(-1/6))))


conf_int_vertibas_1 <- do.call(rbind, conf_int_vertibas_1)
conf_int_vertibas_12 <- do.call(rbind, conf_int_vertibas_12)
conf_int_vertibas_14 <- do.call(rbind, conf_int_vertibas_14)
conf_int_vertibas_16 <- do.call(rbind, conf_int_vertibas_16)


ggplot() +
  geom_ribbon(data = conf_int_vertibas_1, aes(x = t, ymin = lower, ymax = upper, fill = "n^(-1)"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_12, aes(x = t, ymin = lower, ymax = upper, fill = "n^(-1/2)"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_14, aes(x = t, ymin = lower, ymax = upper, fill = "n^(-1/4)"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_16, aes(x = t, ymin = lower, ymax = upper, fill = "n^(-1/6)"), alpha = 0.3, color = NA) +
  
  geom_line(data = conf_int_vertibas_1, aes(x = t, y = estimate, color = "n^(-1)")) +
  geom_line(data = conf_int_vertibas_12, aes(x = t, y = estimate, color = "n^(-1/2)")) +
  geom_line(data = conf_int_vertibas_14, aes(x = t, y = estimate, color = "n^(-1/4)")) +
  geom_line(data = conf_int_vertibas_16, aes(x = t, y = estimate, color = "n^(-1/6)")) +
  
  labs(x = "t", y = "mu") +
  scale_color_manual(name = "loga platums", values = c("n^(-1)" = "blue", "n^(-1/2)" = "red", "n^(-1/4)" = "green", "n^(-1/6)" = "yellow3")) +
  scale_fill_manual(name = "loga platums", values = c("n^(-1)" = "blue", "n^(-1/2)" = "red", "n^(-1/4)" = "green", "n^(-1/6)" = "yellow")) +
  theme_minimal()
dev.off()


############################################iebūvētie loga platumi
png("mu_conf_int_dažādi_glud_qq_i.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)

t_vertibas <- seq(0.1, 0.9, by=0.025)
conf_int_vertibas_nrd0 <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='qq',X, Y, interval=c(-0.5,3), t, alpha=0.95))
conf_int_vertibas_SJ <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='qq',X, Y, interval=c(-0.5,3), t, alpha=0.95, bw=bw.SJ))
conf_int_vertibas_ucv <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='qq',X, Y, interval=c(-0.5,3), t, alpha=0.95, bw=bw.ucv))



conf_int_vertibas_nrd0 <- do.call(rbind, conf_int_vertibas_nrd0)
conf_int_vertibas_SJ <- do.call(rbind, conf_int_vertibas_SJ)
conf_int_vertibas_ucv <- do.call(rbind, conf_int_vertibas_ucv)



ggplot() +
  geom_ribbon(data = conf_int_vertibas_nrd0, aes(x = t, ymin = lower, ymax = upper, fill = "nrd0"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_SJ, aes(x = t, ymin = lower, ymax = upper, fill = "SJ"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_ucv, aes(x = t, ymin = lower, ymax = upper, fill = "ucv"), alpha = 0.3, color = NA) +
  
  geom_line(data = conf_int_vertibas_nrd0, aes(x = t, y = estimate, color = "nrd0")) +
  geom_line(data = conf_int_vertibas_SJ, aes(x = t, y = estimate, color = "SJ")) +
  geom_line(data = conf_int_vertibas_ucv, aes(x = t, y = estimate, color = "ucv")) +

  
  labs(x = "t", y = "mu") +
  scale_color_manual(name = "loga platums", values = c("nrd0" = "blue", "SJ" = "red", "ucv" = "yellow3")) +
  scale_fill_manual(name = "loga platums", values = c("nrd0" = "blue", "SJ" = "red", "ucv" = "yellow")) +
  theme_minimal()
dev.off()

png("mu_conf_int_dažādi_glud_roc_i.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)

t_vertibas <- seq(0.1, 0.9, by=0.025)
conf_int_vertibas_nrd0 <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='roc',X, Y, interval=c(-0.5,3), t, alpha=0.95))
conf_int_vertibas_SJ <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='roc',X, Y, interval=c(-0.5,3), t, alpha=0.95, bw=bw.SJ))
conf_int_vertibas_ucv <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='roc',X, Y, interval=c(-0.5,3), t, alpha=0.95, bw=bw.ucv))



conf_int_vertibas_nrd0 <- do.call(rbind, conf_int_vertibas_nrd0)
conf_int_vertibas_SJ <- do.call(rbind, conf_int_vertibas_SJ)
conf_int_vertibas_ucv <- do.call(rbind, conf_int_vertibas_ucv)



ggplot() +
  geom_ribbon(data = conf_int_vertibas_nrd0, aes(x = t, ymin = lower, ymax = upper, fill = "nrd0"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_SJ, aes(x = t, ymin = lower, ymax = upper, fill = "SJ"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_ucv, aes(x = t, ymin = lower, ymax = upper, fill = "ucv"), alpha = 0.3, color = NA) +
  
  geom_line(data = conf_int_vertibas_nrd0, aes(x = t, y = estimate, color = "nrd0")) +
  geom_line(data = conf_int_vertibas_SJ, aes(x = t, y = estimate, color = "SJ")) +
  geom_line(data = conf_int_vertibas_ucv, aes(x = t, y = estimate, color = "ucv")) +
  
  
  labs(x = "t", y = "mu") +
  scale_color_manual(name = "loga platums", values = c("nrd0" = "blue", "SJ" = "red", "ucv" = "yellow3")) +
  scale_fill_manual(name = "loga platums", values = c("nrd0" = "blue", "SJ" = "red", "ucv" = "yellow")) +
  theme_minimal()
dev.off()

png("mu_conf_int_dažādi_glud_qdiff_i.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)


t_vertibas <- seq(0.1, 0.9, by=0.025)
conf_int_vertibas_nrd0 <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='qdiff',X, Y, interval=c(-0.5,3), t, alpha=0.95))
conf_int_vertibas_SJ <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='qdiff',X, Y, interval=c(-0.5,3), t, alpha=0.95, bw=bw.SJ))
conf_int_vertibas_ucv <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='qdiff',X, Y, interval=c(-0.5,3), t, alpha=0.95, bw=bw.ucv))



conf_int_vertibas_nrd0 <- do.call(rbind, conf_int_vertibas_nrd0)
conf_int_vertibas_SJ <- do.call(rbind, conf_int_vertibas_SJ)
conf_int_vertibas_ucv <- do.call(rbind, conf_int_vertibas_ucv)



ggplot() +
  geom_ribbon(data = conf_int_vertibas_nrd0, aes(x = t, ymin = lower, ymax = upper, fill = "nrd0"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_SJ, aes(x = t, ymin = lower, ymax = upper, fill = "SJ"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_ucv, aes(x = t, ymin = lower, ymax = upper, fill = "ucv"), alpha = 0.3, color = NA) +
  
  geom_line(data = conf_int_vertibas_nrd0, aes(x = t, y = estimate, color = "nrd0")) +
  geom_line(data = conf_int_vertibas_SJ, aes(x = t, y = estimate, color = "SJ")) +
  geom_line(data = conf_int_vertibas_ucv, aes(x = t, y = estimate, color = "ucv")) +
  
  
  labs(x = "t", y = "mu") +
  scale_color_manual(name = "loga platums", values = c("nrd0" = "blue", "SJ" = "red", "ucv" = "yellow3")) +
  scale_fill_manual(name = "loga platums", values = c("nrd0" = "blue", "SJ" = "red", "ucv" = "yellow")) +
  theme_minimal()
dev.off()

png("mu_conf_int_visiem_t.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)
ggplot(conf_int_vertibas, aes(x = t)) +
  geom_linerange(aes(ymin = lower, ymax = upper)) +
  geom_point(aes(y = estimate)) +
  theme_minimal() +
  labs(x = "t", y = "mu", title = "ticamības intervāli priekš mu")
dev.off()
####################################################################################
####################################################################################


