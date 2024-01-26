source("EL_methods_for_shift.R")

data("ToothGrowth")
View(ToothGrowth)

t_vertibas <- seq(0.1, 0.9, by=0.025)

## pirmā hipotēze: deva ietekmē zobu augšanu

png("tooth_growth_dose.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)

X <- ToothGrowth$len[ToothGrowth$dose==2]
Y <- ToothGrowth$len[ToothGrowth$dose==0.5]
conf_int_vertibas_pp <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='pp',X, Y, interval=c(-5,30), t, alpha=0.95))
conf_int_vertibas_roc <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='roc',X, Y, interval=c(-5,30), t, alpha=0.95))
conf_int_vertibas_qdiff <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='qdiff',X, Y, interval=c(-5,30), t, alpha=0.95))


conf_int_vertibas_pp <- do.call(rbind, conf_int_vertibas_pp)
conf_int_vertibas_roc <- do.call(rbind, conf_int_vertibas_roc)
conf_int_vertibas_qdiff <- do.call(rbind, conf_int_vertibas_qdiff)

ggplot() +
  geom_ribbon(data = conf_int_vertibas_pp, aes(x = t, ymin = lower, ymax = upper, fill = "pp"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_roc, aes(x = t, ymin = lower, ymax = upper, fill = "roc"), alpha = 0.3, color = NA) +
  geom_ribbon(data = conf_int_vertibas_qdiff, aes(x = t, ymin = lower, ymax = upper, fill = "qdiff"), alpha = 0.3, color = NA) +
  
  geom_line(data = conf_int_vertibas_pp, aes(x = t, y = estimate, color = "pp")) +
  geom_line(data = conf_int_vertibas_roc, aes(x = t, y = estimate, color = "roc")) +
  geom_line(data = conf_int_vertibas_qdiff, aes(x = t, y = estimate, color = "qdiff")) +
  
  
  labs(x = "t", y = "mu") +
  scale_color_manual(name = "metode", values = c("pp" = "blue", "roc" = "red", "qdiff" = "yellow3")) +
  scale_fill_manual(name = "metode", values = c("pp" = "blue", "roc" = "red", "qdiff" = "yellow")) +
  theme_minimal()
dev.off()

## qq gadījumā statistika neierobežota


## otrā hipotēze: vitamīnu avots ietekmē zobu augšanu
png("tooth_growth_avots.png", width = 800, height = 600, units = "px", pointsize = 12, res = 120)

X <- ToothGrowth$len[ToothGrowth$supp=="OJ"]
Y <- ToothGrowth$len[ToothGrowth$supp=="VC"]

conf_int_vertibas_pp <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='pp',X, Y, interval=c(-5,30), t, alpha=0.95))
conf_int_vertibas_qq <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='qq',X, Y, interval=c(0,10), t, alpha=0.95))
conf_int_vertibas_roc <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='roc',X, Y, interval=c(-5,30), t, alpha=0.95))
conf_int_vertibas_qdiff <- lapply(t_vertibas, function(t) get_location_parameter_confint(method='qdiff',X, Y, interval=c(-10,30), t, alpha=0.95))



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

dev.off()


