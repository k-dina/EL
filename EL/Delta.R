set.seed(123)
n <- 100


# definēsim funkciju, kura uzzīmēs visa veida grafikus.
# bw=c(bw.nrd(X),bw.nrd(Y))

delta_plot <- function(X,Y, bw){
## PP
EL::EL.plot('pp',Y, X, bw=bw)
## QQ
EL::EL.plot('qq',Y, X, bw=bw)
## ROC
EL::EL.plot('roc',Y, X, bw=bw)
##qdiff
EL::EL.plot('qdiff',Y, X,bw=bw)
##fdiff
EL::EL.plot('fdiff',Y, X,bw=bw)
}

plot_bliv <- function(X, Y){
  data <- data.frame(value = c(X, Y),
                     izlase = rep(c("X", "Y"), each = length(X)))
  
  # Построение графика оценок плотности распределения
  ggplot(data, aes(x = value, fill = izlase, color = izlase)) +
    geom_density(alpha = 0.5) +
    labs(title = "",
         x = "Vērtība",
         y = "Blīvums") +
    theme_minimal()
}

## QQ
plot_qq <- function(X, Y, bw, title=''){
  EL::EL.plot('qq',Y, X, bw=bw)
}

### gludināšanas ietekme

gludinasana <- function(X, Y){
  par(mfrow=c(2,2))
  qqplot(X, Y, type='l', xlab = expression(F[X]^{-1}), ylab = expression(F[Y]^{-1}))
  plot_qq(X, Y, bw=c(0.1, 0.1))
  plot_qq(X, Y, bw=c(0.5, 0.5))
  plot_qq(X, Y, bw=c(1, 1))
}


################################
##Divi vienādi sadalījumi

png("vienadi.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
X <- rnorm(n,0,1)
Y <- rnorm(n,0,1)
plot_bliv(X, Y)
dev.off()

png("vienadi_1.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
## PP
EL::EL.plot('pp',Y, X)

dev.off()

png("vienadi_2.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
## QQ
EL::EL.plot('qq',Y, X)

dev.off()

png("vienadi_3.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
## ROC
EL::EL.plot('roc',Y, X)

dev.off()

png("vienadi_4.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
##qdiff
EL::EL.plot('qdiff',Y, X)

dev.off()

png("vienadi_5.png", width = 1200, height = 800, units = "px", pointsize = 12, res = 120)
##fdiff
EL::EL.plot('fdiff',Y, X)
dev.off()

################################
## Lokācijas modelis
png("Lok.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
X <- rnorm(n,0,1)
Y <- rnorm(n,1,1)
plot_bliv(X, Y)
dev.off()



png("lok_1.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
## PP
EL::EL.plot('pp',Y, X)

dev.off()

png("loc_2.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
## QQ
EL::EL.plot('qq',Y, X)

dev.off()

png("loc_3.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
## ROC
EL::EL.plot('roc',Y, X)

dev.off()

png("loc_4.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
##qdiff
EL::EL.plot('qdiff',Y, X)

dev.off()

png("loc_5.png", width = 1200, height = 800, units = "px", pointsize = 12, res = 120)
##fdiff
EL::EL.plot('fdiff',Y, X)
dev.off()


################################
## Mēroga modelis
png("Mer.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
X <- rexp(n, rate=1)
Y <- rexp(n, rate=2)
plot_bliv(X, Y)
dev.off()


png("mer_1.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
## PP
EL::EL.plot('pp',Y, X)

dev.off()

png("mer_2.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
## QQ
EL::EL.plot('qq',Y, X)

dev.off()

png("mer_3.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
## ROC
EL::EL.plot('roc',Y, X)

dev.off()

png("mer_4.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
##qdiff
EL::EL.plot('qdiff',Y, X)

dev.off()

png("mer_5.png", width = 1200, height = 800, units = "px", pointsize = 12, res = 120)
##fdiff
EL::EL.plot('fdiff',Y, X)
dev.off()

################################
### Lokācijas-mēroga modelis
png("LokMer.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
X <- rnorm(n,0,1)
Y <- rnorm(n,1,4)
plot_bliv(X, Y)
dev.off()

png("lokmer_1.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
## PP
EL::EL.plot('pp',Y, X)

dev.off()

png("lokmer_2.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
## QQ
EL::EL.plot('qq',Y, X)

dev.off()

png("lokmer_3.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
## ROC
EL::EL.plot('roc',Y, X)

dev.off()

png("lokmer_4.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
##qdiff
EL::EL.plot('qdiff',Y, X)

dev.off()

png("lokmer_5.png", width = 1200, height = 800, units = "px", pointsize = 12, res = 120)
##fdiff
EL::EL.plot('fdiff',Y, X)
dev.off()


################################
png("Gludinasana.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
gludinasana(X_n, Y_n)
dev.off()

################################
###standarta vērtības
png("Gludinasana_standarta.png", width = 800, height = 800, units = "px", pointsize = 12, res = 120)
par(mfrow=c(2,3))
qqplot(X_n, Y_n, type='l', xlab = expression(F[X]^{-1}), ylab = expression(F[Y]^{-1}))
plot_qq(X_n, Y_n, bw=bw.nrd0)
plot_qq(X_n, Y_n, bw=bw.nrd)
plot_qq(X_n, Y_n, bw=bw.bcv)
plot_qq(X_n, Y_n, bw=bw.ucv)
plot_qq(X_n, Y_n, bw=bw.SJ)
dev.off()
