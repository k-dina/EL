set.seed(123)


library(EL)
source("EL_methods_for_shift.R")



### novērtējam koda izpildes laiku
system.time(simulate(mu=1, interval=c(-0.5, 2.5), t=0.2, n=100))

### pārklājuma precizitāte

rezultati <- simulate(method='pp', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.nrd0)
rezultati <- rbind(rezultati, simulate(method='pp', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.nrd0))
rezultati <- rbind(rezultati, simulate(method='pp', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.nrd0))

rezultati <- rbind(rezultati, simulate(method='pp', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=c(bw.nrd0(X), bw.nrd0(Y))))
rezultati <- rbind(rezultati, simulate(method='pp', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=c(bw.nrd0(X), bw.nrd0(Y))))
rezultati <- rbind(rezultati, simulate(method='pp', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=c(bw.nrd0(X), bw.nrd0(Y))))

rezultati <- rbind(rezultati, simulate(method='pp', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='pp', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='pp', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.nrd))

rezultati <- rbind(rezultati, simulate(method='pp', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='pp', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='pp', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.bcv))

rezultati <- rbind(rezultati, simulate(method='pp', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='pp', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='pp', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.SJ))

rezultati <- rbind(rezultati, simulate(method='pp', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='pp', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='pp', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.ucv))




rezultati <- rbind(rezultati, simulate(method='qq', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.nrd0))
rezultati <- rbind(rezultati, simulate(method='qq', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.nrd0))
rezultati <- rbind(rezultati, simulate(method='qq', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.nrd0))

rezultati <- rbind(rezultati, simulate(method='qq', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='qq', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='qq', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.nrd))

rezultati <- rbind(rezultati, simulate(method='qq', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='qq', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='qq', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.bcv))

rezultati <- rbind(rezultati, simulate(method='qq', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='qq', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='qq', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.SJ))

rezultati <- rbind(rezultati, simulate(method='qq', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='qq', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='qq', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.ucv))




rezultati <- rbind(rezultati, simulate(method='roc', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.nrd0))
rezultati <- rbind(rezultati, simulate(method='roc', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.nrd0))
rezultati <- rbind(rezultati, simulate(method='roc', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.nrd0))

rezultati <- rbind(rezultati, simulate(method='roc', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='roc', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='roc', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.nrd))

rezultati <- rbind(rezultati, simulate(method='roc', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='roc', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='roc', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.bcv))

rezultati <- rbind(rezultati, simulate(method='roc', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='roc', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='roc', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.SJ))

rezultati <- rbind(rezultati, simulate(method='roc', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='roc', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='roc', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.ucv))




rezultati <- rbind(rezultati, simulate(method='qdiff', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.nrd0))
rezultati <- rbind(rezultati, simulate(method='qdiff', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.nrd0))
rezultati <- rbind(rezultati, simulate(method='qdiff', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.nrd0))

rezultati <- rbind(rezultati, simulate(method='qdiff', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='qdiff', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='qdiff', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.nrd))

rezultati <- rbind(rezultati, simulate(method='qdiff', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='qdiff', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='qdiff', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.bcv))

rezultati <- rbind(rezultati, simulate(method='qdiff', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='qdiff', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='qdiff', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.SJ))

rezultati <- rbind(rezultati, simulate(method='qdiff', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='qdiff', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='qdiff', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.ucv))



rezultati <- rbind(rezultati, simulate(method='fdiff', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.nrd0))
rezultati <- rbind(rezultati, simulate(method='fdiff', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.nrd0))
rezultati <- rbind(rezultati, simulate(method='fdiff', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.nrd0))

rezultati <- rbind(rezultati, simulate(method='fdiff', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='fdiff', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='fdiff', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.nrd))

rezultati <- rbind(rezultati, simulate(method='fdiff', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='fdiff', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='fdiff', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.bcv))

rezultati <- rbind(rezultati, simulate(method='fdiff', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='fdiff', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='fdiff', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.SJ))

rezultati <- rbind(rezultati, simulate(method='fdiff', mu=1, interval=c(-0.5,2.5), t=0.2, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='fdiff', mu=1, interval=c(-0.5,2.5), t=0.5, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='fdiff', mu=1, interval=c(-0.5,2.5), t=0.8, n=100, bw=bw.ucv))
