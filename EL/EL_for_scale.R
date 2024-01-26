source("EL_methods_for_scale.R")

rezultati <- simulate(method='pp', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.nrd0)
rezultati <- rbind(rezultati, simulate(method='pp', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.nrd0))
rezultati <- rbind(rezultati, simulate(method='pp', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.nrd0))

rezultati <- rbind(rezultati, simulate(method='pp', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='pp', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='pp', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.nrd))

rezultati <- rbind(rezultati, simulate(method='pp', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='pp', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='pp', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.bcv))

rezultati <- rbind(rezultati, simulate(method='pp', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='pp', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='pp', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.SJ))

rezultati <- rbind(rezultati, simulate(method='pp', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='pp', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='pp', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.ucv))




rezultati <- rbind(rezultati, simulate(method='qq', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.nrd0))
rezultati <- rbind(rezultati, simulate(method='qq', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.nrd0))
rezultati <- rbind(rezultati, simulate(method='qq', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.nrd0))

rezultati <- rbind(rezultati, simulate(method='qq', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='qq', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='qq', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.nrd))

rezultati <- rbind(rezultati, simulate(method='qq', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='qq', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='qq', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.bcv))

rezultati <- rbind(rezultati, simulate(method='qq', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='qq', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='qq', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.SJ))

rezultati <- rbind(rezultati, simulate(method='qq', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='qq', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='qq', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.ucv))




rezultati <- rbind(rezultati, simulate(method='roc', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.nrd0))
rezultati <- rbind(rezultati, simulate(method='roc', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.nrd0))
rezultati <- rbind(rezultati, simulate(method='roc', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.nrd0))

rezultati <- rbind(rezultati, simulate(method='roc', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='roc', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='roc', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.nrd))

rezultati <- rbind(rezultati, simulate(method='roc', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='roc', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='roc', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.bcv))

rezultati <- rbind(rezultati, simulate(method='roc', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='roc', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='roc', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.SJ))

rezultati <- rbind(rezultati, simulate(method='roc', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='roc', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='roc', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.ucv))




rezultati <- rbind(rezultati, simulate(method='qdiff', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.nrd0))
rezultati <- rbind(rezultati, simulate(method='qdiff', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.nrd0))
rezultati <- rbind(rezultati, simulate(method='qdiff', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.nrd0))

rezultati <- rbind(rezultati, simulate(method='qdiff', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='qdiff', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='qdiff', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.nrd))

rezultati <- rbind(rezultati, simulate(method='qdiff', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='qdiff', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='qdiff', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.bcv))

rezultati <- rbind(rezultati, simulate(method='qdiff', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='qdiff', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='qdiff', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.SJ))

rezultati <- rbind(rezultati, simulate(method='qdiff', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='qdiff', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='qdiff', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.ucv))



rezultati <- rbind(rezultati, simulate(method='fdiff', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.nrd0))
rezultati <- rbind(rezultati, simulate(method='fdiff', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.nrd0))
rezultati <- rbind(rezultati, simulate(method='fdiff', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.nrd0))

rezultati <- rbind(rezultati, simulate(method='fdiff', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='fdiff', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.nrd))
rezultati <- rbind(rezultati, simulate(method='fdiff', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.nrd))

rezultati <- rbind(rezultati, simulate(method='fdiff', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='fdiff', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.bcv))
rezultati <- rbind(rezultati, simulate(method='fdiff', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.bcv))

rezultati <- rbind(rezultati, simulate(method='fdiff', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='fdiff', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.SJ))
rezultati <- rbind(rezultati, simulate(method='fdiff', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.SJ))

rezultati <- rbind(rezultati, simulate(method='fdiff', sigma=2, interval=c(1e-6,20), t=0.2, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='fdiff', sigma=2, interval=c(1e-6,20), t=0.5, n=100, bw=bw.ucv))
rezultati <- rbind(rezultati, simulate(method='fdiff', sigma=2, interval=c(1e-6,20), t=0.8, n=100, bw=bw.ucv))




