library(FLAdvice)
# Examples from the gislaSim.pdf document

eq100=gislaSim(FLPar(linf=100))
# produces FLBRP
plot(eq100)
ggplot(eq100[[c("m","mat","stock.wt")]])+geom_line(aes(age,data))+facet_wrap(~qname,scale="free")

refpts(eq100)
# Project from 0 to 2 * FMSY in 50 years
dyn100=fwd(eq100,fbar=FLQuant(seq(0,2,length.out=50)*c(refpts(eq100)["msy","harvest"])))
plot(dyn100)
# If you fwd() project from a BRP, what initial state does it use?


eq.7=gislaSim(FLPar(linf=100),sr=list(model="bevholt",steepness=0.7,vbiomass=1e3))
eq.6=gislaSim(FLPar(linf=100),sr=list(model="bevholt",steepness=0.6,vbiomass=1e3))
plot(FLBRPs("0.7"=eq.7,"0.6"=eq.6))

cod=FLBRPs(codNS=brp(FLBRP(stock)),
sim=gislaSim(FLPar(linf=200)))
ggplot(ldply(cod,function(x)as.data.frame(x[[c("m","mat","stock.wt")]])))+geomline(
aes(age,data,colour=.id))+facetwrap(~qname,scale="free")

# my stuff
data(ple4)
ple=FLBRPs( ple4=brp (FLBRP(ple4) ) ,sim =gislaSim(FLPar(linf=70)))
ggplot(ldply(ple,function(x)as.data.frame(x[[c("m","mat","stock.wt")]])))+geom_line(aes(age,data,colour=.id))+facet_wrap(~qname,scale="free")



