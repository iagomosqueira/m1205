rm(list=ls())
library(FLAdvice)
setwd("C:/Projects/m1205/gislaSim/ICES")
source("C:/Sandbox/pkg/FLAdvice/R/lh.R")
source("C:/Sandbox/pkg/FLAdvice/R/M.R")
source("C:/Sandbox/pkg/FLAdvice/R/growth.R")

#setwd("m:/Projects/MF1205/m1205/gislaSim/ICES")
#source("m:/Sandbox/flr/pkg/FLAdvice/R/lh.R")
#source("m:/Sandbox/flr/pkg/FLAdvice/R/M.R")
#source("m:/Sandbox/flr/pkg/FLAdvice/R/growth.R")
#
#*******************************************************************************
# Loading and looking at the ICES data
load("data/dbICES.RData")
# What's in it?
names(dbICES)
# "FindZeros" - nada
# "overview" - table of stock and wg - 49 stocks
# "ages" - table of stock, wg, min age and fbar range - why more stocks than overview table (87 of them)
# "ts" - table of stock, wg, then recruitment, biomass, ssb, landings, y-s, fbar through time, 49 stocks
# "id" - table of wg, stock and description of stock (291 stocks)
# "pa" - wg, stock, Flim, Fpa, Blim, Bpa
# "ypr" - stock parameters, everything for BRP object, e.g.  - 39 stocks

# We can use the parameters in ypr to compare against ones generated by lh()
# Don't have to stick to cod can do all species
# Will need LH for all species in all areas

# Add another column to ypr with species
dbICES$ypr <- cbind(dbICES$ypr, species = laply(strsplit(x = as.character(dbICES$ypr$stock), split= "-"),
                function(x) return(x[1])))

wgdata <- melt(dbICES$ypr, id.vars = c("wg", "stock", "age", "species"))

# Probably want to focus on species that we have more than data set for
nstocks <- ddply(ddply(wgdata, .(stock), summarise, species = unique(species)),
                  .(species), summarise, number = length(species))
# Maybe just stick to plaice (4), herring (4), cod (7), haddock (6) and sole (7)
# And just keep m, stock.wt and mat
wgdata <- wgdata[(wgdata$species %in% c("cod","ple","her","sol","had")) &
                  (wgdata$variable %in% c("m", "stock.wt", "mat")),]
# Need to tidy up the factors - plot looks bad atm
wgdata$stock <- factor(wgdata$stock)

# plot up stock.wts, m and mat for each species by age
p <- ggplot(wgdata) + geom_point(aes(x = age, y = value, group = stock, colour = stock)) +
  facet_grid(variable ~ species, scales="free")
# Can we scale the plot by max - comparing weights between stocks is bad

#*******************************************************************************

# Level one of data poorness.
# No Selectivity or Virgin Biomass
# Only Spr0 refpt comparison

# Null hypothesis on maturity is that it is knifedge - i.e. without further knowledge
# length at first maturity = length50 = length at 100% maturity
# With more info can add ogive shape

# Just focus on cod for the moment
wgcod <- wgdata[wgdata$species == "cod",]
wgcod$stock <- factor(wgcod$stock)

# Cod in North Sea is not here...
p <- ggplot(wgcod) + geom_point(aes(x = age, y = value, group = stock, colour = stock)) +
  facet_wrap(~variable, scales="free")

# Add Linf, K and mat(?) for each stock
# Just make them all the same as NS cod at the moment
# These values are from Sophy McCully email 24/01/12
age <- 1:30
NScod_linf <- 132
NScod_k <- 0.2
NScod_t0 <- 0
NScod_a <- 0.00653
NScod_b <- 3.097
Lmat_gis_dem <- 0.64 * NScod_linf ^ 0.95
NScod_lmat_wright <- 62.1 # Peter Wright paper Southern North Sea, 2011

lh1 <- FLPar(linf = NScod_linf)
lh2 <- FLPar(linf = NScod_linf, k = NScod_k)
lh3 <- FLPar(linf = NScod_linf, k = NScod_k, a = NScod_a, b = NScod_b)

# Life history parameter schedules
# 1. Linf, Gislason mort, FB mat knifeedge
# 2. Linf, k, Gislason mort, FB mat knifeedge
# 3. Linf, k, a, b, Gislason mort, FB mat knifeedge
# 4. Linf, Gislason mort, Gislason mat knifeedge
# 5. Linf, k, Gislason mort, Gislason mat knifeedge
# 6. Linf, k, a, b, Gislason mort, Gislason mat knifeedge
# 7. Linf, k, a, b, Gislason mort, Wright mat knifeedge


# Use gislason natural mortality function
# Bug with M.R at the moment so need to copy and paste in directly
# mFn("gislason", par, data)
# But data is length and that is calculated internally
# Inside lh() m. mFn dispatches with T we don't have that - interface problem
# From Table 1, Gislason 2010 - no T but needs to be included in interface at the moment
Gislason_model2 <- function(par,len, T) exp(0.55 - 1.61*log(len) + 1.44*log(par["linf"]) + log(par["k"]))

# LH with just Linf
# Where does default value of k come from?
# FLPar("k"=exp(0.5236+c(log(par["linf"]))*-0.4540))
NScodpar1 <- gislasim(lh1)

# LH with Linf and K
NScodpar2 <- gislasim(lh2)
# Split L-W relationship default into gadoid, demersal etc?
# e.g. for cod is it a = 0.01 or a = 0.001

# LH with Linf, K and a-b (L-W)
# Check these values!
NScodpar3 <- gislasim(lh3)


# LH with Linf, K and a-b (L-W) - add maturity
#NScodpar4 <- gislasim(FLPar(linf = NScod_linf, k = NScod_k, a = 0.0653, b = 3.097))
#NScodLH4 <- lh(NScodpar4, mFn = Gislason_model2, age = age)
# maturity by default is modelled using a logistic
# But logistic as function of age
# Uses a50 and ato95
# However, these are set using length, i.e. length at a50
# a50 = 0.8776 Linf - 0.038
# ato95 = 0
# These are lengths
# Then convert to age using invVB
# Then logistic function
# Gislason 2008 uses Lmat - length at first maturity: Lmat = q Linf ^ s, for pelagic, demersal and combined
# http://www.fishbase.org/manual/FishbaseThe_MATURITY_Table.htm
# Froese and Binohlan (2000) have likewise demonstrated that size and age at sexual maturity are strongly correlated with growth, maximum size and longevity.
# The default Lmat is from Fishbase (http://www.fishbase.org/manual/FishbaseThe_MATURITY_Table.htm)
# Lmat = 0.8776 * Linf - 0.038
# Alternatives are from Gislason 2008
# Lmat = 0.72 Linf ^ 0.93 (all NS species)
# Lmat = 0.64 Linf ^ 0.95 (demersal)
# Lmat = 5.10 Linf ^ 0.41 (pelagic)
# Use the Gislason 2008 relationship for Lmat
# Gislason all
#Lmat_all <- 0.72 * NScod_linf ^ 0.93
#Lmat_dem <- 0.64 * NScod_linf ^ 0.95
#a50_all <- invVonB(FLPar(linf = NScod_linf, k = NScod_k, t0 = NScod_t0),Lmat_all)
#a50_dem <- invVonB(FLPar(linf = NScod_linf, k = NScod_k, t0 = NScod_t0),Lmat_gis_dem)
NScodpar4 <- gislasim(FLPar(lh1))
NScodpar4["a50"] <- invVonB(NScodpar4,Lmat_gis_dem)
# ato95 = 0 gives a knife edge to maturity
# ato95 is number of years past a50 that 95% are mature.
# e.g if a50 = 10, then 50% of individuals are mature, if ato95 = 5, then at age 15 95% are mature
NScodpar5 <- gislasim(FLPar(lh2))
NScodpar5["a50"] <- invVonB(NScodpar5,Lmat_gis_dem)
NScodpar6 <- gislasim(FLPar(lh3))
NScodpar6["a50"] <- invVonB(NScodpar6,Lmat_gis_dem)

# Use Peter Wright's maturity value (2011)
NScodpar7 <- gislasim(FLPar(lh3))
NScodpar7["a50"] <- invVonB(NScodpar6,NScod_lmat_wright)


# Put all measures of interest into dataframe for plotting
# Watch units of stock.wt - should come out as kg
pullOutMeasures <- function(brp){
  dat <- rbind(
               cbind(variable = "stock.wt", as.data.frame(stock.wt(brp))),
               cbind(variable = "m", as.data.frame(m(brp))),
               cbind(variable = "mat", as.data.frame(mat(brp)))
              )
  # Chop out unwanted columns
  dat <- dat[,c("variable","age","data")]
  return(dat)
}

# Put all together
NScodpars <- list(lh1 = NScodpar1, lh2 = NScodpar2, lh3 = NScodpar3,
                  lh4 = NScodpar4, lh5 = NScodpar5, lh6 = NScodpar6,
                  lh7 = NScodpar7)
codLHs <- llply(NScodpars, lh, mFn = Gislason_model2, age = age)
codLHsdat <- ldply(codLHs, pullOutMeasures)

# Plot against the WG cod data
p <- ggplot(wgcod) + geom_point(aes(x=age, y = value, colour = stock)) +
                      geom_line(aes(x=age, y = value, colour = stock), linetype=2) +
                     facet_wrap(~variable, scales = "free")
                     
# Add in the LH stocks - change size (outside of aes() because we are not mapping size to variable
# but setting it for the layer
#p <- p + geom_line(aes(x=age, y = data, group = .id, colour = .id), size = 1, data = codLHsdat)
# Add a bit of jitter because maturity happens at the same place
p <- p + geom_line(aes(x=age, y = data, group = .id, colour = .id), size = 1, data = codLHsdat,
                    position = position_jitter(width = 0.2))
# Trim it down to 15yrs
p + scale_x_continuous(limits = c(0, 15))



#plot(codLHs[[1]])




#*******************************************************************************
# Make BRPS of the ICES stocks
# See ICESDataToBrps
#source("M:/Projects/MF1205/m1205/gislaSim/ICES/R/ICESDataToBrps.r")
#source("c:/Projects/m1205/gislaSim/ICES/R/ICESDataToBrps.r")
#load("M:/Projects/MF1205/m1205/gislaSim/ICES/data/wgBrps.Rdata")
load("c:/Projects/m1205/gislaSim/ICES/data/wgBrps.Rdata")

# They are all in brps - 39 of them
length(brps)

# Get the cod stocks
codBrps <-brps[names(brps) %in% unique(wgcod$stock)]

# spr0 - spawners per recruit at zero fishing
# i.e. SSB / Rec at F = 0
# unaffected by SRR or fishing

# spr0 of all WG stocks
llply(brps, spr0)
# spr0 of WG Cod stocks
llply(codBrps, spr0)
# spr0 of my new stocks
llply(codLHs, spr0)

# Table these up
codSpr0s <- rbind(
  ldply(codBrps, function(x) data.frame(spr0 = c(spr0(x)))),
  ldply(codLHs, function(x) data.frame(spr0 = c(spr0(x))))
)

# Difference will be caused by natural mortality
# LH m is much higher on earlier life


#*******************************************************************************
# Set three selectivities
# Flat to the left of maturity
# Flat to the right of maturity
# Dome shaped

# Use dnormal with these parameters:
# selLeft <- FLParam(a1 = amat - 1, sl = 0.1, sr = 1e6)
# selRight <- FLParam(a1 = amat + 1, sl = 0.1, sr = 1e6)
# selDome <- FLParam(a1 = amat - 1, sl = 0.1, sr = 0.2)

clh <- codLHs[[1]]
#NScodpars <- list(lh1 = NScodpar1, lh2 = NScodpar2, lh3 = NScodpar3,
#                  lh4 = NScodpar4, lh5 = NScodpar5, lh6 = NScodpar6,
#                  lh7 = NScodpar7)
#

lh1 <- NScodpars[[1]]

makeSelsLH <- function(lhPars,age)
{
  selLeftPars <- FLPar(a1 = floor(lhPars["a50"]) - 1, sl = 0.5, sr = 1e6)
  selRightPars <- FLPar(a1 = floor(lhPars["a50"]) + 1, sl = 0.5, sr = 1e6)
  selDomePars <- FLPar(a1 = floor(lhPars["a50"]) - 1, sl = 0.5, sr = 5)
  
  selLeft <- dnormal(selLeftPars,age)
  selRight <- dnormal(selRightPars,age)
  selDome <- dnormal(selDomePars,age)
  
  return(list(selLeft = selLeft, selRight = selRight, selDome = selDome))
}

sels <- makeSelsLH(lh1,age)
selDF <- cbind(age = rep(age,3),melt(sels))
ggplot(selDF) + geom_line(aes(x = age, y = value, group = L1, colour = L1))


NScodpars <- list(lh1 = NScodpar1, lh2 = NScodpar2, lh3 = NScodpar3,
                  lh4 = NScodpar4, lh5 = NScodpar5, lh6 = NScodpar6,
                  lh7 = NScodpar7)
                  
# Make more NScodpars with sels
# For non cod stocks might want to pass the age shift and SD on age
# structure
# Also maturity ogive?
#addSelsToLH <- function(lhPar)
#{
#  leftSel <- lhPar
#  leftSel[c("a1","sl","sr")] <- c(floor(lhPar["a50"]) - 1, 0.5, 1e6)
#  rightSel <- lhPar
#  rightSel[c("a1","sl","sr")] <- c(floor(lhPar["a50"]) + 1, 0.5, 1e6)
#  domeSel <- lhPar
#  domeSel[c("a1","sl","sr")] <- c(floor(lhPar["a50"]) - 1, 0.5, 5)
#  return(list(leftSel = leftSel, rightSel = rightSel, domeSel = domeSel))
#}
#
#test <- addSelsToLH(NScodpars[[1]])
#test <- llply(NScodpars, addSelsToLH)
#t2 <- unlist(lapply(test, unlist))
#

NSCodParsSel <- list()
for (i in 1:length(NScodpars))
{
  leftSel <- NScodpars[[i]]
  leftSel[c("a1","sl","sr")] <- c(floor(NScodpars[[i]]["a50"]) - 1, 0.5, 1e6)
  rightSel <- NScodpars[[i]]
  rightSel[c("a1","sl","sr")] <- c(floor(NScodpars[[i]]["a50"]) + 3, 0.5, 1e6)
  domeSel <- NScodpars[[i]]
  domeSel[c("a1","sl","sr")] <- c(floor(NScodpars[[i]]["a50"]) - 1, 0.5, 5)
  NSCodParsSel[[paste(names(NScodpars)[i],"leftSel",sep="_")]] <- leftSel
  NSCodParsSel[[paste(names(NScodpars)[i],"rightSel",sep="_")]] <- rightSel
  NSCodParsSel[[paste(names(NScodpars)[i],"domeSel",sep="_")]] <- domeSel
}

codLHsSel <- llply(NSCodParsSel, lh, mFn = Gislason_model2, age = age)
codLHsSelDat <- ldply(codLHsSel, pullOutMeasures)


# Plot these up to have a look - each block of three is the same
p <- ggplot(wgcod) + geom_point(aes(x=age, y = value, colour = stock)) +
                      geom_line(aes(x=age, y = value, colour = stock), linetype=2) +
                     facet_wrap(~variable, scales = "free")

# Add in the LH stocks - change size (outside of aes() because we are not mapping size to variable
# but setting it for the layer
#p <- p + geom_line(aes(x=age, y = data, group = .id, colour = .id), size = 1, data = codLHsdat)
# Add a bit of jitter because maturity happens at the same place
p <- p + geom_line(aes(x=age, y = data, group = .id, colour = .id), size = 1, data = codLHsSelDat,
                    position = position_jitter(width = 0.2))
# Trim it down to 15yrs
p + scale_x_continuous(limits = c(0, 15))

#refpts(codLHsSel[[1]])

# Get spr0, f.01, fmax
getSpr0F01Fmax <- function(brp){
  f0.1 <- c(refpts(brp)["f0.1","harvest"])
  fmax <- c(refpts(brp)["fmax","harvest"])
  spr0 <- c(spr0(brp))
  return(data.frame(spr0 = spr0, f0.1 = f0.1, fmax = fmax))
}

fRefPtsLH <- ldply(codLHsSel, getSpr0F01Fmax)

# Plot maturity and selectivity for each brp

# Make a df so we can put three sels and a mat on each plot
getMatSel <- function(brp){
  df <- rbind(
    cbind(measure = "mat", as.data.frame(mat(brp))),
    cbind(measure = "sel", as.data.frame(landings.sel(brp)/ c(max(landings.sel(brp)))))
  )
  # remove unwanted columns
  df <- df[,c(1, 2, 8)]
  return(df)
}

LHMatSelDf <- ldply(codLHsSel, getMatSel)
LHMatSelDf <- cbind(LHMatSelDf, ldply(strsplit(LHMatSelDf$.id, "_"), function(x) return(data.frame(lh = x[1], sel = x[2]))))

p <- ggplot(LHMatSelDf) + geom_line(aes(x = age, y = data, group = measure, colour = measure)) +
  facet_grid(sel ~ lh, scales = "free") +
  scale_x_continuous(lim = c(0,15))
# Woohoo!

# Now do the same for the WG data
NSCodParsSelWG <- list()
for (i in 1:length(codBrps))
{
  # Need to get equivalent of a50 from WG data
  # smallest age with mat > 0.5
  age <- as.numeric(dimnames(landings.sel(codBrps[[i]]))$age)
  a50 <- age[min(which(mat(codBrps[[i]]) >= 0.5))]
  leftSel <- codBrps[[i]]
  leftSelParams <- FLPar("a1" = a50 -1, "sl" = 0.5, "sr" = 1e6)
  landings.sel(leftSel)[] <- dnormal(leftSelParams, age)

  rightSel <- codBrps[[i]]
  rightSelParams <- FLPar("a1" = a50 +3, "sl" = 0.5, "sr" = 1e6)
  landings.sel(rightSel)[] <- dnormal(rightSelParams, age)

  domeSel <- codBrps[[i]]
  domeSelParams <- FLPar("a1" = a50 -1, "sl" = 0.5, "sr" = 5)
  landings.sel(domeSel)[] <- dnormal(domeSelParams, age)

  NSCodParsSelWG[[paste(names(codBrps)[i],"leftSel",sep="_")]] <- brp(leftSel)
  NSCodParsSelWG[[paste(names(codBrps)[i],"rightSel",sep="_")]] <- brp(rightSel)
  NSCodParsSelWG[[paste(names(codBrps)[i],"domeSel",sep="_")]] <- brp(domeSel)
}

# calling brp changes the landings.sel???
# F = Fbar * sel
# Mean selectivity over the Fbar range is 1


# Can we plot the sels and mats?
WGMatSelDf <- ldply(NSCodParsSelWG, getMatSel)
WGMatSelDf <- cbind(WGMatSelDf, ldply(strsplit(WGMatSelDf$.id, "_"), function(x) return(data.frame(lh = x[1], sel = x[2]))))

p <- ggplot(WGMatSelDf) + geom_line(aes(x = age, y = data, group = measure, colour = measure)) +
  facet_grid(sel ~ lh, scales = "free") +
  scale_x_continuous(lim = c(0,15))


# Get ref pts
fRefPtsLH <- ldply(codLHsSel, getSpr0F01Fmax)
fRefPtsWG <- ldply(NSCodParsSelWG, getSpr0F01Fmax)

# Look at hists based on selectivity
fRefPts <- rbind(
            cbind(group = "LH", fRefPtsLH),
            cbind(group = "WG", fRefPtsWG)
            )
fRefPts <- cbind(fRefPts, ldply(strsplit(fRefPts$.id, "_"), function(x) return(data.frame(lh = x[1], sel = x[2]))))
fRefPtsDf <- melt(fRefPts, measure.vars = c("spr0", "f0.1", "fmax"))

# compare the spread of the refpoints
p <- ggplot(fRefPtsDf) + geom_boxplot(aes(x = group, y = value)) +
  facet_grid(variable ~ sel, scales= "free")

# Need to check that the right side selectivity is suitable for comparison
#*******************************************************************************
# For each of these BRPs, make three more with three different SRRs
# Get MSY and FMSY

#*******************************************************************************
# M being different is the same as not knowing what value of steepness to use. Who does?



##vb <- function(t, linf, k, t0 = 0)
## return(linf * (1 - exp(-k * (t - t0))))
### age 1 fish =
##vb(1,132,0.2)
#
#
## Next
##add in WG NSea data to dbICES$ypr
##get more cod stock LH params (not just NSea)
##more levels of LH info (+ LW, mat etc)
#
## Get Spr0 from WG (in table already or need to put into BRP)
## Get Spr0 from LH with increasing LHinfo
## Compare

# Then selectivity
# For each of the codLHs above, we want to set upto three types of selectivity
# 1) Flat to the left of maturity
# 2) Flat to the right of maturity
# 3) Dome

# Do this for each codLH and for codWG too?
# I guess for comparison we have to

# ATM we have 7 LH cods
# Blow this up to 21? Sure
# Same for WG cods




#*******************************************************************************


# An aside from Kell
#codM2=read.table("C:/Projects/gbyp-sam/papers/SCRS/SCRS2012-Lifehistory/data/CodM2.csv",sep=",")[-1,-1]
#codM2=FLQuant(unlist(c(codM2)),dimnames=list(age=0:11,year=1963:2000))
#ggplot(codM2)+geom_line(aes(year,data,colour=factor(age)))
#ggplot(codM2)+geom_line(aes(age,data,colour=factor(year)))
#
