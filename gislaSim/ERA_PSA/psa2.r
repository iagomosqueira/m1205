# Try to set F range when making BRP in lh()
# set steepness to 1
# Look at stock and catch of brp object. rec should be constant. Should then see when the Catch is highest
# Not sure we need to set rec = 1, but good if we could just to make sure.


# Running gislasim() on species in PSA (ERA) from DefineIt and M1205
# Started: 13/03/12
# Finlay Scott and Sophy McCully

# Data we can use:
#   Lmax (to give us Linf)
#   Linf (else: log Linf = 0.044 + 0.9841 * log Lmax)
# Growth
#   k (default from Linf, k = 3.15*Linf^(-0.64))
#   a and b (default: a=0.00001,b=3, convert cm -> kg)
#   t0 (default: -0.1)
# Natural mortality
#   M1 (default from Linf and k: M1 = 0.55+1.44*log(Linf)+log(k)
#   M2 (default: -1.61)
# Maturity
#   ato95 (default: 1)
#   a50 (default from Linf: a50=0.72*Linf^0.93
#   asym (default: 1 (asymptotic value))
# Selectivity
#   sl (default: 2)
#   sr (default: 5000)
#   a1 (default: a50 + 2)


# Note that setting a50 must happen after call to gislasim - not passed in as argument
# Should flag this up?
#setwd("m:/Projects/MF1205/m1205/gislaSim/ERA_PSA")
setwd("c:/Projects/m1205/gislaSim/ERA_PSA")
library(FLAdvice)

# Get my version of gislasim() before LK fucks it up
source("mygislasim.r")

# Load dataset
lhdf <- read.csv("Gislasim LH elasmo params v2.csv")
# sort them
lhdf <- lhdf[order(lhdf$species),]

gisparsdf <- ddply(lhdf, .(species,sex,variant), gismysharkup)
# for checking
gisparsall <- cast(gisparsdf, species + sex + variant ~ params, value = "data")

# check that bg bug
gisparsdf[gisparsdf$params %in% c("b","bg"),]
stks <- dlply(gisparsdf, .(species,sex,variant), brpmaker)

# Check GLS
#t <- seq(from = 0, to = 100, by = 1)
#glsp <- FLPar(linf = 605, k = 0.052201, t0 = -0.1)
#l <- vonB(glsp,t)
#w <- 0.00001 * l ^ 3
#plot(t,w)
## It's a whopper!

# Check Spotted ray
#t <- seq(from = 0, to = 100, by = 1)
#srsp <- FLPar(linf = 68.7, k = 0.19, t0 = 0.56)
#l <- vonB(srsp,t)
#w <- 4.2e-6 * l ^ 3.1
#plot(t,w)

# Check Spurdog M
#t <- seq(from = 0, to = 100, by = 1)
#srspM <- FLPar(linf = 81.36, k = 0.11, t0 = -2.166)
#lM <- vonB(srspM,t)
#srspF <- FLPar(linf = 110.66, k = 0.155, t0 = -3.306)
#lF <- vonB(srspF,t)
#

#
# Useful plots?
pullOutMeasures <- function(brp){
  dat <- rbind(
               cbind(variable = "stock.wt", as.data.frame(stock.wt(brp))),
               cbind(variable = "m", as.data.frame(m(brp))),
               cbind(variable = "mat", as.data.frame(mat(brp))),
               cbind(variable = "landings.sel", as.data.frame(landings.sel(brp)))
              )
  # Chop out unwanted columns
  dat <- dat[,c("variable","age","data")]
  return(dat)
}

stk_summary <- ldply(stks, pullOutMeasures)

# Plot against the WG cod data
p <- ggplot(stk_summary) + geom_point(aes(x=age, y = data, group = sex, colour = sex)) +
                      geom_line(aes(x=age, y = data, group = sex, colour = sex))
p <- p + facet_grid(variable~species, scales = "free")


# Just plot wts
pdf(file="stock_wts.pdf", width = 11, height = 8)
p <- ggplot(stk_summary[stk_summary$variable == "stock.wt",]) + geom_point(aes(x=age, y = data, group = sex, colour = sex)) +
                      geom_line(aes(x=age, y = data, group = sex, colour = sex))
p <- p + facet_wrap(~species, scales = "free")
print(p)
dev.off()

# Just plot mat
pdf(file="mat.pdf", width = 11, height = 8)
p <- ggplot(stk_summary[stk_summary$variable == "mat",]) + geom_point(aes(x=age, y = data, group = sex, colour = sex)) +
                      geom_line(aes(x=age, y = data, group = sex, colour = sex))
p <- p + facet_wrap(~species, scales = "free")
print(p)
dev.off()

# Just plot m
pdf(file="m.pdf", width = 11, height = 8)
p <- ggplot(stk_summary[stk_summary$variable == "m",]) + geom_point(aes(x=age, y = data, group = sex, colour = sex)) +
                      geom_line(aes(x=age, y = data, group = sex, colour = sex))
p <- p + facet_wrap(~species, scales = "free")
print(p)
dev.off()



#
# Now get refpts
getSpr0F01Fmax <- function(brp){
  f0.1 <- c(refpts(brp)["f0.1","harvest"])
  fmax <- c(refpts(brp)["fmax","harvest"])
  spr0 <- c(spr0(brp))
  return(data.frame(spr0 = spr0, f0.1 = f0.1, fmax = fmax))
}

fRefPtsLH <- ldply(stks, getSpr0F01Fmax)
write.csv(fRefPtsLH, file="refpts.csv")
# Why fmax so high for female makos?
# should not be higher than males


# Maturity / selectivity plots
getMatSel <- function(brp){
  df <- rbind(
    cbind(measure = "mat", as.data.frame(mat(brp))),
    cbind(measure = "sel", as.data.frame(landings.sel(brp)/ c(max(landings.sel(brp)))))
  )
  # remove unwanted columns
  df <- df[,c(1, 2, 8)]
  return(df)
}

LHMatSelDf <- ldply(stks, getMatSel)
#LHMatSelDf <- cbind(LHMatSelDf, ldply(strsplit(LHMatSelDf$.id, "_"), function(x) return(data.frame(lh = x[1], sel = x[2]))))

p <- ggplot(LHMatSelDf) + geom_line(aes(x = age, y = data, group = measure, colour = measure)) +
  facet_grid(species ~ sex, scales = "free") +
  scale_x_continuous(lim = c(0,20))



