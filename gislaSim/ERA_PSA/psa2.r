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
setwd("M:/Projects/MF1205/m1205/gislaSim/ERA_PSA")
library(FLAdvice)

invVonB <- function(params,data){
    a <- -log(1.0-(data/params["linf"]))/params["k"]+params["t0"]
    return(a)
}

vonB=function(params,data){
  res <- params["linf"]%*%(1.0-exp(-params["k"]%*%(data-params["t0"])))
  dimnames(res) <- dimnames(data)
  return(res)
}


# Load dataset
lhdf <- read.csv("Gislasim LH elasmo params.csv")


# Want data in data.frame with columns:
testdf <- data.frame(species = c("Greenland shark","Shortfin mako","Shortfin mako"),
                     sex = c("MF","F","M"),
                     variant = c(1,1,1), # variant is in case we try different params for same species and sex
                     lmax = c(650,396,396),
                     linf = c(NA,345,302),
                     k = c(NA, 0.203, 0.266),
                     t0 = c(NA,-1,-1),
                     a = c(NA,0.00000280834,0.00000280834),
                     b = c(NA,3.20182,3.20182),
                     # maturity - length based. Need three params: a50, ato95, asym
                     l50 = c(NA,285,210),
                     lto5 = c(NA,195,NA),# assume that length at first maturity is 5% mature
                     asym = c(1,1,1)
                    )

# 1. Get Linf if not available
# 2. Use gislasim to get FLPar
# 3. Correct maturity parameters
# 4. Set selectivity parameters
# Each row of data.frame gets put into gislasim
gismysharkup <- function(df){
    #cat(df$species, "\n")
    # Do we have Linf? If not use Froese and Binohlan 2000
    if (is.na(df$linf)) df$linf <- 10^(0.044 + 0.9841 * log10(df$lmax))
    # Now sort out FLPar
    gispars <- FLPar(unlist(df[,c("linf", "k", "t0", "a", "b")]))
    # chop out NAs so only include LH params that we have values for - all others estimated by gislasim()
    gispars <- gispars[!is.na(gispars)]
    # stuff into gislasim to get rest of pars
    # Check bg
    #browser()
    gispars <- gislasim(gispars)

    # Set maturity
    # gislasim() automatically sets a50 and ato95
    # We want to use our own were available
    # so set a50 based on l50 in dataframe if available and convert from length to age
    if (!is.na(df$l50)) gispars["a50"] <- invVonB(gispars,df$l50)
    # We have no ato95 data but we do have length at first maturity (convert to age at first maturity).
    # We can assume that this age at which 5% are mature.
    # ato95 is then the difference between a50 and ageat5%
    if (!is.na(df$lto5) & !is.na(df$l50)) gispars["ato95"] <- gispars["a50"] - invVonB(gispars,df$lto5) # based on age at 5% mature

    # Set selectivity
    # Just have one to start with
    # knife edge right through a50 - 1
    # so that <50% mature are fully selected
    # a1 parameter is the middle of dnormal, i.e. age at which fully selected
    gispars["a1"] <- gispars["a50"]
    # Tone down LHS so more knife edge
    gispars["sl"] <- 1e-9
    gispars["sr"] <- 1e9

    # Set steepness to 1 to get YPR
    gispars["s"] <- 1
    gispars["v"] <- 1
    
    # Now make the stock.

    #stk <- lh(gispars)
    #return(stk)
    return(as.data.frame(gispars))
}

# 5. Make the stock
brpmaker <- function(df){
    gispars <- FLPar(df$data)
    dimnames(gispars)$params <- df$params
    stk <- lh(gispars)
    return(stk)
}

gisparsdf <- ddply(lhdf, .(species,sex,variant), gismysharkup)
test <- dlply(gisparsdf, .(species,sex,variant), brpmaker)

# Looking at estimated parameters
gisparsall <- cast(gisparsdf, species + sex + variant ~ params, value = "data")


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

test_stk_summary <- ldply(test, pullOutMeasures)

# Plot against the WG cod data
p <- ggplot(test_stk_summary) + geom_point(aes(x=age, y = data, group = sex, colour = sex)) +
                      geom_line(aes(x=age, y = data, group = sex, colour = sex))
p <- p + facet_grid(variable~species, scales = "free")
p

# Look at thresher
lhp <- FLPar(linf = 343.94071, k = 0.07498280, t0 = -0.1)
t <- seq(from =0, to = 40, by = 1)
l <- vonB(lhp,t)
w <- 1.88e-4 * l ^ 2.5188
# = 405kg NOT 6000!
# WTF?!
df <- gisparsdf[gisparsdf$species=="Thresher shark*" & gisparsdf$sex == "MF",]

# Now get refpts
getSpr0F01Fmax <- function(brp){
  f0.1 <- c(refpts(brp)["f0.1","harvest"])
  fmax <- c(refpts(brp)["fmax","harvest"])
  spr0 <- c(spr0(brp))
  return(data.frame(spr0 = spr0, f0.1 = f0.1, fmax = fmax))
}

fRefPtsLH <- ldply(test, getSpr0F01Fmax)
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

LHMatSelDf <- ldply(test, getMatSel)
#LHMatSelDf <- cbind(LHMatSelDf, ldply(strsplit(LHMatSelDf$.id, "_"), function(x) return(data.frame(lh = x[1], sel = x[2]))))

p <- ggplot(LHMatSelDf) + geom_line(aes(x = age, y = data, group = measure, colour = measure)) +
  facet_grid(species ~ sex, scales = "free") +
  scale_x_continuous(lim = c(0,20))



