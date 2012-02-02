# ICESDataToBrps

# Takes the stuff from the dbICES object
# And turns it into a load of BRP objects
# It's really slow!
# Could be sped up

# 1) Create the BRP object from the LH values using the $ypr DF
# This sets the weights etc
brps=FLBRPs(dlply(dbICES$ypr, .(stock), FLBRP))
# SET AVAILABILITY OF EVERYTHING TO 1
brps <- llply(brps, function(x) {x@availability[] <- 1; return(x)})

#test <- lapply(brps, function(x) spr0(x))

# Or do it long hand
#sub <- dbICES$ypr[dbICES$ypr$stock == 'cod-arct',]
## make quants
#dimnms <- list(age = sub$age)
#m <- FLQuant(sub$m, dimnames = dimnms)
#mat <- FLQuant(sub$mat, dimnames = dimnms)
#harvest.spwn <- FLQuant(sub$harvest.spwn, dimnames = dimnms)
#m.spwn <- FLQuant(sub$m.spwn, dimnames = dimnms)
#stock.wt <- FLQuant(sub$stock.wt, dimnames = dimnms)
#catch.wt <- FLQuant(sub$catch.wt, dimnames = dimnms)
#catch.sel <- FLQuant(sub$catch.sel, dimnames = dimnms)
#discards.sel <- FLQuant(sub$discards.sel, dimnames = dimnms)
#discards.wt <- FLQuant(sub$discards.wt, dimnames = dimnms)
#bycatch.harvest <- FLQuant(sub$bycatch.harvest, dimnames = dimnms)
#bycatch.wt <- FLQuant(sub$bycatch.wt, dimnames = dimnms)
#landings.sel <- catch.sel-discards.sel
#landings.wt <- (catch.sel*catch.wt-discards.sel*discards.wt)/(catch.sel+discards.sel)
#
#res=FLBRP(
#             landings.wt    = landings.wt,
#             discards.wt    = discards.wt,
#             landings.sel   = landings.sel,
#             discards.sel   = discards.sel,
#             stock.wt       = stock.wt,
#             m              = m,
#             mat            = mat,
#             bycatch.harvest= bycatch.harvest,
#             harvest.spwn   = harvest.spwn,
#             m.spwn         = m.spwn,
#             bycatch.wt     = FLQuant(0,    dimnames=dimnms),
#             availability   = FLQuant(1,    dimnames=dimnms))
#
#stock.n(res)
#harvest(res)
#spr0(res)
#
## does model affect spr0? - No
#res@params[] <- 10
## and how does res differ to brps[[1]]?
#res2 <- brps[["cod-arct"]]
#res2@availability[] <- 1
#spr0(res2)
#
# Does landings.sel affect it?

# Only keep cod brps
#codBrps <-brps[names(brps) %in% unique(wgcod$stock)]

# Why are those spr0s so different low? Maturity
#laply(codBrps, function(x) spr0(x))
#laply(codLHs, function(x) spr0(x))
#
#stock.n(codBrps[[1]])
#stock.n(codLHs[[1]])
#
#b1 <- codBrps[[1]]
#sweep(stock.n(b1),c(1,3:6),stock.wt(b1) * mat(b1),"*")# * exp(- harvest(b1)*harvest.spwn(b1) - m(b1) * m.spwn(b1))
## spawners (i.e. ssb) # First 'year#, F = 0 / rec at f = 0
#apply(sweep(stock.n(b1),c(1,3:6),stock.wt(b1) * mat(b1),"*"),2:6,sum)
#stock.n(b1)[1,1]
#apply(sweep(stock.n(b1),c(1,3:6),stock.wt(b1) * mat(b1),"*"),2:6,sum)[1,1] / stock.n(b1)[1,1]
#spr0(b1)
#
#l1 <- codLHs[[1]]
#apply(sweep(stock.n(l1),c(1,3:6),stock.wt(l1) * mat(l1),"*"),2:6,sum)
#apply(sweep(stock.n(l1),c(1,3:6),stock.wt(l1) * mat(l1),"*"),2:6,sum)[1,1] / stock.n(l1)[1,1]
#spr0(l1)
#
#range(l1)
#
# 2)
# Set the fbar range
# split $ages fbarage into min and max
# Make a DF of min and max fbar
dbICES$ages <- cbind(dbICES$ages,
               ldply(strsplit(x = as.character(dbICES$ages$fbarage), split= "-"),
                  #function(x) return(data.frame(minfbar = x[1], maxfbar = x[2]))))
                  # Use a regular expression to only include numbers
                  function(x) return(data.frame(minfbar = as.numeric(sub("[^0-9]","",x[1])),
                                                maxfbar = as.numeric(sub("[^0-9]","",x[2]))))))
# Some of these have a , in them
# Split and keep only numbers
#x <- strsplit(as.character(dbICES$ages[31,"fbarage"]), split = "-")[[1]]
#sub("[^0-9]","",x[2])

# check hat minF < maxF
#all(dbICES$ages$minfbar < dbICES$ages$maxfbar)

# This is very hacky
for (brp in names(brps)){
  if (brp %in% dbICES$ages$stock){
    range(brps[[brp]])[c("minfbar","maxfbar")] <-
      as.numeric(dbICES$ages[dbICES$ages$stock == brp, c("minfbar","maxfbar")])
  }
}
# Again check
#all(laply(brps, function(x) range(x)["minfbar"] < range(x)["maxfbar"]))

#codBrps <-brps[names(brps) %in% unique(wgcod$stock)]

# 3)
# Add observed data from ts DF
# data we can add from ts
#   landings.obs
#   rec.obs
#   stock.obs <- biomass.obs (or rename in dataframe)
#   ssb.obs
#   fbar.obs
for (brp in names(brps)){
  # make an FLQuant of the measure
  if (brp %in% dbICES$ts$stock){
    sub <- dbICES$ts[dbICES$ts$stock %in% brp,]
    # Set the slots
    for (slot in c("landings.obs", "rec.obs", "ssb.obs", "fbar.obs")){
      flq <- FLQuant(sub[,slot], dimnames = list(year = sub$year))
      slot(brps[[brp]], slot) <- flq
    }
    # Set the biomass slot seperately as it has a different name
    flq <- FLQuant(sub$biomass.obs, dimnames = list(year = sub$year))
    slot(brps[[brp]], "ssb.obs") <- flq
  }
}

# 4)
# Set precautionary reference points
#If you put fpa in the harvest slot of refpts and leave everything else as na brp will calculate all the other quantities. Ie a check for consistency
#dbICES$pa
#r <- refpts(codBrps[[1]])
# need to add another row to refpts


#save(brps, file = "M:/Projects/MF1205/m1205/gislaSim/ICES/data/wgBrps.Rdata")
