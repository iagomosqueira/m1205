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

test <- lapply(brps, function(x) spr0(x))

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
codBrps <-brps[names(brps) %in% unique(wgcod$stock)]

# 2)
# Set the fbar range
# split $ages fbarage into min and max
# Make a DF of min and max fbar
dbICES$ages <- cbind(dbICES$ages,
               ldply(strsplit(x = as.character(dbICES$ages$fbarage), split= "-"),
                  function(x) return(data.frame(minfbar = x[1], maxfbar = x[2]))))

# This is very hacky
for (brp in names(brps)){
  if (brp %in% dbICES$ages$stock){
    range(brps[[brp]])[c("minfbar","maxfbar")] <-
      as.numeric(dbICES$ages[dbICES$ages$stock == brp, c("minfbar","maxfbar")])
  }
}

# 3)
# Add observed data
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


# save(brps, file = "M:/Projects/MF1205/m1205/gislaSim/ICES/R/wgBrps.Rdata")
