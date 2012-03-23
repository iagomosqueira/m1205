gislasim=function(par,t0=-0.1,a=0.00001,b=3,asym=1.0,ato95=1,sl=2,sr=5000,a1=0,s=0.9,v=1000){

  #browser()

  names(dimnames(par)) <- tolower(names(dimnames(par)))

  if (!("t0"    %in% dimnames(par)$params)) par=rbind(par,FLPar("t0"    =t0, iter=dims(par)$iter))
  if (!("a"     %in% dimnames(par)$params)) par=rbind(par,FLPar("a"     =a,  iter=dims(par)$iter))
  if (!("b"     %in% dimnames(par)$params)) par=rbind(par,FLPar("b"     =b,  iter=dims(par)$iter))
  if (!("bg"    %in% dimnames(par)$params)) par=rbind(par,FLPar("bg"    =par["b"],  iter=dims(par)$iter))
  if (!("sl"    %in% dimnames(par)$params)) par=rbind(par,FLPar("sl"    =sl, iter=dims(par)$iter))
  if (!("sr"    %in% dimnames(par)$params)) par=rbind(par,FLPar("sr"    =sr, iter=dims(par)$iter))
  #if (!("a1"    %in% dimnames(par)$params)) par=rbind(par,FLPar("a1"    =a1, iter=dims(par)$iter))
  if (!("s"     %in% dimnames(par)$params)) par=rbind(par,FLPar("s"     =s,  iter=dims(par)$iter))
  if (!("v"     %in% dimnames(par)$params)) par=rbind(par,FLPar("v"     =v,  iter=dims(par)$iter))

  ## growth parameters
  if (!("k"     %in% dimnames(par)$params)) par=rbind(par,FLPar("k"=3.15*par["linf"]^(-0.64), iter=dims(par)$iter)) # From Gislason et al 2008, all species combined

  # Natural mortality parameters from Model 2, Table 1 Gislason 2010
  par=rbind(par,FLPar(M1=0.55+1.44*log(par["linf"])+log(par["k"]), iter=dims(par)$iter),
                FLPar(M2=-1.61                                   , iter=dims(par)$iter))

  if (!("ato95" %in% dimnames(par)$params)) par=rbind(par,FLPar("ato95" =ato95, iter=dims(par)$iter))
  if (!("sl"    %in% dimnames(par)$params)) par=rbind(par,FLPar("sl"    =sl,    iter=dims(par)$iter))
  if (!("sr"    %in% dimnames(par)$params)) par=rbind(par,FLPar("sr"    =sr,    iter=dims(par)$iter))

  ## maturity parameters from http://www.fishbase.org/manual/FishbaseThe_MATURITY_Table.htm
  if (!("asym"    %in% dimnames(par)$params)) par=rbind(par,FLPar("asym"    =asym, iter=dims(par)$iter))

  if (!("a50" %in% dimnames(par)$params)){
    par=rbind(par,FLPar(a50=0.72*par["linf"]^0.93, iter=dims(par)$iter))
    par["a50"]=invVonB(par,c(par["a50"]))
    }

  ## selectivity guestimate
  selPar=par["a50"]+a1

  dimnames(selPar)$params[1]="a1"

  par=rbind(par,selPar)

#  par=rbind(par,FLPar(s=s,v=v,    iter=dims(par)$iter))

  attributes(par)$units=c("cm","kg","1000s")

  return(par)
}

invVonB <- function(params,data){
    a <- -log(1.0-(data/params["linf"]))/params["k"]+params["t0"]
    return(a)
}

vonB=function(params,data){
  res <- params["linf"]%*%(1.0-exp(-params["k"]%*%(data-params["t0"])))
  dimnames(res) <- dimnames(data)
  return(res)
}


# 1. Get Linf if not available
# 2. Use gislasim to get FLPar
# 3. Correct maturity parameters
# 4. Set selectivity parameters
# Each row of data.frame gets put into gislasim
gismysharkup <- function(df){
    #cat(df$species, "\n")
    # Do we have Linf? If not use Froese and Binohlan 2000
    if (is.na(df$linf)) df$linf <- 10^(0.044 + 0.9841 * log10(df$lmax))
    # Now sort out FLPar - only pass in these values (if we have them)
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
    # Only do this if we have a given value of 150 too - else can get a50 < ageto5
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

