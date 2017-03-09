# create sub Dafaframe based on groups
subDatAvg <-
  function(x) {
    match.pat = x
    col.is.a.match <- apply(var.list, 2, identical, match.pat)
    # Subset var.csv to match selected variables
    all = var.list[col.is.a.match]
    # List variable names you selected
    allVarnames=names(all)
    # subseeting dataframe from selected variables
    Df=subset(iuDatAvg, select=allVarnames)
  }

subDatUse <-
  function(x) {
    match.pat = x
    col.is.a.match <- apply(var.list, 2, identical, match.pat)
    # Subset var.csv to match selected variables
    all = var.list[col.is.a.match]
    # List variable names you selected
    allVarnames=names(all)
    # subseeting dataframe from selected variables
    Df=subset(iuDatUse, select=allVarnames)
  }


# mean functions
meanMean= function(x){
  m=sum(aggregate(x~type, iuDatAvg, mean)[,2]*wg)
  return(m)
}

meanCi= function(x){
  wg=(sapply(match.pat !='', as.numeric)*iuSamp)/sapply(match.pat !='', as.numeric) %*% iuSamp
  va=aggregate(x ~ type, iuDatAvg, var)
  ob=aggregate(x ~ type, iuDatAvg, length)
  se=sqrt(sum(wg^2 * va[,2]/ob[,2] * ((iuSamp - ob[,2])/iuSamp)))
  ci=1.96*se
  return(ci)
}


# satifaction functions
satisTable= function(x){
  #wg=(sapply(match.pat !='', as.numeric)*iuSamp)/sapply(match.pat !='', as.numeric) %*% iuSamp
  #wg=(sapply(match.pat !='', as.numeric)*iuSamp)/sapply(match.pat !='', as.numeric) %*% iuSamp
  # na.action allows to count NAs
  tb=xtabs(~x+type, data=iuDatSat, exclude = 0)
  #aaa=matrix(tb, ncol=4)
  return(tb)
}

wgtTable= function(x){
  wg=(sapply(match.pat !='', as.numeric)*iuSamp)/sapply(match.pat !='', as.numeric) %*% iuSamp
  type=c(1,2,3,4)
  comb=cbind(wg,type)
  return(comb)
}

# usage functions
useTable=
  function(x){
    tb=xtabs(~x+type, data=iuDatUse, exclude = 0)
    aaa=matrix(tb, ncol=4)
    return(tb)
  }
