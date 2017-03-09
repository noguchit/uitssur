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





smallDf <-
  function(x) {
    match.pat = x
    col.is.a.match <- apply(var.list, 2, identical, match.pat)
    # Subset var.csv to match selected variables
    all = var.list[col.is.a.match]
    # List variable names you selected
    allVarnames=names(all)
    # subseeting dataframe from selected variables
    Df=subset(iuDat2, select=allVarnames)
  }


# mean functions
meanMean= function(x){
  m=sum(aggregate(x~type, iuDatAvg, mean)[,2]*wg)
  return(m)
}

#meanMean= function(x){
#  m=sum(aggregate(x~type, dfNames, mean)[,2]*wg)
#  return(m)
#}

meanCi= function(x){
  wg=(sapply(match.pat !='', as.numeric)*iuSamp)/sapply(match.pat !='', as.numeric) %*% iuSamp
  va=aggregate(x ~ type, iuDatAvg, var)
  ob=aggregate(x ~ type, iuDatAvg, length)
  se=sqrt(sum(wg^2 * va[,2]/ob[,2] * ((iuSamp - ob[,2])/iuSamp)))
  ci=1.96*se
  return(ci)
}

#meanCi= function(x){
#  #m=sum(aggregate(x~type, dfNames, mean)[,2]*wg)
#  va=aggregate(x ~ type, dfNames, var)
#  ob=aggregate(x ~ type, dfNames, length)
#  se=sqrt(sum(wg^2 * va[,2]/ob[,2] * ((iuSamp - ob[,2])/iuSamp)))
#  ci=1.96*se
#  return(ci)
#}



# Satisfaction
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


useTable=
  function(x){
    tb=xtabs(~x+type, data=iuDatUse, exclude = 0)
    aaa=matrix(tb, ncol=4)
    return(tb)
  }



  if (sum(aaa[,1])=0=){
    col1=0
  }
  else col1=sum(aaa[,1])/sum(aaa[,1])*wg[1]
  return(col1)
}

  if (sum(aaa[,2]))=0 {
    col2=0
  }
  else col2=sum(aaa[4,2],aaa[5,2],aaa[6,2])/sum(aaa[,2])*wg[2]
  if (sum(aaa[,3])=0) {
    col3=0
  }
  else col3=sum(aaa[4,3],aaa[5,3],aaa[6,3])/sum(aaa[,3])*wg[3]
  if (sum(aaa[,4])=0) {
    col4=0
  }
  else col4=sum(aaa[4,4],aaa[5,4],aaa[6,4])/sum(aaa[,4])*wg[4]

  allSum=sum(col1,col2,col3,col4)
  #aaa=aaa[-1,]
  #bbb=dim(aaa) #find dimmension, you need 6x4 dims
  #Add row 4, 5, and 6
  #bbb=sum(
  #  sum(aaa[4,1],aaa[5,1],aaa[6,1])/sum(aaa[,1])*wg[1],
  #  sum(aaa[4,2],aaa[5,2],aaa[6,2])/sum(aaa[,2])*wg[2],
  #  sum(aaa[4,3],aaa[5,3],aaa[6,3])/sum(aaa[,3])*wg[3],
  #  sum(aaa[4,4],aaa[5,4],aaa[6,4])/sum(aaa[,4])*wg[4]
  #return(tb)
  return(allSum)
}

ugSatis=lapply(y, satisfunc1)

head(ugDf)


satisTable=function(x){
  tb=xtabs(~x+type, data=ugDf3, exclude = 0)
  #aaa=matrix(tb, ncol=4)
  return(tb)
  }

# sums the each column, ugSatis is input data (list)
colCheck=lapply(ugSatis, colSums)





# recode ugDF data; sepearate 2 groups and re-set as data.frame, no type column
ugDf2=data.frame(apply(ugDf, 2, function(x) recode(x, "c(1,2,3)=99;c(4,5)=88")))
ugDf15=data.frame(apply(ugDf, 2, function(x) recode(x, "c(1,2,3,4,5)=15")))
ugDf345=data.frame(apply(ugDf, 2, function(x) recode(x, "c(1,2)=0;c(3,4,5)=345")))

# mpt add tjos ma
#is.na(ugDf2) = ugDf2==0

# add type column
ugDf3=cbind(ugDf2, type=iuData$type)

#ugDf123=cbind(ugDf15, type=iuData$type)
#ugDf345=cbind(ugDf345, type=iuData$type)


y=ugDf3[1:length(ugDf3)-1]
xtabs(~q15+type, data=ugDf3, exclude = 0)


y=ugDf2[1:length(ugDf2)-1]

