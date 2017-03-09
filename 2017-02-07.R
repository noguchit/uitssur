# car pacakge required; recode function needed
library(car)

setwd("~/Box Sync/work/rWork")
iubFull = data.frame(read.csv('UITS16_IUB_Final.csv', header = TRUE))

#check variable names
names(iubFull)

# I need from q1 --q32, and type
iubFull <-iubFull[c((2:73), 89)]

#recoding 99797 values --> for calculating average
rec9997=apply(iubFull, 2, function(x) recode(x, "9997=0"))

# Set missing values
is.na(rec9997) = rec9997 > 8
iuData = data.frame((rec9997))

# load vars.csv file
var.list = data.frame(read.csv('vars.csv', header =TRUE ))

# set pattern of ug
ugDf <-subDf(c('','','u','g'))




# http://mattimeyer.github.io/2015-09-30-recoding-items/
# recoding  エラー

#csvのデータは、stat/transferを使った

#all case
library(car)
detach("package:dplyr", unload=TRUE)

setwd("~/Box Sync/work/rWork")
iubFull = data.frame(read.csv('UITS16_IUB_Final.csv', header = TRUE))
#check variable names
names(iubFull)
# I need from q1 --q32, and type
iubFull <-iubFull[c((2:73), 89)]

#recoding 99797 values --> for calculating average
rec9997=apply(iubFull, 2, function(x) recode(x, "9997=0"))

# Set missing values
is.na(rec9997) = rec9997 > 8
iuData = data.frame((rec9997))

var.list = data.frame(read.csv('vars.csv', header =TRUE ))

# create sub Dafaframe based on groups
subDf <-
  function(x) {
    match.pat = x
    col.is.a.match <- apply(var.list, 2, identical, match.pat)
    # Subset var.csv to match selected variables
    all = var.list[col.is.a.match]
    # List variable names you selected
    allVarnames=names(all)
    # subseeting dataframe from selected variables
    Df=subset(iuData, select=allVarnames)
  }

# ugDf, fsgDf, fDf, fsDf, fugDf, fgDf

allDf <-subDf(c('f','s','u','g'))
ugDf <-subDf(c('','','u','g'))
fsgDf <-subDf(c('f','s','','g'))
fDf <-subDf(c('f','','',''))
fsDf <-subDf(c('f','s','',''))
fugDf <-subDf(c('f','','u','g'))
fgDf <-subDf(c('f','','','g'))

# add type variables

dfNames=c("allDf","ugDf")

addType <-
  function(x){
    apply((cbind(x, type=iuData$type))
  }

#addType(wholeDf)

allDf=(cbind(allDf, type=iuData$type))
ugDf=(cbind(ugDf, type=iuData$type))


# Set sample size of iub
iuF=3056
iuSf=5771
iuUg=36418
iuGr=9997

# sample size and total
iuSamp=c(iuF,iuSf, iuUg, iuGr)
iuTot=iuF + iuSf + iuUg +  iuGr

# set match pattern; defining sub groups
match.pat=c('','','u','g')


#weight
wg=c(iuF/iuTot, iuSf/iuTot, iuUgt/iuTot, iuGr/iuTot)

# list of DF names
dfNames=allDf
dfNames=ugDf


# mean functions
meanMean= function(x){
  m=sum(aggregate(x~type, dfNames, mean)[,2]*wg)
  return(m)
}

meanCi= function(x){
  #m=sum(aggregate(x~type, dfNames, mean)[,2]*wg)
 va=aggregate(x ~ type, dfNames, var)
 ob=aggregate(x ~ type, dfNames, length)
 se=sqrt(sum(wg^2 * va[,2]/ob[,2] * ((iuSamp - ob[,2])/iuSamp)))
 ci=1.96*se
 return(ci)
 }

#x=names(allDf)[1:length(allDf)-1]

y=allDf[1:length(allDf)-1]
#y=ugDf[1:length(ugDf)-1]
y=ugDf


# get average and ave. CI
ugMean=lapply(y, meanMean)
ugMeanCi=lapply(y, meanCi)

aaa=data.frame(cbind(ugMean, ugMeanCi))
names(aaa)
# sort DF
sortList=order(saDf$L1)
saDfFinal=saDf[sortList,]


# let's use "ug" data to get avereage and CI




write.csv(cbind(ugMean,ugMeanCi), file="ugMean.csv")

write.csv(allMean, file =
            "allMean.csv")

allMean=lapply(all_vars, meanCol1)
allMeanCi=lapply(all_vars, meanCol2)

write.csv(allMean, file =
            "allMean.csv")

write.csv(allMeanCi, file =
            "allMeanCi.csv")
