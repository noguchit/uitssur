source("misAvg.R")

# subset data frame
subDf <-subDatAvg(c('f','s','',''))
# ugDf <-subDf(c('','','u','g'))

# set pattern of group
match.pat=c('f','s','','')

# find weight on each sub group
wg=(sapply(match.pat !='', as.numeric)*iuSamp)/sapply(match.pat !='', as.numeric) %*% iuSamp

#############start average section###########
# get average and ave. CI
mea=lapply(subDf, meanMean)
meaCi=lapply(subDf, meanCi)

# round 2
mea=lapply(mea, round, 2)
meaCi=lapply(meaCi, round, 2)

meaComb=cbind(melt(mea, value.name="Mean"), melt(meaCi, value.name="MeanCI"))

# reorder the columns
meaComb=meaComb[c(2,1,3,4)] [-4]

# sort DF
sortList=order(meaComb$L1)
meanFinal=meaComb[sortList,]

meanFinal$Average=paste(meanFinal$Mean, "+/-", meanFinal$MeanCI, sep="")

# keep L1 and Average columns
meanFinal=meanFinal[c(1,4)]
#############start average section###########

source("misSat.R")

#################start satisfaction section##################
# recode to get counts 1-5
# recode to get counts 3-5
subDf15=data.frame(apply(subDf, 2, function(x) recode(x, "c(1,2,3,4,5)=1")))
subDf345 =data.frame(apply(subDf, 2, function(x) recode(x, "c(1,2)=0;c(3,4,5)=1")))

#subDf15=data.frame(apply(subDf, 2, function(x) recode(x, "c(1,2,3,4,5)=1")))
#subDf345 =data.frame(apply(subDf, 2, function(x) recode(x, "c(1,2)=0;c(3,4,5)=1")))

#subDf15=data.frame(apply(ugDf, 2, function(x) recode(x, "c(1,2,3,4,5)=1")))
#subDf345 =data.frame(apply(ugDf, 2, function(x) recode(x, "c(1,2)=0;c(3,4,5)=1")))

# prepare melt data for using melt function
melt1=lapply(subDf15, satisTable)
melt2=lapply(subDf345 , satisTable)

satis15=melt(melt1, value.name="counts1-5")
satis345=melt(melt2, value.name="counts3-5")

# drop type var, location is at 2 (type var, duplicate)
saComb=cbind(satis15,satis345)[-2]

# find weight on each sub group
wg=(sapply(match.pat !='', as.numeric)*iuSamp)/sapply(match.pat !='', as.numeric) %*% iuSamp

# prepare type variable, and add tye variable on weight
type=c(1,2,3,4)
wgVar=cbind(type,wg)

# set sub population sample size
Nh=(sapply(match.pat !='', as.numeric)*iuSamp)/sapply(match.pat !='', as.numeric)
NhVar=cbind(type,Nh)

# merge weight, 123/45 value variables, and subsample variable
saDf=merge(saComb, wgVar, by="type")
saDf=merge(saDf, NhVar, by="type")

# new varialble, counts123 x weight
saDf$`counts3-5Wg`=saDf$`counts3-5`*saDf$wg

# new variable, proportion
saDf$prop=saDf$`counts3-5`/saDf$`counts1-5`

# new variable, satisrate
saDf$satis=saDf$`counts3-5Wg`/saDf$`counts1-5`

# get nh variable to estimate se
saDf$nh=saDf$`counts1-5`

# create var varialbe
# don't create se variable, only var needed
saDf$var=saDf$wg^2 * saDf$prop * (1-saDf$prop) /
  (saDf$nh -1) * ((saDf$Nh -saDf$nh) /saDf$Nh)

# sort DF
sortList=order(saDf$L1)
saDf=saDf[sortList,]
#saDfFinal=saDf[sortList,]

# satis rate
satis=aggregate(saDf$satis ~ L1,saDf, sum)

# multiply by 100, and round 1
satis$satis=round(satis$`saDf$satis`*100, 1)

# satis variance
satisVar=aggregate(saDf$var ~ L1,saDf, sum)

# 95 % area and multiply by 100, and round 1. Add % to values
satisVar$Ci=paste(round(sqrt(satisVar$`saDf$var`)*1.96 * 100, 1), "%", sep="")

#satisCi=sqrt(aggregate(saDf$var ~ L1,saDf, sum) [,2] ) * 1.96

#finalDf=cbind(satis,satisCi)
satisFinal=cbind(satis, satisVar)

satisFinal$Satisfaction=paste(satisFinal$satis, "+/-", satisFinal$Ci, sep="")

# keep L1 and Satisfaction columns
satisFinal=satisFinal[c(1,7)]

#################start usage section##################
source("misUse.R")

# subset out of iuData according to group pattern
subDf <-subDatUse(c('f','s','',''))
#ugDf <-subDf(c('','','u','g'))

# set value 0 as NA
#subDf[subDf==0] =NA

# set value 1:5 as 1,  w/ set value 9 as NA
subDf15=data.frame(apply(subDf, 2, function(x) recode(x, "c(1,2,3,4,5)=1;9=NA")))
#subDf15=data.frame(apply(ugDf, 2, function(x) recode(x, "c(1,2,3,4,5)=1;9=NA")))

# set value 1:9 as 1
subDf19=data.frame(apply(subDf, 2, function(x) recode(x, "c(1,2,3,4,5,9)=1")))
#ugDf19=data.frame(apply(ugDf, 2, function(x) recode(x, "c(1,2,3,4,5,9)=1")))

# prepare melt data for using melt function, useTable is custom defined function

melt1=lapply(subDf15, useTable)
use15=melt(melt1, value.name="counts1-5")
#melt1=lapply(subDf15, useTable)
#use15=melt(melt1, value.name="counts1-5")

melt2=lapply(subDf19, useTable)
use19=melt(melt2, value.name="counts1-9")

#melt2=lapply(ugDf19, useTable)
#use19=melt(melt2, value.name="counts1-9")

# drop type var, location is at 2 (type var, duplicate)
useComb=cbind(use15,use19)[-2]

# find weight on each sub group
wg=(sapply(match.pat !='', as.numeric)*iuSamp)/sapply(match.pat !='', as.numeric) %*% iuSamp

# prepare type variable, and add tye variable on weight
type=c(1,2,3,4)
wgVar=cbind(type,wg)

# merge weight, 123/45 value variables, and subsample variable
useDf=merge(useComb, wgVar, by="type")

# new varialble, counts1-5 x weight
useDf$`counts1-5Wg`=useDf$`counts1-5`*useDf$wg

# new variable, proportion
useDf$`prop`=useDf$`counts1-5Wg`/useDf$`counts1-9`

# use rate
use=aggregate(useDf$prop ~ L1, useDf, sum)

# sort DF
sortList=order(use$L1)
use=use[sortList,]
use$Usage=paste(round(use$`useDf$prop`, 4) *100, "%", sep="")

# keep L1 and Usage columns
#useFinal=useFinal[c(1,3)]

# combined satis and satisCI
finalDf=cbind(meanFinal, satisFinal, use)
finalDf=finalDf[c(1,2,4,7)]

#################start usage section##################

fsDf=finalDf
write.csv(finalDf, file =
            "fs.csv")

