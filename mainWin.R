# set working deirectory and master data
setwd("~/Box Sync/work/rWork")

# load data
#iuDat = data.frame(read.csv('UITS16_IUPUI_Final.csv', header = TRUE))

iuDat = data.frame(read.csv('UITS16_IUB_Final.csv', header = TRUE))

# set sample size of iu campus
# IUB
iuF=3051
iuSf=5829
iuUg=38364
iuGr=10150

#iuF=3056
#iuSf=5771
#iuUg=36418
#iuGr=9997

#IUSB
#iuF=478
#iuSf=276
#iuUg=7016
#iuGr=558

#IUN
#iuF=358
#iuSf=232
#iuUg=5486
#iuGr=362

#IUPUI
#iuF=4033
#iuSf=4169
#iuUg=21985
#iuGr=8120

#IUE
#iuF=293
#iuSf=164
#iuUg=4562
#iuGr=154

# sample sizes and total
iuSamp=c(iuF,iuSf, iuUg, iuGr)
iuTot=iuF + iuSf + iuUg +  iuGr

# set weights
wg=c(iuF/iuTot, iuSf/iuTot, iuUg/iuTot, iuGr/iuTot)

# set all functions
source("functions.R")

# car pacakge required for recode function
library(car)

# reshpae2pacakge required formelt function
library(reshape2)

# in case you have dplyr package loaded
#detach("package:dplyr", unload=TRUE)


#iuData = data.frame(read.csv('UITS16_IUB_Final.csv', header = TRUE))
#iubFull = data.frame(read.csv('UITS16_IUB_Final.csv', header = TRUE))

#check variable names
names(iuDat)
#names(iubFull)

# prepare reduced data (q1 --q32, including type var)
iuDatR <-iuDat[c((2:73), 89)]

# load vars.csv file
var.list = data.frame(read.csv('vars.csv', header =TRUE ))

############not sure I need these#############


# set data frame; ugDf, fsgDf, fDf, fsDf, fugDf, fgDf using samllDf function
#allDf <-smallDf(c('f','s','u','g'))
#ugDf <-smallDf(c('','','u','g'))
#fsgDf <-smallDf(c('f','s','','g'))
#fDf <-smallDf(c('f','','',''))
#fsDf <-smallDf(c('f','s','',''))
#fugDf <-smallDf(c('f','','u','g'))
#fgDf <-smallDf(c('f','','','g'))
############not sure I need these#############

# run for each sug group
source("all.R")
source("ug.R")
source("fsg.R")
source("f.R")
source("fs.R")
source("fug.R")
source("fg.R")

# combined all statistics
finalDf=rbind(allDf,ugDf,fsgDf,fDf,fsDf,fugDf,fgDf)

# sort dataframe
sortList=order(finalDf$L1, decreasing = FALSE)
finalDf=finalDf[sortList,]

# create csv file

write.csv(finalDf, file =
            "final.csv")

###############special variables################
#source("misAvg.R")

iuSpeDat=iuDatR

# sample sizes and total
iuSamp=c(iuF,iuSf, iuUg, iuGr)
iuTot=iuF + iuSf + iuUg +  iuGr

# set missing values for average
is.na(iuSpeDat) = iuSpeDat > 8

table(iuSpeDat$q04,iuSpeDat$type)

# Q4
q4=xtabs(~q04+type, data=iuSpeDat)

# Q5
q5_aTable=xtabs(~q05_a+type, data=iuSpeDat)
q5_a=paste(round(sum(prop.table(q5_aTable,2)[2,]*iuSamp/iuTot), 4) * 100, "%", sep="")

q5_bTable=xtabs(~q05_b+type, data=iuSpeDat)
q5_b=paste(round(sum(prop.table(q5_bTable,2)[2,]*iuSamp/iuTot), 4) * 100, "%", sep="")

q5_cTable=xtabs(~q05_c+type, data=iuSpeDat)
q5_c=paste(round(sum(prop.table(q5_cTable,2)[2,]*iuSamp/iuTot), 4) * 100, "%", sep="")

q5_dTable=xtabs(~q05_d+type, data=iuSpeDat)
q5_d=paste(round(sum(prop.table(q5_dTable,2)[2,]*iuSamp/iuTot), 4) * 100, "%", sep="")

# Q24
options(digits=3)

q24Table=xtabs(~q24+type, data=iuSpeDat)
q24=prop.table(q24Table,2)

# faculty [,1], using sprintf function
q24F=paste(sprintf("%.1f", round(q24[,1] *100, 1)), "%", sep="")

# staff [2,], using sprintf function
q24S=paste(sprintf("%.1f", round(q24[,2] *100, 1)), "%", sep="")


###############special variables################
