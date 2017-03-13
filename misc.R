
# prepare master data .sav file. the variable names should be q1 -> q01 and no underscored _.
# prepare group data; vars.csv
# match the variable names (both vars.csv, data file and html)
# load data
# put sample sizes
# valicate data
#"counts1-5" counts3-5" "L1"
#

for (i in 1: length(melt1)) {
  print (names(melt1)[i])
}



#barchar functions
multbars=function(x)
  {
  #title=names(melt1)[i]
  #plot1=barplot(type~x, main = "title[i]", xlab="efg")
  #return(plot1)
  for (i in 1: length(melt1)) {
    print (names(melt1)[i])
    }
  #return(title)
}

lapply(melt1, multbars)

  barchart(type~x, main = "x", xlab="efg"))


names(saDf)
aaa=saDf[c(4,6,3)]
aaa$`counts3-5`

counts1_5=aggregate(saDf$`counts1-5` ~ L1, saDf, sum)
counts3_5=aggregate(saDf$`counts3-5` ~ L1, saDf, sum)

# rename variable names
colnames(counts1_5)[2] = "counts1-5"
colnames(counts3_5)[2] = "counts3-5"

# validate variables
valDat=cbind(counts1_5, counts3_5)[-3]


#melt1, 1-5
#melt2, 3-5
#melt1=lapply(subDf15, useTable)
library(lattice)
melt1$q06_a
eft=lapply(melt1, function(x) barchart(type~x, main = "x", xlab="efg"))
barchart(melt1$q06_a|type)
barchart(melt1$q06 ~ type, data=melt1)
pdf(eft)

pdf("abc.pdf")
lapply(melt1, function(x) barplot(x,main = "x", xlab="type"))
#barplot(melt1$q06_a, main="Response 1-5", xlab="type")
dev.off()

barplot(subDf15$q06_a)
barchart(subDf15$q06_a)

subDf345


####test
attach(mtcars)
# scatterplot matrix
splom(mtcars[c(1,3,4,5,6)],
      main="MTCARS Data")
library(lattice)
barchart(Species~Reason,data=Reasonstats,groups=Catergory,
         scales=list(x=list(rot=90,cex=0.8)))

####test
# Simple Bar Plot
counts <- table(mtcars$gear)
barplot(counts, main="Car Distribution",
        xlab="Number of Gears")

### data validation
### identical function does not work with list objects
### unlist list objects and compare with using identical function
identical(meltest1, meltest2)
abc=unlist(meltest1)
efg=unlist(meltest2)
identical(abc,efg)




