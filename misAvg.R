# set average data from iuDatR (reduced)
iuDatAvg = iuDatR
#recoding 9997 values --> for calculating average
rec9997=apply(iuDatAvg, 2, function(x) recode(x, "9997=0"))

# set missing values for average
is.na(rec9997) = rec9997 > 8

# set iuData that function subDatAvg can be used
iuDatAvg = data.frame(rec9997)
