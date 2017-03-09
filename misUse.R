# set average data from iuDatR (reduced)
iuDatUse = iuDatR

#recoding 99797 values --> for calculating usage
rec9997=apply(iuDatUse, 2, function(x) recode(x, "9997=0"))

# Set missing values for usage
is.na(rec9997) = rec9997 > 9

# set iuData that function subDf can be used
iuDatUse = data.frame((rec9997))
