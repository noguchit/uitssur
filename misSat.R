# set average data from iuDatR (reduced)
iuDatSat = iuDatR

# recoding 9997 values --> for calculating average and satisfaction
rec9997=apply(iuDatSat, 2, function(x) recode(x, "9997=0"))

# set missing values for satisfaction
is.na(rec9997) = rec9997 > 8


# set iuDatSat that can be used to get satisfaction rate and satifactin CI
iuDatSat = data.frame(rec9997)
