library(plyr)
library(dplyr)
library(data.table)

ds = read.csv("train.csv",na.strings=c("-1","-1.0"))
todrop = which(grepl("calc",names(ds)))
ds = ds[,-todrop]
ds = as.data.table(ds)

#feature engineering
ds$nact = apply(ds,1,function(x) sum(is.na(x)))
ds[,nalots := ifelse(nact>4,1,0)]
ds[,indsum := ps_ind_06_bin+ps_ind_07_bin+ps_ind_08_bin+ps_ind_09_bin+ps_ind_10_bin+ps_ind_11_bin+ps_ind_12_bin+ps_ind_13_bin+ps_ind_16_bin+ps_ind_17_bin+ps_ind_18_bin]

facmode = function(x){
    tab = table(x)
    names(tab)[tab==max(tab)]
}

ds = as.data.frame(ds)

print("replacing NA's with the mode")
for (i in 2:ncol(ds)){
    if (class(ds[,i]) == "factor" & sum(is.na(ds[,i])) > 0){
        ds[which(is.na(ds[,i])),i] = facmode(ds[,i])
    } else if (sum(is.na(ds[,i])) > 0){
        ds[which(is.na(ds[,i])),i] = median(ds[,i],na.rm=T)
    }
}
write.csv(ds,"interm_train.csv",row.names=F)
