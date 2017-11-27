newpolicies = read.csv("holdback_ordered_output.csv")
modelpolicies = read.csv("test_ordered_output.csv")
# test gbm 0.247, test med 0.235
# hb gbm 0.282, hb med 0.262
cats = 5
cc = 10000
gain = 200
#gain=.1
losses_gbm = rep(0,cats)
losses_med = rep(0,cats)
actual_losses_gbm = rep(0,cats)
actual_losses_med = rep(0,cats)
#get the risk groups
sect = seq(1,nrow(modelpolicies)+10,by=round(nrow(modelpolicies)/cats,digits=0))
act_sect = seq(1,nrow(newpolicies)+10,by=round(nrow(newpolicies)/cats,digits=0))
for (i in 1:cats){
    losses_gbm[i] = length(which(modelpolicies[sect[i]:sect[i+1],1]==1))*cc
    losses_med[i] = length(which(modelpolicies[sect[i]:sect[i+1],2]==1))*cc
    actual_losses_gbm[i] = length(which(newpolicies[act_sect[i]:act_sect[i+1],1]==1))*cc
    actual_losses_med[i] = length(which(newpolicies[act_sect[i]:act_sect[i+1],2]==1))*cc
}
#prem_gbm = round((losses_gbm/(sect[2]-1))*(1+gain),digits=2)
#prem_med = round((losses_med/(sect[2]-1))*(1+gain),digits=2)
prem_gbm = round((losses_gbm/(sect[2]-1))+gain,digits=2)
prem_med = round((losses_med/(sect[2]-1))+gain,digits=2)
print("premium charged per risk category using the better performing model:")
print(prem_gbm)
print("premium charged per risk category using the poorer performing model:")
print(prem_med)

