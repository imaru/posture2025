
fl<-choose.files()
btemp<-read.table(fl, skip=1)
colnames(btemp)<-c('Time','CpX','CpY','BL','BR','TL','TR','Wgt','RBL','RBR','RTL','RTR')
ndat<-nrow(btemp)

# change matrix
# 電源ボタン側にある身体位置を指定（顔0、背中1、右手2、左手3）
pside<-1
if (pside==0){
  btemp$CpY<--btemp$CpY
  btemp$CpX<--btemp$CpX
}else if (pside==2){
  tCpX<-btemp$CpX
  btemp$CpX<--btemp$CpY
  btemp$CpY<--tCpX
}else if (pside==3){
  tCpX<-btemp$CpX
  btemp$CpX<-btemp$CpY
  btemp$CpY<-tCpX
}

blx<-btemp[,9]
bly<-btemp[,4]
res<-lm(bly~blx)

#(Intercept)          blx  
#-616.9503       0.1065  

