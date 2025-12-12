library(tidyverse)

# 結果ファイルの読み込み
fl<-choose.files()
btemp<-read.table(fl, skip=1)
colnames(btemp)<-c('Time','CpX','CpY','BL','BR','TL','TR','Wgt','RBL','RBR','RTL','RTR')
ndat<-nrow(btemp)

# 身体方向の変換
# 電源ボタン側にある身体位置を指定（顔0、背中1、右手2、左手3）
pside<-2
if (pside==0){
  btemp$CpY<--btemp$CpY
  btemp$CpX<--btemp$CpX
}else if (pside==2){
  tCpX<-btemp$CpX
  btemp$CpX<--btemp$CpY
  btemp$CpY<-tCpX
}else if (pside==3){
  tCpX<-btemp$CpX
  btemp$CpX<-btemp$CpY
  btemp$CpY<--tCpX
}

# キャリブレーションパラメータの確認
blx<-btemp[,9]
bly<-btemp[,4]
res<-lm(bly~blx)

# 使用したバランスボードの値(2025/12/02)
#(Intercept)          blx  
#-616.9503       0.1065  

# 重心データのみ抜き出し
copdat<-btemp[,1:3]

# 最初の10フレームのデータで標準化（平均を0付近にするだけ）
copdat[,2]<-copdat[,2]-mean(copdat[1:10,]$CpX)
copdat[,3]<-copdat[,3]-mean(copdat[1:10,]$CpY)

# フレーム間座標変化
diffdat<-cbind(diff(copdat[,2]),diff(copdat[,3]))

# フレーム間距離
distdat<-sqrt(rowSums(diffdat^2))

# 重心データにフレーム間距離を追加
copdat<-cbind(copdat,c(0,distdat))
colnames(copdat)<-c('time','CpX','CpY','Dist')

# 記述統計算出
ddat<-paste0('x:',round(sum(abs(copdat$CpX)),digits = 2),' y:',round(sum(abs(copdat$CpY)), digits = 2),' dist:',round(sum(copdat$Dist), digits = 2))
print(ddat)

# ggplotでグラフを描くためにデータをlong型に変換
lcopdat<-pivot_longer(copdat, cols=c('CpX','CpY','Dist'))

# グラフ作成
gcop<-ggplot(data=lcopdat, aes(x=time, y=value, color=name))+geom_line()
gcop<-gcop+labs(title=paste(basename(fl),ddat))
plot(gcop)

      