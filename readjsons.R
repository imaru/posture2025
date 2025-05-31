library(jsonlite)
library(tidyr)
library(ggplot2)
library(dplyr)
library(rlist)
library(pipeR)

jntn<-c('Head','Neck','rSldr','rElbw','rWrst','lSldr','lElbw','lWrst','Wst','rHip','rknee','rAnkl','lHip','lKnee','lAnkl','rEye','lEye','rEar','lEar','lToe','lHeel','rToe','rHeel')
fr<-30

# ここからの6行を変更する必要がある
xwid<-720 # 動画の縦サイズ
ywid<-1280 # 動画の横サイズ
height<-165 # 被写体の身長
jd<-'data' # データが入っているフォルダ名
begT<-10 # 解析開始時間（秒）、0の場合は最初から
durT<-10 # 解析時間の長さ（秒）、0の場合は最後まで。
jnt1<-4 # 表示する関節1, 関節番号はスクリプトの末尾参照
jnt2<-7 # 表示する関節2

# jd<-file.choose()

files<-dir(jd,'*.json')
nf<-length(files)
njnt<-23
jofi<-c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 24)


jdf<-data.frame()
for (i in 1:nf){
  jsdt<-read_json(paste(jd, '/', files[i], sep=''))
  datkp<-jsdt$people[[1]]$pose_keypoints_2d
  xdt<-unlist(datkp[jofi*3+1])
  ydt<-unlist(datkp[jofi*3+2])
  cdt<-unlist(datkp[jofi*3+3])
  thisd<-cbind(i, jofi, xdt, ydt, cdt)
  jdf<-rbind(jdf,thisd)
}
colnames(jdf)<-c('frame','joint','xdt','ydt','cdt')
jdf[which(jdf[,3]==0),3:5]<-NA


jdf<-jdf[jdf$frame>=begT*fr,]
if (durT>0){
  jdf<-jdf[jdf$frame<=begT*fr+durT*fr,]
}

tall<-abs(max(jdf[which(jdf$joint==0),4],na.rm=T)-min(jdf[which(jdf$joint==21),4],na.rm=T))
#smr<-data.frame(matrix(NA,length(jofi),9))
smr<-data.frame()
for (i in 1:length(jofi)){
  smr[i,1]<-jntn[i]
  smr[i,2:5]<-c(min(jdf[which(jdf$joint==jofi[i]),3],na.rm=T),max(jdf[which(jdf$joint==jofi[i]),3],na.rm=T),min(jdf[which(jdf$joint==jofi[i]),4],na.rm=T),max(jdf[which(jdf$joint==jofi[i]),4],na.rm=T))
}

smr<-cbind(smr,smr[,3]-smr[,2])
smr<-cbind(smr,smr[,5]-smr[,4])
smr[,8:9]<-smr[,6:7]/tall*height
colnames(smr)<-c('jnt','minX','maxX','minY','maxY','rangeX','rangeY','rangeX(cm)','rangeY(cm)')

jdf$joint<-as.factor(jdf$joint)
g<-ggplot(data=jdf[which(jdf$joint==jnt1 |jdf$joint==jnt2 ),], aes(x=xdt, y=ywid-ydt, color=joint))+geom_point(na.rm=T)
#g<-ggplot(data=jdf, aes(x=xdt, y=500-ydt, color=joint))+geom_point()
g<-g+xlim(c(0,xwid))+ylim(c(0,ywid))+scale_color_hue(labels=c(jntn[jnt1==jofi],jntn[jnt2==jofi]))
plot(g)

if (begT>0 | durT){
  print(paste0(as.character(begT),'s-',as.character((begT+durT)), 's'))
}
print(smr)

# 0: head
# 1: neck
# 2: right shoulder
# 3: right elbow
# 4: right wrist
# 5: left shoulder
# 6: left elbow
# 7: left wrist
# 8: waist
# 9: right hip
# 10: right knee
# 11: right ankle
# 12: left hip
# 13: left knee
# 14: left ankle
# 15: right eye
# 16: left eye
# 17: right ear
# 18: left ear
# 19: left toe
# 21: left heel
# 22: right toe
# 24: right heel