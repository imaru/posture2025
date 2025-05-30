library(jsonlite)
library(tidyr)
library(ggplot2)
library(dplyr)
library(rlist)
library(pipeR)

jntn<-c('Head','Neck','rSldr','rElbw','rWrst','lSldr','lElbw','lWrst','Wst','rHip','rknee','rAnkl','lHip','lKnee','lAnkl','rEye','lEye','rEar','lEar','lToe','lHeel','rToe','rHeel')
xwid<-720 # 動画の縦サイズ
ywid<-1280 # 動画の横サイズ
height<-165 # 被写体の身長

jd<-choose.dir('.')
files<-dir(jd,'*.json')
nf<-length(files)
njnt<-23
jofi<-c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 24)

jnt1<-4
jnt2<-7

jdf<-data.frame()
for (i in 1:nf){
  jsdt<-read_json(paste(jd, '\\', files[i], sep=''))
  datkp<-jsdt$people[[1]]$pose_keypoints_2d
  xdt<-unlist(datkp[jofi*3+1])
  ydt<-unlist(datkp[jofi*3+2])
  cdt<-unlist(datkp[jofi*3+3])
  thisd<-cbind(i, jofi, xdt, ydt, cdt)
  jdf<-rbind(jdf,thisd)
}
jdf[which(jdf[,3]==0),3:5]<-NA

tall<-abs(max(jdf[which(jdf$joint==0),4],na.rm=T)-min(jdf[which(jdf$joint==21),4],na.rm=T))
smr<-data.frame(matrix(NA,length(jofi),9))
for (i in 1:length(jofi)){
  print(i)
  smr[i,1:5]<-c(jntn[i],min(jdf[which(jdf$joint==jofi[i]),3],na.rm=T),max(jdf[which(jdf$joint==jofi[i]),3],na.rm=T),min(jdf[which(jdf$joint==jofi[i]),4],na.rm=T),max(jdf[which(jdf$joint==jofi[i]),4],na.rm=T))
}

smr[,6]<-smr[,3]-smr[,2]
smr[,7]<-smr[,5]-smr[,4]
smr[,8:9]<-smr[,6:7]/tall*height
colnames(smr)<-c('jnt','minX','maxX','minY','maxY','rangeX','rangeY','rangeX(cm)','rangeY(cm)')

colnames(jdf)<-c('frame','joint','xdt','ydt','cdt')
jdf$joint<-as.factor(jdf$joint)
g<-ggplot(data=jdf[which(jdf$joint==jnt1 |jdf$joint==jnt2 ),], aes(x=xdt, y=ywid-ydt, color=joint))+geom_point(na.rm=T)
#g<-ggplot(data=jdf, aes(x=xdt, y=500-ydt, color=joint))+geom_point()
g<-g+xlim(c(0,xwid))+ylim(c(0,ywid))+scale_color_hue(labels=c(jntn[jnt1==jofi],jntn[jnt2==jofi]))
plot(g)

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