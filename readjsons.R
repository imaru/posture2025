library(jsonlite)
library(tidyr)
library(ggplot2)
library(dplyr)
library(rlist)
library(pipeR)

jd<-'C:\\\\Users\\imaru\\Dropbox\\Class\\2025\\2025mcap\\json\\'

files<-dir(jd,'*.json')
nf<-length(files)
njnt<-22

jofi<-c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 24)

jdf<-data.frame()
for (i in 1:nf){
  jsdt<-read_json(paste(jd,files[i],sep=''))
  datkp<-jsdt$people[[1]]$pose_keypoints_2d
  xdt<-unlist(datkp[jofi*3+1])
  ydt<-unlist(datkp[jofi*3+2])
  cdt<-unlist(datkp[jofi*3+3])
  thisd<-cbind(i, jofi, xdt, ydt, cdt)
  jdf<-rbind(jdf,thisd)
  #jdf<-rbind(jdf, c(i, xdt,ydt,cdt))
}

colnames(jdf)<-c('frame','joint','xdt','ydt','cdt')
jdf$joint<-as.factor(jdf$joint)

g<-ggplot(data=jdf[which(jdf$joint==4 |jdf$joint==7 ),], aes(x=xdt, y=ydt, color=joint))+geom_point()
#g<-ggplot(data=jdf, aes(x=xdt, y=500-ydt, color=joint))+geom_point()
g<-g+xlim(c(400,800))+ylim(c(0,400))
plot(g)
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