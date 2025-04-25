library(jsonlite)
library(tidyr)
library(ggplot2)
library(ggpp)
library(gridExtra)

joi<-c('WRIST_RIGHT', 'HEAD','SHOULDER_RIGHT','HIP_RIGHT', 'KNEE_RIGHT')

fn<-file.choose()
dat<-jsonlite::read_json(fn, simplifyVector = TRUE)

bfn<-file.choose()
bdat<-read.table(bfn, skip=1)
colnames(bdat)<-c('Time','CpX','CpY','BL','BR','TL','TR','Wgt','RBL','RBR','RTL','RTR')
lbdat<-pivot_longer(data=bdat, cols=-Time,values_to = 'pos', names_to = 'param')

nfrm<-length(dat$frames$bodies)

joich<-array(dim=length(joi))
for (i in 1:length(joi)){
  joich[i]<-which(dat$joint_names==joi[i])  
}

joidat<-data.frame(matrix(nrow=nfrm*length(joi), ncol=5))

joidat<-data.frame()
joidist<-data.frame()

for (i in 1:nfrm){
  cord<-dat$frames[1]$bodies[[i]]$joint_positions[[1]][joich,]
  if (!is.null(cord)){
    joitemp<-cbind(rep(i, length(joi)), joi, cord)
    joidat<-rbind(joidat, joitemp)
  }
}




colnames(joidat)<-c('frame','joi','x','y','z')
ljoi<-pivot_longer(joidat, cols=c('x','y','z'), values_to = 'pos')
ljoi$frame<-as.numeric(ljoi$frame)
ljoi$pos<-as.numeric(ljoi$pos)
ljoi$time<-ljoi$frame/33

wjoi<-pivot_wider(joidat, names_from = 'joi', values_from = c('x','y','z'))
wjoi<-data.frame(lapply(wjoi,as.numeric))

dst<-data.frame(matrix(ncol=length(joi),nrow=nrow(wjoi)-1))

for (i in 1:length(joi)){
  thiscol<-seq(1,length(joi)*2+1,length(joi))+i
  dst[,i]<-sqrt(apply((wjoi[2:nrow(wjoi),thiscol]-wjoi[1:nrow(wjoi)-1,thiscol])^2,1,sum))
}

dst<-cbind(seq(1,nrow(dst)),dst)


colnames(dst)<-c('frame',joi)

ldst<-pivot_longer(data=dst, cols = -frame, values_to = 'dist')

lwrst<-ldst[grep('WRIST', ldst$name),]
lbody<-ldst[grep('WRIST', ldst$name,invert=T),]


ccc<-lapply(dst,mean)
cbind(joi,ccc[2:length(ccc)])

gwrst<-ggplot(data=lwrst, aes(x=frame, y=dist, colour = name))+geom_line(alpha=0.3)
gwrst<-gwrst+scale_color_manual(values=c("#111111","#555555"))

gbody<-ggplot(data=lbody, aes(x=frame, y=dist, colour=name))+geom_line(alpha=0.3)#+ylim(c(0,300))
gbody<-gbody+scale_color_manual(values=c("#FF0000","#FF8888","#00FF00","#88FF88","#0000FF","#8888FF"))

ghead<-ggplot(data=ljoi[which(ljoi$joi=='HEAD'|ljoi$joi=='SHOULDER_RIGHT'),], aes(x=time, y=pos, color=name, linetype =joi))+geom_line()+xlim(0,180)

gbalance<-ggplot(lbdat[grep('Cp',lbdat$param),], aes(x=Time, y=pos, color=param))+geom_line()+xlim(0,180)

grid.arrange(ghead,gbalance, nrow=2)
