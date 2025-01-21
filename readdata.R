library(jsonlite)
library(tidyr)
library(ggplot2)
library(ggpp)
library(gridExtra)

joi<-c('WRIST_RIGHT', 'WRIST_LEFT','SHOULDER_RIGHT','SHOULDER_LEFT','HIP_RIGHT','HIP_LEFT','KNEE_RIGHT','KNEE_LEFT')

fn<-file.choose()
dat<-jsonlite::read_json(fn, simplifyVector = TRUE)

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

wjoi<-pivot_wider(joidat, names_from = 'joi', values_from = c('x','y','z'))
wjoi<-data.frame(lapply(wjoi,as.numeric))

dst<-data.frame(matrix(ncol=length(joi),nrow=nrow(wjoi)-1))

for (i in 1:length(joi)){
  thiscol<-seq(1,17,8)+i
  dst[,i]<-sqrt(apply((wjoi[2:nrow(wjoi),thiscol]-wjoi[1:nrow(wjoi)-1,thiscol])^2,1,sum))
}

dst<-cbind(seq(1,nrow(dst)),dst)


colnames(dst)<-c('frame',joi)

ldst<-pivot_longer(data=dst, cols = -frame, values_to = 'dist')

lwrst<-ldst[grep('WRIST', ldst$name),]
lbody<-ldst[grep('WRIST', ldst$name,invert=T),]


ccc<-lapply(dst,mean)
cbind(joi,ccc[2:length(joi)+1])

gwrst<-ggplot(data=lwrst, aes(x=frame, y=dist, colour = name))+geom_line(alpha=0.3)
gwrst<-gwrst+scale_color_manual(values=c("#111111","#555555"))

gbody<-ggplot(data=lbody, aes(x=frame, y=dist, colour=name))+geom_line(alpha=0.3)+ylim(c(0,100))
gbody<-gbody+scale_color_manual(values=c("#FF0000","#FF8888","#00FF00","#88FF88","#0000FF","#8888FF"))

grid.arrange(gwrst,gbody,nrow=2)
