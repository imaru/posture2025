rm(list=ls())
library(jsonlite)
library(tidyr)
library(ggplot2)
library(ggpp)
library(stringr)
library(gridExtra)
library(dplyr)
library(brms)
library(rstan)

if(Sys.info()["sysname"] == "Windows"){
  if(as.integer(str_extract(Sys.info()["release"], "^[0-9]+")) >=8){
    family_sans <- "Yu Gothic"
    family_serif <- "Yu Mincho"
  } else {
    family_sans <- "MS Gothic"
    family_serif <- "MS Mincho"
  }
} else if(Sys.info()["sysname"] == "Linux") {
  family_sans <- "Noto Sans CJK JP"
  family_serif <- "Noto Serif CJK JP"
} else if(Sys.info()["sysname"] == "Darwin"){
  family_serif <- "Hiragino Mincho ProN"
  family_sans <- "Hiragino Sans"
} else {
  # インストールすればとりあえず動く
  family_sans <- "Noto Sans CJK JP"
  family_serif <- "Noto Serif CJK JP"
}


listfl<-choose.files()
datlist<-read.csv(listfl)
proclist<-datlist[which(datlist$proc==1),]

datfld<-choose.dir()



# 実験開始時の大きな動きの検出閾値 1の場合、この設定は使用しない
thr<-0

# 測定時間の長さ。計測終了からこの時間の長さだけを解析対象とする
tlen<-8
#tlen<-1
lendur<-10

for (i in 1:nrow(proclist)){
  i<-14
  fname<-paste("joi_", proclist[i,2], ".pdf", sep="")
  if (Sys.info()['sysname']=='Windows'){
    cairo_pdf(fname, family = "Yu Gothic")
  }else if(Sys.info()["sysname"] == "Linux") {
    cairo_pdf(fname, family = "Noto Sans CJK JP")
  }else{
    quartz(type="pdf", file=fname, width=8, height=10)
  }
  for (j in 1:6){
    j<-4
    kfreq<-30
    kfn<-paste0(datfld,'\\',proclist[i,8+j],'.json')
    dat<-jsonlite::read_json(kfn, simplifyVector = TRUE)
    joi=dat$joint_names
    nfrm<-length(dat$frames$bodies)
    joich<-array(dim=length(joi))
    for (ch in 1:length(joi)){
      joich[ch]<-which(dat$joint_names==joi[ch])  
    }
    
    joidat<-data.frame()
    joidist<-data.frame()
    
    # jsonデータから値の取得, 時間がかかる
    for (frm in 1:nfrm){
      cord<-dat$frames[1]$bodies[[frm]]$joint_positions[[1]][joich,]
      if (!is.null(cord)){
        joitemp<-cbind(rep(frm, length(joi)), joi, cord)
        joidat<-rbind(joidat, joitemp)
      }
    }
    colnames(joidat)<-c('frame','joi','x','y','z')
    
    # Kinectデータの正規化
    stddat<-data.frame()
    for (jnt in 1:length(joi)){
      temp <- cbind(joidat[which(joidat$joi==joi[jnt]),c(3:5)] %>% lapply(as.numeric) %>% data.frame  %>% lapply(scale) %>% data.frame)
      head <- joidat[which(joidat$joi==joi[jnt]),c(2,1)]
      temp <- cbind(joi[jnt],head,as.numeric(head[,2])/30, temp)
      stddat<-rbind(stddat, temp)
    }
    colnames(stddat)<-c('joi','frame','time' ,'x','y','z')
    rlen<-((j-1)%%2)*(tlen-1)+1
    maxtpos<-max(nfrm/kfreq-rlen*60)*kfreq
    stddat<-stddat[which(stddat$frame>maxtpos),]
    joilen=data.frame()
    for (jnt in 1:length(joi)){
      thisjoidat<-stddat[stddat$joi==joi[jnt],]
      nprd<-floor(nrow(thisjoidat)/kfreq/lendur)
      #print(nrow(thisjoidat))
      attach(thisjoidat)
      thislen=array(nrow(thisjoidat)-1)
      for (frm in 2:nrow(thisjoidat)){
        thislen[frm-1]=sqrt((x[frm]-x[frm-1])^2+(y[frm]-y[frm-1])^2+(z[frm]-z[frm-1])^2)
        #print(tlen[j-1])
      }
      joilen<-rbind(joilen, c(joi[jnt],sum(thislen)))
    
    
      #nprd<-floor(nrow(tlen)/(kfreq*lendur))
      leng<-array(NA,c(nprd,1))
      for (k in 1:nprd){
        leng[k]<-sum(thislen[(1+(k-1)*(kfreq*lendur)):((kfreq*lendur)*k)])
      }
      cumleng<-cumsum(leng)
      names(cumleng)<-as.factor(seq(1,nprd)*lendur)
    
      par(mfcol=c(1,2))
      barplot(cumleng, xlab='time(sec)', ylab='trace length')
      title(paste('id',proclist[i,2],proclist[i,3+j%/%3],joi[jnt]))
      barplot(t(leng), names.arg = as.factor(seq(1,nprd)))
      #res<-nls(cumleng~a*seq(1,nprd)^b, start=c(a=1, b=1))
      
    }
    save.image(paste0(as.character(i),'_',as.character(j),'.RData'))
  }
  dev.off()
}

