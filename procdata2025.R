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

# Kinect で結果を図示する関節
joi<-c('WRIST_RIGHT','HEAD','SHOULDER_RIGHT','HIP_RIGHT', 'KNEE_RIGHT')
#joi<-c('WRIST_RIGHT','HEAD','SHOULDER_LEFT','HIP_LEFT', 'KNEE_LEFT')
# 測定開始時点検出のための関節
wrst<-'WRIST_RIGHT'

# 実験開始時の大きな動きの検出閾値 1の場合、この設定は使用しない
thr<-0

# 測定時間の長さ。計測終了からこの時間の長さだけを解析対象とする
tlen<-8
#tlen<-1

# Kinectのサンプリングレート
kfreq<-30

rslt<-data.frame()
for (i in 1:nrow(proclist)){
  fname<-paste("id_", proclist[i,2], ".pdf", sep="")
  if (Sys.info()['sysname']=='Windows'){
    cairo_pdf(fname, family = "Yu Gothic")
  }else if(Sys.info()["sysname"] == "Linux") {
    cairo_pdf(fname, family = "Noto Sans CJK JP")
  }else{
    quartz(type="pdf", file=fname, width=8, height=10)
  }
  for (j in 1:6){
    
    bfreq<-proclist[i,15+j]
    bfn<-paste0(datfld,'\\',proclist[i,8+j],'.txt')
    btemp<-read.table(bfn, skip=1)
    colnames(btemp)<-c('Time','CpX','CpY','BL','BR','TL','TR','Wgt','RBL','RBR','RTL','RTR')
    ndat<-nrow(btemp)
    rlen<-((j-1)%%2)*(tlen-1)+1
    # 解析対象時間の設定
    # maxtbal<-max(which(abs(scale(btemp$CpX))>thr & btemp$Time < max(btemp$Time)-tlen*60))
    maxtbal<-max(which(btemp$Time < max(btemp$Time)-60*rlen))-2
    bdat<-btemp[maxtbal:ndat,]
    datlen<-nrow(bdat)
    
    # 重心方向の算出
    bdat$deg <- atan(bdat$CpY/bdat$CpX)*180/pi
    bdat[which(bdat$CpX<=0),]$deg <- bdat[which(bdat$CpX<=0),]$deg + 180
    bdat[which(bdat$CpX>0 & bdat$CpY<0),]$deg <- bdat[which(bdat$CpX>0 & bdat$CpY<0),]$deg + 360
    # 重心までの距離の算出
    bdat$scl <- sqrt(bdat$CpX^2 + bdat$CpY^2)
    
    # 軌跡長の算出
    # 集計単位
    lendur <- 10
    
    difflen<-array(NA,c(datlen-1,1))
    for (k in 2:datlen){
      difflen[k-1]<-sqrt((bdat$CpX[k]-bdat$CpX[k-1])^2+(bdat$CpY[k]-bdat$CpY[k-1])^2)
    }
    nprd<-floor(datlen/(bfreq*lendur))
    leng<-array(NA,c(nprd,1))
    for (k in 1:nprd){
      leng[k]<-sum(difflen[(1+(k-1)*(bfreq*lendur)):((bfreq*lendur)*k)])
    }
    cumleng<-cumsum(leng)
    names(cumleng)<-as.factor(seq(1,nprd)*lendur)
    
    
    par(mfcol=c(1,2))
    barplot(cumleng, xlab='time(sec)', ylab='trace length')
    title(paste('id',proclist[i,2],proclist[i,3+j%/%3]))
    barplot(t(leng), names.arg = as.factor(seq(1,nprd)))
    res<-nls(cumleng~a*seq(1,nprd)^b, start=c(a=1, b=1))
    
    # long形式への変換
    lbdat<-pivot_longer(data=bdat, cols=-Time,values_to = 'value', names_to = 'param')
    
    # 重心方向と距離のグラフ作成
    gdeg<-ggplot(lbdat[grep('deg',lbdat$param),], aes(x=Time, y=value))+geom_point()+ggtitle('orientation')
    gscl<-ggplot(lbdat[grep('scl',lbdat$param),], aes(x=Time, y=value))+geom_line()+ggtitle('weight')
    grid.arrange(gdeg,gscl, nrow=2)
    
    #print(res)
    rslt[(i-1)*6+j,1]<-proclist[i,2]
    rslt[(i-1)*6+j,2]<-proclist[i,3+(j-1)%/%2]
    rslt[(i-1)*6+j,3]<-(j-1)%%2+1
    rslt[(i-1)*6+j,4]<-sum(leng)
    rslt[(i-1)*6+j,5]<-coef(res)[2]
    rslt[(i-1)*6+j,6]<-coef(res)[1]
    rslt[(i-1)*6+j,7]<-proclist[i,22+(j-1)%%2*3+(j-1)%/%2*9]
    rslt[(i-1)*6+j,8]<-proclist[i,23+(j-1)%%2*3+(j-1)%/%2*9]
    rslt[(i-1)*6+j,9]<-proclist[i,24+(j-1)%%2*3+(j-1)%/%2*9]
    if (j%%2==0){
      rslt[(i-1)*6+j,10]<-proclist[i,28+(j-1)%/%2*9]
      rslt[(i-1)*6+j,11]<-proclist[i,29+(j-1)%/%2*9]
    }
    else{
      rslt[(i-1)*6+j,10]<-NA
      rslt[(i-1)*6+j,11]<-NA
    }
    rslt[(i-1)*6+j,12]<-(j-1)%/%2+1
  }
  dev.off()
}

colnames(rslt)<-c('ID','condition','period','trace','beta','alpha','stime','control','fatigue','self','double','order')

smr<-rslt %>% group_by(condition, period) %>% summarise(M_trace = mean(trace), SD_trace=sd(trace), M_beta=mean(beta), SD_beta=sd(beta))


s_rslt<-rslt[rslt$period==2,]
s_rslt$strace <-rslt[rslt$period==2,]$trace/rslt[rslt$period==1,]$trace

gtrace<-ggplot(data=rslt, aes(x=condition,y=trace,colour = as.factor(period)))+geom_boxplot()+geom_jitter(width=0.1, height=0)
plot(gtrace)

gstrace<-ggplot(data=s_rslt, aes(x=condition,y=strace))+geom_boxplot()+geom_jitter(width=0.1, height=0)
plot(gstrace)

gexp<-ggplot(data=s_rslt,aes(x=condition, y=beta, colour = as.factor(period)))+geom_boxplot()+geom_jitter(width=0.1, height=0)
plot(gexp)

gstime<-ggplot(data=s_rslt, aes(x=condition, y=stime, color=as.factor(period)))+geom_boxplot()+geom_jitter(width=0.1, height=0)
plot(gstime)

gcon<-ggplot(data=s_rslt, aes(x=condition, y=control, color=as.factor(period)))+geom_boxplot()+geom_jitter(width=0.1, height=0)
plot(gcon)

gftg<-ggplot(data=s_rslt, aes(x=condition, y=fatigue, color=as.factor(period)))+geom_boxplot()+geom_jitter(width=0.1, height=0)
plot(gftg)

gself<-ggplot(data=s_rslt, aes(x=condition, y=self, color=as.factor(period)))+geom_boxplot()+geom_jitter(width=0.1, height=0)
plot(gself)

gdouble<-ggplot(data=s_rslt, aes(x=condition, y=double, color=as.factor(period)))+geom_boxplot()+geom_jitter(width=0.1, height=0)
plot(gdouble)


lendat<-rslt[,c('ID','condition','period','trace')]
slendat<-s_rslt[,c('ID','condition','strace')]
expdat<-s_rslt[,c('ID','condition','beta')]
ctldat<-s_rslt[,c('ID','condition','control')]
ftgdat<-s_rslt[,c('ID','condition','fatigue')]
stimedat<-s_rslt[,c('ID','condition','stime')]
selfdat<-s_rslt[,c('ID','condition','self')]

source('anovakun_489.txt')
anovakun(lendat, "sAB", long=T)
anovakun(slendat, "sA", long=T, geta=T)
anovakun(expdat, "sA", long=T)
anovakun(ctldat, "sA", long=T)
anovakun(ftgdat, "sA", long=T)
anovakun(stimedat, "sA", long=T)

hist(slendat$strace)
hist(expdat$beta)

brmres<-brm(formula=trace~condition+as.factor(period)+as.factor(order),
            family=gaussian(),
            data=rslt,
            seed=1,
            iter=3000,
            warmup = 1000,
)
summary(brmres)

s_brmres<-brm(formula=strace~condition+as.factor(order),
            family=shifted_lognormal(),
            data=s_rslt,
            seed=1,
            iter=3000,
            warmup = 1000,
)
summary(s_brmres)

expres<-brm(formula=beta~condition+as.factor(order),
            family=gaussian(),
            data=s_rslt,
            seed=1,
            iter=3000,
            warmup = 1000,
)
summary(expres)

# Kinectデータ読み込み
fn<-file.choose()
dat<-jsonlite::read_json(fn, simplifyVector = TRUE)
nfrm<-length(dat$frames$bodies)
joich<-array(dim=length(joi))
for (i in 1:length(joi)){
  joich[i]<-which(dat$joint_names==joi[i])  
}

joidat<-data.frame()
joidist<-data.frame()

# jsonデータから値の取得, 時間がかかる
for (i in 1:nfrm){
  cord<-dat$frames[1]$bodies[[i]]$joint_positions[[1]][joich,]
  if (!is.null(cord)){
    joitemp<-cbind(rep(i, length(joi)), joi, cord)
    joidat<-rbind(joidat, joitemp)
  }
}
colnames(joidat)<-c('frame','joi','x','y','z')

# Kinectデータの正規化
stddat<-data.frame()
for (i in 1:length(joi)){
  temp <- cbind(joidat[which(joidat$joi==joi[i]),c(3:5)] %>% lapply(as.numeric) %>% data.frame  %>% lapply(scale) %>% data.frame)
  temp <- cbind(joi[i],min(as.numeric(joidat$frame)):max(as.numeric(joidat$frame)),min(as.numeric(joidat$frame)):max(as.numeric(joidat$frame))/30, temp)
  stddat<-rbind(stddat, temp)
}
colnames(stddat)<-c('joi','frame','time' ,'x','y','z')
tlen<-1
# 右手の高さと計測時間から計測地点開始時の設定
wrsd<-stddat[which(stddat$joi==wrst),]$y
oth<-which(abs(wrsd)>thr)
maxtpos<-max(oth[which(oth<(nfrm/kfreq-tlen*60)*kfreq)])
#maxtpos<-max(which(abs(wrsd)>thr))
stddat<-stddat[which(stddat$frame>maxtpos),]

# 正規化データのlong形式への変換
lstddat<-pivot_longer(stddat, cols=c('x','y','z'), values_to = 'pos')

# 頭部グラフn
ghead<-ggplot(data=lstddat[which(lstddat$joi=='HEAD'),], aes(x=time, y=pos, color=name, linetype =joi))+geom_line()+xlim(maxtpos/30,nfrm/30)+theme(legend.position = "top")

# 右半身グラフ
gshrd<-ggplot(data=lstddat[which(lstddat$joi=='SHOULDER_RIGHT'),], aes(x=time, y=pos, color=name, linetype =joi))+geom_line()+xlim(maxtpos/30,nfrm/30)+theme(legend.position = "top")
ghip<-ggplot(data=lstddat[which(lstddat$joi=='HIP_RIGHT'),], aes(x=time, y=pos, color=name, linetype =joi))+geom_line()+xlim(maxtpos/30,nfrm/30)+theme(legend.position = "top")
gwrst<-ggplot(data=lstddat[which(lstddat$joi=='WRIST_RIGHT'),], aes(x=time, y=pos, color=name, linetype =joi))+geom_line()+xlim(maxtpos/30,nfrm/30)+theme(legend.position = "top")



# 左半身グラフ
#gshrd<-ggplot(data=lstddat[which(lstddat$joi=='SHOULDER_LEFT'),], aes(x=time, y=pos, color=name, linetype =joi))+geom_line()+xlim(maxtpos/30,nfrm/30)+theme(legend.position = "top")
#ghip<-ggplot(data=lstddat[which(lstddat$joi=='HIP_LEFT'),], aes(x=time, y=pos, color=name, linetype =joi))+geom_line()+xlim(maxtpos/30,nfrm/30)+theme(legend.position = "top")

# バランスボード生データグラフ
gbalance<-ggplot(lbdat[grep('Cp',lbdat$param),], aes(x=Time, y=value, color=param))+geom_line()+theme(legend.position = "top")

# グラフ一括表示
grid.arrange(ghead,gshrd,ghip,nrow=3)

# バランスボード積分値表示
print(c(mean(abs(bdat$CpX)),mean(abs(bdat$CpY))))

