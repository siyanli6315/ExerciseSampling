library(ggplot2)
library("grid")
library("gridExtra")
#library("latex2exp")
library("grDevices")
library(showtext)
showtext.auto(enable=TRUE)

#setwd("~/Downloads/抽样作业")
#dat=read.csv("LoanStats3c.csv",header=T,skip=1)
#annual_inc=na.omit(dat$annual_inc)
#save(auual_inc,file="annual_inc.rda")
load("annual_inc.rda")

#对总体进行合适的分组
cut=c(min(annual_inc),(1:10)*10000,150000,(2:5)*100000,max(annual_inc))
zt=data.frame(annual_inc=annual_inc,fenzu=cut(annual_inc,breaks=cut))
zt=na.omit(zt)
zt$rowname=as.integer(row.names(zt))
n=nrow(zt)

zt_fz=data.frame(table(zt$fenzu))
zt_fz$Var2=as.factor((cut[1:16]+cut[2:17])/2)
zt_fz$Freq2=zt_fz$Freq/sum(zt_fz$Freq)
zt_fz$Freq3=paste(round(zt_fz$Freq2*100,2),"%",sep="")

#确定样本容量
x=seq(6,10,length=40)
m=round(exp(x),0)

#简单随机抽样
fun1=function(x){
  index=sample(zt$rowname,x)
  s=zt[index,]
  s_fz=data.frame(table(s$fenzu))
  s_fz$Freq2=s_fz$Freq/sum(s_fz$Freq)+0.0000000001
  I=sum((zt_fz$Freq2-s_fz$Freq2)*log(zt_fz$Freq2/s_fz$Freq2))
  Q=exp(-I)
  return(list(s_fz=s_fz,Q=Q))
}

#分层抽样
fun2=function(x){
  m1=round(zt_fz$Freq2*x,0)
  index=unlist(apply(cbind(c(1:16),m1),1,function(x)sample(zt$rowname[zt$fenzu==zt_fz$Var1[x[1]]],x[2])))
  s=zt[index,]
  s_fz=data.frame(table(s$fenzu))
  s_fz$Freq2=s_fz$Freq/sum(s_fz$Freq)+0.0000000001
  I=sum((zt_fz$Freq2-s_fz$Freq2)*log(zt_fz$Freq2/s_fz$Freq2))
  Q=exp(-I)
  return(list(s_fz=s_fz,Q=Q))
}

#画图部分
#图像1:年收入频率分布
ph1=ggplot()+
  labs(x="年收入",y="频率")+
  geom_bar(data=zt_fz,
           aes(x=Var2,y=Freq2),
           stat="identity",
           fill="black",
           color="white",
           alpha=0.6)+
  geom_text(data=zt_fz,
            aes(x=Var2,y=Freq2,label=Freq3),
            vjust=-0.1,
            hjust=0.5,
            size=3)+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.title.x=element_text(size=20,
                                  margin=margin(10,0,0,0)),
        axis.title.y=element_text(size=20,
                                  margin=margin(0,20,0,0)),
        axis.text.x=element_text(size=12,
                                 color="black",
                                 margin=margin(10,0,0,0),
                                 angle=45),
        axis.text.y=element_text(size=12,
                                 color="black",
                                 margin=margin(10,0,0,0)))
print(ph1)
ggsave("年收入频率分布.pdf")

#图像2:总体和样本的比较
pfun1=function(x){
  srs1=fun1(x)$s_fz$Freq2
  ss1=fun2(x)$s_fz$Freq2
  pd=data.frame(x=zt_fz$Var2,
                y=c(zt_fz$Freq2,srs1,ss1),
                fill=factor(rep(c("总体","简单随机抽样","分层抽样"),
                                each=16),
                            levels=c("总体","简单随机抽样","分层抽样")))
  ph=ggplot(data=pd,aes(x=x,y=y,fill=fill))+
    labs(x="年收入",y="频率",fill="",title=paste("样本量为",x,sep=""))+
    geom_bar(stat="identity",
             position="dodge",
             color="white",
             width=0.9)+
    theme_bw()+
    theme(panel.grid=element_blank(),
          axis.title.x=element_text(size=20,
                                    margin=margin(10,0,0,0)),
          axis.title.y=element_text(size=20,
                                    margin=margin(0,20,0,0)),
          axis.text.x=element_text(size=12,
                                   color="black",
                                   margin=margin(10,0,0,0),angle=45),
          axis.text.y=element_text(size=12,
                                   color="black",
                                   margin=margin(0,10,0,0)),
          legend.position="top",
          legend.key.width=unit(1,"cm"),
          legend.key=element_rect(colour='white',
                                  fill='white',
                                  size=1),
          legend.text=element_text(size=18),
          legend.key.size=unit(0.7,'cm'),
          plot.title=element_text(size=18))
  print(ph)
}

#pdf(file="分层抽样比较.pdf",width=8,height=5)
#pushViewport(viewport(layout=grid.layout(5,7)))
#vplayout=function(x,y){
#  viewport(layout.pos.row=x,layout.pos.col=y)
#}
#print(ph1,vp=vplayout(1:2,1:4))
#print(ph3,vp=vplayout(3:5,1:4))
#print(ph2,vp=vplayout(1:2,5:7))
#print(ph4,vp=vplayout(3:5,5:7))
#dev.off()

set.seed(1)
m1=round(c(quantile(m,prob=c(0,0.25,0.75,1))))
for(i in 1:4){
  pfun1(m1[i])
  ggsave(paste("样本量为",m1[i],".pdf",sep=""))
}

#两种抽样方式比较
pfun2=function(m,x=NULL){
  set.seed(x)
  srs=apply(as.matrix(m),1,fun1)
  q1=rep(NA,40)
  for(i in 1:40) q1[i]=srs[[i]]$Q
  ss=apply(as.matrix(m),1,fun2)
  q2=rep(NA,40)
  for(i in 1:40) q2[i]=ss[[i]]$Q
  pd=data.frame(x=m,
                y=c(q1,q2),
                fill=factor(rep(c("简单随机抽样","分层抽样"),
                                each=40),
                            levels=c("简单随机抽样","分层抽样")))
  ph=ggplot(data=pd,aes(x=x,y=y,color=fill,shape=fill))+
    labs(x="样本容量",y="样本质量",color="抽样方法",shape="抽样方法")+
    scale_x_continuous(limits=c(0,10000))+
    geom_line(alpha=0.8,size=1.2)+
    geom_point(size=3,alpha=0.8)+
    theme_bw()+
    theme(panel.grid=element_blank(),
          axis.title.x=element_text(size=20,
                                    margin=margin(20,0,0,0)),
          axis.title.y=element_text(size=20,
                                    margin=margin(0,20,0,0)),
          axis.text.x=element_text(size=12,
                                   color="black",
                                   margin=margin(10,0,0,0)),
          axis.text.y=element_text(size=12,
                                   color="black",
                                   margin=margin(10,0,0,0)),
          legend.key.width=unit(2,"cm"),
          legend.key=element_rect(colour='white',
                                  fill='white',size=1),
          legend.title=element_text(size=20),
          legend.text=element_text(size=15),
          legend.key.size=unit(1.5,'cm'),
          legend.position=c(0.8,0.3))
  print(ph)
}

for(x in 1:4){
  pfun2(m,x)
  ggsave(paste("随机种子为",x,".pdf",sep=""))
}

