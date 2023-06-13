
# setting -----------------------------------------------------------------


library(tidyverse)
library(haven)
library(openxlsx)
Sys.setlocale(locale = "persian")
library(srvyr)
library(survey)
library(knitr)
library(flextable)
library(foreign)
library(RColorBrewer)
library(rgdal)
library(dplyr)
library(readstata13)
library(RColorBrewer)
library(labelled)
library(tidyverse)
library(haven)
library(dplyr)
library(openxlsx)
Sys.setlocale(locale = "persian")
library(srvyr)
library(survey)
library(knitr)
library(flextable)
library(labelled)
library(ggplot2)
library(upstartr)

library("RColorBrewer")

data.f <- read_dta(".../data.dta")

data.f=data.f%>% mutate(age_group=if_else(age<=24 ,"18-24",if_else(age<=44,"25-44",if_else(age<=60,"45-60","60+"))))


data.f <- data.f  %>%  mutate( smoking=if_else(s1b==1,0,if_else(s1b==0 & s1a==1,1,2)))
data.f <- data.f  %>%  mutate( Bmi=if_else(bmi>=30,0,if_else(bmi>=25 & bmi <30,1,2)))
data.f <- data.f  %>%  mutate( chol=if_else(CH02l>=240,0,if_else((CH02l>=200& CH02l<240)|(CH02l<200 & h14a==1),1,2)))
data.f <- data.f  %>%  mutate( BP=if_else(MeanSys>=140 | MeanDias>=90,0,if_else(((MeanSys>=120 & MeanSys<140)| (MeanDias>=80 & MeanDias<90))|(MeanSys<120 & MeanDias<80 & h3c==1),1,2)))
data.f <- data.f  %>%  mutate( fpg=if_else(GLUC3>=126,0,if_else((GLUC3>=100& GLUC3<126)|(GLUC3<100 & h88ma==1),1,2)))

data.f=data.f %>% mutate(work_moderate22=work_moderate22/4,
                         work_vigorous22=work_vigorous22/8,
                         transport_met22=transport_met22/4,
                         recreation_vigorous22=recreation_vigorous22/8,
                         recreation_moderate22=recreation_moderate22/4,
                         moderate=work_moderate22+transport_met22+recreation_moderate22,
                         vigorous=work_vigorous22+recreation_vigorous22,
                         moderate_vigorous=moderate+vigorous,
                         activity=if_else(moderate==0 & vigorous==0,0,if_else(moderate<150 | vigorous<75| moderate_vigorous<150,1,2)))

data.f <- data.f  %>%  mutate( frve=if_else(d1*d2+d3*d4>=31.5,1,0),
                               fish=if_else(d11a>=3,1,0),
                               fibr=if_else(d20n>=5,1,0),
                               sodium=if_else(sodium_24<=1500,1,0),
                               sugar=if_else(d19<=3,1,0),
                               diet=if_else(frve+fish+fibr+sodium+sugar<2,0,if_else(frve+fish+fibr+sodium+sugar<4,1,2)))

var_label(data.f$smoking)="smoking"
var_label(data.f$diet)="Healthy diet score"
var_label(data.f$activity)="Physical activity level"
var_label(data.f$Bmi)="Body mass index"
var_label(data.f$BP)="Blood pressure"
var_label(data.f$chol)="Total cholesterol"
var_label(data.f$fpg)="Fasting plasma glucose"

data.f=data.f %>% mutate(smoking_1=if_else(smoking<2,0,1))
data.f=data.f %>% mutate(diet_1=if_else(diet<2,0,1))
data.f=data.f %>% mutate(activity_1=if_else(activity<2,0,1))
data.f=data.f %>% mutate(Bmi_1=if_else(Bmi<2,0,1))
data.f=data.f %>% mutate(BP_1=if_else(BP<2,0,1))
data.f=data.f %>% mutate(chol_1=if_else(chol<2,0,1))
data.f=data.f %>% mutate(fpg_1=if_else(fpg<2,0,1))

data.f=data.f %>% 
  group_by(familymemberid)%>% 
  mutate(LS7_Ideal=sum(smoking_1,diet_1,activity_1,Bmi_1,BP_1,chol_1,fpg_1,na.rm=T)) %>% 
  ungroup()

data.f=data.f %>% mutate(smoking_1=if_else(smoking==1,1,0))
data.f=data.f %>% mutate(diet_1=if_else(diet==1,1,0))
data.f=data.f %>% mutate(activity_1=if_else(activity==1,1,0))
data.f=data.f %>% mutate(Bmi_1=if_else(Bmi==1,1,0))
data.f=data.f %>% mutate(BP_1=if_else(BP==1,1,0))
data.f=data.f %>% mutate(chol_1=if_else(chol==1,1,0))
data.f=data.f %>% mutate(fpg_1=if_else(fpg==1,1,0))

data.f=data.f %>% 
  group_by(familymemberid)%>% 
  mutate(LS7_Intermediate=sum(smoking_1,diet_1,activity_1,Bmi_1,BP_1,chol_1,fpg_1,na.rm=T)) %>% 
  ungroup()


data.f=data.f %>% mutate(smoking_1=if_else(smoking>0,0,1))
data.f=data.f %>% mutate(diet_1=if_else(diet>0,0,1))
data.f=data.f %>% mutate(activity_1=if_else(activity>0,0,1))
data.f=data.f %>% mutate(Bmi_1=if_else(Bmi>0,0,1))
data.f=data.f %>% mutate(BP_1=if_else(BP>0,0,1))
data.f=data.f %>% mutate(chol_1=if_else(chol>0,0,1))
data.f=data.f %>% mutate(fpg_1=if_else(fpg>0,0,1))

data.f=data.f %>% 
  group_by(familymemberid)%>%
  mutate(LS7_Poor=sum(smoking_1,diet_1,activity_1,Bmi_1,BP_1,chol_1,fpg_1,na.rm=T)) %>% 
  ungroup()




data.f=data.f %>% group_by(familymemberid)%>% mutate(LS7_1=sum(smoking,diet,activity,Bmi,BP,chol,fpg,na.rm=T)) %>% ungroup()

data.f=data.f %>% mutate(h17ac=if_else(h17ac==-555,0,as.numeric(h17ac)))
data.f=data.f %>% mutate(h17bc=if_else(h17bc==-555,0,as.numeric(h17bc)))
data.f=data.f %>% mutate(h7c=if_else(h7c==-555,0,as.numeric(h7c)))
data.f=data.f %>% mutate(h2y=if_else(h2y==-555,0,as.numeric(h2y)))
data.f=data.f %>% mutate(h13b=if_else(h13b==-555,0,as.numeric(h13b)))
data.f=data.f %>% mutate(r1=if_else(r1==-555,0,as.numeric(r1)))





pop=read_dta(".../population.dta")
pop$age=as.numeric(pop$age)

pop=pop %>% filter(Province!=25)
pop=pop[pop$year==1395 & pop$age>=18 ,]


pop$age_cat=NA
pop$age_cat[pop$age<25]=18
pop$age_cat[pop$age>=25 & pop$age<35]=25
pop$age_cat[pop$age>=35 & pop$age<45]=35
pop$age_cat[pop$age>=45 & pop$age<55]=45
pop$age_cat[pop$age>=55 & pop$age<65]=55
pop$age_cat[pop$age>=65 & pop$age<70]=65
pop$age_cat[pop$age>=70 ]=70

pop=aggregate(pop$pred2pop~pop$sex_name+pop$area_name+pop$age_cat,FUN = sum)
colnames(pop)=c("c1","area","age_cat","pop")
pop= pop %>% rename(pop18=pop) %>%  mutate(pop25=if_else(age_cat>=25,pop18,0))

p25=pop %>%group_by(age_cat) %>% summarise(p=sum(pop18)) %>% filter(age_cat>=25)
p25_s=pop %>%group_by(age_cat,c1) %>% summarise(p=sum(pop18)) %>% filter(age_cat>=25)
p25_a=pop %>%group_by(age_cat,area) %>% summarise(p=sum(pop18)) %>% filter(age_cat>=25)
p25_sa=pop %>%group_by(age_cat,c1,area) %>% summarise(p=sum(pop18)) %>% filter(age_cat>=25)

p18=pop %>%group_by(age_cat) %>% summarise(p=sum(pop18)) 
p18_s=pop %>%group_by(age_cat,c1) %>% summarise(p=sum(pop18)) 
p18_a=pop %>%group_by(age_cat,area) %>% summarise(p=sum(pop18)) 
p18_sa=pop %>%group_by(age_cat,c1,area) %>% summarise(p=sum(pop18)) 

setwd("E:/paper rersult final version/")
ogrListLayers("shape file/IRN/E14.shp")
map1 = readOGR("shape file/IRN/E14.shp", layer="E14")
ogrListLayers("shape file/Khazar_P_Gulf/Khazar_P_Gulf.shp")
map2 = readOGR("shape file/Khazar_P_Gulf/Khazar_P_Gulf.shp", layer="Khazar_P_Gulf")



# stacked bar -------------------------------------------------------------
data.f.a <- data.f %>% filter(age>=25)  %>%
  as_survey_design(strata= i07, weights = W_Laboratory) 
data.f.q <- data.f %>% filter(age>=25)  %>%
  as_survey_design(strata= i07, weights = W_Laboratory)
data.f.l <- data.f %>% filter(age>=25)  %>%
  as_survey_design(strata= i07, weights = W_Laboratory) 



var_q=c("smoking","activity")
var_a=c("BP","Bmi")
var_l=c("chol","fpg","diet")
i="smoking"
a=data.f.q %>% filter(!is.na(!!!sym(i))) %>%group_by(!!!sym(i)) %>% summarise(percent=round(survey_prop(vartype = "ci")*100,2))%>% 
  rename("Category"=i) %>% mutate("Variable"=i) %>% relocate(Variable)
for (i in var_q[-1]) {
  
b=data.f.q %>% filter(!is.na(!!!sym(i))) %>%group_by(!!!sym(i)) %>% summarise(percent=round(survey_prop(vartype = "ci")*100,2))%>% 
rename("Category"=i) %>% mutate("Variable"=i) %>% relocate(Variable)
  
a=a %>% bind_rows(b)
}

for (i in var_a) {
  
  b=data.f.a %>% filter(!is.na(!!!sym(i))) %>%group_by(!!!sym(i)) %>% summarise(percent=round(survey_prop(vartype = "ci")*100,2))%>% 
    rename("Category"=i) %>% mutate("Variable"=i) %>% relocate(Variable)
  
  a=a %>% bind_rows(b)
}

for (i in var_l) {
  
  b=data.f.l %>% filter(!is.na(!!!sym(i))) %>%group_by(!!!sym(i)) %>% summarise(percent=round(survey_prop(vartype = "ci")*100,2))%>% 
    rename("Category"=i) %>% mutate("Variable"=i) %>% relocate(Variable)
  
  a=a %>% bind_rows(b)
}


a$Category=factor(a$Category,levels = c(0,1,2))

jpeg(paste0(".../plot/stacked bar .jpeg"),width=2800,height=1800,res=300)

ggplot(a, aes(x=Variable, y=percent,fill= Category)) + 
  geom_bar(stat="identity") +
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5,ncol=1,title.position="top",title.hjust=0.5,title=""))+
  scale_fill_manual(breaks=c(0,1,2),
                    labels=c("Poor","Intermediate","Ideal"),values =brewer.pal(9,"Blues")[c(4,6,8)])+
  # ggtitle("") +
  labs(x = "LS7 Component" , y = "Participants, %")  +
  theme_bw() +theme(axis.text.x = element_text(angle =0, hjust = 0.5,vjust=0.5, size=13))+		
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.y = element_text(size = 10 , face = "bold")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.title.y = element_text(size = 10 , face = "bold"),
        axis.text.x = element_text(size = 8 , face = "bold"),
        legend.key=element_blank(),
        legend.text=element_text(size=8),plot.title = element_text(hjust = 0.5))


dev.off()



# Map ---------------------------------------------------------------------



data.f.l <- data.f %>% filter(age>=25)  %>%
  as_survey_design(strata= i07, weights = W_Laboratory)  %>% 
  svystandardize(by = ~age_cat,over = ~i07,population =p25$p )


b=data.f.l %>% group_by(i07) %>% summarise(percent=round(survey_mean(LS7_1,vartype = "ci"),2))

n=5
n=n+1
leg = c(1:n)
sub=b 
sub$col = "#575757"
head(sub)  
xtile  = quantile(sub$percent ,probs = seq(0,1,by=1/(n-1)))
sub$cat = findInterval(sub$percent ,   data.frame(xtile)[-c(1,n),1] ) 
col=rev(brewer.pal(n-1,"RdYlGn"))
sub$col = col[sub$cat+1]  
# sub$col[is.na(sub$col)]="#575757"
jpeg(paste0(".../plot/map mean LS7.jpeg"), units = "in",res=1500 ,width=5 , height=4)
par(mar=c(1, 0, 1, 0))
plot(map1,col=sub$col[match(map1$PROVINCEID,sub$i07)],lwd=0.7,main = "")    
plot(map2 , add=T , col="#AADAFF")
leg=rep(NA,n-1) 
for (k in 2:(n-2)) leg[k] = paste0(round(xtile[k],2),"-",round(xtile[k+1],2))
leg[1]=paste0("<",round(xtile[2],2))
leg[n-1]=paste0(">",round(xtile[n-1],2))
legend("bottomleft",cex=0.4,pt.cex=0.9 ,legend=leg, col=rev(brewer.pal(n-1,"RdYlGn")) , title="LS7 Score" ,pch=15 , bty="n",text.font=2, ncol=1)
dev.off()



# radar diagram for LF7`s component ---------------------------------------

library(plotrix)

data.f.a <- data.f %>% filter(age>=25)  %>%
  as_survey_design(strata= i07, weights = W_Laboratory)  %>% 
  svystandardize(by = ~age_cat,over = ~i07,population =p25$p )
data.f.q <- data.f %>% filter(age>=25)  %>%
  as_survey_design(strata= i07, weights = W_Laboratory)  %>% 
  svystandardize(by = ~age_cat,over = ~i07,population =p25$p )
data.f.l <- data.f %>% filter(age>=25)  %>%
  as_survey_design(strata= i07, weights = W_Laboratory)  %>% 
  svystandardize(by = ~age_cat,over = ~i07,population =p25$p )



var_q=c("smoking","activity")
var_a=c("BP","Bmi")
var_l=c("chol","fpg","diet")
var=c(var_q,var_a,var_l)
i="smoking"

a=data.f.q %>% 
  filter(!is.na(!!!sym(i))) %>%
  group_by(i07,!!!sym(i)) %>% 
  summarise(percent=round(survey_prop(vartype = "ci")*100,2))%>% 
  rename("Category"=i) %>% 
  mutate("Variable"=i) %>% 
  relocate(Variable)

for (i in var_q[-1]) {
  
  b=data.f.q %>% filter(!is.na(!!!sym(i))) %>%group_by(i07,!!!sym(i)) %>% summarise(percent=round(survey_prop(vartype = "ci")*100,2))%>% 
    rename("Category"=i) %>% mutate("Variable"=i) %>% relocate(Variable)
  
  a=a %>% bind_rows(b)
}

for (i in var_a) {
  
  b=data.f.a %>% filter(!is.na(!!!sym(i))) %>%group_by(i07,!!!sym(i)) %>% summarise(percent=round(survey_prop(vartype = "ci")*100,2))%>% 
    rename("Category"=i) %>% mutate("Variable"=i) %>% relocate(Variable)
  
  a=a %>% bind_rows(b)
}

for (i in var_l) {
  
  b=data.f.l %>% filter(!is.na(!!!sym(i))) %>%group_by(i07,!!!sym(i)) %>% summarise(percent=round(survey_prop(vartype = "ci")*100,2))%>% 
    rename("Category"=i) %>% mutate("Variable"=i) %>% relocate(Variable)
  
  a=a %>% bind_rows(b)
}

a=a %>% filter(!is.na(Category))

province=read.xlsx(" Province Codes.xlsx",sheet = "Sheet2")

a=a %>% left_join(province)


for(i in var){

b1=a %>% filter(Variable==i & Category==2)
b2=a %>% filter(Variable==i & Category==1)
b3=a %>% filter(Variable==i & Category==0)



jpeg(paste0(".../plot/radar ",i,".jpeg"), units = "in",res=1500 ,width=14 , height=10)

par(cex.axis=0.85,cex.lab=0.7)

radial.plot(b1$percent,grid.col="gray80",cex.axis=20,cex.lab=8,start=0,rp.type="p",lwd=3,line.col="indianred1",
            labels=b1$province,rad.col="gray",radlab=FALSE,mar=c(4,3,6,3),show.grid.labels=6,radial.lim=c(0,100))

radial.plot(b2$percent,grid.col="gray80",rp.type="p",cex.lab=8,lwd=4,start=0,line.col="steelblue", 
            labels=b2$province,rad.col="blue",show.grid.labels=25,radial.lim=c(0,100),add=TRUE)

radial.plot(b3$percent,grid.col="gray80",rp.type="p",cex.lab=8,lwd=4,start=0,line.col="mediumseagreen", 
            labels=b2$province,rad.col="blue",show.grid.labels=25,radial.lim=c(0,100),add=TRUE)

legend("topright" ,cex =1,title=i,lwd =3, inset=c(-0.3,0),c("Ideal","Intermediate","Poor"),
       col=c("indianred1","steelblue","mediumseagreen"),lty=1)

dev.off()


}



#  Bar chart  -------------------------------------------------------------


data.f=data.f %>% mutate(smoking_1=if_else(smoking<2,0,1))
data.f=data.f %>% mutate(diet_1=if_else(diet<2,0,1))
data.f=data.f %>% mutate(activity_1=if_else(activity<2,0,1))
data.f=data.f %>% mutate(Bmi_1=if_else(Bmi<2,0,1))
data.f=data.f %>% mutate(BP_1=if_else(BP<2,0,1))
data.f=data.f %>% mutate(chol_1=if_else(chol<2,0,1))
data.f=data.f %>% mutate(fpg_1=if_else(fpg<2,0,1))

data.f=data.f %>% group_by(familymemberid)%>% mutate(LS7=sum(smoking_1,diet_1,activity_1,Bmi_1,BP_1,chol_1,fpg_1,na.rm=T)) %>% ungroup()

data.f.l <- data.f %>% filter(age>=25)  %>%
  as_survey_design(strata= i07, weights = W_Laboratory)  %>% 
  svystandardize(by = ~c1+age_cat,over = ~i07,population =p25_s$p )

data.f.ll <- data.f %>% filter(age>=25)  %>%
  as_survey_design(strata= i07, weights = W_Laboratory)  %>% 
  svystandardize(by = ~age_cat,over = ~i07,population =p25$p )

b=data.f.l %>% group_by(i07,c1) %>% summarise(percent=round(survey_mean(LS7,vartype = "ci"),2))
a=b %>% filter(c1==0)
b=b %>% filter(c1==1)
c=data.f.l %>% group_by(i07) %>% summarise(percent=round(survey_mean(LS7,vartype = "ci"),2))


province=read.xlsx(" Province Codes.xlsx",sheet = "Sheet2")

a=a %>% left_join(province)





jpeg(".../plot/barplot of mean LS7 by provinces.jpeg", units = "in",res=1500 ,width=5 , height=4)


ggplot(a) + geom_bar(aes(x=reorder(province,percent),y=percent, fill = percent), stat = 'identity') + 
  scale_fill_viridis_c() +
  coord_flip() +
  ylab("Mean of LS7") + 
  xlab("Provinces") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.title = element_blank()) 

dev.off()



