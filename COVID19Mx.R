###########################################################
######         Analisis COVID19 en Mexico            ######
######                  02/06/2020                   ######
######          Diego Camacho/Victor Nieto           ######
###########################################################

library(ggplot2)
library(grid)
library("UpSetR")
library(tidyverse)

### Evolucion de la pandemia, casos nuevos por dia y por entidad federativa
t<-read.csv("Casos_Diarios_Estado_Nacional_Confirmados.csv",header = T)
Nac_sin_norm<- NULL
Nac_sin_norm$Estado<-t$nombre
Nac_sin_norm$Contagios<- apply(t[4:length(colnames(t))],1,sum)
Nac_sin_norm<- data.frame(Nac_sin_norm)
Nac_sin_norm

#Orden<-t$nombre[order(t$poblacion,decreasing = T)]
#Nac_sin_norm$Estado<-factor(Nac_sin_norm$Estado,levels = (Orden))


Orden<-t$nombre[order(Nac_sin_norm$Contagios,decreasing = T)]
Nac_sin_norm$Estado<-factor(Nac_sin_norm$Estado,levels = (Orden))
ggplot(Nac_sin_norm,aes(x=Estado,y=Contagios)) + geom_bar(stat = "identity",fill="darkred") + coord_flip() + labs(y="Cuentas crudas", x="Entidades")+ggtitle("Contagios por entidad no normalizados")+theme(axis.text=element_text(size=7),axis.title=element_text(size=12,face="bold"))

Nac_sin_norm$Pobg<-t$poblacion
Nac_sin_norm$Norm<-Nac_sin_norm$Contagios/Nac_sin_norm$Pobg

ggplot(Nac_sin_norm,aes(x=Estado,y=Norm)) + geom_bar(stat = "identity",fill="darkgreen") + coord_flip() + labs(y="Cuentas/Población", x="Entidades")+ggtitle("Contagios por entidad normalizados")+geom_text(aes(label=formatC(Pobg,format = "e",digits = 1)),nudge_y = +0.0002,size=4) +theme(axis.text=element_text(size=7),axis.title=element_text(size=12,face="bold"))

t_n<-t[,4:length(colnames(t))]
p<-t(t_n)
p
P<-data.frame(p)

day<-NULL
day$New<-P$X33
day$Day<-colnames(t_n)
day<-data.frame(day)
day

ggplot(day,aes(x=c(1:138),y=New,group=106)) + geom_line(color="red") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

Odays<-39

cumday<-day[39:138,]
rownames(cumday)<-1:100

ggplot(cumday,aes(x=c(1:100),y=New)) + geom_bar(stat="identity",color="black",fill="blue") + geom_vline(xintercept = 23,color="red") +labs(y="Casos",x="Días desde el caso 1")+
  ggtitle("Casos nuevos por día") 

ggplot(cumday,aes(x=c(1:100),y=cumsum(New),group=68)) +geom_line(color="red") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#https://www.cdc.gov/coronavirus/2019-ncov/cases-updates/cases-in-us.html#anchor_1586790730
Usa<-c(1,0,1,0,3,0,0,0,0,2,1,0,3,0,0,0,0,0,0,0,1,0,1,0,0,0,0,0,0,0,2,0,0,0,0,0,1,0,8,6,23,25,20,66,47,64,147,225,290,278,414,267,338,1237,755,2797,3419,4777,3528,5836,8821,10934,10115,13987,16916,17965,19332,18251,22635,22562,27043,26135,34864,30683,26065,43438,21597,31534,31705,33251,33288,29145,24156,26385,27158,29164,29002,29916,25995,29468,26490,25858,37144,29873,33161,29256,23371,23901,25512,31787,30369,29794,29763,19138,22303,23366,30861,25996,26660,23792,18106,21467,20869,27191,22977,31967,13284,24481,23405,22860,20522,24268,26229,15342,24958,16429,19680,21304,18123)
USA_Mdays<-Usa[1:100]
cumday$USA<-USA_Mdays
ggplot(cumday,aes(x=c(1:100),y=cumsum(New),group=68)) +geom_line(color="red") +geom_line(aes(x=c(1:100),y=cumsum(USA),group=68))+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#https://github.com/pcm-dpc/COVID-19/tree/master/dati-andamento-nazionale
I<-read.csv("Italy.tsv",sep="\t")
I_Mdays<-c(c(14,30,100),I$nuovi_positivi)   

cumday$It<-I_Mdays

### Comparacion con otros paises
#https://data.europa.eu/euodp/en/data/dataset/covid-19-coronavirus-data
wwde<- read.csv("download.txt")
Spain<- Spain<- wwde %>% filter(countriesAndTerritories=="Spain") %>% filter(cases>=1)
Sp_Mxday<- Spain$cases[92:1]
Sp_Mxday<-c(1,0,1,0,0,0,0,1,Sp_Mxday)
cumday$Sp<-Sp_Mxday

ggplot(cumday,aes(x=1:100,y=cumsum(New)/127792286,group=68,color="México")) +
geom_point() +
geom_point(shape=2,aes(x=1:100,y=cumsum(USA)/328200000,group=68,color="USA"))+
geom_point(shape=5,aes(x=1:100,y=cumsum(It)/60359546,group=68,color="Italia"))+
geom_point(shape=7,aes(x=1:100,y=cumsum(Sp)/46940000,group=68,color="España"))+
geom_point(shape=9,aes(x=1:100,y=cumsum(New)*23/127792286,group=68,color="México con factor\n correcvtivo (x23)"))+
labs(color = "País",y="Casos",x="Días desde el caso 1")+
ggtitle("Evolución Covid-19 por país normalizado población total") 
theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


ggplot(cumday,aes(x=1:100,y=cumsum(New)*23,group=68,color="Mexico con factor \n correctivo (x23)")) +
  geom_point() +
  geom_point(shape=2,aes(x=1:100,y=cumsum(USA),group=68,color="USA"))+
  geom_point(shape=5,aes(x=1:100,y=cumsum(It),group=68,color="Italia"))+
  geom_point(shape=7,aes(x=1:100,y=cumsum(Sp),group=68,color="España"))+
  labs(color = "País",y="Casos",x="Días desde el caso 1")+
  ggtitle("Evolución Covid-19 por país") 
theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

cumnorm<-cumday
cumnorm$New<-cumday$New/127792286
cumnorm$USA<-cumday$USA/328200000
cumnorm$It<-cumday$It/60359546
cumnorm$Sp<-cumday$Sp/46940000

U_M<-aov(New~USA,data=cumnorm)
I_M<-aov(New~It,data=cumnorm)
S_M<-aov(New~Sp,data = cumnorm)

plot(TukeyHSD(U_M),las=1)

#data2 <- data.frame(Country = c(rep("Mexico",68),rep("Spain",68)), Cases = c(cumday$New,cumday$Sp))
data <- data.frame(Country = c(rep("México",100),rep("USA",100),rep("Italia",100),rep("España",100)), 
                   Cases = c(cumday$New/127792286,cumday$USA/328200000,cumday$It/60359546,cumday$Sp/46940000))
  
ggplot(data,aes(x=Country,y=Cases))+ geom_boxplot()

#ggplot(data2, aes(x = Country, y = Cases)) +
 # geom_boxplot(fill = "grey80", colour = "blue") +
  #scale_x_discrete() + xlab("Country") +
  #ylab("Cases") +
  #geom_signif(comparisons = list(c("Mexico", "Spain")), 
   #           map_signif_level=TRUE)
library(ggsignif)
ggplot(data, aes(x = Country, y = Cases)) + 
  geom_boxplot(fill="blue",colour="darkblue") +
  geom_signif(test = "t.test",comparisons = list(c("México","USA"),c("México","Italia"),c("México","España")), map_signif_level = TRUE)+
  labs(x = "País",y="Casos") + ggtitle("Distribución de casos normalizados por país") 

### Riesgo relativo, co-ocurrencia con otras condiciones medicas
COVID_May30 <- read.csv("~/200530COVID19MEXICO.csv")
head(COVID_May30)

length(unique((COVID_May30$ID_REGISTRO)))

COVID_Riesgo <- COVID_May30[(COVID_May30$RESULTADO==1 & COVID_May30$NACIONALIDAD==1),c(2,6,10,13:16,18,20:29)]
head(COVID_Riesgo)
COVID_Riesgo <- COVID_Riesgo[,-c(2,4,6)]
COVID_Riesgo <- cbind(COVID_Riesgo, PACIENTE=1:87096)
head(COVID_Riesgo)
colnames(COVID_Riesgo)

Emb <- (COVID_Riesgo[COVID_Riesgo$EMBARAZO==1 & COVID_Riesgo$TIPO_PACIENTE==2, "PACIENTE"])
Diab <- (COVID_Riesgo[COVID_Riesgo$DIABETES==1 & COVID_Riesgo$TIPO_PACIENTE==2, "PACIENTE"])
EPOC <- (COVID_Riesgo[COVID_Riesgo$EPOC==1 & COVID_Riesgo$TIPO_PACIENTE==2, "PACIENTE"])
Asma <- (COVID_Riesgo[COVID_Riesgo$ASMA==1 & COVID_Riesgo$TIPO_PACIENTE==2, "PACIENTE"])
Inmuno <- (COVID_Riesgo[COVID_Riesgo$INMUSUPR==1 & COVID_Riesgo$TIPO_PACIENTE==2, "PACIENTE"])
Hiper <- (COVID_Riesgo[COVID_Riesgo$HIPERTENSION==1 & COVID_Riesgo$TIPO_PACIENTE==2, "PACIENTE"])
Otras <- (COVID_Riesgo[COVID_Riesgo$OTRA_COM==1 & COVID_Riesgo$TIPO_PACIENTE==2, "PACIENTE"])
Cardio <- (COVID_Riesgo[COVID_Riesgo$CARDIOVASCULAR==1 & COVID_Riesgo$TIPO_PACIENTE==2, "PACIENTE"])
Obes <- (COVID_Riesgo[COVID_Riesgo$OBESIDAD==1 & COVID_Riesgo$TIPO_PACIENTE==2, "PACIENTE"])
Renal <- (COVID_Riesgo[COVID_Riesgo$RENAL_CRONICA==1 & COVID_Riesgo$TIPO_PACIENTE==2, "PACIENTE"])
Tab <- (COVID_Riesgo[COVID_Riesgo$TABAQUISMO==1 & COVID_Riesgo$TIPO_PACIENTE==2, "PACIENTE"])

all<-c(Emb,Diab, EPOC,Asma,Inmuno, Hiper,Otras, Cardio,Obes,Renal, Tab)

Hospitalizados <- COVID_Riesgo[COVID_Riesgo$TIPO_PACIENTE==2,"PACIENTE"]
Sanos <- setdiff(Hospitalizados,unique(all))

Sanos_Up<-setdiff((COVID_Riesgo[COVID_Riesgo$TIPO_PACIENTE==2 & COVID_Riesgo$EDAD>=60,"PACIENTE"]), unique(all))

Sanos_Down<-setdiff((COVID_Riesgo[COVID_Riesgo$TIPO_PACIENTE==2 & COVID_Riesgo$EDAD<60,"PACIENTE"]), unique(all))

library(grid)
listInput <- list(Embarazo = c(Emb), Diabetes = c(Diab), Cardiovasculares = c(Cardio), EPOC=c(EPOC),Asma=c(Asma), Inmunosupresion=c(Inmuno), Hipertension=c(Hiper),Otras=c(Otras),Obesidad=c(Obes), Insuficiencia_Renal=c(Renal), Tabaquismo=c(Tab), SP_May60=c(Sanos_Up),SP_Men60=c(Sanos_Down))

upset(fromList(listInput),nsets = 13,
      sets.bar.color=colorRampPalette(c("red", "blue"))(13) ,
      matrix.color="Darkgreen",
      main.bar.color = "Steelblue",
      cutoff = 5,
      mainbar.y.label = "Frecuencias\nabsolutas",
      text.scale=c(1.5,1.2, 1.2, 1.2, 1.2, 0.8),
      sets.x.label = "Número de casos",
      nintersects = 30,
      mb.ratio = c(0.60, 0.40), order.by = "freq")
grid.text("Hospitalizaciones COVID-19 México \nCo-ocurrencia con otras condiciones\nmédicas,30 de mayo", x = 0.73, y=0.85, gp=gpar(fontsize=13, fontface="bold"))

Emb <- (COVID_Riesgo[COVID_Riesgo$EMBARAZO==1 & COVID_Riesgo$INTUBADO==1, "PACIENTE"]
Diab <- (COVID_Riesgo[COVID_Riesgo$DIABETES==1 & COVID_Riesgo$INTUBADO==1, "PACIENTE"]
EPOC <- (COVID_Riesgo[COVID_Riesgo$EPOC==1 & COVID_Riesgo$INTUBADO==1, "PACIENTE"]
Asma <- (COVID_Riesgo[COVID_Riesgo$ASMA==1 & COVID_Riesgo$INTUBADO==1, "PACIENTE"]
Inmuno <- (COVID_Riesgo[COVID_Riesgo$INMUSUPR==1 & COVID_Riesgo$INTUBADO==1, "PACIENTE"]
Hiper <- (COVID_Riesgo[COVID_Riesgo$HIPERTENSION==1 & COVID_Riesgo$INTUBADO==1, "PACIENTE"]
Otras <- (COVID_Riesgo[COVID_Riesgo$OTRA_COM==1 & COVID_Riesgo$INTUBADO==1, "PACIENTE"]
Cardio <- (COVID_Riesgo[COVID_Riesgo$CARDIOVASCULAR==1 & COVID_Riesgo$INTUBADO==1, "PACIENTE"]
Obes <- (COVID_Riesgo[COVID_Riesgo$OBESIDAD==1 & COVID_Riesgo$INTUBADO==1, "PACIENTE"]
Renal <- (COVID_Riesgo[COVID_Riesgo$RENAL_CRONICA==1 & COVID_Riesgo$INTUBADO==1, "PACIENTE"]
Tab <- (COVID_Riesgo[COVID_Riesgo$TABAQUISMO==1 & COVID_Riesgo$INTUBADO==1, "PACIENTE"]

all<-c(Emb,Diab, EPOC,Asma,Inmuno, Hiper,Otras, Cardio,Obes,Renal, Tab)

Intubados <- COVID_Riesgo[COVID_Riesgo$INTUBADO==1,"PACIENTE"]
Sanos <- setdiff(Hospitalizados,unique(all))

Sanos_Up<-setdiff((COVID_Riesgo[COVID_Riesgo$INTUBADO==1 & COVID_Riesgo$EDAD>=60,"PACIENTE"]), unique(all))

Sanos_Down<-setdiff((COVID_Riesgo[COVID_Riesgo$INTUBADO==1 & COVID_Riesgo$EDAD<60,"PACIENTE"]), unique(all))


listInput <- list(Embarazo = c(Emb), Diabetes = c(Diab), Cardiovasculares = c(Cardio), EPOC=c(EPOC),Asma=c(Asma), Inmunosupresion=c(Inmuno), Hipertension=c(Hiper),Otras=c(Otras),Obesidad=c(Obes), Insuficiencia_Renal=c(Renal),Tabaquismo=c(Tab),SP_May60=c(Sanos_Up),SP_Men60=c(Sanos_Down))

upset(fromList(listInput),nsets = 13,
      sets.bar.color=colorRampPalette(c("red", "blue"))(13) ,
      matrix.color="Darkgreen",
      main.bar.color = "Steelblue",
      mainbar.y.label = "Frecuencias\nabsolutas",
      text.scale=c(1.5,1.2, 1.2, 1.2, 1.2, 0.8),
      sets.x.label = "Número de casos",
      nintersects = 30,
      mb.ratio = c(0.60, 0.40), order.by = "freq")
grid.text("Intubaciones COVID-19 México \nCo-ocurrencia con otras condiciones\nmédicas,30 de mayo",x = 0.73, y=0.90, gp=gpar(fontsize=13,fontface="bold"))
