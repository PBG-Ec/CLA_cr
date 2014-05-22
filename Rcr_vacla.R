#########################################################################################
#Variation in Milk Conjugated Linoleic Acid (CLA) related to Seasonal Variation and 
#Production Asociated factors 
#Costa Rica National representative-data 2007
#Philippe Belmont Guerrón 
#Rafael Monge

#Base de Grasas _ produc lechera_an_CR 
read.table("fdto1.csv", sep=",",h=T)->fd

###################################################################################################################
#Fatty acids_Dry season_Rainy (epoca=0_invierno 1_verano) season_P value / mean & 95 percentil   / Numero de casos 
###################################################################################################################

as.numeric(c(t.test(fd[which(fd$epoca==0),1])[5],t.test(fd[which(fd$epoca==0),1])$conf.int[1:2]))->b
i=3
list()-> rs
for (i in 3:62) as.numeric(c(t.test(fd[which(fd$epoca==0),i])[5],t.test(fd[which(fd$epoca==0),i])$conf.int[1:2]))->rs[[i]]
for (i in 3:62) cbind(b,rs[[i]])->b
t(b[,-1])->b
cbind(rep(1,60),names(fd[,3:62]),b)->b
bb<-b

as.numeric(c(t.test(fd[which(fd$epoca==1),1])[5],t.test(fd[which(fd$epoca==1),1])$conf.int[1:2]))->b
i=3
list()-> rs
for (i in 3:62) as.numeric(c(t.test(fd[which(fd$epoca==1),i])[5],t.test(fd[which(fd$epoca==1),i])$conf.int[1:2]))->rs[[i]]
for (i in 3:62) cbind(b,rs[[i]])->b
t(b[,-1])->b
cbind(rep(0,60),names(fd[,3:62]),b)->b

cbind(b,bb)->b
as.data.frame(b)->b
names(b)<-c("epoca","AG","Mean","CI low","CI hi","epoca","AG","Mean","CI low","CI hi")
#P value entre epoca  
i=3
t(t.test(fd[which(fd$epoca==1),i], fd[which(fd$epoca==0),i], "t", 0, T, T, 0.95)[3])->cc
for (i in 4:62) rbind(cc,t(t.test(fd[which(fd$epoca==1),i], fd[which(fd$epoca==0),i],"t", 0, T, T, 0.95)[3]))->cc
# Casos Missing for (i in 3:62) print(length(which(is.na(fd[which(fd$epoca==0),i]))))
# Total de casos
cbind(b,cc)->b
write.table(b,file="FAvsSeason.txt")
#781 casos
#################################################################################
#Type of fatty vs. acid_Dry season_P value_Rainy season_P value / Numero de casos
#################################################################################
read.table("fincadato.txt",sep="\t",h=T)->fed
# : construir tipo de  
fed[which(fed$tpasto==2),"pex"]<-1
fed[which(fed$tpasto==1 | fed$tpasto==3 | fed$tpasto==4),"pex"]<-0
#cbind(fd,fed[match(fd$id,fed[,1]),])->fd
tolower(names(fd))->names(fd)
# exclusivo 2 Segun  pc:99:128,151 Otroalim: 136:154,156 ensil:157 Concentr:160  RTM:164 
for (i in c(99:128,151,129:134,136:154,156:157,160,163)) as.numeric(fd[,i])->fd[,i]
rowSums(fd[,c(99:128,151,136:154,156,157,160,164)],na.rm=T)->fd$rsum
rowSums(fd[,c(99:128,151,136:154,156,157,160)],na.rm=T)->fd$rsum
rowSums(fd[,c(136:154,156,157,160)],na.rm=T)->fd$rsum
rowSums(fd[,c(136:154,156,157)],na.rm=T)->fd$rsum
fd$pex2<-0;fd[which(fd$rsum==0),"pex2"]<-1
############################################################################
#_Partial 1_grazing_Exclusive 0_grazing__Partial  epoca=0_invierno 1_verano
############################################################################
#Invierno / parc graz
i=3
as.numeric(c(t.test(fd[which(fd$epoca==0 & fd$pex==0),i])[5],t.test(fd[which(fd$epoca==0 & fd$pex==0),i])$conf.int[1:2]))->b
list()-> rs
for (i in 4:62) as.numeric(c(t.test(fd[which(fd$epoca==0 & fd$pex==0),i])[5],t.test(fd[which(fd$epoca==0 & fd$pex==0),i])$conf.int[1:2]))->rs[[i]]
for (i in 4:62) cbind(b,rs[[i]])->b
t(b)->b
bb<-b
#Invierno / ex graz
i=3
as.numeric(c(t.test(fd[which(fd$epoca==0 & fd$pex==1),i])[5],t.test(fd[which(fd$epoca==0 & fd$pex==1),i])$conf.int[1:2]))->b
list()-> rs
for (i in 4:62) as.numeric(c(t.test(fd[which(fd$epoca==0 & fd$pex==1),i])[5],t.test(fd[which(fd$epoca==0 & fd$pex==1),i])$conf.int[1:2]))->rs[[i]]
for (i in 4:62) cbind(b,rs[[i]])->b
t(b)->b
cbind(rep(0,60),rep(1,60),names(fd[,3:62]),b)->b
bbb<-b

#Verano / parc graz
i=3
as.numeric(c(t.test(fd[which(fd$epoca==1 & fd$pex==0),i])[5],t.test(fd[which(fd$epoca==1 & fd$pex==0),i])$conf.int[1:2]))->b
list()-> rs
for (i in 4:62) as.numeric(c(t.test(fd[which(fd$epoca==1 & fd$pex==0),i])[5],t.test(fd[which(fd$epoca==1 & fd$pex==0),i])$conf.int[1:2]))->rs[[i]]
for (i in 4:62) cbind(b,rs[[i]])->b
t(b)->b
bbbb<-b

#Verano / excl graz
i=3
as.numeric(c(t.test(fd[which(fd$epoca==1 & fd$pex==1),i])[5],t.test(fd[which(fd$epoca==1 & fd$pex==1),i])$conf.int[1:2]))->b
list()-> rs
for (i in 4:62) as.numeric(c(t.test(fd[which(fd$epoca==1 & fd$pex==1),i])[5],t.test(fd[which(fd$epoca==1 & fd$pex==1),i])$conf.int[1:2]))->rs[[i]]
for (i in 4:62) cbind(b,rs[[i]])->b
t(b)->b
cbind(rep(1,60),rep(1,60),names(fd[,3:62]),b)->b



cbind(bbb,bb)->binv
cbind(b,bbbb)->bvera
as.data.frame(binv)->binv;as.data.frame(bvera)->bvera
names(bvera)<-c("epoca","Grztype","AG","Mean","CI low","CI hi","Mean","CI low","CI hi")
names(binv)<-c("epoca","Grztype","AG","Mean","CI low","CI hi","Mean","CI low","CI hi")

#P value entre tipo de  por epoca:  
#Invierno
i=3
t(t.test(fd[which(fd$epoca==0 & fd$pex==0),i], fd[which(fd$epoca==0 & fd$pex==1),i], "t", 0, F, T, 0.95)[3])->cc
for (i in 4:62) rbind(cc,t(t.test(fd[which(fd$epoca==0 & fd$pex==0),i], fd[which(fd$epoca==0 &  fd$pex==1),i], "t", 0, F, T, 0.95)[3]))->cc
cc->cinv
#Verano
i=3
t(t.test(fd[which(fd$epoca==1 & fd$pex==0),i], fd[which(fd$epoca==1 & fd$pex==1),i], "t", 0, F, T, 0.95)[3])->ccc
for (i in 4:62) rbind(ccc,t(t.test(fd[which(fd$epoca==1 & fd$pex==0),i], fd[which(fd$epoca==1 &  fd$pex==1),i], "t", 0, F, T, 0.95)[3]))->ccc
ccc->cvera
cbind(binv,cinv)->binv
cbind(bvera,cvera)->bvera
write.table(as.data.frame(binv),file="GrzFAvsSeasoninv.txt")
############################################################################
#_Pastoreo 2 PEX2 0_grazing_Exclusive 1_grazing__Partial  epoca=0_invierno 1_verano
############################################################################
#Invierno / parc graz
i=3
as.numeric(c(t.test(fd[which(fd$epoca==0 & fd$pex2==0),i])[5],t.test(fd[which(fd$epoca==0 & fd$pex2==0),i])$conf.int[1:2]))->b
list()-> rs
for (i in 4:62) as.numeric(c(t.test(fd[which(fd$epoca==0 & fd$pex2==0),i])[5],t.test(fd[which(fd$epoca==0 & fd$pex2==0),i])$conf.int[1:2]))->rs[[i]]
for (i in 4:62) cbind(b,rs[[i]])->b
t(b)->b
bb<-b
#Invierno / ex graz
i=3
as.numeric(c(t.test(fd[which(fd$epoca==0 & fd$pex2==1),i])[5],t.test(fd[which(fd$epoca==0 & fd$pex2==1),i])$conf.int[1:2]))->b
list()-> rs
for (i in 4:62) as.numeric(c(t.test(fd[which(fd$epoca==0 & fd$pex2==1),i])[5],t.test(fd[which(fd$epoca==0 & fd$pex2==1),i])$conf.int[1:2]))->rs[[i]]
for (i in 4:62) cbind(b,rs[[i]])->b
t(b)->b
cbind(rep(0,60),rep(1,60),names(fd[,3:62]),b)->b
bbb<-b

#Verano / parc graz
i=3
as.numeric(c(t.test(fd[which(fd$epoca==1 & fd$pex2==0),i])[5],t.test(fd[which(fd$epoca==1 & fd$pex2==0),i])$conf.int[1:2]))->b
list()-> rs
for (i in 4:62) as.numeric(c(t.test(fd[which(fd$epoca==1 & fd$pex2==0),i])[5],t.test(fd[which(fd$epoca==1 & fd$pex2==0),i])$conf.int[1:2]))->rs[[i]]
for (i in 4:62) cbind(b,rs[[i]])->b
t(b)->b
bbbb<-b

#Verano / excl graz
i=3
as.numeric(c(t.test(fd[which(fd$epoca==1 & fd$pex2==1),i])[5],t.test(fd[which(fd$epoca==1 & fd$pex2==1),i])$conf.int[1:2]))->b
list()-> rs
for (i in 4:62) as.numeric(c(t.test(fd[which(fd$epoca==1 & fd$pex2==1),i])[5],t.test(fd[which(fd$epoca==1 & fd$pex2==1),i])$conf.int[1:2]))->rs[[i]]
for (i in 4:62) cbind(b,rs[[i]])->b
t(b)->b
cbind(rep(1,60),rep(1,60),names(fd[,3:62]),b)->b

cbind(bbb,bb)->binv
cbind(b,bbbb)->bvera
as.data.frame(binv)->binv;as.data.frame(bvera)->bvera
names(bvera)<-c("epoca","Grztype","AG","Mean","CI low","CI hi","Mean","CI low","CI hi")
names(binv)<-c("epoca","Grztype","AG","Mean","CI low","CI hi","Mean","CI low","CI hi")

#P value entre tipo de  por epoca:  
#Invierno
i=3
t(t.test(fd[which(fd$epoca==0 & fd$pex2==0),i], fd[which(fd$epoca==0 & fd$pex2==1),i], "t", 0, F, T, 0.95)[3])->cc
for (i in 4:62) rbind(cc,t(t.test(fd[which(fd$epoca==0 & fd$pex2==0),i], fd[which(fd$epoca==0 &  fd$pex2==1),i], "t", 0, F, T, 0.95)[3]))->cc
cc->cinv
#Verano
i=3
t(t.test(fd[which(fd$epoca==1 & fd$pex2==0),i], fd[which(fd$epoca==1 & fd$pex2==1),i], "t", 0, F, T, 0.95)[3])->ccc
for (i in 4:62) rbind(ccc,t(t.test(fd[which(fd$epoca==1 & fd$pex2==0),i], fd[which(fd$epoca==1 &  fd$pex2==1),i], "t", 0, F, T, 0.95)[3]))->ccc
ccc->cvera
cbind(binv,cinv)->binv
cbind(bvera,cvera)->bvera
#################################################################################################################################
#_Partial 1_grazing_Exclusive 0_grazing__Partial  epoca=0_invierno 1_verano
#Characteristic_finca (sobre base total) Partial    / grazing_Exclusive / grazing_P value : 
# 14 HA_FINCA / 15 HA_PA_TOT / 16 HA_PA_PRO / 17 VA_PRO // 16HA_PA_PRO/17VA_PRO // 2 LAT+3 LOG / 23 LECHE_DIA 
#// 23/17 LECHE_DIA/VA_PRO // /21 REL_LC / 22 VA_SUP/ Raza Predom 49
fed$hapvac<-fed[,16]/fed[,17]
fed$lola<-as.numeric(fed[,2])+as.numeric(fed[,3])
fed$ledia<-as.numeric(fed[,23])+as.numeric(fed[,17])
fed[,24:43]/rowSums(fed[,24:43],na.rm=T)->a
names(a)<-paste("%_",names(a),sep="")
cbind(fed,a)->fed

cbind(fd,fed[match(fd$id,fed[,1]),])->fd

fd[-which(is.na(fd[,1])),]->fdd
write.table(fdd,file="fdd.txt",sep=",")->fdd

##################################################Partial Grazing
######
rs<-list()
as.numeric(c(t.test(fdd[which(fdd$pex==0),i])[5],t.test(fdd[which(fdd$pex==0),i])$conf.int[1:2]))->b
for (i in c(180,181,182,183,218,219,169,168,189,220,187,188,221:240)) as.numeric(c(t.test(fdd[which(fdd$pex==0),i])[5],t.test(fdd[which(fdd$pex==0),i])$conf.int[1:2]))->rs[[i]]
for (i in c(180,181,182,183,218,219,169,168,189,220,187,188,221:240)) cbind(b,rs[[i]])->b
t(b[,-1])->bpart
############Exclu Grazing
rs<-list()
as.numeric(c(t.test(fdd[which(fdd$pex==1),i])[5],t.test(fdd[which(fdd$pex==0),i])$conf.int[1:2]))->b
for (i in c(180,181,182,183,218,219,169,168,189,220,187,188,221:240)) as.numeric(c(t.test(fdd[which(fdd$pex==1),i])[5],t.test(fdd[which(fdd$pex==1),i])$conf.int[1:2]))->rs[[i]]
for (i in c(180,181,182,183,218,219,169,168,189,220,187,188,221:240)) cbind(b,rs[[i]])->b
t(b[,-1])->bexcl
##P value 
i=180
t.test(fdd[which(fdd$pex==0),i], fdd[which(fdd$pex==1),i], "t", 0, F, T, 0.95)[3]->d
for (i in c(181,182,183,218,219,169,168,189,220,187,188,221:240)) rbind(d,(t.test(fdd[which(fdd$pex==0),i], fdd[which(fdd$pex==1),i], "t", 0, F, T, 0.95)[3]))->d
d->dcar

cbind(as.data.frame(bexcl),as.data.frame(bpart),as.data.frame(dcar))->bexcl
cbind(c("Area de la finca (ha)","Area total de  (ha)","Area de  en producci?n (ha)","Vacas en producci?n (n)",
"Area de  por vaca (ha)","Ubicaci?n de la finca (Longitud +Latitud)","Longitud","Latitud","Leche/ d?a (L)","Leche/vaca/d?a (L)",
"Relaci?n LC","VA-Sup",names(fdd[,221:240])),bexcl)->bexcl


####tipo de razas  "p_holst"   "p_jersey"  "p_pardo"   "p_tauros"  "p_indicus"
i=211
rs<-list()
as.numeric(c(t.test(fdd[which(fdd$pex==0),i])[5],t.test(fdd[which(fdd$pex==0),i])$conf.int[1:2]))->b
for (i in c(211:215)) as.numeric(c(t.test(fdd[which(fdd$pex==0),i])[5],t.test(fdd[which(fdd$pex==0),i])$conf.int[1:2]))->rs[[i]]
for (i in c(211:215)) cbind(b,rs[[i]])->b
t(b[,-1])->bpart
############Exclu Grazing
rs<-list()
as.numeric(c(t.test(fdd[which(fdd$pex==1),i])[5],t.test(fdd[which(fdd$pex==0),i])$conf.int[1:2]))->b
for (i in c(211:215)) as.numeric(c(t.test(fdd[which(fdd$pex==1),i])[5],t.test(fdd[which(fdd$pex==1),i])$conf.int[1:2]))->rs[[i]]
for (i in c(211:215)) cbind(b,rs[[i]])->b
t(b[,-1])->bexcl
##P value 
i=211
t.test(fdd[which(fdd$pex==0),i], fdd[which(fdd$pex==1),i], "t", 0, F, T, 0.95)[3]->d
for (i in c(212:215)) rbind(d,(t.test(fdd[which(fdd$pex==0),i], fdd[which(fdd$pex==1),i], "t", 0, F, T, 0.95)[3]))->d
d->dcar
cbind(as.data.frame(bexcl),as.data.frame(bpart),as.data.frame(dcar))->bexcl
cbind(names(fdd[,211:215]),bexcl)->bexcl


##################################################################################
#########################Model  v18_2_9c_11t_cla rep #############################
##################################################################################
read.table("fdd.txt",sep=",",h=T)->fdd; fdd[,-1]->fdd

###Cuadro de fincas : tipo de pastoreo : 
#tipo de pasto (68;96) /de corta (99;150):  0:caliente seco 1:caliente  humedo 2:caliente templado humedo 3:frio humedo
fdd$stpst0<-rowSums(fdd[,c(68,79,96,91)],na.rm=T)
fdd$stpst1<-rowSums(fdd[,c(69,71,72,73,70,76,74,77,78,81,84,83,86,87,88,90,93,94)],na.rm=T)
fdd$stpst2<-rowSums(fdd[,c(75,82,95,89)],na.rm=T)
fdd$stpst3<-rowSums(fdd[,c(80,85,92)],na.rm=T)
fdd$stpcrt0<- rowSums(fdd[,c(100,104,121,126)],na.rm=T)
fdd$stpcrt1<- rowSums(fdd[,c(103,102,105,106,108,110,111,114,117,116,119,118,122,123,124,125,127)],na.rm=T)
fdd$stpcrt2<-	rowSums(fdd[,c(107,109,112,115)],na.rm=T)
fdd$stpcrt3<-	rowSums(fdd[,c(99,101,113,120,150)],na.rm=T)
 #Son 241:248
for (i in 1:1650){
  if (length(which(fdd[i,c(241:244)]==max(fdd[i,c(241:244)],na.rm=T)))>1) {
    fdd$stpstm[i]<-5
    } else {
      fdd$stpstm[i]<-as.numeric(which(fdd[i,c(241:244)]==max(fdd[i,c(241:244)],na.rm=T)))
    }
}

for (i in 1:1650){
  if (length(which(fdd[i,c(245:248)]==max(fdd[i,c(245:248)],na.rm=T)))>1) {
    fdd$stpcrtm[i]<-5
  } else {
    fdd$stpcrtm[i]<-as.numeric(which(fdd[i,c(245:248)]==max(fdd[i,c(245:248)],na.rm=T)))
  }
}


rs<-c(100*table(fdd[,68], fdd$pex)[2]/sum(table(fdd[,68], fdd$pex)),
      100*table(fdd[,68], fdd$pex)[4]/sum(table(fdd[,68], fdd$pex)))
rbind(rs,rs)->rs
rownames(rs)<-NULL
as.data.frame(rs)->rs
#names(rs)<-c("Tipo de Alimento","%_Past.excl","%_Past.semiexcl","pvalue")
#tipo_otro_aliment t_pulpas harina heno descr__ensilaje code_tipo_ensilaje code_tipo_concentrado
#134 142 144 145 157 158 160
i=68
list()->a
for (i in  c(68:96,99:133,135:141,143:143,146:156,159,161:163)) {
  #fdd[which(is.na(fdd[,i])),i]<-0
  rs[i-67,]<-c(100*table(fdd[,i], fdd$pex)[2]/sum(table(fdd[,i], fdd$pex)),
               100*table(fdd[,i], fdd$pex)[4]/sum(table(fdd[,i], fdd$pex)))
}

rs[-which(is.na(rs[,1])),]->rs
cbind(names(fdd)[c(68:96,99:133,135:141,143:143,146:156,159,161:163)],rs)->rs

##P value 
i=68
t.test(fdd[which(fdd$pex==0),i], fdd[which(fdd$pex==1),i], "t", 0, F, T, 0.95)[3]->d
for (i in c(69:96,99:133,135:141,143:143,146:156,159,161:163)) rbind(d,(t.test(fdd[which(fdd$pex==0),i], fdd[which(fdd$pex==1),i], "t", 0, F, T, 0.95)[3]))->d

cbind(rs,d)->rs

names(rs)<-c("tipo_alim","%_parcial","%_pexclu","pvalue")





##AG trans ==~ 4% AG:
#18:1 trans-11 (1.5%) ALC cis-9_trans-11 (0,5%) 18:2 trans-9 trans-12 cis-9_trans-13 trans-11, cis-15 (0,1_0,2%) 
#ALnC cis-9 (ac linoleic) trans-11 cis-15 (0,05_0,1%) (Jensen, 2002, Vlae minck et al., 2006, Plourde et al., 2007a, Ferlay et al., 2008)

#Intrinsect effect:
#Race: amplitud limited effect (Palmquist et al., 1993)
#Frisona, Holstein y Montbeliardes >> Jersey, Brown Swiss, y otras (DePeters 1995; Lawless 1999; White 2001)
#No diferencia raza :(Kelsey 2003; Palladino 2010)

#Gestation stade: effect++

#Extrinsec effect: Alimentation: rapid effect/ reversible (Shingfield 2008)
#ensilado de alfalfa y ma?z ens. algodon-> .41 .44% 
#Past->1.21% // past.5_.8%+conc_cebad->1.6_1.9% (Dhiman 1999; White 2001; Kay 2004)
# CLA_p_fresc > p_cort1 > p_cort2 > Conc_maiz (Dewhurst 2001; Elgersman 2003)
#Error Variable Estabulado en doble 69
fd[,-69]->fd
#error en tipos_pasto: 
rowSums(fd[,c(68:96)])->fd$tipos_pasto

names(fd)[which(names(fd)=="v18_2_9c_11t_cla")]<-"cla"
names(fd)[which(names(fd)=="raza.predominante")]<-"rzprd"

cut(fd$lat, breaks=c(quantile(fd$lat, probs = seq(0, 1, by = 0.20),na.rm=T)),labels=c("0-20","20-40","40-60","60-80","80-100"))->fd$ltqtil
cut(fd$long, breaks=c(quantile(fd$long, probs = seq(0, 1, by = 0.20),na.rm=T)),labels=c("0-20","20-40","40-60","60-80","80-100"))->fd$lgqtil
fd[-which(is.na(fd[,1])),]->fdd

#Complete model : cla ~ pex + (pex2) + epoca + hapvac + z_vida_0 + raza.predominante +    #+ s(long,lat)
# Tipo de pasto : ?
# Tipo de pasto de corta : ?
# Concentrados : ?
# Otros alimentos
# Minerales
# Fertilizantes


glm(formula=  cla ~ pex , family=identity)
  
#+s(lat,long)  (GAM= generalized additive model) 


#Modelo 1
#CLA  = pex +?rea_qtil+s(lat,long) +hapvaqtil + VA_SUP_qtil + rzprd
#Modelo 2
#CLA=pex+?rea_qtil+s(lat,long) + hapva_qtil+ VA_SUP_qtil+raza predominante + tipo de pasto 
# + tipo de pasto de corta +concentrado +otros alimentos +minerales + fertilizantes

# Razas   vs CLA
i=211
rs<-list()
for (i in c(211:215)) print(as.numeric(c(t.test(fdd[which(fdd[,i]>0),52])[5],t.test(fdd[which(fdd[,i]>0),52])$conf.int[1:2])))->rs[[i]]


library(ggplot2)
p <- ggplot(fdd[which(fdd$rzprd!=3),], aes(factor(rzprd), cla))
p + geom_boxplot(notch = TRUE)

p <- ggplot(fdd[which(fdd$rzprd!=3),], aes(factor(stpcrtm), cla))
p + geom_boxplot(notch = TRUE)

p <- ggplot(fdd[which(fdd$rzprd!=3),], aes(factor(stpstm), cla))
p + geom_boxplot(notch = TRUE)


as.factor(fdd$pex)->fdd$pex
as.factor(fdd$rzprd)->fdd$rzprd
as.factor(fdd$epoca)->fdd$epoca
as.factor(fdd$stpcrtm)->fdd$stpcrtm
as.factor(fdd$stpstm)->fdd$stpstm

as.logical(fdd$usa_otros_alimentos)->fdd$usa_otros_alimentos
as.logical(fdd$usa_concentrado)->fdd$usa_concentrado
as.logical(fdd$usa_ensilaje)->fdd$usa_ensilaje
as.logical(fdd$usa_fertilizante)->fdd$usa_fertilizante
as.logical(fdd$usa_minerales)->fdd$usa_minerales

fdd[which(fdd[,249]==1),"pt1"]<-1
fdd[which(fdd[,249]==2),"pt2"]<-1
fdd[which(fdd[,249]==3),"pt3"]<-1
fdd[which(fdd[,249]==4),"pt4"]<-1
fdd[which(fdd[,249]==5),"pt5"]<-1

fdd[which(fdd[,249]==1),"pc1"]<-1
fdd[which(fdd[,249]==2),"pc2"]<-1
fdd[which(fdd[,249]==3),"pc3"]<-1
fdd[which(fdd[,249]==4),"pc4"]<-1
fdd[which(fdd[,249]==5),"pc5"]<-1

for (i in 251:260) fdd[which(is.na(fdd[,i])),i]<-0

#write.table(fdd,file="fddf.txt")
read.table("fddf.txt",h=T,sep=" ")->fdd

# Intensidad de la produccion :  "vatotal" "hapvac"
# Tipo Pasto de corte : stpcrtm
# tipo Pasto stpstm
# usa_concentrado 159
# usa_ensilaje 156
# usa_minerales 161
# usa_fertilizante 162
# 137  c_mani
# 139  c_soya
# 148 palma_africana
# 155 grasa
# 128 usa_otros_alimentos

#Solo efecto de alimento 
fmg<-as.formula(cla~pex+stpcrtm+stpstm+usa_otros_alimentos+usa_concentrado+usa_ensilaje+usa_fertilizante+usa_minerales)
lmg<-lm(fmg,data=fdd[which(fdd$epoca==0),])

step(lmg, direction = c("both"),step=1000)
lmstpf<-lm(formula = cla ~ pex + stpstm + usa_otros_alimentos + usa_concentrado + 
       usa_ensilaje + usa_fertilizante + usa_minerales, data = fdd[which(fdd$epoca == 0), ])
summary(lmstpf)
fmg<-as.formula(cla~pex+pt2+pt3+pt4+pt5+pc2+pc3+pc4+pc5+stpstm+usa_otros_alimentos+usa_concentrado+usa_ensilaje+usa_fertilizante+usa_minerales-1)
lmg<-lm(fmg,data=fdd[which(fdd$epoca==0),])

step(lmg, direction = c("both"),step=1000)

lmstpf<-lm(formula = cla ~ pex + pt2 + pt3 + pt4 + usa_otros_alimentos + 
     usa_concentrado + usa_ensilaje + usa_fertilizante + usa_minerales -1, 
   data = fdd[which(fdd$epoca == 0), ])
summary(lmstpf)


#Modelo total +hapvac+rzprd+
fmt<-as.formula(cla~hapvac+rzprd+long + lat +pex+pt1+pt2+pt3+pt4+pt5+pc1+pc2+pc3+pc4+pc5+stpstm+usa_otros_alimentos+usa_concentrado+usa_ensilaje+usa_fertilizante+usa_minerales-1)
lmt<-lm(fmt,data=fdd[which(fdd$epoca==0),])

step(lmt, direction = c("both"),step=1000)

lmtf<-lm(formula = cla ~ hapvac + rzprd + pex  + pt3 + pt4 + usa_otros_alimentos + usa_concentrado + usa_ensilaje + usa_minerales -1, data = fdd[which(fdd$epoca == 0),])
summary(lmtf)


fmtf<-as.formula(cla ~ hapvac+ s(long,lat) + rzprd + pex + pt4 + pt5 + usa_otros_alimentos + usa_concentrado + usa_ensilaje + usa_minerales)
library(mgcv)

gam(fmtf, family=gaussian(link = "identity"),data=fdd[which(fdd$epoca == 0),])->gmf
summary(gmf)


lm(as.formula(cla~p_holst+p_jersey+p_tauros+p_indicus), data=fdd)->lmv
summary(lmv)
lm(as.formula(cla~rzprd+epoca), data=fdd)->lmv
summary(lmv)

lm(as.formula(cla~rzprd+pex+epoca), data=fdd)->lmv2
summary(lmv)

lm(as.formula(cla~rzprd+pex), data=fdd)->lmv
summary(lmv)



library(mgcv)

gam(as.formula(cla~ pex + s(long,lat)), family=gaussian(link = "identity"),data=fdd)->lmv3
summary(lmv3)

gam(as.formula(cla~ pex +rzprd + s(long,lat)), family=gaussian(link = "identity"),data=fdd)->lmv3
summary(lmv3)
vis.gam(lmv3,theta=30,ticktype="detailed")

gam(as.formula(cla~ pex +rzprd +epoca + s(long,lat)), family=gaussian(link = "identity"),data=fdd)->lmv3
summary(lmv3)


lm(as.formula(cla~pex+long+lat), data=fdd)->lmv
summary(lmv)


lm(as.formula(cla~rzprd), data=fdd)->lmv2
summary(lmv2)

formula=TotSym ~ s(X,Y)

glm(as.formula(cla~rzprd), family="poisson",data=fdd)->lmv2
summary(lmv2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
####Cuadros Ultimos 
#Rzprd 1 Hol 2 jer 3 pardo 4 Indicus 5 Tauro 6 Mixto
# Caracteristicas de las fincas 
read.table("fddf.txt",h=T,sep=" ")->fdd
fdd[which(fdd$epoca==1),]->fr
fr[which (fr$rzprd==3),"rzprd"]<-5
fr[-which(is.na(fr$long) | is.na(fr$pastoreo)| is.na(fr$rzprd)),]->fr
fr$ldvapr<-fr$leche_dia/fr$va_pro
#Recalculo de procentajes 
fr[which(fr$rzprd==1),"xhol"]<-rowSums(fr[which(fr$rzprd==1),c("hostein","holstein_rojo")],na.rm=T)/rowSums(fr[which(fr$rzprd==1),c(190:209)])
fr[which(fr$rzprd==2),"xjer"]	<-fr[which(fr$rzprd==2),"jersey"]/rowSums(fr[which(fr$rzprd==2),c(190:209)])
fr[which(fr$rzprd==4),"xindic"]	<-rowSums(fr[which(fr$rzprd==4),c("brahman","gyr","nelore","cebu_jersey","cebu_pardo","o_cebu")],na.rm=T)/rowSums(fr[which(fr$rzprd==4),c(190:209)])
fr[which(fr$rzprd==5),"xtaur"]	<-rowSums(fr[which(fr$rzprd==5),c("pardo","guernsey","criollo","montbeliard","holstein_jersey","o_europea"	)],na.rm=T)/rowSums(fr[which(fr$rzprd==5),c(190:209)])
fr[which(fr$rzprd==6),"xmixt"]	<-rowSums(fr[which(fr$rzprd==6),c("simental","ayrshire","senepol","sahiwal"	)],na.rm=T)/rowSums(fr[which(fr$rzprd==6),c(190:209)])
# Area-finca  "ha_finca" __Area_tprod  "ha_pa_pro"__V_prod "va_pro"__Long "long"__Lat "lat"__Leche/dia "leche_dia"
#Leche/vaca/d?a (L) "ldvapr" #__Relleche:concentrado "rel_lc" 
#Rzprd 1Holstein:  2 Jersey 4 Indicus 5 Taurus 6 Mixta
######################################################Variables de fincas
######
match(c("ha_finca","ha_pa_pro","va_pro","long","lat","leche_dia","ldvapr",
        "rel_lc","xhol","xjer","xindic","xtaur","xmixt"),names(fr)) -> vv
##################################################Partial Grazing
i=180
rs<-list()
as.numeric(c(t.test(fr[which(fr$pex==0),i])[5],t.test(fr[which(fr$pex==0),i])$conf.int[1:2]))->b
for (i in vv) as.numeric(c(t.test(fr[which(fr$pex==0),i])[5],t.test(fr[which(fr$pex==0),i])$conf.int[1:2]))->rs[[i]]
for (i in vv) cbind(b,rs[[i]])->b
t(b[,-1])->bpart
############Exclu Grazing
i=180
rs<-list()
as.numeric(c(t.test(fr[which(fr$pex==1),i])[5],t.test(fr[which(fr$pex==1),i])$conf.int[1:2]))->b
for (i in vv) as.numeric(c(t.test(fr[which(fr$pex==1),i])[5],t.test(fr[which(fr$pex==1),i])$conf.int[1:2]))->rs[[i]]
for (i in vv) cbind(b,rs[[i]])->b
t(b[,-1])->bexcl

##P value 
i=180
t.test(fr[which(fr$pex==0),i], fr[which(fr$pex==1),i], "t", 0, F, T, 0.95)[3]->d
for (i in vv[-1]) rbind(d,(t.test(fr[which(fr$pex==0),i], fr[which(fr$pex==1),i], "t", 0, F, T, 0.95)[3]))->d
d->dcar

cbind(as.data.frame(bexcl),as.data.frame(bpart),as.data.frame(dcar))->bexcl
cbind(c("Area de la finca (ha)","Area total de pastoreo (ha)","Vacas en producci?n (n)",
        "Longitud (grados O)","Latitud (grados N)","Leche/ d?a (L)","Leche/vaca/d?a (L)",
        "Relaci?n leche:concentrado","%hol","%jer","%indic","%taur","%mixt"),bexcl)->bexcl
names(bexcl)<-c("variable","mean","CI95-","CI95+","mean","CI95-","CI95+","pvalue")
######
######################################################Variables de Acido Grasos
######
3:62 -> vv
##################################################Partial Grazing
i=3
rs<-list()
as.numeric(c(t.test(fr[which(fr$pex==0),i])[5],t.test(fr[which(fr$pex==0),i])$conf.int[1:2]))->b
for (i in vv) as.numeric(c(t.test(fr[which(fr$pex==0),i])[5],t.test(fr[which(fr$pex==0),i])$conf.int[1:2]))->rs[[i]]
for (i in vv) cbind(b,rs[[i]])->b
t(b[,-1])->bpart
############Exclu Grazing
i=3
rs<-list()
as.numeric(c(t.test(fr[which(fr$pex==1),i])[5],t.test(fr[which(fr$pex==1),i])$conf.int[1:2]))->b
for (i in vv) as.numeric(c(t.test(fr[which(fr$pex==1),i])[5],t.test(fr[which(fr$pex==1),i])$conf.int[1:2]))->rs[[i]]
for (i in vv) cbind(b,rs[[i]])->b
t(b[,-1])->bexcl

##P value 
i=3
t.test(fr[which(fr$pex==0),i], fr[which(fr$pex==1),i], "t", 0, F, T, 0.95)[3]->d
for (i in vv[-1]) rbind(d,(t.test(fr[which(fr$pex==0),i], fr[which(fr$pex==1),i], "t", 0, F, T, 0.95)[3]))->d
d->dcar

cbind(as.data.frame(bexcl),as.data.frame(bpart),as.data.frame(dcar))->bexcl
cbind(names(fr)[3:62],bexcl)->bexcl
names(bexcl)<-c("variable","mean","CI95-","CI95+","mean","CI95-","CI95+","pvalue")
######
######################################################Mixt Model 

# Caracteristicas de las fincas 
read.table("fddf.txt",h=T,sep=" ")->fdd
fdd[which(fdd$epoca==1),]->fr
# y1 CLA dif  CLA verano -CLA invierno epoca=0_invierno 1_verano
fr$y1<-fdd[which(fdd$epoca==1),"cla"]-fdd[which(fdd$epoca==0),"cla"]
# y2 CLA promedio inv + veran
fr$y2<- (fdd[which(fdd$epoca==1),"cla"]+fdd[which(fdd$epoca==0),"cla"])/2
fr[which (fr$rzprd==3),"rzprd"]<-5
fr[-which(is.na(fr$long) | is.na(fr$pastoreo)| is.na(fr$rzprd)),]->fr
fr$ldvapr<-fr$leche_dia/fr$va_pro
as.factor(fr$rzprd)->fr$rzprd
as.factor(fr$pex)->fr$pex

# xi: grazing, raza y localizacion (lat?)
lm(as.formula(y2~pex+lat+rzprd-1), data=fr)->lmv
summary(lmv)
lm(as.formula(y1~pex+lat+rzprd-1), data=fr)->lmv
summary(lmv)

#Mix effect 
library(lme4)
#y2 Promedio de CLA
#pex eff pval rzprd =
lmecla = lmer(y2~pex+lat+rzprd-1 + (1|pex/rzprd),data = fr)
summary(lmecla)
#rzprd eff pval within pex =
lmecla = lmer(y2~pex+lat+rzprd-1 + (1|rzprd/pex),data = fr)
summary(lmecla)
#y1 Diff CLAinv CLAvera

#pex eff pval rzprd =
lmeclad = lmer(y1~pex+lat+rzprd-1 + (1|pex/rzprd),data = fr)
summary(lmeclad)
#rzprd eff pval within pex =
lmeclad = lmer(y1~pex+lat+rzprd-1 + (1|rzprd/pex),data = fr)
summary(lmeclad)
