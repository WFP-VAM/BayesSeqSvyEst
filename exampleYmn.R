##################
### Data Prep  ###
##################
library(xlsx)
library(survey)
library(plyr)

#mvam Suvey- data we will use to update baseline model
ymnRspDF <- data.table::fread('C:/Users/gaurav.singhal/Desktop/ymnSeqMdl/mVAMYmnObs.csv',sep=';',na.strings='')

#DHS Survey - demographic data to use for posterior simulation
dhsDF <- data.table::fread('C:/Users/gaurav.singhal/Desktop/ymnSeqMdl/DHS2013.csv',sep=';',na.strings='')

#EFSNA face-t0-face survey - data to fit baseline model
efsnaDF <- data.table::fread('C:/Users/gaurav.singhal/Desktop/ymnSeqMdl/ymnEFSNA2016.csv',sep=';',na.strings='')

#Population Data
ymnPopDF <- read.xlsx('C:/Users/gaurav.singhal/Desktop/ymnSeqMdl/cso_2017_population_projection_sexage_disagreggated.xlsx',
                      sheetIndex=1)

#Create table for IPC Phase
svyYmn <- svydesign(ids=~Cluster_No+ID,
                    strata=~ADM1_NAME+NULL,
                    weights=~weight,
                    pps='brewer',
                    data=efsnaDF,
                    nest=TRUE)

ymn.FCG.2016 <- prop.table(svytable(~ADM1_NAME+FCS_Cat,svyYmn),1)
colnames(ymn.FCG.2016) <- c('2016.Severe','2016.Moderate','2016.OK')
ymn.FCG.2016 <- rbind(ymn.FCG.2016,ymn.FCG.2016[c("Hajjah","Hadramaut"),])
rownames(ymn.FCG.2016)[dim(ymn.FCG.2016)[1]-1] <- "Sa'ada"
rownames(ymn.FCG.2016)[dim(ymn.FCG.2016)[1]] <- "Al Maharah"
ymn.FCG.2016 <- as.data.frame(ymn.FCG.2016)

ymn.FCG.2016$IPCclass <- 'Emergency'
stressed <- c('Hadramaut','Al Maharah')
ymn.FCG.2016$IPCclass[rownames(ymn.FCG.2016) %in% stressed] <- 'Stressed'
crisis <- c('Al Jawf','Marib','Amran','Amanat Al Asimah','Al Mahwit','Al Hudaydah','Dhamar','Raymah','Ibb')
ymn.FCG.2016$IPCclass[rownames(ymn.FCG.2016) %in% crisis] <- 'Crisis'
ymn.FCG.2016$ADM1_NAME <- rownames(ymn.FCG.2016)
rm(svyYmn,stressed,crisis)

#Filter mVAM dataset for latest questionnaire and Merge
colsmVAM <- c('ObsID','RspID','SvyID','SelectWt','CmbAdjWt','ADM1_NAME','ADM2_NAME',
              'rCSI','FCS','FCG','FoodInsecure','isUrban','isIDP',
              'HHSizeGrp','HoHSex_is_F','HoHEdu','H2OSrc','FuelSrc','HH_has_Elctrc','Toilet_is_Flush')

mvamDF <- as.data.frame(ymnRspDF[ymnRspDF$SvyID>=181,])
mvamDF <- mvamDF[,colSums(is.na(mvamDF))<0.9*nrow(mvamDF)]
mvamDF <- mvamDF[complete.cases(mvamDF[,colsmVAM]),]
mvamDF <- mvamDF[,append(colsmVAM,setdiff(colnames(mvamDF),colsmVAM))]
mvamDF <- merge(mvamDF,ymn.FCG.2016,all.x=TRUE,by='ADM1_NAME')
mvamDF$FreeResponse <- NULL
mvamDF$FreeResponseEng <- NULL

#Filter DHS dataset for complete.cases and merge
colsDHS <- c('HHID','ADM1_NAME','wgt','HHS_Class','HHS_Score','isUrban',
             'HHSizeGrp','HoHSex_is_F','HoHEdu','H2OSrc','FuelSrc','HH_has_Elctrc','Toilet_is_Flush')
dmgDF <- as.data.frame(dhsDF)
dmgDF <- dmgDF[complete.cases(dmgDF[,colsDHS]),]
dmgDF <- dmgDF[,append(colsDHS,setdiff(colnames(dhsDF),colsDHS))]
dmgDF <- merge(dmgDF,ymn.FCG.2016,all.x=TRUE,by='ADM1_NAME')

#Filter EFSNA df for complete.cases and merge
colsEFSNA <- c('ID','ADM1_NAME','wgt','isUrban','FCS','isIDP',
               'HHSizeGrp','HoHSex_is_F','HoHEdu','H2OSrc','FuelSrc','HH_has_Elctrc','Toilet_is_Flush')
efsnaDF <- as.data.frame(efsnaDF)
baseDF <- efsnaDF[complete.cases(efsnaDF[,colsEFSNA]),]
baseDF <- baseDF[,colsEFSNA]
baseDF <- merge(baseDF,ymn.FCG.2016,all.x=TRUE,by='ADM1_NAME')
baseDF$FCS[baseDF$FCS<12] <- 12
baseDF$logFCS.2016 <- log(baseDF$FCS)

#add rows for Sa'ada and Al Maharah as they are missing the original baseline data
extDF <- baseDF[baseDF$ADM1_NAME %in% c('Hajjah','Hadramaut'),]
extDF$ADM1_NAME <- revalue(extDF$ADM1_NAME,c('Hajjah'="Sa'ada",'Hadramaut'='Al Maharah'))
baseDF <- rbind(baseDF,extDF)
baseDF$ADM1_NAME <- as.character(baseDF$ADM1_NAME)
rm(extDF)

#################################################
### Create initial baseline and offset models ###
#################################################

##(1) fit initial baseline model on EFSNA data
#(A) first fit lme model random effect as prior for rstanarm model
base.mdl <- lmer(log(FCS) ~ HoHSex_is_F+HH_has_Elctrc+Toilet_is_Flush+isUrban+
                   (1|HHSizeGrp)+(1|HoHEdu)+(1|H2OSrc)+(1|FuelSrc)+(1|IPCclass/ADM1_NAME),
                 data=baseDF,weights=wgt)

#(B) fit rstanarm with lme prior
base.mdl.stan <- stan_lmer(log(FCS) ~  HoHSex_is_F+HH_has_Elctrc+Toilet_is_Flush+isUrban+
                             (1|HHSizeGrp)+(1|HoHEdu)+(1|H2OSrc)+(1|FuelSrc)+(1|IPCclass/ADM1_NAME),
                           data=baseDF,weights=wgt,
                           iter=3000,chains=4,cores=4,thin=2,control=list(adapt_delta=.965),QR=TRUE,
                           prior_intercept = normal(offset.mdl@beta[1],sqrt(vcov(base.mdl)[1,1])*10))
rm(base.mdl)

##(2) fit offset model to correct for mode effect using the residuals of the baseline model simulated on mVAM data

#massage mvam data first because Sa'ada and Al Maharah governoates missing from baseline survey
mvamDF$ADM1_NAME <- as.factor(mvamDF$ADM1_NAME)
mvamDF$ADM1_NAME_ACTUAL <- mvamDF$ADM1_NAME
mvamDF$ADM1_NAME <- droplevels(revalue(mvamDF$ADM1_NAME,c("Sa'ada"="Hajjah","Al Maharah"="Hadramaut")))

#posterior simulations of baseline model on mVAM data
mvamDF$logFCS.prior <- rowMeans(t(posterior_predict(base.mdl.stan,newdata=mvamDF[,c(colsmVAM,'IPCclass')],draws=500)))
#compute residuals (we know actual FCS in the mVAM)
mvamDF$logFCS.diff <- with(mvamDF,log(FCS)-logFCS.prior)

#revert governorates
mvamDF$ADM1_NAME <- mvamDF$ADM1_NAME_ACTUAL
mvamDF$ADM1_NAME_ACTUAL <- NULL

#(A) first fit lme model as prior for rstanarm model 
offset.mdl <- lmer(logFCS.diff ~ HoHSex_is_F+HH_has_Elctrc+Toilet_is_Flush+isUrban+
                     (1|HHSizeGrp)+(1|HoHEdu)+(1|H2OSrc)+(1|FuelSrc)+(1|IPCclass/ADM1_NAME),
                   data=mvamDF[mvamDF$SvyID==181,],weights=SelectWt)
#NOTE: SvyID==181 is a filter as to use only 1 round of information
#(B) fit rstanarm with lme prior
offset.mdl.stan <- stan_lm er(logFCS.diff ~ HoHSex_is_F+HH_has_Elctrc+Toilet_is_Flush+isUrban+
                               (1|HHSizeGrp)+(1|HoHEdu)+(1|H2OSrc)+(1|FuelSrc)+(1|IPCclass/ADM1_NAME),
                             data=mvamDF[mvamDF$SvyID==181,],weights=SelectWt,
                             iter=3000,chains=4,cores=4,thin=2,control=list(adapt_delta=.965),QR=TRUE,
                             prior_intercept = normal(offset.mdl@beta[1],sqrt(vcov(offset.mdl)[1,1])*10))
rm(offset.mdl)


#################################################
### Estimate with Sequential Survey Estimator ###
#################################################

#source code
source('seqSvyEst.R')

##(1) Estimate models over time windows (using cross-sectional modelling approach)
seq.mdls <- seq.estimator.XS(mvamDF[,c(colsmVAM,'IPCclass','logFCS.prior')],base.mdl.stan,mvamDF$ObsDate)

##(2) Return estimates via simulating models on DHS data for each window
rslt.DHS <- mdl.simulator(seq.mdls,offset.mdl.stan,dmgDF[,c(colsDHS,'IPCclass')],'ADM1_NAME','wgt')


#################################################
### Summarize and Plot ###
#################################################

library(ggplot2)
library(reshape2)
library(scales)
library(stringr)
library(stringi)

#function to rowbind when column names are different
force_bind = function(df1, df2) {
  colnames(df2) = colnames(df1)
  bind_rows(df1, df2)
}

#(A)
ggplot(data=rslt.DHS$str.est %>% group_by(ADM1_NAME) %>% mutate(avgFCS=mean(FCSMean)),
       mapping=aes(x=End.Date,FCSMean,CI.Lo.FCS,CI.Hi.FCS,color=avgFCS,fill=avgFCS))+
  geom_line(aes(y = FCSMean))+
  geom_ribbon(aes(ymin=CI.Lo.FCS, ymax=CI.Hi.FCS),alpha=0.5,color=NA)+facet_wrap(~ADM1_NAME)+
  scale_fill_gradient(low='red',high='blue')+scale_colour_gradient(low='red',high='blue')+
  scale_x_date(breaks = pretty_breaks(14))+
  scale_y_continuous(minor_breaks = seq(80,80,5),breaks = seq(30,80,10))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major.y = element_line(colour="grey", size=0.5))


#(B)
ggplot(force_bind(cbind(rslt.DHS$str.est[,c(1,3,7,8,9)],'threshold'='poor'),
                  cbind(rslt.DHS$str.est[,c(1,3,10,11,12)],'threshold'='poorbrdr')),
       mapping=aes(x=End.Date,PctPoor,CI.Hi.Poor,CI.Lo.Poor,fill=threshold,color=threshold))+
  geom_line(aes(y = PctPoor))+
  geom_ribbon(aes(ymin=CI.Lo.Poor, ymax=CI.Hi.Poor),alpha=0.5,show.legend=FALSE,colour=NA)+facet_wrap(~ADM1_NAME)+
  scale_x_date(breaks = pretty_breaks(14))+
  scale_y_continuous(minor_breaks = seq(15,75,5)/100,breaks = seq(15,75,10)/100)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major.y = element_line(colour="grey", size=0.5))

#(C)
ggplot(data=rslt.Base$str.est %>% group_by(ADM1_NAME) %>% mutate(avgFCS=mean(FCSMean)),
       mapping=aes(x=End.Date,FCSMean,CI.Lo.FCS,CI.Hi.FCS,color=avgFCS,fill=avgFCS))+
  geom_line(aes(y = FCSMean))+
  geom_ribbon(aes(ymin=CI.Lo.FCS, ymax=CI.Hi.FCS),alpha=0.5,color=NA)+facet_wrap(~ADM1_NAME)+
  scale_fill_gradient(low='red',high='blue')+scale_colour_gradient(low='red',high='blue')+
  scale_x_date(breaks = pretty_breaks(14))+
  scale_y_continuous(minor_breaks = seq(80,80,5),breaks = seq(30,80,10))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid.major.y = element_line(colour="grey", size=0.5))


#(D)
ggplot(force_bind(cbind(rslt.Base$str.est[,c(1,3,7,8,9)],'threshold'='poor'),
                  cbind(rslt.Base$str.est[,c(1,3,10,11,12)],'threshold'='poorbrdr')),
       mapping=aes(x=End.Date,PctPoor,CI.Hi.Poor,CI.Lo.Poor,fill=threshold,color=threshold))+
  geom_line(aes(y = PctPoor))+
  geom_ribbon(aes(ymin=CI.Lo.Poor, ymax=CI.Hi.Poor),alpha=0.5,show.legend=FALSE,colour=NA)+facet_wrap(~ADM1_NAME)+
  scale_x_date(breaks = pretty_breaks(14))+
  scale_y_continuous(minor_breaks = seq(15,75,5)/100,breaks = seq(15,75,10)/100)+
  labs(title='% per FCS group by Week',y='Prevalence (from 0-1)',x='')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = 'top',
        panel.grid.major.y = element_line(colour="grey", size=0.5))

#(E) 3d contour plot of pdf
library(plotly)
df <- rslt.DHS$pdf.est %>% filter(ADM1_NAME=='Yemen') %>% select(-ADM1_NAME)
plot_ly(x=df$End.Date,y=c(20:60),
        z=t(as.matrix(df[,as.character(c(20:60))]))/10000,type='contour',autocontour=TRUE) %>%
  layout(xaxis=list(title='Date'),yaxis=list(title='FCS'),title='Evolution of FCS distribution from Apr-June')
rm(df)

#(F) Map of Prevalence
shp.DF <- fortify(ymnGDF) %>%
  left_join(data_frame(id=rownames(ymnGDF@data), name=ymnGDF@data$ADM1_NAME)) %>%
  select(-id) %>% rename(id=name)

dat.DF <- rslt.Base$str.est[rslt.Base$str.est$ADM1_NAME!='Yemen',c('ADM1_NAME','End.Date','PctPoor','NumPoor')]
colnames(dat.DF)[1] <- 'id'

breaks.prv <- seq(15,55,5)
breaks.lbl <- sprintf("%2.1f-%s", breaks.prv, percent(lead(breaks.prv/100))) %>%
  stri_replace_all_regex(c("^0.0", "-NA%"), c("0", "%"), vectorize_all=FALSE) %>% head(-1)
dat.DF <- dat.DF %>% mutate(`PoorFCS%`=cut(PctPoor,breaks.prv/100,breaks.lbl))

ggplot()+
  geom_polygon(data=shp.DF,aes(x=long,y=lat, group=group),fill='white',color='#7f7f7f',size=0.15)+
  geom_map(data=dat.DF, map=shp.DF,aes(map_id=id, fill=`PoorFCS%`),color="#7f7f7f", size=0.15)+
  scale_fill_manual(values=colorRampPalette(c("yellow", "red"))(length(breaks.lbl)))+
  guides(fill=guide_legend(override.aes=list(colour=NA)))+
  labs(title="% Poor FCS by Week")+
  facet_wrap(~End.Date)+
  theme(legend.position="top")
rm(breaks.lbl,breaks.prv)

#(G) Map of People in Need

breaks.num <- seq(0,1500000,100000)
breaks.lbl <- sprintf("%2.0f-%2.0fk", breaks.num/1000, lead(breaks.num)/1000) %>% head(-1)
dat.DF <- dat.DF %>% mutate(`NumPoorFCS`=cut(NumPoor,breaks.num,breaks.lbl))

ggplot()+
  geom_polygon(data=shp.DF,aes(x=long,y=lat, group=group),fill='white',color='#7f7f7f',size=0.15)+
  geom_map(data=dat.DF, map=shp.DF,aes(map_id=id, fill=`NumPoorFCS`),color="#7f7f7f", size=0.15)+
  scale_fill_manual(values=colorRampPalette(c("yellow", "red"))(length(breaks.lbl)))+
  guides(fill=guide_legend(override.aes=list(colour=NA)))+
  labs(title="Num Poor FCS by Week")+
  facet_wrap(~End.Date)+
  theme(legend.position="top")
rm(breaks.lbl,breaks.num)

rm(shp.DF,dat.DF)

##(7)Compare to mVAM results
cmpr.DF <- read.xlsx('/home/pato/Dropbox/ymnSeqEstimates.xlsx',sheetName='plot')
cmpr.DF$NumPop <- round(cmpr.DF$NumPop)
colnames(cmpr.DF)[1] <- 'id'

#(A) Num Ppl food Insecure
shp.DF <- fortify(ymnGDF) %>%
  left_join(data_frame(id=rownames(ymnGDF@data), name=ymnGDF@data$ADM1_NAME)) %>%
  select(-id) %>% rename(id=name)

ggplot()+
  geom_polygon(data=shp.DF,aes(x=long,y=lat, group=group),fill='white',color='#7f7f7f',size=0.15)+
  geom_map(data=cmpr.DF, map=shp.DF,aes(map_id=id, fill=NumPop),color="#7f7f7f", size=0.15)+
  scale_fill_gradient(low='blue',high='red',
                      guide = guide_legend(
                        label.theme = element_text(angle = 90)))+
  guides(fill=guide_legend(title='Estimated Population'))+
  labs(title="Num Poor FCS by Source")+
  facet_wrap(~Type)+
  theme(legend.position="top")

#(B) %Diff
ggplot()+
  geom_polygon(data=shp.DF,aes(x=long,y=lat, group=group),fill='white',color='#7f7f7f',size=0.15)+
  geom_map(data=cmpr.DF[cmpr.DF$Type!='May 2018 Targeted',], 
           map=shp.DF,aes(map_id=id, fill=PctDiff*100),color="#7f7f7f", size=0.15)+
  scale_fill_gradient2(low='red',high='blue',mid='white',
                       guide = guide_legend(
                         label.theme = element_text(angle = 90)))+
  guides(fill=guide_legend(title='%Diff'))+
  labs(title="% Difference with May Food Distribution Targets")+
  facet_wrap(~Type,ncol=2)+
  theme(legend.position="top")

