library(tidyverse)
library(reshape2)
library(plyr)
library(mgcv)
library(rstan)
library(brms)
library(bayesplot)
library(bayesdfa)
library(ggplot2)
#source("./scripts/stan_utils.R")
library(MARSS)
theme_set(theme_bw())

setwd("~/Documents/DFA projects/Ecosystem state indicator/goa")

#Load GOA Climate data
w.ak <- read.csv('data/WGOA_EcoState_Data_Jan2023.csv',na = c("NA","na"))
#w.ak=w.ak[,-1]
w.ak_meta <- read.csv('data/WGOA_metadata_BF.csv',na = c("NA","na"))
#w.ak_meta <- read.csv('data/WGOA_metadata_BF.csv',na = c("NA","na")) #full model
# View(w.ak)

names(w.ak_meta) <- tolower(names(w.ak_meta))
names(w.ak)[1] <- "year"
w.akl <- tidyr::gather(w.ak, code, value, -year) #%>% na.omit()
w.akl$value <- as.numeric(w.akl$value)
w.dak <- inner_join(w.akl, select(w.ak_meta, all.bio_indicator, code, type)) %>% #na.omit bf_indicato 
  filter(all.bio_indicator == 1) %>%
  select(-all.bio_indicator)
w.dak = w.dak[w.dak$year>=1985,]
w.dak=w.dak[,-4]

#wgoa.all= c('CHI.EULACHON.CPUE'='Chiniak eulachon','CHI.JELLY.CPUE'='Chiniak jellyfish', 'CHI.SHRIMP.CPUE'='Chiniak pink shrimp', 
            #'CHLA.BIOM'='Chla biomass','CHLA.PEAK' ='Chla peak bloom','FOCI.ICH.AMMPER'= 'Larval sandlance',
            #'FOCI.ICH.ATHSTO'='Larval arrowtooth','FOCI.ICH.GADCHA'='Larval pollock','FOCI.ICH.HIPELA'='Larval flathead sole',
            #'FOCI.ICH.LEPPOL'='Larval nor. rock sole','FOCI.ICH.SEBSPP'='Larval rockfishes',
            #'MID.IL.CAPELIN'='Prop. capelin in diet','PAV.CAPELIN.CPUE'='Pavlof capelin', "PAV.SHRIMP.CPUE" = "Pavlof shrimp",
            #'SB.MEANHATCH.BLKW.EAMT'='Kittiwake hatch','SB.MEANHATCH.PAAU.CHOW'='Parakeet auklet hatch',
            #'SB.MEANHATCH.TUPU.CHOW'='Tufted puffin hatch','SB.REPSUC.BLK.CHOW'='Kittiwake prod.',
            #'SB.REPSUC.PAUK.CHOW'='Parakeet auklet prod.','SB.REPSUC.TPUF.CHOW'='Tufted puffin prod.','SSL.WEST.PUP.PRED'= 'Sea lion pup counts',
            #'SWDLN.LG.CAL.COP.SPR'='Lg calanoid copepod','SWDLN.SM.CAL.COP.SPR'='Sm calanoid copepod')

#No Sea lion pups
wgoa.all= c('CHI.EULACHON.CPUE'='Chiniak eulachon','CHI.JELLY.CPUE'='Chiniak jellyfish', 'CHI.SHRIMP.CPUE'='Chiniak pink shrimp', 
            'CHLA.BIOM'='Chla biomass','CHLA.PEAK' ='Chla peak bloom','FOCI.ICH.AMMPER'= 'Larval sandlance',
            'FOCI.ICH.ATHSTO'='Larval arrowtooth','FOCI.ICH.GADCHA'='Larval pollock','FOCI.ICH.HIPELA'='Larval flathead sole',
            'FOCI.ICH.LEPPOL'='Larval nor. rock sole','FOCI.ICH.SEBSPP'='Larval rockfishes',
            'MID.IL.CAPELIN'='Prop. capelin in diet','PAV.CAPELIN.CPUE'='Pavlof capelin', "PAV.SHRIMP.CPUE" = "Pavlof shrimp",
            'SB.MEANHATCH.BLKW.EAMT'='Kittiwake hatch','SB.MEANHATCH.PAAU.CHOW'='Parakeet auklet hatch',
            'SB.MEANHATCH.TUPU.CHOW'='Tufted puffin hatch','SB.REPSUC.BLK.CHOW'='Kittiwake prod.',
            'SB.REPSUC.PAUK.CHOW'='Parakeet auklet prod.','SB.REPSUC.TPUF.CHOW'='Tufted puffin prod.',
            'SWDLN.LG.CAL.COP.SPR'='Lg calanoid copepod','SWDLN.SM.CAL.COP.SPR'='Sm calanoid copepod')
            
wgoa.ltl= c('CHLA.BIOM'='Chla biomass','CHLA.PEAK' ='Chla peak bloom','FOCI.ICH.AMMPER'= 'Larval sandlance','FOCI.ICH.ATHSTO'='Larval arrowtooth','FOCI.ICH.BATSPP'='Larval ronquils','FOCI.ICH.GADCHA'='Larval pollock','FOCI.ICH.GADMAC'='Larval Alaska pollock', 'FOCI.ICH.HIPELA'='Larval flathead sole','FOCI.ICH.HIPSTE'= 'Larval P. halibut','FOCI.ICH.LEPBIL' = 'Larval rock sole',
             'FOCI.ICH.LEPPOL'='Larval nor. rock sole','FOCI.ICH.PLASTE' = 'Larval starry flounder', 'FOCI.ICH.SEBSPP'='Larval rockfishes','FOCI.ICH.STELEU' = 'Larval nor. lampfish',"SWDLN.COP.CMMTY.SZ.SPR"='Copepod community size',
             'SWDLN.LG.CAL.COP.FLL'='Lg calanoid copepod fall','SWDLN.LG.CAL.COP.SPR'='Lg calanoid copepod spr','SWDLN.SM.CAL.COP.FLL'='Sm calanoid copepod fall','SWDLN.SM.CAL.COP.SPR'='Sm calanoid copepod spr')


#wgoa.mtl=c('CH.CO'='Chignik coho salmon','CH.PI'='Chignik pink salmon','CHI.DOGFISH.CPUE'='Chiniak dogfish','CHI.EULACHON.CPUE'='Chiniak eulachon','CHI.JELLY.CPUE'='Chiniak jellyfish',
           #'CHI.P.COD.CPUE'='Chiniak P. cod','CHI.POLLOCK.CPUE'='Chiniak pollock','CHI.SHRIMP.CPUE'='Chiniak pink shrimp','KOD.CO'='Kodiak coho', 'KOD.PI'='Kodiak pink',
           #'MID.IL.CAPELIN'='Prop. capelin in diet','PAV.SHRIMP.CPUE'= 'Pavlof shrimp', 'PAV.CAPELIN.CPUE'='Pavlof capelin','PAV.EULACHON.CPUE' = 'Pavlof Eulachon', 'PAV.P.COD.CPUE'='Pavlof P. cod', 
           #'PAV.POLLOCK.CPUE'='Pavlof pollock', 'PAV.JELLY.CPUE' = 'Pavlof Jellyfish', "PWS.CO" = 'PWS coho salmon', "PWS.PI" = 'PWS pink salmon', "CI.CO" = 'Cook Inlet coho salmon',
           #"CI.PI" = 'Cook Inlet pink salmon', "SP.CO" = 'South Peninsula coho')  

wgoa.mtl=c('CHI.EULACHON.CPUE'='Chiniak eulachon','CHI.JELLY.CPUE'='Chiniak jellyfish','CHI.SHRIMP.CPUE'='Chiniak pink shrimp',
'MID.IL.CAPELIN'='Prop. capelin in diet','PAV.SHRIMP.CPUE'= 'Pavlof shrimp', 'PAV.CAPELIN.CPUE'='Pavlof capelin','PAV.EULACHON.CPUE' = 'Pavlof Eulachon',  
'PAV.JELLY.CPUE' = 'Pavlof Jellyfish','SSL.WEST.PUP.PRED'= 'Sea lion pup counts')

wgoa.sb= c('SB.MEANHATCH.COMU.CHOW'='Common murre hatch', 'SB.MEANHATCH.TBMU.CHOW'='Thick-billed murre hatch', 'SB.MEANHATCH.PAAU.CHOW'='Parakeet auklet hatch','SB.MEANHATCH.HOPU.CHOW'='Horned puffin hatch', 
           'SB.MEANHATCH.TUPU.CHOW'='Tufted puffin hatch','SB.MEANHATCH.GWGU.CHOW'='Glaucous-winged gull hatch','SB.MEANHATCH.BLKW.EAMT'='Kittiwake hatch',
           'SB.REPSUC.COMU.CHOW' = 'Common murre prod.','SB.REPSUC.TBMU.CHOW' = 'Thick-billed murre prod.','SB.REPSUC.HPUFF.CHOW' = 'Horned puffin prod.','SB.REPSUC.TPUF.CHOW'='Tufted puffin prod.',
           'SB.REPSUC.BLK.CHOW'='Kittiwake prod.', 'SB.REPSUC.PAUK.CHOW'='Parakeet auklet prod.','SB.REPSUC.BLK.AMAT'= 'Black-legged kittiwake prod.', 'SB.REPSUC.FTSP.AMAT'='Fork-tailed storm petrel prod.')

wgoa.climate=c("AKCLIM_GAK1_salinity20m_FMA"="Salinity (spring)", "AKCLIM_GOA_N60W146"="Upwelling (N60W146)","AKCLIM_GOA_N60W149"="Upwelling (N60W149)", 
"AKCLIM_GOA_wgoa.spr.sst"= "Spring SST (AKCLIM)", "AKCLIM_GOA_wgoa.win.sst"="Winter SST (AKCLIM)",  "BTS.WGOA.TEMP.SUM.195TO205M"="BTS summer deep temp", "BTS.WGOA.TEMP.SUM.1TO5M"="BTS summer SST",
"EKE.REGC.DJF"="EKE (winter)", "LL.WGOA.TEMP.SUM.246TO255M"="Summer shelf edge temp", "SHEL.APRMAY.WINDDIR"="Spring Shelikof wind dir,", "SST.COASTWATCH.JUNJULAUG"="Summer SST (Satel.)",
"SWLN.TEMP.SPR.0TO10M"="Spring SST (Seward line")

ggplot(w.dak, aes(year,value))+
  geom_line()+geom_point()+facet_wrap(~code,scales='free_y',labeller=labeller(code=wgoa.all))
ggsave("/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/wgoa/rawdata.all.pdf",width = 10, height = 7)

Y <- dcast(w.dak, code ~ year)
names = Y$code
Y = as.matrix(Y[,-which(names(Y) == "code")])

# # set up forms of R matrices
levels.R = c("diagonal and equal",
             "diagonal and unequal",
              "equalvarcov")
model.data = data.frame()


 ## changing convergence criterion to ensure convergence
 cntl.list = list(minit=200, maxit=20000, allow.degen=FALSE, conv.test.slope.tol=0.1, abstol=0.0001)
 
 # fit models & store results
 for(R in levels.R) {
   for(m in 1:2) {  # allowing up to 2 trends
     dfa.model = list(A="zero", R=R, m=m)
     kemz = MARSS(Y, model=dfa.model,
                  form="dfa", z.score=TRUE, control=cntl.list)
     model.data = rbind(model.data,
                        data.frame(R=R,
                                   m=m,
                                   logLik=kemz$logLik,
                                   K=kemz$num.params,
                                   AICc=kemz$AICc,
                                   stringsAsFactors=FALSE))
     assign(paste("kemz", m, R, sep="."), kemz)
   } # end m loop
 } # end R loop

  
# # calculate delta-AICc scores, sort in descending order, and compare
 model.data$dAICc <- model.data$AICc-min(model.data$AICc)
 model.data <- model.data %>%
   arrange(dAICc)
 model.data
 
 ###################################################
 ### makemodeltable
 ###################################################
 # calculate delta-AICc
 model.data$delta.AICc = model.data$AICc - min(model.data$AICc)
 # calculate Akaike weights
 wt = exp(-0.5*model.data$delta.AICc)
 model.data$Ak.wt = wt/sum(wt)
 # sort results
 model.tbl = model.data[order(model.data$AICc),-4]
 # drop AICc from table
 # calculate cumulative wts
 model.tbl$Ak.wt.cum = cumsum(model.tbl$Ak.wt)
 model.tbl = model.tbl[,-6]
 print(model.tbl)
 
 write.csv(model.tbl,'/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/wgoa/DFA_AIC_all.goa.csv')
 write.csv(w.dak,'/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/wgoa/allgoa_data.csv')
 

model.list = list(A="zero", m=1, R="diagonal and equal")
dfa.mod = MARSS(Y, model=model.list, z.score=TRUE, form="dfa") 

saveRDS(dfa.mod,"/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/wgoa/wgoa_all.goa.rds")

dfa.mod=readRDS('/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/wgoa/mtl_model_noSLpups/wgoa_mtl.rds')


# get CI and plot loadings...
modCI <- MARSSparamCIs(dfa.mod)
modCI ## positive loadings for all three TS

loadings <- data.frame(names = wgoa.mtl,
                       loading = modCI$par$Z,
                       upCI = modCI$par.upCI$Z,
                       lowCI = modCI$par.lowCI$Z)
              

#loadings$names <- reorder(loadings$names, loadings$order)
write.csv(loadings,'/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/wgoa/mtl_model_noSLpups/wgoa_mtl_loadings.csv')

#quartz()
ggplot(loadings, aes(names, loading)) +
  geom_bar(stat="identity", fill="light grey") +
  geom_errorbar(aes(ymin=lowCI, ymax=upCI), width=0.2) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=57, vjust=1, hjust=1))+
  ylab("Loading")

ggsave("/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/wgoa/loadings.all.pdf",width = 8, height=6)


# plot trend
trend <- data.frame(year = 1985:2022,
                    trend = as.vector(dfa.mod$states),
                    ymin = as.vector(dfa.mod$states-1.96*dfa.mod$states.se),
                    ymax = as.vector(dfa.mod$states+1.96*dfa.mod$states.se))

write.csv(trend,'/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/wgoa/wgoa.all.model.trend.csv')

#quartz()
trend.plot <- ggplot(trend, aes(year, trend)) +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="grey90") +
  geom_line(color="black") +
  geom_point(color="black") +
  geom_hline(yintercept = 0) +
  theme(axis.title.x = element_blank()) +
  ylab("Trend") +
  scale_x_continuous(breaks = seq(1985, 2025, 5))

ggsave("/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/wgoa/trend.all.pdf",width = 8, height=6)





