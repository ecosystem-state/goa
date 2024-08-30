library(tidyverse)
library(reshape2)
library(plyr)
#library(mgcv)
library(rstan)
library(brms)
#library(bayesplot)
#library(bayesdfa)
library(ggplot2)
#source("./scripts/stan_utils.R")
library(MARSS)
theme_set(theme_bw())

setwd("~/Documents/DFA projects/Ecosystem state indicator/goa")

#Load GOA data
e.ak <- read.csv('data/EGOA_EcoState_Data_Jan2023.csv',na = c("NA","na"))
e.ak_meta <- read.csv('data/EGOA_metadata.csv',na = c("NA","na"))
# View(w.ak)

names(e.ak_meta) <- tolower(names(e.ak_meta))
names(e.ak)[1] <- "year"
e.akl <- tidyr::gather(e.ak, code, value, -year)
e.akl$value <- as.numeric(e.akl$value)
e.dak <- inner_join(e.akl, select(e.ak_meta, include_allbio, code, type)) %>% #choose indicator type - match to metadata column heading
  filter(include_allbio == 1) %>%
  select(-include_allbio)
e.dak = e.dak[e.dak$year>=1985,] 
e.dak=e.dak[,-4]


## These are all of the indicator types and the individual code names. Here we are renaming the variables.

egoa.all=c("CHLA.BIOM"="Chla biomass","CHLA.PEAK"="Chla peak bloom","CRG.HERR.MATBIOM"="Herring biomass (Craig)","GB.HUMP.CRUD.BR.JA"="Humpback whale birth date",
           "SB.REPSUC.COMU.LAZ"="Common murre prod.", "SB.REPSUC.FTSP.LAZ"="Fork-tailed storm petrel prod.",
"SB.REPSUC.LSP.LAZ"="Leach's storm petrel prod.","SB.REPSUC.TBMU.LAZ"="Thick-billed murre prod.",
"SBRD.MEANHATCH.COMU.LAZ"="Common murre hatch","SBRD.MEANHATCH.FTSP.LAZ"="Fork-tailed storm petrel hatch",
"SBRD.MEANHATCH.GWGU.LAZ"="Glaucous-winged gull hatch","SBRD.MEANHATCH.LHSP.LAZ"= "Leach's storm petrel hatch ",
"SBRD.MEANHATCH.RHAU.LAZ"="Rhinocerus auklet hatch","SBRD.MEANHATCH.TBMU.LAZ"="Thick billed murre hatch",
"SECM.CHIN.CPUE"="Juv chinook salmon cpue", "SECM.CHUM.CPUE"="Juv chum salmon cpue","SECM.COHO.CPUE"="Juv coho salmon cpue",  
"SECM.MESOZOOP.DENS"= "Mesozooplankton", "SECM.PINK.CPUE"="Juv pink salmon cpue", "SECM.SOCK.CPUE"="Juv sockeye salmon cpue", 
"STKA.HERR.MATBIOM"="Herring biomass (Sitka)") 
#"SSL.SEAK.PUP.PRED" = "Sea lion pup count", 

egoa.ltl=c("CHLA.BIOM"="Chla biomass","CHLA.PEAK"="Chla peak bloom", "SECM.COP.COMM.SIZE"="Copepod community size","SECM.EUPH.DENS"="Euphausiid abundance", "SECM.GAST.DENS" ="Gastropod abundance",
           "SECM.HYP.AMPH" ="Amphipod abundance","SECM.LG.CAL.COP.DENS"="Large copepod abundance","SECM.MESOZOOP.DENS"="Mesozooplankton abundance","SECM.SM.CAL.COP.DENS"="Small copepod abundance") 

egoa.mtl=c("CRG.HERR.MATBIOM"="Herring biomass (Craig)","GB.HUMP.CRUD.BR.JA"= "Humpback whale birth date", "SECM.CHIN.CPUE"="Juv chinook salmon cpue",
           "SECM.CHUM.CPUE"="Juv chum salmon cpue","SECM.COHO.CPUE"="Juv coho salmon cpue", "SECM.PINK.CPUE"="Juv pink salmon cpue", 
           "SECM.SOCK.CPUE"="Juv sockeye salmon cpue","STKA.HERR.MATBIOM"="Herring biomass (Sitka)") 


egoa.sb=c("SB.REPSUC.COMU.LAZ"="Common murre prod.", "SB.REPSUC.FTSP.LAZ"="Fork-tailed storm petrel prod.",
           "SB.REPSUC.LSP.LAZ"="Leach's storm petrel prod.","SB.REPSUC.TBMU.LAZ"="Thick-billed murre prod.","SBRD.MEANHATCH.COMU.LAZ"="Common murre hatch",
           "SBRD.MEANHATCH.FTSP.LAZ"="Fork-tailed storm petrel hatch","SBRD.MEANHATCH.GWGU.LAZ"="Glaucous-winged gull hatch",
           "SBRD.MEANHATCH.LHSP.LAZ"= "Leach's storm petrel hatch ","SBRD.MEANHATCH.RHAU.LAZ"="Rhinocerus auklet hatch",
           "SBRD.MEANHATCH.TBMU.LAZ"="Thick billed murre hatch")
           
egoa.climate=c("AKCLIM_GOA_egoa.spr.sst"= "Spring SST (AKCLIM)", "AKCLIM_GOA_egoa.win.sst"="Winter SST (AKCLIM)",
               "AKCLIM_GOA_N54W134"="Upwelling (N54W134)","AKCLIM_GOA_N57W137"="Upwelling (N57W137)", 
                 "BTS.EGOA.TEMP.SUM.195TO205M"="BTS summer deep temp", "BTS.EGOA.TEMP.SUM.1TO5M"="BTS summer SST", "EKE.REGA.DJF" = "EKE (winter)",
               "LL.EGOA.TEMP.SUM.246TO255M"="Summer shelf edge temp", "PTI.LAT"= "Papa advection Index (Lat)",
               "SST.COASTWATCH.JUNJULAUG"="Summer SST (Satel.)")

#all.climate= c("AKCLIM_GAK1_salinity20m_FMA"="Salinity (spring)","AKCLIM_GOA_egoa.spr.sst"= "Spring SST (E)", "AKCLIM_GOA_egoa.win.sst"="Winter SST (E)",
               #"AKCLIM_GOA_N54W134"="Upwelling (N54W134)","AKCLIM_GOA_N57W137"="Upwelling (N57W137)", "AKCLIM_GOA_N60W146"="Upwelling (N60W146)",
               #"AKCLIM_GOA_N60W149"="Upwelling (N60W149)", "AKCLIM_GOA_wgoa.spr.sst"= "Spring SST (WGOA)", "AKCLIM_GOA_wgoa.win.sst"="Winter SST (WGOA)",
               #"BTS.EGOA.TEMP.SUM.195TO205M"="BTS summer deep temp (E)", "BTS.EGOA.TEMP.SUM.1TO5M"="BTS summer SST (E)","BTS.WGOA.TEMP.SUM.195TO205M"="BTS summer deep temp (W)", 
               #"BTS.WGOA.TEMP.SUM.1TO5M"="BTS summer SST (W)",  "LL.EGOA.TEMP.SUM.246TO255M"="Summer shelf edge temp (E)", "LL.WGOA.TEMP.SUM.246TO255M"="Summer shelf edge temp (W)", 
               #"PTI.LAT"= "Papa advection Index (Lat)","SHEL.APRMAY.WINDDIR"="Spring Shelikof wind dir", "SST.EGOA.COASTWATCH.JUNJULAUG"="Summer Satel. SST (E)",
               #"SST.WGOA.COASTWATCH.JUNJULAUG"="Summer Satel. SST (W)","SWLN.TEMP.SPR.0TO10M"="Spring SST (Seward line)")

ggplot(e.dak, aes(year,value))+
  geom_line()+geom_point()+facet_wrap(~code,scales='free_y',labeller=labeller(code=egoa.all))
ggsave("/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/egoa/all_egoa_biology_model/rawdata.egoa_all.pdf",width = 11, height = 7)


Y <- dcast(e.dak, code ~ year)
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

write.csv(model.tbl,'/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/egoa/all_egoa_biology_model/DFA_AIC_egoa_all.csv')
write.csv(e.dak,'/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/egoa/all_egoa_biology_model/egoa_all_data.csv')

model.list = list(A="zero", m=2, R= "diagonal and unequal")
dfa.mod = MARSS(Y, model=model.list, z.score=TRUE, form="dfa")

saveRDS(dfa.mod,"/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/egoa/all_egoa_biology_model/egoa.allbio_trend.rds")

## Use this code to read in .rds file to change plotting, etc.
#dfa.mod=readRDS('/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/egoa/seabird_model/egoa_sb.rds')

#### To plot 1 trend model

# get CI and plot loadings...
modCI <- MARSSparamCIs(dfa.mod)
modCI 

loadings <- data.frame(names = egoa.all,
                       loading = modCI$par$Z,
                       upCI = modCI$par.upCI$Z,
                       lowCI = modCI$par.lowCI$Z)

#loadings$names <- reorder(loadings$names, loadings$order)
write.csv(loadings,'/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/egoa/mtl_model/egoa_mtl_loadings.csv')

#quartz()
loadings.plot=ggplot(loadings, aes(names, loading)) +
  geom_bar(stat="identity", fill="light grey") +
  geom_errorbar(aes(ymin=lowCI, ymax=upCI), width=0.2) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=57, vjust=1, hjust=1))+
  ylab("Loading")

ggsave("/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/egoa/mtl_model/loadings.mtl.pdf",width = 8, height=6)


trend <- data.frame(year = 1985:2022,
                    trend = as.vector(dfa.mod$states),
                    ymin = as.vector(dfa.mod$states-1.96*dfa.mod$states.se),
                    ymax = as.vector(dfa.mod$states+1.96*dfa.mod$states.se))


write.csv(trend,'/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/egoa/mtl_model/egoa_mtl.model.trend.csv')

quartz()
trend.plot <- ggplot(trend, aes(year, trend)) +
  geom_ribbon(aes(ymin=ymin, ymax=ymax), fill="grey90") +
  geom_line(color="black") +
  geom_point(color="black") +
  geom_hline(yintercept = 0) +
  theme(axis.title.x = element_blank()) +
  ylab("Trend") +
  scale_x_continuous(breaks = seq(1985, 2025, 5))

ggsave("/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/egoa/all_egoa_biology_model/trend.egoa_mtl.pdf",width = 8, height=6)

#dfa.mod=readRDS('/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/egoa/mtl_model/2 trend model/egoa.mtl_2trend.rds')


####### To plot 2 trend model

## For loadings:
# add the par.lowCI and par.upCI to the fit
dfa.mod <- MARSSparamCIs(dfa.mod)

# get the rotation matrix for the Z
Z <- coef(dfa.mod, type = "matrix")$Z
H.inv <- varimax(Z)$rotmat

#fits/ loadings
Z.rot <- Z %*% H.inv

#upper and lower CIs
Z.rot.up<-coef(dfa.mod, type="matrix", what="par.upCI")
Z.rot.up<-Z.rot.up$Z %*% H.inv

Z.rot.low<-coef(dfa.mod, type="matrix", what="par.lowCI")
Z.rot.low <- Z.rot.low$Z %*% H.inv

# rotate trends
trends.rot = solve(H.inv) %*% dfa.mod$states
ts.trends = t(trends.rot)

trends.se.rot = solve(H.inv) %*% dfa.mod$states.se
ts.trends.se=t(trends.se.rot)


## Plot loadings
loadings1 <- data.frame(names = egoa.all,
                       loading.1 = Z.rot[,1],
                       upCI.1 = Z.rot.up[,1],
                       lowCI.1 = Z.rot.low[,1])

loadings2 <- data.frame(names = egoa.all,
                        loading.2 = Z.rot[,2],
                        upCI.2 = Z.rot.up[,2],
                        lowCI.2 = Z.rot.low[,2])

df=loadings2[,2:4]
df.loadings=cbind(loadings1,df)

#write.csv(df.loadings,'/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/egoa/all_egoa_biology_model/egoa.allbio_2trend_loadings.csv')

#loadings$names <- reorder(loadings$names, loadings$order)

#quartz()
loadings.plot=ggplot(loadings1, aes(names, loading.1)) +
  geom_bar(stat="identity", fill="light grey") +
  geom_errorbar(aes(ymin=lowCI.1, ymax=upCI.1), width=0.2) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle=57, vjust=1, hjust=1))+
  ylab("Loading")


##Plot trends

trend1 <- data.frame(year = 1985:2022,
                    trend.1 = as.vector(ts.trends[,1]),
                    ymin.1 = as.vector(ts.trends[,1]-1.96*ts.trends.se[,1]),
                    ymax.1 = as.vector(ts.trends[,1]+1.96*ts.trends.se[,1]))

trend2 <- data.frame(year = 1985:2022,
                     trend.2 = as.vector(ts.trends[,2]),
                     ymin.2 = as.vector(ts.trends[,2]-1.96*ts.trends.se[,2]),
                     ymax.2 = as.vector(ts.trends[,2]+1.96*ts.trends.se[,2]))

df=trend2[,2:4]
df.trends=cbind(trend1,df)

write.csv(df.trends,'/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/egoa/all_egoa_biology_model/egoa.allbio_trends.csv')


#quartz()
trend.plot <- ggplot(trend1, aes(year, trend.1)) +
  geom_ribbon(aes(ymin=ymin.1, ymax=ymax.1), fill="grey90") +
  geom_line(color="black") +
  geom_point(color="black") +
  geom_hline(yintercept = 0) +
  theme(axis.title.x = element_blank()) +
  ylab("Trend1") +
  scale_x_continuous(breaks = seq(1985, 2025, 5))


# Combine trend and loading plots into a single figure
#png("./figs/cod_DFA_loadings_trend.png", width=7, height=3, units='in', res=300)

ggpubr::ggarrange(trend.plot, loadings.plot,
ncol=2,
#labels=c("a", "b"),
widths=c(0.8,1))

ggsave("/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/egoa/all_egoa_biology_model/egoa.allbio_combined.plot1.pdf",width = 8, height=6)



