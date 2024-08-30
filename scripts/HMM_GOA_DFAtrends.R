#Code for running 
#Western GOA climate trend

library(tidyverse)
library(hmmTMB) #for HMM models
library(patchwork) #for plotting

# Data --------------------------------------------------------------------
#read data
dfa.mod<-read_csv("/Users/mary.hunsicker/Documents/DFA projects/Ecosystem state indicator/goa/MARSS results/V2_Spring2024/wgoa/climate_model_long/wgoa.cli.model.trend.csv")
dfa.mod<-readRDS("/wgoa_cli.rds")
dfa.mod=as.data.frame(dfa.mod)

# Plotting Data -----------------------------------------------------------

#Easier to plot from long format
quartz()
ggplot(dfa.mod) + geom_point(aes(x = year, y = trend)) 

# Models ---------------------------------------------------------
#2-state
dat=dfa.mod[,2]
dat=as.data.frame(dat)

#distributions
dfa_dists<-lapply(dat, function(x){x = "norm"})

#initial values
#based on plots and what makes it converge
DFA_inits2<-list()
DFA_inits2$dat<-list(mean = c(2.5, -2.5), sd = c(1,1))

#setting up model
DFA_hid2 <- MarkovChain$new(data = dat, n_states = 2)
DFA_obs2 <- Observation$new(data = dat, n_states = 2, dists = dfa_dists, par = DFA_inits2)

DFA_hmm2 <- HMM$new(obs = DFA_obs2, hid = DFA_hid2)

#fit model
DFA_hmm2$fit(silent = TRUE)
DFA_hmm2$out()

DFA_hmm2$par() 
DFA_hmm2$viterbi() 

DFA_hmm2$AIC_conditional()

saveRDS(DFA_hmm2$par(), ".rds")

#3-state
DFA_inits3<-list()
DFA_inits3$dat<-list(mean = c(2.5, 0.5, -2.5), sd = c(1,1,1))

#setting up model
DFA_hid3 <- MarkovChain$new(data = dat, n_states = 3)
DFA_obs3 <- Observation$new(data = dat, n_states = 3, dists = dfa_dists, par = DFA_inits3)

DFA_hmm3 <- HMM$new(obs = DFA_obs3, hid = DFA_hid3)

#fit model
DFA_hmm3$fit(silent = TRUE)
DFA_hmm3$out()

DFA_hmm3$par() 
DFA_hmm3$viterbi() 

DFA_hmm3$AIC_conditional()

saveRDS(DFA_hmm3$par(), ".rds")

# Results -----------------------------------------------------------------


#summary table
Summ_table<-function(obspar, nstate ){
  obspar1<-obspar %>% as_tibble(rownames = "Ind") %>% 
    separate_wider_delim(Ind,delim = ".", names = c("Indicator", "par")) %>% 
    pivot_longer(all_of(3:(2+nstate)), names_to = "state", values_to = "estimate") %>%
    pivot_wider(id_cols = c(Indicator, state), names_from = "par", values_from = "estimate") %>% 
    mutate(state = as.integer(str_remove_all(state, "state "))) 
  return(obspar1)
}

# 2 state model plot
par2<-DFA_hmm2$par()
obs_ests<-Summ_table(par2$obs, 2)
DFA_tab<-obs_ests %>% 
  mutate(lower = qnorm(0.025, mean, sd), upper = qnorm(0.975, mean, sd))

#write_csv(DFA_tab, "climate_model_long_regimes_summary.csv")

#estimated states
DFA_sts<-tibble(year = seq(1970, 2022, by = 1), state = DFA_hmm2$viterbi())
names(DFA_sts)[1]="year"

plot.dat=dfa.mod %>% left_join(DFA_sts, by = "year") %>% 
  left_join(DFA_tab, by = c("state"))
  
col<-c("#11c2b5", "#677e8e")

#plot data and estimated states
plot1<-plot.dat %>%
  ggplot() + 
  geom_point(aes(x = year, y = mean, color = as.factor(state), shape = as.factor(state))) +
  geom_linerange(aes(x = year, ymin = lower, ymax = upper, color = as.factor(state))) +
  geom_point(aes(x = year, y = trend)) + 
  scale_shape_manual(values = c(8,17)) +
  scale_color_manual(values = col) + 
  scale_x_continuous(breaks = seq(1970, 2022, by = 5)) +
  labs(y = "Trend value") + 
  theme_light() + theme(legend.position = "none", 
                        strip.background = element_blank(), 
                        strip.text = element_text(color = "black", size = 11), 
                        axis.text = element_text(size = 10),
                        axis.title = element_text(size = 13),
                        legend.text = element_text(size = 12), 
                        legend.title = element_text(size =13), 
                        panel.grid.minor = element_blank())

plot1
ggsave("climate_model_long_regimes.png", plot1, dpi = 600)



