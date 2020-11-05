# Opposing life stage-specific effects of ocean warming at source and sink populations of range-shifting coral-reef fishes #
# Monaco et al. 2020, Journal of Animal Ecology 

#author = cristianmonaco
###################################################################################
rm(list=ls())

# Load packages -----------------------------------------------------------
pacman::p_load(tidyverse, padr, forecast, lmtest, bbmle, scales, beanplot, timetk, scales, lubridate)


# Load and organize data --------------------------------------------------

# Settleres #
denS_env_full_SYSW <- read_csv("data/data_denS_env_SYSW_mthly.csv")      
denS_env_full_SYSE <- read_csv("data/data_denS_env_SYSE_mthly.csv")

# Work with mean of density and RECALCULATE r
means <- (denS_env_full_SYSE[,2:53] + denS_env_full_SYSW[,2:53])/2

denS_env_full <- cbind(denS_env_full_SYSW$date, means, denS_env_full_SYSW[,54:77]) %>% 
  as_tibble()

denS_env_full <- denS_env_full %>% mutate(rabva = log(Nabva/Nabva_1))

denS_env_full <- denS_env_full %>% rename(date = `denS_env_full_SYSW$date`) 

denS_env <- denS_env_full %>% mutate(date = as.Date(date, format = "%Y-%m-%d"),
                                     year = year(date),
                                     month = month(date),
                                     qt = quarter(date)) %>% 
  filter(year %in% c(2004:2017),
         rabva != "NA")

denS_env_SYSW <- denS_env_full_SYSW %>% mutate(date = as.Date(date, format = "%Y-%m-%d"),
                                               year = year(date),
                                               month = month(date),
                                               qt = quarter(date)) %>% 
  filter(year %in% c(2004:2017),
         rabva != "NA")

denS_env_SYSE <- denS_env_full_SYSE %>% mutate(date = as.Date(date, format = "%Y-%m-%d"),
                                               year = year(date),
                                               month = month(date),
                                               qt = quarter(date)) %>% 
  filter(year %in% c(2004:2017),
         rabva != "NA")

# write_csv(denS_env, 'denS_env.csv')


# Juveniles #
denL_env_full_SYSW <- read_csv("data/data_denL_env_SYSW_mthly.csv")      
denL_env_full_SYSE <- read_csv("data/data_denL_env_SYSE_mthly.csv")

# Work with mean of density and recalculate r
means <- (denL_env_full_SYSE[,2:52] + denL_env_full_SYSW[,2:52])/2

denL_env_full <- cbind(denL_env_full_SYSW$date, means, denL_env_full_SYSW[,54:77])

denL_env_full <- tbl_df(denL_env_full)

denL_env_full <- denL_env_full %>% mutate(rabva = log(Nabva/Nabva_1))

denL_env_full <- denL_env_full %>% rename(date = `denL_env_full_SYSW$date`) 

denL_env <- denL_env_full %>% mutate(date = as.Date(date, format = "%Y-%m-%d"),
                                     year = year(date),
                                     month = month(date),
                                     qt = quarter(date)) %>% 
  filter(year %in% c(2004:2017),
         rabva != "NA")

denL_env_SYSW <- denL_env_full_SYSW %>% mutate(date = as.Date(date, format = "%Y-%m-%d"),
                                               year = year(date),
                                               month = month(date),
                                               qt = quarter(date)) %>% 
  filter(year %in% c(2004:2017),
         rabva != "NA")

denL_env_SYSE <- denL_env_full_SYSE %>% mutate(date = as.Date(date, format = "%Y-%m-%d"),
                                               year = year(date),
                                               month = month(date),
                                               qt = quarter(date)) %>% 
  filter(year %in% c(2004:2017),
         rabva != "NA")

# write_csv(denL_env, 'denL_env.csv')


# Settlers and juveniles #
# First add settler and juveniles, then average their densities and calculate r

# Settlers
denS_env_full_SYSW <- read_csv("data/data_denS_env_SYSW_mthly.csv") %>% 
  select(date:Nabva_2, BRANv:year)
denS_env_full_SYSE <- read_csv("data/data_denS_env_SYSE_mthly.csv") %>% 
  select(date:Nabva_2, BRANv:year)
# Juveniles
denL_env_full_SYSW <- read_csv("data/data_denL_env_SYSW_mthly.csv") %>% 
  select(date:Nabva_2, BRANv:year)     
denL_env_full_SYSE <- read_csv("data/data_denL_env_SYSE_mthly.csv") %>% 
  select(date:Nabva_2, BRANv:year)

# Add settlers and juveniles from SYSW
den_S_L_SYSW <- denS_env_full_SYSW[2:4] + denL_env_full_SYSW[2:4] - 0.1 # to avoid adding 0.1 twice, one for each site
den_S_L_env_full_SYSW <- cbind(denS_env_full_SYSW$date, den_S_L_SYSW, denS_env_full_SYSW[,5:28]) %>% 
  tbl_df(.) %>% 
  rename(date = `denS_env_full_SYSW$date`) %>% 
  filter(year %in% c(2004:2017))

# Add settlers and juveniles from SYSE
den_S_L_SYSE <- denS_env_full_SYSE[2:4] + denL_env_full_SYSE[2:4] - 0.1 # to avoid adding 0.1 twice, one for each site
den_S_L_env_full_SYSE <- cbind(denS_env_full_SYSE$date, den_S_L_SYSE, denS_env_full_SYSE[,5:28]) %>% 
  tbl_df(.) %>% 
  rename(date = `denS_env_full_SYSE$date`) %>% 
  filter(year %in% c(2004:2017))

# Mean density between SYSW and SYSE
means_S_L <- (den_S_L_env_full_SYSW[,2:4] + den_S_L_env_full_SYSE[,2:4])/2
den_S_L_env_full <- cbind(den_S_L_env_full_SYSW$date, means_S_L, den_S_L_env_full_SYSW[,5:28]) %>% 
  tbl_df(.) %>% 
  rename(date = `den_S_L_env_full_SYSW$date`)


# Calculate r
den_S_L_env_full <- den_S_L_env_full %>% mutate(rabva = log(Nabva/Nabva_1))
den_S_L_env <- den_S_L_env_full %>% mutate(date = as.Date(date, format = "%Y-%m-%d"),
                                           year = year(date),
                                           month = month(date),
                                           qt = quarter(date)) %>% 
  filter(year %in% c(2004:2017),
         rabva != "NA")
# write_csv(den_S_L_env, 'den_S_L_env.csv')


## Load data    ####
denL_env <- read_csv('data/denL_env.csv')
denS_env <- read_csv('data/denS_env.csv')
den_S_L_env <- read_csv('data/den_S_L_env.csv')


# SETTLERS ----------------------------------------------------------------

# Prove that we can't link the two life stages ----------------------------

# 1. effect of settler's r on juvenile's r ####
lin_mod <- summary(glm(denS_env$rabva~denL_env$rabva))
(evidence_ratio <- lin_mod$null.deviance/lin_mod$deviance)

# 2. effect of juvenile's r on the top-ranked model of settlers ####
# for these lines to work, All the models below need to be ran first !!

d_juv_r <- data_juveniles %>% dplyr::select(r) %>% 
  rename(r_juv = r)

d_set_juv <- data_setters %>% bind_cols(d_juv_r)

inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, epsilon = 1, phi=-0.7, sigma=1)
M22_Juv_r <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSST + gamma*sSWAIN_SST_3 +
                              epsilon*r_juv +
                              phi*qt, sd=sigma),
                  parnames = list("rm","logK", "beta","gamma", "epsilon",  "phi","sigma"),
                  start = inits, method = 'Nelder-Mead', data=d_set_juv, control = list(maxit = 10000))
summary(M22_Juv_r)

AICctab(M22_Juv_r, top1_mod_settlers,
        delta = TRUE, weights = TRUE, nobs=85)


# . Negative log likelihood functions  ####

# .. Define model variables ----
date <- denS_env$date[1:86]
r <- denS_env$rabva[1:86]
logN <- log(denS_env$Nabva_1[1:86])
logN_2 <- log(denS_env$Nabva_2[1:86])
sBRANv <- scale(denS_env$BRANv_1[1:86]) # scaled BRANv
sSST <- scale(denS_env$sst_1[1:86]) # scaled sst
sChla <- scale(denS_env$chla_1[1:86]) # scaled chla
sChla_GBR_2 <- scale(denS_env$chla_SWAIN_2[1:86]) # scaled chla SWAIN t-2 sst
sChla_GBR_3 <- scale(denS_env$chla_SWAIN_3[1:86]) # scaled chla SWAIN t-3 sst
sSWAIN_SST_2 <- scale(denS_env$sst_SWAIN_2[1:86]) # scaled SWAIN_2 sst
sSWAIN_SST_3 <- scale(denS_env$sst_SWAIN_3[1:86]) # scaled SWAIN_3 sst
r_abvaL <- scale(denL_env$rabva)[1:86]

qt <- denS_env$qt[1:86]
d <- tibble(date = date, r=r,logN=logN, logN2=logN_2, sBRANv=sBRANv, sSST=sSST, sChla=sChla, 
            sSWAIN_SST_3=sSWAIN_SST_3, sSWAIN_SST_2=sSWAIN_SST_2, qt=qt, r_abvaL = r_abvaL)   

# .. Test linearity of the effect of season  ----

summary(lm(d$r~d$qt))


# png("linearMod_Qt_vs_r_Settlers.png", width = 5, height = 4, units = 'in', res = 600)
d %>% 
  ggplot(aes(x = qt, y = r)) +
  geom_smooth(method = 'lm') +
  geom_jitter(shape = 21, width = 0, size = 4) +
  annotate("text", x=4, y=4.5, label = "italic(R) ^ 2 == 0.06", parse = TRUE, hjust=1, size = 4)+
  annotate("text", x=4, y=3.8, label = "p == 0.02", parse = TRUE, hjust=1) +
  theme_bw() +
  theme(axis.title.x=element_text(size=16, family="Arial", colour="black", vjust=-0.1),
        axis.title.y=element_text(size=16, family="Arial", colour="black", vjust=2),
        axis.text.x = element_text(size=12, colour="black"),
        axis.text.y = element_text(size=12, colour="black"),
        plot.margin = unit(c(2, 0.2, 2, 2), "line"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  ylab("r") +
  xlab("Season (quarter)") 
# dev.off()


# .. MODEL RW - random walk, rm=0: r=0+error ----      

# Define inits
inits_r_mRW_abva_s <- list(rm=0, sigma = 10)

# Find values that minimize the NegLogLik
r_mRW_abva_s <- mle2(r ~ dnorm(mean=rm, sd=sigma), 
                     parnames = list("rm","sigma"),
                     start = inits_r_mRW_abva_s, 
                     fixed=list(rm=0), method = "L-BFGS-B", lower = c(-10), upper = c(10), data=d)
summary(r_mRW_abva_s) 
AIC(r_mRW_abva_s)
LL.rw <- as.numeric(logLik(r_mRW_abva_s))

# .. MODEL EX - exponential growth, K=Inf: r = rm + error ----      
# Define inits
inits_r_mEX_abva_s <- list(rm=0, sigma = 1)
# Find values that minimize the NegLogLik
r_mEX_abva_s <- mle2(r ~ dnorm(mean=rm, sd=sigma), start = inits_r_mEX_abva_s, 
                     method = "L-BFGS-B", lower = c(-10), upper = c(10), data=d)
summary(r_mEX_abva_s) 
AIC(r_mEX_abva_s)
LL.mEX <- as.numeric(logLik(r_mEX_abva_s))

# .. MODEL GL - Gompertz Logistic simple: r=rm(1-(logN/logK))+error ----    

# Define inits
inits_r_mGL_abva_s <- list(rm = 1, logK = 1, sigma = 1)
# Find values that minimize the NegLogLik
r_mGL_abva_s <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)), sd=sigma), parnames = list("rm","logK","sigma"), 
                     start = inits_r_mGL_abva_s, method = 'Nelder-Mead', data=d)
summary(r_mGL_abva_s) 
AIC(r_mGL_abva_s)
LL.mGL <- as.numeric(logLik(r_mGL_abva_s))


# .. GL MODELS with combinations of env variables ----    

# Possible model combinations
#2^5=32

regMat <- expand.grid(c(1,0), c(1,0), c(1,0), c(1,0),  c(1,0))
names(regMat) <- c('BRANv','SST','SSTgbr2', 'SSTgbr3','CHL')

# M1
regMat[1,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1,
              epsilon_2 = 1, epsilon_3 = 1, phi=-0.7, sigma=1)
M1 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSST + kappa*sChla + 
                       epsilon_2*sSWAIN_SST_2+epsilon_3*sSWAIN_SST_3+phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa","epsilon_2","epsilon_3","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M2
regMat[2,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, epsilon = 1, phi=-0.7, sigma=1)
M2 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSST + gamma*sSWAIN_SST_2 +
                       kappa*sSWAIN_SST_3 + epsilon*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa","epsilon","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M3
regMat[3,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, epsilon = 1, phi=-0.7, sigma=1)
M3 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sSWAIN_SST_2 +
                       kappa*sSWAIN_SST_3 + epsilon*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa","epsilon","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M4
regMat[4,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M4 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSWAIN_SST_2 + gamma*sSWAIN_SST_3 + kappa*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M5
regMat[5,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, epsilon = 1, phi=-0.7, sigma=1)
M5 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sSST + kappa*sSWAIN_SST_3 + epsilon*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa", "epsilon","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M6
regMat[6,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M6 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSST + gamma*sSWAIN_SST_3 + kappa*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M7
regMat[7,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M7 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sSWAIN_SST_3 + kappa*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M8
regMat[8,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M8 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSWAIN_SST_3 + gamma*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M9
regMat[9,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, epsilon = 1, phi=-0.7, sigma=1)
M9 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sSST + kappa*sSWAIN_SST_2 + epsilon*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa", "epsilon","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M10
regMat[10,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M10 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSST + gamma*sSWAIN_SST_2 + kappa*sChla +
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma","kappa", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M11
regMat[11,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M11 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sSWAIN_SST_2 + kappa*sChla +
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma","kappa", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M12
regMat[12,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M12 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSWAIN_SST_2 + gamma*sChla +
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M13
regMat[13,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M13 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sSST + kappa*sChla +
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma", "kappa", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M14
regMat[14,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M14 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSST + gamma*sChla + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M15
regMat[15,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M15 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sChla + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M16
regMat[16,]
inits <- list(rm = 1, logK=1, beta = 1, phi=-0.7, sigma=1)
M16 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sChla +
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M17
regMat[17,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, epsilon = 1, phi=-0.7, sigma=1)
M17 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSST + kappa*sSWAIN_SST_2 + epsilon*sSWAIN_SST_3 +
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma","kappa","epsilon","phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M18
regMat[18,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M18 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSST + gamma*sSWAIN_SST_2 + kappa*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma","kappa","phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M19
regMat[19,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M19 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSWAIN_SST_2 + kappa*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma","kappa","phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M20
regMat[20,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M20 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSWAIN_SST_2 + gamma*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma","phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M21
regMat[21,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M21 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSST + kappa*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma", "kappa", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M22
regMat[22,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M22 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSST + gamma*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma",  "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M22
regMat[22,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M22_t <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSST + gamma*sSWAIN_SST_3 + 
                          phi*qt*sSST, sd=sigma),
              parnames = list("rm","logK", "beta","gamma",  "phi","sigma"),
              start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M23
regMat[23,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M23 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma",  "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M24
regMat[24,]
inits <- list(rm = 1, logK=1, beta = 1, phi=-0.7, sigma=1)
M24 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta",  "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M25
regMat[25,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M25 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSST + kappa*sSWAIN_SST_2 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "gamma", "kappa", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M26
regMat[26,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M26 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSST + gamma*sSWAIN_SST_2 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "gamma", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M27
regMat[27,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M27 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSWAIN_SST_2 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "gamma", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M28
regMat[28,]
inits <- list(rm = 1, logK=1, beta = 1,  phi=-0.7, sigma=1)
M28 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSWAIN_SST_2 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta",  "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M29
regMat[29,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M29 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSST + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "gamma", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M30
regMat[30,]
inits <- list(rm = 1, logK=1, beta = 1, phi=-0.7, sigma=1)
M30 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSST + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M31
regMat[31,]
inits <- list(rm = 1, logK=1, beta = 1, phi=-0.7, sigma=1)
M31 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M32
regMat[32,]
inits <- list(rm = 1, logK=1, phi=-0.7, sigma=1)
M32 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))



# . Ranking of models ####################################################################################################

# ALL MODELS FOR PAPER  #      

# Likelihood vector
model_list <- c('RW','EX', 'GL', paste0('M', c(1:32)))
Lik.vec <- vector(length=35)
Lik.vec[1] <- as.numeric(logLik(r_mRW_abva_s))
Lik.vec[2] <- as.numeric(logLik(r_mEX_abva_s))
Lik.vec[3] <- as.numeric(logLik(r_mGL_abva_s))
for(i in 4:35){
  Lik.vec[i] <- logLik(eval(parse(text = model_list[i])))
}

# Percent deviance  
dev.ref <- -2*LL.rw
dev.vec <- -2*Lik.vec
pc.dev.vec <- 100*(dev.ref-dev.vec)/dev.ref


# AIC all models  

AIC <- AICctab(r_mRW_abva_s, 
               r_mEX_abva_s, 
               r_mGL_abva_s, 
               M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18,
               M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32,
               delta = TRUE, weights = TRUE, nobs=85, sort = FALSE)
class(AIC) <- "data.frame"


# BIC all models
BIC <- bbmle::BICtab(r_mRW_abva_s, 
                     r_mEX_abva_s, 
                     r_mGL_abva_s,
                     M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18,
                     M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32,
                     delta = TRUE, weights = TRUE, nobs=85, sort = FALSE)
class(BIC) <- "data.frame"


# Create table and sort by wAIC
summary_table <- tibble(LogLik = Lik.vec,
                        k = AIC$df,
                        dAICc = AIC$dAICc,
                        wAICc = AIC$weight,
                        dBIC = BIC$dBIC,
                        wBIC = BIC$weight,
                        pcdev = pc.dev.vec) %>% 
  add_column(model_list)

regMat_df <- as_tibble(regMat) %>% 
  add_row(BRANv = 0, SST = 0 , SSTgbr2 = 0, SSTgbr3 = 0,  CHL = 0, .before = 1) %>% 
  add_row(BRANv = 0, SST = 0 , SSTgbr2 = 0, SSTgbr3 = 0,  CHL = 0, .before = 1) %>% 
  add_row(BRANv = 0, SST = 0 , SSTgbr2 = 0, SSTgbr3 = 0,  CHL = 0, .before = 1)


summary.table_full_paper <- summary_table %>% bind_cols(regMat_df) %>% 
  arrange(desc(wAICc)) 
summary.table_full_paper
# write.csv(summary.table_full_paper, "summary.table_full_JAE_abva_s_SY.csv")

# Summary top-ranked model
summary(M20)


# EXCLUDE SSTGBR-2
model_list <- c('RW','EX', 'GL', paste0('M', c(5:8)), paste0('M', c(13:16)), paste0('M', c(21:24)), paste0('M', c(29:32)))
# Likelihood vector
Lik.vec <- vector(length=19)
Lik.vec[1] <- as.numeric(logLik(r_mRW_abva_s))
Lik.vec[2] <- as.numeric(logLik(r_mEX_abva_s))
Lik.vec[3] <- as.numeric(logLik(r_mGL_abva_s))
for(i in 4:19){
  Lik.vec[i] <- logLik(eval(parse(text = model_list[i])))
}

# Percent deviance  
dev.ref <- -2*LL.rw
dev.vec <- -2*Lik.vec
pc.dev.vec <- 100*(dev.ref-dev.vec)/dev.ref

# AIC all models  
AIC <- AICctab(r_mRW_abva_s, 
               r_mEX_abva_s, 
               r_mGL_abva_s, 
               M5, M6, M7, M8, M13, M14, M15, M16, M21, M22, M23, M24, M29, M30, M31, M32,
               delta = TRUE, weights = TRUE, nobs=85, sort = FALSE)
class(AIC) <- "data.frame"

# BIC all models
BIC <- bbmle::BICtab(r_mRW_abva_s, 
                     r_mEX_abva_s, 
                     r_mGL_abva_s,
                     M5, M6, M7, M8, M13, M14, M15, M16, M21, M22, M23, M24, M29, M30, M31, M32,
                     delta = TRUE, weights = TRUE, nobs=85, sort = FALSE)
class(BIC) <- "data.frame"

# Create table and sort by wAIC
summary_table <- tibble(LogLik = Lik.vec,
                        k = AIC$df,
                        dAICc = AIC$dAICc,
                        wAICc = AIC$weight,
                        dBIC = BIC$dBIC,
                        wBIC = BIC$weight,
                        pcdev = pc.dev.vec) %>% 
  add_column(model_list)

regMat_df <- as_tibble(regMat) %>% filter(SSTgbr2 == 0) %>% 
  add_row(BRANv = 0, SST = 0 , SSTgbr2 = 0, SSTgbr3 = 0,  CHL = 0, .before = 1) %>% 
  add_row(BRANv = 0, SST = 0 , SSTgbr2 = 0, SSTgbr3 = 0,  CHL = 0, .before = 1) %>% 
  add_row(BRANv = 0, SST = 0 , SSTgbr2 = 0, SSTgbr3 = 0,  CHL = 0, .before = 1)


summary.table_full_s_paper_19M <- summary_table %>% bind_cols(regMat_df) %>% 
  arrange(desc(wAICc))
# write.csv(summary.table_full_s_paper_19M, "summary.table_full_JAE_abva_s_19M_SY.csv")

# Summary top-ranked model
summary(eval(parse(text = summary.table_full_s_paper_19M$model_list[1])))
summary(r_mGL_abva_s) 
summary(r_mEX_abva_s)
summary(r_mRW_abva_s)
top1_mod_settlers <- M22
AIC_top1_mod_settlers <- AIC['M22',]$weight
top2_mod_settlers <- M21
AIC_top2_mod_settlers <- AIC['M21',]$weight
top3_mod_settlers <- M6
AIC_top3_mod_settlers <- AIC['M6',]$weight
data_setters <- d


# JUVENILES ---------------------------------------------------------------

# Prove that we can't link the two life stages ----------------------------

# 1. effect of settlers' r on the top-ranked model of juveniles ####
# All the models below need to be computed for these lines to work!!

d_sett_r <- data_setters %>% dplyr::select(r) %>% 
  rename(r_sett = r)

d_juv_sett <- data_juveniles %>% bind_cols(d_sett_r)

inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M30_Sett_r <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSST +
                               gamma*r_sett +
                               phi*qt, sd=sigma),
                   parnames = list("rm","logK", "beta","gamma", "phi","sigma"),
                   start = inits, method = 'Nelder-Mead', data=d_juv_sett, control = list(maxit = 10000))
summary(M30_Sett_r)

AICctab(M30_Sett_r, top1_mod_juveniles,
        delta = TRUE, weights = TRUE, nobs=85)



# . Negative log likelihood functions  ####

# .. Define model variables ----
date <- denL_env$date[1:86]
r <- denL_env$rabva[1:86]
logN <- log(denL_env$Nabva_1[1:86])
logN_2 <- log(denL_env$Nabva_2[1:86])
sBRANv <- scale(denL_env$BRANv_1[1:86]) # scaled BRANv
sSST <- scale(denL_env$sst_1[1:86]) # scaled sst
sChla <- scale(denL_env$chla_1[1:86]) # scaled chla
sChla_GBR_2 <- scale(denL_env$chla_SWAIN_2[1:86]) # scaled chla SWAIN t-2 sst
sChla_GBR_3 <- scale(denL_env$chla_SWAIN_3[1:86]) # scaled chla SWAIN t-3 sst
sSWAIN_SST_2 <- scale(denL_env$sst_SWAIN_2[1:86]) # scaled SWAIN_2 sst
sSWAIN_SST_3 <- scale(denL_env$sst_SWAIN_3[1:86]) # scaled SWAIN_3 sst
r_abvaL <- scale(denL_env$rabva)[1:86]

qt <- denL_env$qt[1:86]
d <- tibble(date = date, r=r,logN=logN, logN2=logN_2, sBRANv=sBRANv, sSST=sSST, sChla=sChla, 
            sSWAIN_SST_3=sSWAIN_SST_3, sSWAIN_SST_2=sSWAIN_SST_2, qt=qt, r_abvaL = r_abvaL)   

# .. Test linearity of the effect of season  ----

summary(lm(d$r~d$qt))


# png("linearMod_Qt_vs_r_Juveniles.png", width = 5, height = 4, units = 'in', res = 600)
d %>% 
  ggplot(aes(x = qt, y = r)) +
  geom_smooth(method = 'lm') +
  geom_jitter(shape = 21, width = 0, size = 4) +
  annotate("text", x=4, y=4.5, label = "italic(R) ^ 2 == 0.11", parse = TRUE, hjust=1, size = 4)+
  annotate("text", x=4, y=3.8, label = "p == 0.001", parse = TRUE, hjust=1) +
  theme_bw() +
  theme(axis.title.x=element_text(size=16, family="Arial", colour="black", vjust=-0.1),
        axis.title.y=element_text(size=16, family="Arial", colour="black", vjust=2),
        axis.text.x = element_text(size=12, colour="black"),
        axis.text.y = element_text(size=12, colour="black"),
        plot.margin = unit(c(2, 0.2, 2, 2), "line"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  ylab("r") +
  xlab("Season (quarter)") 
# dev.off()


# .. MODEL RW - random walk, rm=0: r=0+error ----      
# Define inits
inits_r_mRW_abva_l <- list(rm=0, sigma = 1)
# Find values that minimize the NegLogLik
r_mRW_abva_l <- mle2(r ~ dnorm(mean=rm, sd=sigma), 
                     parnames = list("rm","sigma"),
                     start = inits_r_mRW_abva_l, 
                     fixed=list(rm=0), method = "L-BFGS-B", lower = c(-10), upper = c(10), data=d)
summary(r_mRW_abva_l) 
AIC(r_mRW_abva_l)
LL.rw <- as.numeric(logLik(r_mRW_abva_l))

# .. MODEL EX - exponential growth, K=Inf: r = rm + error ----      
# Define inits
inits_r_mEX_abva_l <- list(rm=0, sigma = 1)
# Find values that minimize the NegLogLik
r_mEX_abva_l <- mle2(r ~ dnorm(mean=rm, sd=sigma), 
                     start = inits_r_mEX_abva_l, 
                     method = "L-BFGS-B", lower = c(-10), upper = c(10), data=d)
summary(r_mEX_abva_l) 
AIC(r_mEX_abva_l)
LL.mEX <- as.numeric(logLik(r_mEX_abva_l))

# .. MODEL GL - Gompertz Logistic simple: r=rm(1-(logN/logK))+error ----    
# Define inits
inits_r_mGL_abva_l <- list(rm = 1, logK = 1, sigma = 1)
# Find values that minimize the NegLogLik
r_mGL_abva_l <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)), sd=sigma), parnames = list("rm","logK","sigma"), 
                     start = inits_r_mGL_abva_l, method = 'Nelder-Mead', data=d)
summary(r_mGL_abva_l) 
AIC(r_mGL_abva_l)
LL.mGL <- as.numeric(logLik(r_mGL_abva_l))

# .. GL MODELS with combinations of env variables ----    
# Possible model combinations
#2^5=32

regMat <- expand.grid(c(1,0), c(1,0), c(1,0), c(1,0),  c(1,0))
names(regMat) <- c('BRANv','SST','SSTgbr2', 'SSTgbr3','CHL')

# M1
regMat[1,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1,
              epsilon_2 = 1, epsilon_3 = 1, phi=-0.7, sigma=1)
M1 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSST + kappa*sChla + 
                       epsilon_2*sSWAIN_SST_2+epsilon_3*sSWAIN_SST_3+phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa","epsilon_2","epsilon_3","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M2
regMat[2,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, epsilon = 1, phi=-0.7, sigma=1)
M2 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSST + gamma*sSWAIN_SST_2 +
                       kappa*sSWAIN_SST_3 + epsilon*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa","epsilon","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M3
regMat[3,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, epsilon = 1, phi=-0.7, sigma=1)
M3 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sSWAIN_SST_2 +
                       kappa*sSWAIN_SST_3 + epsilon*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa","epsilon","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M4
regMat[4,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M4 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSWAIN_SST_2 + gamma*sSWAIN_SST_3 + kappa*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M5
regMat[5,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, epsilon = 1, phi=-0.7, sigma=1)
M5 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sSST + kappa*sSWAIN_SST_3 + epsilon*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa", "epsilon","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M6
regMat[6,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M6 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSST + gamma*sSWAIN_SST_3 + kappa*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M7
regMat[7,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M7 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sSWAIN_SST_3 + kappa*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M8
regMat[8,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M8 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSWAIN_SST_3 + gamma*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M9
regMat[9,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, epsilon = 1, phi=-0.7, sigma=1)
M9 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sSST + kappa*sSWAIN_SST_2 + epsilon*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa", "epsilon","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M10
regMat[10,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M10 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSST + gamma*sSWAIN_SST_2 + kappa*sChla +
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma","kappa", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M11
regMat[11,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M11 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sSWAIN_SST_2 + kappa*sChla +
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma","kappa", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M12
regMat[12,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M12 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSWAIN_SST_2 + gamma*sChla +
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M13
regMat[13,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M13 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sSST + kappa*sChla +
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma", "kappa", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M14
regMat[14,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M14 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSST + gamma*sChla + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M15
regMat[15,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M15 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sChla + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M16
regMat[16,]
inits <- list(rm = 1, logK=1, beta = 1, phi=-0.7, sigma=1)
M16 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sChla +
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M17
regMat[17,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, epsilon = 1, phi=-0.7, sigma=1)
M17 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSST + kappa*sSWAIN_SST_2 + epsilon*sSWAIN_SST_3 +
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma","kappa","epsilon","phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M18
regMat[18,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M18 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSST + gamma*sSWAIN_SST_2 + kappa*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma","kappa","phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M19
regMat[19,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M19 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSWAIN_SST_2 + kappa*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma","kappa","phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M20
regMat[20,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M20 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSWAIN_SST_2 + gamma*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma","phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M21
regMat[21,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M21 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSST + kappa*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma", "kappa", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M22
regMat[22,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M22 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSST + gamma*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma",  "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M23
regMat[23,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M23 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma",  "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M24
regMat[24,]
inits <- list(rm = 1, logK=1, beta = 1, phi=-0.7, sigma=1)
M24 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta",  "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M25
regMat[25,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M25 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSST + kappa*sSWAIN_SST_2 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "gamma", "kappa", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M26
regMat[26,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M26 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSST + gamma*sSWAIN_SST_2 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "gamma", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M27
regMat[27,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M27 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSWAIN_SST_2 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "gamma", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M28
regMat[28,]
inits <- list(rm = 1, logK=1, beta = 1,  phi=-0.7, sigma=1)
M28 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSWAIN_SST_2 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta",  "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M29
regMat[29,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M29 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSST + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "gamma", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M30
regMat[30,]
inits <- list(rm = 1, logK=1, beta = 1, phi=-0.7, sigma=1)
M30 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSST + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M31
regMat[31,]
inits <- list(rm = 1, logK=1, beta = 1, phi=-0.7, sigma=1)
M31 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M32
regMat[32,]
inits <- list(rm = 1, logK=1, phi=-0.7, sigma=1)
M32 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# . Ranking of models ####################################################################################################

# ALL MODELS FOR PAPER  #      

# Likelihood vector
model_list <- c('RW','EX', 'GL', paste0('M', c(1:32)))
Lik.vec <- vector(length=35)
Lik.vec[1] <- as.numeric(logLik(r_mRW_abva_l))
Lik.vec[2] <- as.numeric(logLik(r_mEX_abva_l))
Lik.vec[3] <- as.numeric(logLik(r_mGL_abva_l))
for(i in 4:35){
  Lik.vec[i] <- logLik(eval(parse(text = model_list[i])))
}

# Percent deviance  
dev.ref <- -2*LL.rw
dev.vec <- -2*Lik.vec
pc.dev.vec <- 100*(dev.ref-dev.vec)/dev.ref


# AIC all models  

AIC <- AICctab(r_mRW_abva_l, 
               r_mEX_abva_l, 
               r_mGL_abva_l, 
               M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18,
               M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32,
               delta = TRUE, weights = TRUE, nobs=85, sort = FALSE)
class(AIC) <- "data.frame"


# BIC all models
BIC <- bbmle::BICtab(r_mRW_abva_l, 
                     r_mEX_abva_l, 
                     r_mGL_abva_l,
                     M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18,
                     M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32,
                     delta = TRUE, weights = TRUE, nobs=85, sort = FALSE)
class(BIC) <- "data.frame"


# Create table and sort by wAIC
summary_table <- tibble(LogLik = Lik.vec,
                        k = AIC$df,
                        dAICc = AIC$dAICc,
                        wAICc = AIC$weight,
                        dBIC = BIC$dBIC,
                        wBIC = BIC$weight,
                        pcdev = pc.dev.vec) %>% 
  add_column(model_list)

regMat_df <- as_tibble(regMat) %>% 
  add_row(BRANv = 0, SST = 0 , SSTgbr2 = 0, SSTgbr3 = 0,  CHL = 0, .before = 1) %>% 
  add_row(BRANv = 0, SST = 0 , SSTgbr2 = 0, SSTgbr3 = 0,  CHL = 0, .before = 1) %>% 
  add_row(BRANv = 0, SST = 0 , SSTgbr2 = 0, SSTgbr3 = 0,  CHL = 0, .before = 1)


summary.table_full_paper <- summary_table %>% bind_cols(regMat_df) %>% 
  arrange(desc(wAICc)) 
summary.table_full_paper
# write.csv(summary.table_full_paper, "summary.table_full_JAE_abva_l_SY.csv")


# EXCLUDE SSTGBR-2
model_list <- c('RW','EX', 'GL', paste0('M', c(5:8)), paste0('M', c(13:16)), paste0('M', c(21:24)), paste0('M', c(29:32)))
# Likelihood vector
Lik.vec <- vector(length=19)
Lik.vec[1] <- as.numeric(logLik(r_mRW_abva_l))
Lik.vec[2] <- as.numeric(logLik(r_mEX_abva_l))
Lik.vec[3] <- as.numeric(logLik(r_mGL_abva_l))
for(i in 4:19){
  Lik.vec[i] <- logLik(eval(parse(text = model_list[i])))
}

# Percent deviance  
dev.ref <- -2*LL.rw
dev.vec <- -2*Lik.vec
pc.dev.vec <- 100*(dev.ref-dev.vec)/dev.ref

# AIC all models  
AIC <- AICctab(r_mRW_abva_l, 
               r_mEX_abva_l, 
               r_mGL_abva_l, 
               M5, M6, M7, M8, M13, M14, M15, M16, M21, M22, M23, M24, M29, M30, M31, M32,
               delta = TRUE, weights = TRUE, nobs=85, sort = FALSE)
class(AIC) <- "data.frame"

# BIC all models
BIC <- bbmle::BICtab(r_mRW_abva_l, 
                     r_mEX_abva_l, 
                     r_mGL_abva_l,
                     M5, M6, M7, M8, M13, M14, M15, M16, M21, M22, M23, M24, M29, M30, M31, M32,
                     delta = TRUE, weights = TRUE, nobs=85, sort = FALSE)
class(BIC) <- "data.frame"

# Create table and sort by wAIC
summary_table <- tibble(LogLik = Lik.vec,
                        k = AIC$df,
                        dAICc = AIC$dAICc,
                        wAICc = AIC$weight,
                        dBIC = BIC$dBIC,
                        wBIC = BIC$weight,
                        pcdev = pc.dev.vec) %>% 
  add_column(model_list)

regMat_df <- as_tibble(regMat) %>% filter(SSTgbr2 == 0) %>% 
  add_row(BRANv = 0, SST = 0 , SSTgbr2 = 0, SSTgbr3 = 0,  CHL = 0, .before = 1) %>% 
  add_row(BRANv = 0, SST = 0 , SSTgbr2 = 0, SSTgbr3 = 0,  CHL = 0, .before = 1) %>% 
  add_row(BRANv = 0, SST = 0 , SSTgbr2 = 0, SSTgbr3 = 0,  CHL = 0, .before = 1)


summary.table_full_paper_19M <- summary_table %>% bind_cols(regMat_df) %>% 
  arrange(desc(wAICc))
# write.csv(summary.table_full_paper_19M, "summary.table_full_JAE_abva_l_19M_SY.csv")

# Summary top-ranked model
summary(eval(parse(text = summary.table_full_paper_19M$model_list[15])))
summary(r_mGL_abva_l) 
summary(r_mEX_abva_l)
summary(r_mRW_abva_l)
top1_mod_juveniles <- M30
AIC_top1_mod_juveniles <- AIC['M30',]$weight
top2_mod_juveniles <- M14
AIC_top2_mod_juveniles <- AIC['M14',]$weight
top3_mod_juveniles <- M29
AIC_top3_mod_juveniles <- AIC['M29',]$weight
data_juveniles <- d


# SETTLERS AND JUVENILES --------------------------------------------------

# . Negative log likelihood functions  ####

# .. Define model variables ----
date <- den_S_L_env$date[1:86]
r <- den_S_L_env$rabva[1:86]
logN <- log(den_S_L_env$Nabva_1[1:86])
logN_2 <- log(den_S_L_env$Nabva_2[1:86])
sBRANv <- scale(den_S_L_env$BRANv_1[1:86]) # scaled BRANv
sSST <- scale(den_S_L_env$sst_1[1:86]) # scaled sst
sChla <- scale(den_S_L_env$chla_1[1:86]) # scaled chla
sChla_GBR_2 <- scale(den_S_L_env$chla_SWAIN_2[1:86]) # scaled chla SWAIN t-2 sst
sChla_GBR_3 <- scale(den_S_L_env$chla_SWAIN_3[1:86]) # scaled chla SWAIN t-3 sst
sSWAIN_SST_2 <- scale(den_S_L_env$sst_SWAIN_2[1:86]) # scaled SWAIN_2 sst
sSWAIN_SST_3 <- scale(den_S_L_env$sst_SWAIN_3[1:86]) # scaled SWAIN_3 sst

qt <- den_S_L_env$qt[1:86]
d <- tibble(date = date, r=r,logN=logN, logN2=logN_2, sBRANv=sBRANv, sSST=sSST, sChla=sChla, 
            sSWAIN_SST_3=sSWAIN_SST_3, sSWAIN_SST_2=sSWAIN_SST_2, qt=qt)   


# .. MODEL RW - random walk, rm=0: r=0+error ----      
# Define inits
inits_r_mRW_abva_s_l <- list(rm=0, sigma = 10)
# Find values that minimize the NegLogLik
r_mRW_abva_s_l <- mle2(r ~ dnorm(mean=rm, sd=sigma), 
                       parnames = list("rm","sigma"),
                       start = inits_r_mRW_abva_s_l, 
                       fixed=list(rm=0), method = "L-BFGS-B", lower = c(-10), upper = c(10), data=d)
summary(r_mRW_abva_s_l) 
AIC(r_mRW_abva_s_l)
LL.rw <- as.numeric(logLik(r_mRW_abva_s_l))

# .. MODEL EX - exponential growth, K=Inf: r = rm + error ----      
# Define inits
inits_r_mEX_abva_s_l <- list(rm=0, sigma = 1)
# Find values that minimize the NegLogLik
r_mEX_abva_s_l <- mle2(r ~ dnorm(mean=rm, sd=sigma), start = inits_r_mEX_abva_s_l, 
                       method = "L-BFGS-B", lower = c(-10), upper = c(10), data=d)
summary(r_mEX_abva_s_l) 
AIC(r_mEX_abva_s_l)
LL.ex <- as.numeric(logLik(r_mEX_abva_s_l))

# .. MODEL GL - Gompertz Logistic simple: r=rm(1-(logN/logK))+error ----    
# Define inits
inits_r_mGL_abva_s_l <- list(rm = 1, logK = 1, sigma = 1)
# Find values that minimize the NegLogLik
r_mGL_abva_s_l <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)), sd=sigma), parnames = list("rm","logK","sigma"), 
                       start = inits_r_mGL_abva_s_l, method = 'Nelder-Mead', data=d)
summary(r_mGL_abva_s_l) 
AIC(r_mGL_abva_s_l)
LL.mGL <- as.numeric(logLik(r_mGL_abva_s_l))

# .. GL MODELS with combinations of env variables ----    
# Possible model combinations
#2^5=32

regMat <- expand.grid(c(1,0), c(1,0), c(1,0), c(1,0),  c(1,0))
names(regMat) <- c('BRANv','SST','SSTgbr2', 'SSTgbr3','CHL')

# M1
regMat[1,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1,
              epsilon_2 = 1, epsilon_3 = 1, phi=-0.7, sigma=1)
M1 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSST + kappa*sChla + 
                       epsilon_2*sSWAIN_SST_2+epsilon_3*sSWAIN_SST_3+phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa","epsilon_2","epsilon_3","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M2
regMat[2,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, epsilon = 1, phi=-0.7, sigma=1)
M2 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSST + gamma*sSWAIN_SST_2 +
                       kappa*sSWAIN_SST_3 + epsilon*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa","epsilon","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M3
regMat[3,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, epsilon = 1, phi=-0.7, sigma=1)
M3 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sSWAIN_SST_2 +
                       kappa*sSWAIN_SST_3 + epsilon*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa","epsilon","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M4
regMat[4,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M4 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSWAIN_SST_2 + gamma*sSWAIN_SST_3 + kappa*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M5
regMat[5,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, epsilon = 1, phi=-0.7, sigma=1)
M5 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sSST + kappa*sSWAIN_SST_3 + epsilon*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa", "epsilon","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M6
regMat[6,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M6 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSST + gamma*sSWAIN_SST_3 + kappa*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M7
regMat[7,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M7 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sSWAIN_SST_3 + kappa*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M8
regMat[8,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M8 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSWAIN_SST_3 + gamma*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M9
regMat[9,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, epsilon = 1, phi=-0.7, sigma=1)
M9 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sSST + kappa*sSWAIN_SST_2 + epsilon*sChla +
                       phi*qt, sd=sigma),
           parnames = list("rm","logK", "beta","gamma","kappa", "epsilon","phi","sigma"),
           start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M10
regMat[10,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M10 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSST + gamma*sSWAIN_SST_2 + kappa*sChla +
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma","kappa", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M11
regMat[11,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M11 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sSWAIN_SST_2 + kappa*sChla +
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma","kappa", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M12
regMat[12,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M12 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSWAIN_SST_2 + gamma*sChla +
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M13
regMat[13,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M13 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sSST + kappa*sChla +
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma", "kappa", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M14
regMat[14,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M14 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sSST + gamma*sChla + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M15
regMat[15,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M15 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sBRANv + gamma*sChla + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M16
regMat[16,]
inits <- list(rm = 1, logK=1, beta = 1, phi=-0.7, sigma=1)
M16 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK)) + beta*sChla +
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M17
regMat[17,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, epsilon = 1, phi=-0.7, sigma=1)
M17 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSST + kappa*sSWAIN_SST_2 + epsilon*sSWAIN_SST_3 +
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma","kappa","epsilon","phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M18
regMat[18,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M18 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSST + gamma*sSWAIN_SST_2 + kappa*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma","kappa","phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M19
regMat[19,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M19 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSWAIN_SST_2 + kappa*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma","kappa","phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M20
regMat[20,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M20 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSWAIN_SST_2 + gamma*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma","phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M21
regMat[21,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M21 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSST + kappa*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma", "kappa", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M22
regMat[22,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M22 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSST + gamma*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma",  "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M23
regMat[23,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M23 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta","gamma",  "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M24
regMat[24,]
inits <- list(rm = 1, logK=1, beta = 1, phi=-0.7, sigma=1)
M24 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSWAIN_SST_3 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta",  "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M25
regMat[25,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, kappa = 1, phi=-0.7, sigma=1)
M25 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSST + kappa*sSWAIN_SST_2 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "gamma", "kappa", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M26
regMat[26,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M26 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSST + gamma*sSWAIN_SST_2 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "gamma", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M27
regMat[27,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M27 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSWAIN_SST_2 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "gamma", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M28
regMat[28,]
inits <- list(rm = 1, logK=1, beta = 1,  phi=-0.7, sigma=1)
M28 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSWAIN_SST_2 + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta",  "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M29
regMat[29,]
inits <- list(rm = 1, logK=1, beta = 1, gamma = 1, phi=-0.7, sigma=1)
M29 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + gamma*sSST + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "gamma", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M30
regMat[30,]
inits <- list(rm = 1, logK=1, beta = 1, phi=-0.7, sigma=1)
M30 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sSST + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M31
regMat[31,]
inits <- list(rm = 1, logK=1, beta = 1, phi=-0.7, sigma=1)
M31 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+beta*sBRANv + 
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "beta", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# M32
regMat[32,]
inits <- list(rm = 1, logK=1, phi=-0.7, sigma=1)
M32 <- mle2(r ~ dnorm(mean=rm * (1-(logN/logK))+
                        phi*qt, sd=sigma),
            parnames = list("rm","logK", "phi","sigma"),
            start = inits, method = 'Nelder-Mead', data=d, control = list(maxit = 10000))

# . Ranking of models ####################################################################################################

# ALL MODELS FOR PAPER  #      

# Likelihood vector
model_list <- c('RW','EX', 'GL', paste0('M', c(1:32)))
Lik.vec <- vector(length=35)
Lik.vec[1] <- as.numeric(logLik(r_mRW_abva_s_l))
Lik.vec[2] <- as.numeric(logLik(r_mEX_abva_s_l))
Lik.vec[3] <- as.numeric(logLik(r_mGL_abva_s_l))
for(i in 4:35){
  Lik.vec[i] <- logLik(eval(parse(text = model_list[i])))
}

# Percent deviance  
dev.ref <- -2*LL.rw
dev.vec <- -2*Lik.vec
pc.dev.vec <- 100*(dev.ref-dev.vec)/dev.ref


# AIC all models  

AIC <- AICctab(r_mRW_abva_s_l, 
               r_mEX_abva_s_l, 
               r_mGL_abva_s_l, 
               M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18,
               M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32,
               delta = TRUE, weights = TRUE, nobs=85, sort = FALSE)
class(AIC) <- "data.frame"


# BIC all models
BIC <- bbmle::BICtab(r_mRW_abva_s_l, 
                     r_mEX_abva_s_l, 
                     r_mGL_abva_s_l,
                     M1, M2, M3, M4, M5, M6, M7, M8, M9, M10, M11, M12, M13, M14, M15, M16, M17, M18,
                     M19, M20, M21, M22, M23, M24, M25, M26, M27, M28, M29, M30, M31, M32,
                     delta = TRUE, weights = TRUE, nobs=85, sort = FALSE)
class(BIC) <- "data.frame"


# Create table and sort by wAIC
summary_table <- tibble(LogLik = Lik.vec,
                        k = AIC$df,
                        dAICc = AIC$dAICc,
                        wAICc = AIC$weight,
                        dBIC = BIC$dBIC,
                        wBIC = BIC$weight,
                        pcdev = pc.dev.vec) %>% 
  add_column(model_list)

regMat_df <- as_tibble(regMat) %>% 
  add_row(BRANv = 0, SST = 0 , SSTgbr2 = 0, SSTgbr3 = 0,  CHL = 0, .before = 1) %>% 
  add_row(BRANv = 0, SST = 0 , SSTgbr2 = 0, SSTgbr3 = 0,  CHL = 0, .before = 1) %>% 
  add_row(BRANv = 0, SST = 0 , SSTgbr2 = 0, SSTgbr3 = 0,  CHL = 0, .before = 1)


summary.table_full_paper <- summary_table %>% bind_cols(regMat_df) %>% 
  arrange(desc(wAICc)) 
summary.table_full_paper
# write.csv(summary.table_full_paper, "summary.table_full_JAE_abva_s_l_SY.csv")


# EXCLUDE SSTGBR-2
model_list <- c('RW','EX', 'GL', paste0('M', c(5:8)), paste0('M', c(13:16)), paste0('M', c(21:24)), paste0('M', c(29:32)))
# Likelihood vector
Lik.vec <- vector(length=19)
Lik.vec[1] <- as.numeric(logLik(r_mRW_abva_s_l))
Lik.vec[2] <- as.numeric(logLik(r_mEX_abva_s_l))
Lik.vec[3] <- as.numeric(logLik(r_mGL_abva_s_l))
for(i in 4:19){
  Lik.vec[i] <- logLik(eval(parse(text = model_list[i])))
}

# Percent deviance  
dev.ref <- -2*LL.rw
dev.vec <- -2*Lik.vec
pc.dev.vec <- 100*(dev.ref-dev.vec)/dev.ref

# AIC all models  
AIC <- AICctab(r_mRW_abva_s_l, 
               r_mEX_abva_s_l, 
               r_mGL_abva_s_l, 
               M5, M6, M7, M8, M13, M14, M15, M16, M21, M22, M23, M24, M29, M30, M31, M32,
               delta = TRUE, weights = TRUE, nobs=85, sort = FALSE)
class(AIC) <- "data.frame"

# BIC all models
BIC <- bbmle::BICtab(r_mRW_abva_s_l, 
                     r_mEX_abva_s_l, 
                     r_mGL_abva_s_l,
                     M5, M6, M7, M8, M13, M14, M15, M16, M21, M22, M23, M24, M29, M30, M31, M32,
                     delta = TRUE, weights = TRUE, nobs=85, sort = FALSE)
class(BIC) <- "data.frame"

# Create table and sort by wAIC
summary_table <- tibble(LogLik = Lik.vec,
                        k = AIC$df,
                        dAICc = AIC$dAICc,
                        wAICc = AIC$weight,
                        dBIC = BIC$dBIC,
                        wBIC = BIC$weight,
                        pcdev = pc.dev.vec) %>% 
  add_column(model_list)

regMat_df <- as_tibble(regMat) %>% filter(SSTgbr2 == 0) %>% 
  add_row(BRANv = 0, SST = 0 , SSTgbr2 = 0, SSTgbr3 = 0,  CHL = 0, .before = 1) %>% 
  add_row(BRANv = 0, SST = 0 , SSTgbr2 = 0, SSTgbr3 = 0,  CHL = 0, .before = 1) %>% 
  add_row(BRANv = 0, SST = 0 , SSTgbr2 = 0, SSTgbr3 = 0,  CHL = 0, .before = 1)


summary.table_full_JAE_abva_s_l_19M_SY <- summary_table %>% bind_cols(regMat_df) %>% 
  arrange(desc(wAICc))
# write.csv(summary.table_full_JAE_abva_s_l_19M_SY, "summary.table_full_JAE_abva_s_l_19M_SY.csv")

# Summary top-ranked model
summary(eval(parse(text = summary.table_full_paper_19M$model_list[16])))
summary(r_mGL_abva_s_l) 
summary(r_mEX_abva_s_l)
summary(r_mRW_abva_s_l)
r_top_abva_s_l <- M32

# PARTIAL RESIDUAL PLOTS --------------------------------------------------

# .. Model for settlers and juveniles aggregated ####

# No environmental predictors highlighted in this model

# .. Model for settlers, SST GBR-3 ####
res <- residuals(top1_mod_settlers)
gamma.fit <- top1_mod_settlers@coef[['gamma']]
gamma.fit_res <- gamma.fit * data_setters$sSWAIN_SST_3
pres <- res + gamma.fit_res
tmp_d_setllers <- tibble(pres = pres, sst_SWAIN_3 = denS_env$sst_SWAIN_3[1:86])

lm <- lm(tmp_d_setllers$pres ~ tmp_d_setllers$sst_SWAIN_3)
summary(lm)
-coef(lm)[1]/coef(lm)[2] # Intercept with zero

# png("Fig_SST_GBR_vs_ParRes_abva_Small_JAE.png", width = 6, height = 5, units = 'in', res = 600)
ggplot(tmp_d_setllers, aes(x=sst_SWAIN_3, y=pres))+
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_point(color='#2980B9', size=1.5) +
  geom_smooth(method="lm", color='#2C3E50') +
  annotate("text", x=21.5, y=3, label = "italic(R) ^ 2 == 0.2000", parse = TRUE, hjust=0)+
  annotate("text", x=21.5, y=2.5, label = "p < 0.01", parse = TRUE, hjust=0) +
  scale_x_continuous(limits=c(21.5, 28.5), breaks=seq(22,28,1)) +
  scale_y_continuous(limits=c(-3, 3), breaks=seq(-3,3,1)) +
  theme_bw() +
  theme(axis.title.x=element_text(size=16, family="Arial", colour="black", vjust=-0.1),
        axis.title.y=element_text(size=16, family="Arial", colour="black", vjust=2),
        axis.text.x = element_text(size=12, colour="black"),
        axis.text.y = element_text(size=12, colour="black"),
        plot.margin = unit(c(2, 0.2, 2, 2), "line"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  ylab("Population growth rate partial residual") +
  xlab("SST at the Great Barrier Reef, lag 3 (?C)")
# dev.off()

# .. Model for settlers, SST SYD ####

res <- residuals(top1_mod_settlers)
beta.fit <- top1_mod_settlers@coef[['beta']]
beta.fit_res <- beta.fit * data_setters$sSST
pres <- res + beta.fit_res
tmp_d_setllers <- data_frame(pres = pres, SST = denS_env$sst_1[1:86])

lm <- lm(tmp_d_setllers$pres ~ tmp_d_setllers$SST)
summary(lm)
-coef(lm)[1]/coef(lm)[2] # Intercept with zero

# png("Fig_SST_SYD_vs_ParRes_abva_Small_JAE_Ago2020.png", width = 6, height = 5, units = 'in', res = 600)
ggplot(tmp_d_setllers, aes(x=SST, y=pres))+
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_point(color='#2980B9', size=1.5) +
  geom_smooth(method="lm", color='#2C3E50') +
  annotate("text", x=17, y=3, label = "italic(R) ^ 2 == 0.39", parse = TRUE, hjust=0)+
  annotate("text", x=17, y=2.5, label = "p < 0.001", parse = TRUE, hjust=0) +
  scale_x_continuous(limits=c(17, 24.5), breaks=seq(17,25,1)) +
  scale_y_continuous(limits=c(-3, 3), breaks=seq(-3,3,1)) +
  theme_bw() +
  theme(axis.title.x=element_text(size=16, family="Arial", colour="black", vjust=-0.1),
        axis.title.y=element_text(size=16, family="Arial", colour="black", vjust=2),
        axis.text.x = element_text(size=12, colour="black"),
        axis.text.y = element_text(size=12, colour="black"), 
        plot.margin = unit(c(2, 0.2, 2, 2), "line"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  ylab("Population growth rate partial residual") +
  xlab("SST in Sydney, lag 1 (?C)") 
# dev.off()


# .. Model for juveniles ####
res <- residuals(top1_mod_juveniles)
beta.fit <- top1_mod_juveniles@coef[['beta']]
beta.fit_res <- beta.fit * data_juveniles$sSST
pres <- res + beta.fit_res
tmp_d_juveniles <- data_frame(pres = pres, SST = denL_env$sst_1[1:86])

summary(lm(tmp_d_juveniles$pres ~ tmp_d_juveniles$SST))

# png("Fig_SST_SYD_vs_ParRes_abva_Large_JAE_Ago2020.png", width = 6, height = 5, units = 'in', res = 600)
ggplot(tmp_d_juveniles, aes(x=SST, y=pres))+
  geom_hline(yintercept = 0, linetype = 'dashed') +
  geom_point(color='#2980B9', size=1.5) +
  geom_smooth(method="lm", color='#2C3E50') +
  annotate("text", x=17, y=3, label = "italic(R) ^ 2 == 0.21", parse = TRUE, hjust=0)+
  annotate("text", x=17, y=2.5, label = "p < 0.001", parse = TRUE, hjust=0) +
  scale_x_continuous(limits=c(17, 24.5), breaks=seq(17,25,1)) +
  scale_y_continuous(limits=c(-3, 3), breaks=seq(-3,3,1)) +
  theme_bw() +
  theme(axis.title.x=element_text(size=16, family="Arial", colour="black", vjust=-0.1),
        axis.title.y=element_text(size=16, family="Arial", colour="black", vjust=2),
        axis.text.x = element_text(size=12, colour="black"),
        axis.text.y = element_text(size=12, colour="black"), 
        plot.margin = unit(c(2, 0.2, 2, 2), "line"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  ylab("Population growth rate partial residual") +
  xlab("SST in Sydney, lag 1 (?C)") 
# dev.off()


# PREDS RCP4.5 AND RCP8.5 -------------------------------------

# Get 'present' reference SSTswain - Jan 2006    
sst2006_gbr <- denS_env_full$sst_SWAIN_3[73]
sst2006_syd <- denS_env_full$sst_1[73]

# Get SST projections data (temperature anomalies from year 2006)    
monthly_SST_anom_RCP4.5 <- read_csv("data/extracted_monthly_SST_RCP4.5_ACCESS.csv")
monthly_SST_anom_RCP8.5 <- read_csv("data/extracted_monthly_SST_RCP8.5_ACCESS.csv")

sst_projRCP45_gbr <- monthly_SST_anom_RCP4.5 %>% filter(site == 'SWAIN')
sst_projRCP45_syd <- monthly_SST_anom_RCP4.5 %>% filter(site == 'SYSW')
sst_projRCP85_gbr <- monthly_SST_anom_RCP8.5 %>% filter(site == 'SWAIN')
sst_projRCP85_syd <- monthly_SST_anom_RCP8.5 %>% filter(site == 'SYSW')

# Integrate sst projections (anomalies) with present (Jan 2006) data
sst_projRCP45_gbr_abs <- sst_projRCP45_gbr$SST + 26.5 # scaling value determined visualy b/c temp used for model are off from the projections

sst_projRCP85_gbr_abs <- sst_projRCP85_gbr$SST + 26.5
plot(sst_projRCP85_gbr_abs, type='b', col='red')
lines(sst_projRCP45_gbr_abs, col='blue')

# .. Plot temp projections vs Time ---------------------------------

sst_gbr_projRCP45 <- data_frame(model = "RCP 4.5", temp = sst_projRCP45_gbr_abs, date = sst_projRCP45_gbr$year)
sst_gbr_projRCP85 <- data_frame(model = "RCP 8.5", temp = sst_projRCP85_gbr_abs, date = sst_projRCP85_gbr$time)
sst_gbr_projRCPs <- bind_rows(sst_gbr_projRCP45, sst_gbr_projRCP85)

# png("sst_gbr_RCPs.png", width = 5, height = 4, units = 'in', res = 600)
ggplot(data = sst_gbr_projRCPs, aes(x=date, y=temp, colour = model)) +
  geom_line(alpha = 0.5) +
  scale_colour_manual(values = c('blue', 'red')) +
  geom_smooth(size = 1.5) +
  # scale_size_manual(values = c(2, 2)) +
  # scale_shape_manual(values = c(19, 2)) +
  # scale_x_date(labels = date_format("%Y"), limits = as.Date(c('2010-01-01', '2100-01-01'))) +
  scale_y_continuous(limits=c(17, 31), breaks=seq(16, 31, 2)) +
  theme_bw() +
  theme(axis.title.x=element_text(size=16, family="Arial", colour="black", vjust=-0.1),
        axis.title.y=element_text(size=16, family="Arial", colour="black", vjust=2),
        axis.text.x = element_text(size=12, colour="black"),
        axis.text.y = element_text(size=12, colour="black"), 
        plot.margin = unit(c(2, 0.2, 2, 2), "line"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.15, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(size=12)) +
  ylab("Projected SST (?C)") +
  xlab("Time") 
# dev.off()


plot(sst_projRCP85_gbr_abs)
lines(denS_env_full$sst_SWAIN_3[1:72], col = 'red')

t <- c(denS_env_full$sst_SWAIN_3[1:72], sst_projRCP85_gbr_abs) 
plot(t, type='b')

sst_projRCP45_syd_abs <- sst_projRCP45_syd$SST + 23
plot(sst_projRCP45_syd_abs, type='b')

t <- c(denS_env_full$sst_1[1:72], sst_projRCP45_syd_abs) 
plot(t, type='b')

sst_projRCP85_syd_abs <- sst_projRCP85_syd$SST + 23
plot(sst_projRCP85_syd_abs, type='b', col='red')
lines(sst_projRCP45_syd_abs, col='blue')

# .. Plot temp projections vs Time ---------------------------------

sst_syd_projRCP45 <- data_frame(model = "RCP 4.5", temp = sst_projRCP45_syd_abs, date = sst_projRCP45_syd$year)
sst_syd_projRCP85 <- data_frame(model = "RCP 8.5", temp = sst_projRCP85_syd_abs, date = sst_projRCP85_syd$time)
sst_syd_projRCPs <- bind_rows(sst_syd_projRCP45, sst_syd_projRCP85)

# png("sst_syd_RCPs.png", width = 5, height = 4, units = 'in', res = 600)
ggplot(data = sst_syd_projRCPs, aes(x=date, y=temp, colour = model)) +
  geom_line(alpha = 0.5) +
  geom_smooth(size = 1.5) +
  # scale_size_manual(values = c(2, 2)) +
  # scale_shape_manual(values = c(19, 2)) +
  scale_colour_manual(values = c('blue', 'red')) +
  # scale_x_continuous(limits=c(2000, 2100), breaks=seq(2000, 2100, 10)) +
  scale_y_continuous(limits=c(17, 31), breaks=seq(18, 30, 2)) +
  theme_bw() +
  theme(axis.title.x=element_text(size=16, family="Arial", colour="black", vjust=-0.1),
        axis.title.y=element_text(size=16, family="Arial", colour="black", vjust=2),
        axis.text.x = element_text(size=12, colour="black"),
        axis.text.y = element_text(size=12, colour="black"), 
        plot.margin = unit(c(2, 0.2, 2, 2), "line"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  ylab("Projected SST (?C)") +
  xlab("Time") 
# dev.off()




## Scale the Great Barrier Reef test data 
# RCP 4.5 GBR
sstGBR_3_projRCP45_gbr.scaled <- scale(sst_projRCP45_gbr_abs, center = attr(sSWAIN_SST_3, "scaled:center"), 
                                       scale = attr(sSWAIN_SST_3, "scaled:scale"))
sstGBR_3_projRCP45_gbr <- sst_projRCP45_gbr %>% 
  mutate(absSST = sst_projRCP45_gbr_abs, 
         sSST = sstGBR_3_projRCP45_gbr.scaled, 
         qt = quarter(year),
         sSST_3 = lag(sSST, 3))

# RCP 8.5 GBR
sstGBR_3_projRCP85_gbr.scaled <- scale(sst_projRCP85_gbr_abs, center = attr(sSWAIN_SST_3, "scaled:center"), 
                                       scale = attr(sSWAIN_SST_3, "scaled:scale"))
sstGBR_3_projRCP85_gbr <- sst_projRCP85_gbr %>% 
  mutate(absSST = sst_projRCP85_gbr_abs, 
         sSST = sstGBR_3_projRCP85_gbr.scaled, 
         qt = quarter(time),
         sSST_3 = lag(sSST, 3))

## Scale the SYDNEY test data using center and scale from training data set
# RCP 4.5 SYD
sst_projRCP45_syd.scaled <- scale(sst_projRCP45_syd_abs, center = attr(sSST, "scaled:center"), 
                                  scale = attr(sSST, "scaled:scale"))
sst_projRCP45_syd <- sst_projRCP45_syd %>% 
  mutate(absSST = sst_projRCP45_gbr_abs, 
         sSST = sst_projRCP45_syd.scaled, 
         qt = quarter(year),
         sSST_1 = lag(sSST))

# RCP 8.5 SYD
sst_projRCP85_syd.scaled <- scale(sst_projRCP85_syd_abs, center = attr(sSST, "scaled:center"), 
                                  scale = attr(sSST, "scaled:scale"))
sst_projRCP85_syd <- sst_projRCP85_syd %>% 
  mutate(absSST = sst_projRCP85_gbr_abs, 
         sSST = sst_projRCP85_syd.scaled, 
         qt = quarter(time),
         sSST_1 = lag(sSST))


## Make predictions of r using new data ##
env_full <- denS_env_full_SYSW %>% mutate(date = as.Date(date, format = "%Y-%m-%d"),
                                          year = year(date),
                                          month = month(date),
                                          qt = quarter(date)) %>% 
  filter(year %in% c(2004:2017)) %>% 
  dplyr::select(date, BRANv_1, sst_1, chla_1, sst_SWAIN_2, sst_SWAIN_3) %>% 
  pad()

env_full <- env_full %>% 
  mutate(sBRANv_1 = scale(BRANv_1, center = attr(sBRANv, 'scaled:center'), scale = attr(sBRANv, 'scaled:scale')),
         sSST_1 = scale(sst_1, center = attr(sSST, 'scaled:center'), scale = attr(sSST, 'scaled:scale')),
         sChla_1 = scale(chla_1, center = attr(sChla, 'scaled:center'), scale = attr(sChla, 'scaled:scale')),
         sSST_SWAIN_2 = scale(sst_SWAIN_2, center = attr(sSWAIN_SST_2, 'scaled:center'), scale = attr(sSWAIN_SST_2, 'scaled:scale')),
         sSST_SWAIN_3 = scale(sst_SWAIN_3, center = attr(sSWAIN_SST_3, 'scaled:center'), scale = attr(sSWAIN_SST_3, 'scaled:scale')))

# Start in January 2006, using the mean of densities recorded on January
logN_ini <- d %>% filter(month(date) == 1) %>% 
  dplyr::select(logN) %>% 
  summarise(mean_logN_ini = mean(logN, na.rm=T)) %>% unlist()

# Get the mean of densities to run simulations of r
logN_mean <- mean(d$logN)

# FIX env variables. Get mean of every month, and then repeat to fill the 95 years
fx_env_mth <- env_full %>% 
  group_by(month = month(date)) %>% 
  summarise(sBRANv_1 = mean(sBRANv_1, na.rm=TRUE), sChla_1 = mean(sChla_1))
nyears <- 95
fx_env_mth <- bind_rows(replicate(nyears, fx_env_mth, simplify = FALSE))



#  . SETTLERS --------------------------------------------------------------

#  .. Pred GL.SSTSYD.SSTGBR(t-3).S RCP4.5 ---------------------------------
# Run loop
pars <- coef(top1_mod_settlers)
r_new <- vector(length = nrow(sst_projRCP45_syd))

for(i in 4:nrow(sst_projRCP45_syd)){
  r_new[i] <- pars['rm'] * (1-(logN_mean/pars['logK'])) + 
    pars['beta'] * sst_projRCP45_syd$sSST_1[i] + 
    pars['gamma'] * sstGBR_3_projRCP45_gbr$sSST_3[i] +
    pars['phi'] * sst_projRCP45_syd$qt[i]
}

r_new_SSTsyd.SSTgbr_3.S <- r_new
r_new_ts <- ts(r_new, start = c(2006,4), end = c(2100, 12), frequency = 12)
plot(r_new_ts)
# Seasonal decomposition
fit_stl_r <- stl(r_new_ts, s.window="period")
plot(fit_stl_r)
trend_r <- fit_stl_r$time.series[,2]
plot(trend_r)
trend_r_tk <- tk_tbl(trend_r)
trend_r_tk <- trend_r_tk %>% mutate(date = as.Date(index))

r_GL.SSTsyd.SSTgbr_3_RCP45 <- tibble(model = 'GL.SSTsyd.SSTgbr_3 GL.SSTgbr_3_RCP45',
                                     r = r_new_SSTsyd.SSTgbr_3.S,
                                     trend = c(0.1, 0.1, 0.1, trend_r_tk$value),
                                     date = sst_projRCP45_gbr$year) %>% 
  filter(between(date, as.Date("2020-01-01"), as.Date("2100-12-01"))) %>% 
  thicken(interval  = '10 year')

ggplot(data = r_GL.SSTsyd.SSTgbr_3_RCP45, aes(x = as.factor(date_10_year), y = r)) +
  geom_boxplot() + 
  geom_jitter(position = position_jitter(width = .1)) 

#  .. Pred GL.EACs.SSTSYD.SSTGBR(t-3).S RCP4.5 ---------------------------------
# Run loop
pars <- coef(top2_mod_settlers)
r_new <- vector(length = nrow(sst_projRCP45_syd))

for(i in 4:nrow(sst_projRCP45_syd)){
  r_new[i] <- pars['rm'] * (1-(logN_mean/pars['logK'])) +
    pars['beta'] * fx_env_mth$sBRANv_1[i] + 
    pars['gamma'] * sst_projRCP45_syd$sSST_1[i] + 
    pars['kappa'] * sstGBR_3_projRCP45_gbr$sSST_3[i] +
    pars['phi'] * sst_projRCP45_syd$qt[i]
}

r_new_EAC.SSTsyd.SSTgbr_3.S <- r_new
r_new_ts <- ts(r_new, start = c(2006,4), end = c(2100, 12), frequency = 12)
plot(r_new_ts)
# Seasonal decomposition
fit_stl_r <- stl(r_new_ts, s.window="period")
plot(fit_stl_r)
trend_r <- fit_stl_r$time.series[,2]
plot(trend_r)
trend_r_tk <- tk_tbl(trend_r)
trend_r_tk <- trend_r_tk %>% mutate(date = as.Date(index))

r_GL.EAC.SSTsyd.SSTgbr_3_RCP45 <- tibble(model = 'r_new_EAC.SSTsyd.SSTgbr_3.S',
                                         r = r_new_EAC.SSTsyd.SSTgbr_3.S,
                                         trend = c(0.1, 0.1, 0.1, trend_r_tk$value),
                                         date = sst_projRCP45_gbr$year) %>% 
  filter(between(date, as.Date("2020-01-01"), as.Date("2100-12-01"))) %>% 
  thicken(interval  = '10 year')

ggplot(data = r_GL.EAC.SSTsyd.SSTgbr_3_RCP45, aes(x = as.factor(date_10_year), y = r)) +
  geom_boxplot() + 
  geom_jitter(position = position_jitter(width = .1)) 


#  .. Pred GL.CHL.SSTSYD.SSTGBR(t-3).S RCP4.5 ---------------------------------
# Run loop
pars <- coef(top3_mod_settlers)
r_new <- vector(length = nrow(sst_projRCP45_syd))

for(i in 4:nrow(sst_projRCP45_syd)){
  r_new[i] <- pars['rm'] * (1-(logN_mean/pars['logK'])) +
    pars['beta'] * sst_projRCP45_syd$sSST[i] + 
    pars['gamma'] * sstGBR_3_projRCP45_gbr$sSST_3[i] +
    pars['kappa'] * fx_env_mth$sChla_1[i] + 
    pars['phi'] * sst_projRCP45_syd$qt[i]
}

r_new_CHL.SSTsyd.SSTgbr_3.S <- r_new
r_new_ts <- ts(r_new, start = c(2006,4), end = c(2100, 12), frequency = 12)
plot(r_new_ts)
# Seasonal decomposition
fit_stl_r <- stl(r_new_ts, s.window="period")
plot(fit_stl_r)
trend_r <- fit_stl_r$time.series[,2]
plot(trend_r)
trend_r_tk <- tk_tbl(trend_r)
trend_r_tk <- trend_r_tk %>% mutate(date = as.Date(index))

r_GL.CHL.SSTsyd.SSTgbr_3_RCP45 <- tibble(model = 'r_new_CHL.SSTsyd.SSTgbr_3.S',
                                         r = r_new_CHL.SSTsyd.SSTgbr_3.S,
                                         trend = c(0.1, 0.1, 0.1, trend_r_tk$value),
                                         date = sst_projRCP45_gbr$year) %>% 
  filter(between(date, as.Date("2020-01-01"), as.Date("2100-12-01"))) %>% 
  thicken(interval  = '10 year')

ggplot(data = r_GL.CHL.SSTsyd.SSTgbr_3_RCP45, aes(x = as.factor(date_10_year), y = r)) +
  geom_boxplot() + 
  geom_jitter(position = position_jitter(width = .1)) 


#  .. Pred GL.SSTSYD.SSTGBR(t-3).S RCP8.5 ---------------------------------
# Run loop
pars <- coef(top1_mod_settlers)
r_new <- vector(length = nrow(sst_projRCP85_syd))

for(i in 4:nrow(sst_projRCP85_syd)){
  r_new[i] <- pars['rm'] * (1-(logN_mean/pars['logK'])) + 
    pars['beta'] * sst_projRCP85_syd$sSST_1[i] + 
    pars['gamma'] * sstGBR_3_projRCP85_gbr$sSST_3[i] +
    pars['phi'] * sst_projRCP85_syd$qt[i]
}

r_new_SSTsyd.SSTgbr_3.S <- r_new
r_new_ts <- ts(r_new, start = c(2006,4), end = c(2100, 12), frequency = 12)
plot(r_new_ts)
# Seasonal decomposition
fit_stl_r <- stl(r_new_ts, s.window="period")
plot(fit_stl_r)
trend_r <- fit_stl_r$time.series[,2]
plot(trend_r)
trend_r_tk <- tk_tbl(trend_r)
trend_r_tk <- trend_r_tk %>% mutate(date = as.Date(index))

r_GL.SSTsyd.SSTgbr_3_RCP85 <- tibble(model = 'GL.SSTsyd.SSTgbr_3_RCP85',
                                     r = r_new_SSTsyd.SSTgbr_3.S,
                                     trend = c(0.1, 0.1, 0.1, trend_r_tk$value),
                                     date = sst_projRCP85_gbr$time) %>% 
  filter(between(date, as.Date("2020-01-01"), as.Date("2100-12-01"))) %>% 
  thicken(interval  = '10 year')

ggplot(data = r_GL.SSTsyd.SSTgbr_3_RCP85, aes(x = as.factor(date_10_year), y = r)) +
  geom_boxplot() + 
  geom_jitter(position = position_jitter(width = .1)) 

#  .. Pred GL.EACs.SSTSYD.SSTGBR(t-3).S RCP8.5 ---------------------------------
# Run loop
pars <- coef(top2_mod_settlers)
r_new <- vector(length = nrow(sst_projRCP85_syd))

for(i in 4:nrow(sst_projRCP85_syd)){
  r_new[i] <- pars['rm'] * (1-(logN_mean/pars['logK'])) +
    pars['beta'] * fx_env_mth$sBRANv_1[i] + 
    pars['gamma'] * sst_projRCP85_syd$sSST[i] + 
    pars['kappa'] * sstGBR_3_projRCP85_gbr$sSST_3[i] +
    pars['phi'] * sst_projRCP85_syd$qt[i]
}

r_new_EAC.SSTsyd.SSTgbr_3.S <- r_new
r_new_ts <- ts(r_new, start = c(2006,4), end = c(2100, 12), frequency = 12)
plot(r_new_ts)
# Seasonal decomposition
fit_stl_r <- stl(r_new_ts, s.window="period")
plot(fit_stl_r)
trend_r <- fit_stl_r$time.series[,2]
plot(trend_r)
trend_r_tk <- tk_tbl(trend_r)
trend_r_tk <- trend_r_tk %>% mutate(date = as.Date(index))

r_GL.EAC.SSTsyd.SSTgbr_3_RCP85 <- tibble(model = 'r_new_EAC.SSTsyd.SSTgbr_3.S',
                                         r = r_new_EAC.SSTsyd.SSTgbr_3.S,
                                         trend = c(0.1, 0.1, 0.1, trend_r_tk$value),
                                         date = sst_projRCP85_gbr$time) %>% 
  filter(between(date, as.Date("2020-01-01"), as.Date("2100-12-01"))) %>% 
  thicken(interval  = '10 year')

ggplot(data = r_GL.EAC.SSTsyd.SSTgbr_3_RCP85, aes(x = as.factor(date_10_year), y = r)) +
  geom_boxplot() + 
  geom_jitter(position = position_jitter(width = .1)) 


#  .. Pred GL.CHL.SSTSYD.SSTGBR(t-3).S RCP8.5 ---------------------------------
# Run loop
pars <- coef(top3_mod_settlers)
r_new <- vector(length = nrow(sst_projRCP85_syd))

for(i in 4:nrow(sst_projRCP85_syd)){
  r_new[i] <- pars['rm'] * (1-(logN_mean/pars['logK'])) +
    pars['beta'] * sst_projRCP85_syd$sSST[i] + 
    pars['gamma'] * sstGBR_3_projRCP85_gbr$sSST_3[i] +
    pars['kappa'] * fx_env_mth$sChla_1[i] + 
    pars['phi'] * sst_projRCP85_syd$qt[i]
}

r_new_CHL.SSTsyd.SSTgbr_3.S <- r_new
r_new_ts <- ts(r_new, start = c(2006,4), end = c(2100, 12), frequency = 12)
plot(r_new_ts)
# Seasonal decomposition
fit_stl_r <- stl(r_new_ts, s.window="period")
plot(fit_stl_r)
trend_r <- fit_stl_r$time.series[,2]
plot(trend_r)
trend_r_tk <- tk_tbl(trend_r)
trend_r_tk <- trend_r_tk %>% mutate(date = as.Date(index))

r_GL.CHL.SSTsyd.SSTgbr_3_RCP85 <- tibble(model = 'r_new_CHL.SSTsyd.SSTgbr_3.S',
                                         r = r_new_CHL.SSTsyd.SSTgbr_3.S,
                                         trend = c(0.1, 0.1, 0.1, trend_r_tk$value),
                                         date = sst_projRCP85_gbr$time) %>% 
  filter(between(date, as.Date("2020-01-01"), as.Date("2100-12-01"))) %>% 
  thicken(interval  = '10 year')

ggplot(data = r_GL.CHL.SSTsyd.SSTgbr_3_RCP85, aes(x = as.factor(date_10_year), y = r)) +
  geom_boxplot() + 
  geom_jitter(position = position_jitter(width = .1)) 


#  .. Export predictions --------------------------------------------------

r_pred_abvaS <- bind_rows(r_GL.SSTsyd.SSTgbr_3_RCP45, r_GL.SSTsyd.SSTgbr_3_RCP85, 
                          r_GL.EAC.SSTsyd.SSTgbr_3_RCP45, r_GL.EAC.SSTsyd.SSTgbr_3_RCP85,
                          r_GL.CHL.SSTsyd.SSTgbr_3_RCP45, r_GL.CHL.SSTsyd.SSTgbr_3_RCP85)



# ..Multimodel Inference Model Averaging ------------------------------------

# Averaged parameters by AICw
rm_mmi <- coef(top1_mod_settlers)['rm'] * AIC_top1_mod_settlers +
  coef(top2_mod_settlers)['rm'] * AIC_top2_mod_settlers +
  coef(top3_mod_settlers)['rm'] * AIC_top3_mod_settlers 
logK_mmi <- coef(top1_mod_settlers)['logK'] * AIC_top1_mod_settlers +
  coef(top2_mod_settlers)['logK'] * AIC_top2_mod_settlers +
  coef(top3_mod_settlers)['logK'] * AIC_top3_mod_settlers 
sBRANv_mmi <- 0 * AIC_top1_mod_settlers +
  coef(top2_mod_settlers)['beta'] * AIC_top2_mod_settlers +
  0 * AIC_top3_mod_settlers 
sSSTsyd1_mmi <- coef(top1_mod_settlers)['beta'] * AIC_top1_mod_settlers +
  coef(top2_mod_settlers)['gamma'] * AIC_top2_mod_settlers +
  coef(top3_mod_settlers)['beta'] * AIC_top3_mod_settlers 
sChla_mmi <- 0 * AIC_top1_mod_settlers +
  0 * AIC_top2_mod_settlers +
  coef(top3_mod_settlers)['kappa'] * AIC_top3_mod_settlers 
sSSTgbr3_mmi <- coef(top1_mod_settlers)['gamma'] * AIC_top1_mod_settlers +
  coef(top2_mod_settlers)['kappa'] * AIC_top2_mod_settlers +
  coef(top3_mod_settlers)['gamma'] * AIC_top3_mod_settlers 
qt_mmi <-  coef(top1_mod_settlers)['phi'] * AIC_top1_mod_settlers +
  coef(top2_mod_settlers)['phi'] * AIC_top2_mod_settlers +
  coef(top3_mod_settlers)['phi'] * AIC_top3_mod_settlers 

# Function      
MMI_sim <- function(sBRANv_fx, sSSTsyd1, sChla_fx, sSSTgbr2, sSSTgbr3, qt){
  r_new = as.numeric(rm_mmi) * (1-(logN_mean/as.numeric(logK_mmi))) + 
    as.numeric(sBRANv_mmi) * sBRANv_fx +
    as.numeric(sSSTsyd1_mmi) * sSSTsyd1 +
    as.numeric(sChla_mmi) * sChla_fx +
    as.numeric(sSSTgbr3_mmi) * sSSTgbr3 +
    as.numeric(qt_mmi) * qt
  return(r_new)
}   

# .. Settlers RCP4.5 Run simulation ####
data_sim_RCP45 <- tibble(date = sstGBR_3_projRCP45_gbr$year,
                         sBRANv_fx = fx_env_mth$sBRANv_1,
                         sSSTsyd1 = sst_projRCP45_syd$sSST_1,
                         sChla_fx = fx_env_mth$sChla_1, 
                         sSSTgbr3 =  sstGBR_3_projRCP45_gbr$sSST_3, 
                         qt = sstGBR_3_projRCP45_gbr$qt)
data_sim_RCP45 <- na.exclude(data_sim_RCP45)

r_new <- MMI_sim(sBRANv_fx = data_sim_RCP45$sBRANv_fx, sSSTsyd1 = data_sim_RCP45$sSSTsyd1, sChla_fx = data_sim_RCP45$sChla_fx, 
                 sSSTgbr3 = data_sim_RCP45$sSSTgbr3, qt = data_sim_RCP45$qt)

data_sim_RCP45_settlers <- data_sim_RCP45 %>%
  mutate(model = 'RCP45') %>%
  mutate(r_new = r_new) %>% 
  filter(date >= as.Date('2020-01-01')) %>% 
  filter(date <= as.Date('2099-12-31')) %>% 
  # filter(date %in% c(as.Date('2020-01-01'):as.Date('2099-1-1'))) %>% 
  padr::thicken(interval  = '10 year', start_val = as.Date('2020-01-01'))


# .. Settlers RCP8.5 Run simulation ####
data_sim_RCP85 <- tibble(date = sstGBR_3_projRCP85_gbr$time,
                         sBRANv_fx = fx_env_mth$sBRANv_1,
                         sSSTsyd1 = sst_projRCP85_syd$sSST_1,
                         sChla_fx = fx_env_mth$sChla_1, 
                         sSSTgbr3 =  sstGBR_3_projRCP85_gbr$sSST_3, 
                         qt = sstGBR_3_projRCP85_gbr$qt)
data_sim_RCP85 <- na.exclude(data_sim_RCP85)

r_new <- MMI_sim(sBRANv_fx = data_sim_RCP85$sBRANv_fx, sSSTsyd1 = data_sim_RCP85$sSSTsyd1, sChla_fx = data_sim_RCP85$sChla_fx, 
                 sSSTgbr3 = data_sim_RCP85$sSSTgbr3, qt = data_sim_RCP85$qt)

data_sim_RCP85_settlers <- data_sim_RCP85 %>%
  mutate(model = 'RCP85') %>%
  mutate(r_new = r_new) %>% 
  filter(date >= as.Date('2020-01-01')) %>% 
  filter(date <= as.Date('2099-12-31')) %>% 
  # filter(date %in% c(as.Date('2020-01-01'):as.Date('2099-1-1'))) %>% 
  padr::thicken(interval  = '10 year', start_val = as.Date('2020-01-01'))


# .. Merge and plot RCP 4.5 and 8.5 simulations ####

data_sim_RCPs_settlers <- bind_rows(data_sim_RCP45_settlers, data_sim_RCP85_settlers)

# Plot

# png("r_sim_RCPs_abva_Settlers_Ago2020.png", width = 5, height = 4, units = 'in', res = 600)
ggplot(data = data_sim_RCPs_settlers, aes(x = as.factor(date_10_year), y = r_new, fill = model)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_boxplot(position=position_dodge(0.9), outlier.shape = NA, na.rm = TRUE) + 
  scale_fill_manual(values = c('blue', 'red')) +
  geom_jitter(size = 0.5, position = position_jitterdodge(dodge.width = 0.9), alpha = 0.5) +
  scale_x_discrete(name = "Decade",labels = c('2020', '2030', '2040', '2050', '2060', '2070', '2080', '2090')) +
  scale_y_continuous(limits=c(-2.2, 2), breaks=seq(-2, 2, 1)) +
  theme_bw() +
  theme(axis.title.x=element_text(size=16, family="Arial", colour="black", vjust=-0.1),
        axis.title.y=element_text(size=16, family="Arial", colour="black", vjust=2),
        axis.text.x = element_text(size=12, colour="black", angle = 45, vjust = 1.2, hjust = 1.2),
        axis.text.y = element_text(size=12, colour="black"), 
        plot.margin = unit(c(2, 0.2, 2, 2), "line"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  ylab("Simulated population growth, r") 
# dev.off()


# .. Plot Number of months r > 0 vs Time ---------------------------------

## RPC 45
numMonth_posGr_a <- data_frame(group = 'observed', date = data_setters$date, r = data_setters$r)
numMonth_posGr_b <- data_frame(group = 'simulated', date = data_sim_RCP45_settlers$date, 
                               r = data_sim_RCP45_settlers$r_new)

numMonth_posGr_RCP45_settlers <- bind_rows(numMonth_posGr_a, numMonth_posGr_b) %>% 
  filter(r > 0) %>%
  group_by(year = year(date)) %>%
  summarise(num = n()) 

numMonth_posGr_RCP45_settlers <- data_frame(year = as.Date(as.character(numMonth_posGr_RCP45_settlers$year), "%Y"),
                                            n = numMonth_posGr_RCP45_settlers$num) %>%
  pad() %>% 
  mutate(n = replace_na(n, 0)) %>% mutate(model = 'RCP 4.5')

## RPC 85
numMonth_posGr_a <- data_frame(group = 'observed', date = data_setters$date, r = data_setters$r)
numMonth_posGr_b <- data_frame(group = 'simulated', date = data_sim_RCP85_settlers$date, 
                               r = data_sim_RCP85_settlers$r_new)

numMonth_posGr_RCP85_settlers <- bind_rows(numMonth_posGr_a, numMonth_posGr_b) %>% 
  filter(r > 0) %>%
  group_by(year = year(date)) %>%
  summarise(num = n()) 

numMonth_posGr_RCP85_settlers <- data_frame(year = as.Date(as.character(numMonth_posGr_RCP85_settlers$year), "%Y"),
                                            n = numMonth_posGr_RCP85_settlers$num) %>%
  pad() %>% 
  mutate(n = replace_na(n, 0)) %>% mutate(model = 'RCP 8.5')


numMonth_posGr_all_settlers <- bind_rows(numMonth_posGr_RCP45_settlers, numMonth_posGr_RCP85_settlers) %>% 
  mutate(year = year(year))

# png("Months_Pos_r_sim_RCPs_abva_Settlers_Ago2020.png", width = 5, height = 4, units = 'in', res = 600)
numMonth_posGr_all_settlers %>%
  filter(!year %in% c(2017, 2018, 2019)) %>%  # Filter out unsampled years 
  ggplot(aes(x=year, y=n, colour = model, shape = model)) +
  geom_point(aes(size = model)) +
  geom_smooth() +
  scale_size_manual(values = c(2, 2)) +
  scale_shape_manual(values = c(19, 2)) +
  scale_colour_manual(values = c('blue', 'red')) +
  scale_x_continuous(limits=c(2000, 2100), breaks=seq(2000, 2100, 10)) +
  scale_y_continuous(limits=c(0, 13), breaks=seq(0, 12, 2)) +
  theme_bw() +
  theme(axis.title.x=element_text(size=16, family="Arial", colour="black", vjust=-0.1),
        axis.title.y=element_text(size=16, family="Arial", colour="black", vjust=2),
        axis.text.x = element_text(size=12, colour="black", angle = 45, vjust = 1.2, hjust = 1.2),
        axis.text.y = element_text(size=12, colour="black"), 
        plot.margin = unit(c(2, 0.2, 2, 2), "line"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.15, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(size=12)) +
  ylab("Months per year with positive r") +
  xlab("Time") 
# dev.off()


#  . JUVENILES   --------------------------------------------------------------

#  .. Pred GL.SSTSYD.S RCP4.5 ---------------------------------
# Run loop
pars <- coef(top1_mod_juveniles)
r_new <- vector(length = nrow(sst_projRCP45_syd))

for(i in 4:nrow(sst_projRCP45_syd)){
  r_new[i] <- pars['rm'] * (1-(logN_mean/pars['logK'])) + 
    pars['beta'] * sst_projRCP45_syd$sSST_1[i] +
    pars['phi'] * sst_projRCP45_syd$qt[i]
}

r_new_SSTsyd_RCP45 <- r_new
r_new_ts <- ts(r_new, start = c(2006,4), end = c(2100, 12), frequency = 12)
plot(r_new_ts)
# Seasonal decomposition
fit_stl_r <- stl(r_new_ts, s.window="period")
plot(fit_stl_r)
trend_r <- fit_stl_r$time.series[,2]
plot(trend_r)
trend_r_tk <- tk_tbl(trend_r)
trend_r_tk <- trend_r_tk %>% mutate(date = as.Date(index))

r_GL.SSTsyd_RCP45 <- tibble(model = 'GL.SSTsyd',
                            r = r_new_SSTsyd_RCP45,
                            trend = c(0.1, 0.1, 0.1, trend_r_tk$value),
                            date = sst_projRCP45_gbr$year) %>% 
  filter(between(date, as.Date("2020-01-01"), as.Date("2100-12-01"))) %>% 
  thicken(interval  = '10 year')

ggplot(data = r_GL.SSTsyd_RCP45, aes(x = as.factor(date_10_year), y = r)) +
  geom_boxplot() + 
  geom_jitter(position = position_jitter(width = .1)) 


#  .. Pred GL.CHL.SSTSYD.S RCP4.5 ---------------------------------
# Run loop
pars <- coef(top2_mod_juveniles)
r_new <- vector(length = nrow(sst_projRCP45_syd))

for(i in 4:nrow(sst_projRCP45_syd)){
  r_new[i] <- pars['rm'] * (1-(logN_mean/pars['logK'])) +
    pars['beta'] * sst_projRCP45_syd$sSST_1[i] + 
    pars['gamma'] * fx_env_mth$sChla_1[i] +
    pars['phi'] * sst_projRCP45_syd$qt[i]
}

r_new_CHL.SSTsyd.S <- r_new
r_new_ts <- ts(r_new, start = c(2006,4), end = c(2100, 12), frequency = 12)
plot(r_new_ts)
# Seasonal decomposition
fit_stl_r <- stl(r_new_ts, s.window="period")
plot(fit_stl_r)
trend_r <- fit_stl_r$time.series[,2]
plot(trend_r)
trend_r_tk <- tk_tbl(trend_r)
trend_r_tk <- trend_r_tk %>% mutate(date = as.Date(index))

r_new_CHL.SSTsyd_RCP45 <- tibble(model = 'r_new_CHL.SSTsyd.S',
                                 r = r_new_CHL.SSTsyd.S,
                                 trend = c(0.1, 0.1, 0.1, trend_r_tk$value),
                                 date = sst_projRCP45_gbr$year) %>% 
  filter(between(date, as.Date("2020-01-01"), as.Date("2100-12-01"))) %>% 
  thicken(interval  = '10 year')

ggplot(data = r_new_CHL.SSTsyd_RCP45, aes(x = as.factor(date_10_year), y = r)) +
  geom_boxplot() + 
  geom_jitter(position = position_jitter(width = .1)) 


#  .. Pred GL.EAC.SSTSYD.S RCP4.5 ---------------------------------
# Run loop
pars <- coef(top3_mod_juveniles)
r_new <- vector(length = nrow(sst_projRCP45_syd))

for(i in 4:nrow(sst_projRCP45_syd)){
  r_new[i] <- pars['rm'] * (1-(logN_mean/pars['logK'])) +
    pars['beta'] * fx_env_mth$sBRANv_1[i] +
    pars['gamma'] * sst_projRCP45_syd$sSST_1[i] +
    pars['phi'] * sst_projRCP45_syd$qt[i]
}

r_new_EAC.SSTsyd.S <- r_new
r_new_ts <- ts(r_new, start = c(2006,4), end = c(2100, 12), frequency = 12)
plot(r_new_ts)
# Seasonal decomposition
fit_stl_r <- stl(r_new_ts, s.window="period")
plot(fit_stl_r)
trend_r <- fit_stl_r$time.series[,2]
plot(trend_r)
trend_r_tk <- tk_tbl(trend_r)
trend_r_tk <- trend_r_tk %>% mutate(date = as.Date(index))

r_new_EAC.SSTsyd_RCP45 <- tibble(model = 'r_new_EAC.SSTsyd.S',
                                 r = r_new_EAC.SSTsyd.S,
                                 trend = c(0.1, 0.1, 0.1, trend_r_tk$value),
                                 date = sst_projRCP45_gbr$year) %>% 
  filter(between(date, as.Date("2020-01-01"), as.Date("2100-12-01"))) %>% 
  thicken(interval  = '10 year')

ggplot(data = r_new_EAC.SSTsyd_RCP45, aes(x = as.factor(date_10_year), y = r)) +
  geom_boxplot() + 
  geom_jitter(position = position_jitter(width = .1)) 

#  .. Pred GL.SSTSYD.S RCP8.5 ---------------------------------
# Run loop
pars <- coef(top1_mod_juveniles)
r_new <- vector(length = nrow(sst_projRCP85_syd))

for(i in 4:nrow(sst_projRCP85_syd)){
  r_new[i] <- pars['rm'] * (1-(logN_mean/pars['logK'])) + 
    pars['beta'] * sst_projRCP85_syd$sSST_1[i] +
    pars['phi'] * sst_projRCP85_syd$qt[i]
}

r_new_SSTsyd_RCP85 <- r_new
r_new_ts <- ts(r_new, start = c(2006,4), end = c(2100, 12), frequency = 12)
plot(r_new_ts)
# Seasonal decomposition
fit_stl_r <- stl(r_new_ts, s.window="period")
plot(fit_stl_r)
trend_r <- fit_stl_r$time.series[,2]
plot(trend_r)
trend_r_tk <- tk_tbl(trend_r)
trend_r_tk <- trend_r_tk %>% mutate(date = as.Date(index))

r_GL.SSTsyd_RCP85 <- tibble(model = 'GL.SSTsyd',
                            r = r_new_SSTsyd_RCP85,
                            trend = c(0.1, 0.1, 0.1, trend_r_tk$value),
                            date = sst_projRCP85_gbr$time) %>% 
  filter(between(date, as.Date("2020-01-01"), as.Date("2100-12-01"))) %>% 
  thicken(interval  = '10 year')

ggplot(data = r_GL.SSTsyd_RCP85, aes(x = as.factor(date_10_year), y = r)) +
  geom_boxplot() + 
  geom_jitter(position = position_jitter(width = .1)) 


#  .. Pred GL.CHL.SSTSYD.S RCP4.5 ---------------------------------
# Run loop
pars <- coef(top2_mod_juveniles)
r_new <- vector(length = nrow(sst_projRCP85_syd))

for(i in 4:nrow(sst_projRCP85_syd)){
  r_new[i] <- pars['rm'] * (1-(logN_mean/pars['logK'])) +
    pars['beta'] * sst_projRCP85_syd$sSST_1[i] + 
    pars['gamma'] * fx_env_mth$sChla_1[i] +
    pars['phi'] * sst_projRCP85_syd$qt[i]
}

r_new_CHL.SSTsyd.S <- r_new
r_new_ts <- ts(r_new, start = c(2006,4), end = c(2100, 12), frequency = 12)
plot(r_new_ts)
# Seasonal decomposition
fit_stl_r <- stl(r_new_ts, s.window="period")
plot(fit_stl_r)
trend_r <- fit_stl_r$time.series[,2]
plot(trend_r)
trend_r_tk <- tk_tbl(trend_r)
trend_r_tk <- trend_r_tk %>% mutate(date = as.Date(index))

r_new_CHL.SSTsyd_RCP85 <- tibble(model = 'r_new_CHL.SSTsyd.S',
                                 r = r_new_CHL.SSTsyd.S,
                                 trend = c(0.1, 0.1, 0.1, trend_r_tk$value),
                                 date = sst_projRCP85_gbr$time) %>% 
  filter(between(date, as.Date("2020-01-01"), as.Date("2100-12-01"))) %>% 
  thicken(interval  = '10 year')

ggplot(data = r_new_CHL.SSTsyd_RCP85, aes(x = as.factor(date_10_year), y = r)) +
  geom_boxplot() + 
  geom_jitter(position = position_jitter(width = .1)) 


#  .. Pred GL.EAC.SSTSYD.S RCP8.5 ---------------------------------
# Run loop
pars <- coef(top3_mod_juveniles)
r_new <- vector(length = nrow(sst_projRCP85_syd))

for(i in 4:nrow(sst_projRCP85_syd)){
  r_new[i] <- pars['rm'] * (1-(logN_mean/pars['logK'])) +
    pars['beta'] * fx_env_mth$sBRANv_1[i] +
    pars['gamma'] * sst_projRCP85_syd$sSST_1[i] +
    pars['phi'] * sst_projRCP85_syd$qt[i]
}

r_new_EAC.SSTsyd.S <- r_new
r_new_ts <- ts(r_new, start = c(2006,4), end = c(2100, 12), frequency = 12)
plot(r_new_ts)
# Seasonal decomposition
fit_stl_r <- stl(r_new_ts, s.window="period")
plot(fit_stl_r)
trend_r <- fit_stl_r$time.series[,2]
plot(trend_r)
trend_r_tk <- tk_tbl(trend_r)
trend_r_tk <- trend_r_tk %>% mutate(date = as.Date(index))

r_new_EAC.SSTsyd_RCP85 <- tibble(model = 'r_new_EAC.SSTsyd.S',
                                 r = r_new_EAC.SSTsyd.S,
                                 trend = c(0.1, 0.1, 0.1, trend_r_tk$value),
                                 date = sst_projRCP85_gbr$time) %>% 
  filter(between(date, as.Date("2020-01-01"), as.Date("2100-12-01"))) %>% 
  thicken(interval  = '10 year')

ggplot(data = r_new_EAC.SSTsyd_RCP85, aes(x = as.factor(date_10_year), y = r)) +
  geom_boxplot() + 
  geom_jitter(position = position_jitter(width = .1)) 


#  .. Export predictions --------------------------------------------------

r_pred_abvaJ <- bind_rows(r_GL.SSTsyd_RCP45, r_GL.SSTsyd_RCP85, 
                          r_new_CHL.SSTsyd_RCP45, r_new_CHL.SSTsyd_RCP85,
                          r_new_EAC.SSTsyd_RCP45, r_new_EAC.SSTsyd_RCP85)



# ..Multimodel Inference Model Averaging ------------------------------------

# Averaged parameters by AICw
rm_mmi <- coef(top1_mod_juveniles)['rm'] * AIC_top1_mod_juveniles +
  coef(top2_mod_juveniles)['rm'] * AIC_top2_mod_juveniles +
  coef(top3_mod_juveniles)['rm'] * AIC_top3_mod_juveniles 
logK_mmi <- coef(top1_mod_juveniles)['logK'] * AIC_top1_mod_juveniles +
  coef(top2_mod_juveniles)['logK'] * AIC_top2_mod_juveniles +
  coef(top3_mod_juveniles)['logK'] * AIC_top3_mod_juveniles 
sBRANv_mmi <- 0 * AIC_top1_mod_juveniles +
  0 * AIC_top2_mod_juveniles +
  coef(top3_mod_juveniles)['beta'] * AIC_top3_mod_juveniles 
sSSTsyd1_mmi <- coef(top1_mod_juveniles)['beta'] * AIC_top1_mod_juveniles +
  coef(top2_mod_juveniles)['beta'] * AIC_top2_mod_juveniles +
  coef(top3_mod_juveniles)['gamma'] * AIC_top3_mod_juveniles 
sChla_mmi <- 0 * AIC_top1_mod_juveniles +
  coef(top2_mod_juveniles)['gamma'] * AIC_top2_mod_juveniles +
  0 * AIC_top3_mod_juveniles
qt_mmi <-  coef(top1_mod_juveniles)['phi'] * AIC_top1_mod_juveniles +
  coef(top2_mod_juveniles)['phi'] * AIC_top2_mod_juveniles +
  coef(top3_mod_juveniles)['phi'] * AIC_top3_mod_juveniles 

# Function      
MMI_sim <- function(sBRANv_fx, sSSTsyd1, sChla_fx, qt){
  r_new = as.numeric(rm_mmi) * (1-(logN_mean/as.numeric(logK_mmi))) + 
    as.numeric(sBRANv_mmi) * sBRANv_fx +
    as.numeric(sSSTsyd1_mmi) * sSSTsyd1 +
    as.numeric(sChla_mmi) * sChla_fx +
    as.numeric(qt_mmi) * qt
  return(r_new)
}   

# .. Settlers RCP4.5 Run simulation ####
data_sim_RCP45 <- tibble(date = sstGBR_3_projRCP45_gbr$year,
                         sBRANv_fx = fx_env_mth$sBRANv_1,
                         sSSTsyd1 = sst_projRCP45_syd$sSST_1,
                         sChla_fx = fx_env_mth$sChla_1, 
                         qt = sstGBR_3_projRCP45_gbr$qt)
data_sim_RCP45 <- na.exclude(data_sim_RCP45)

r_new <- MMI_sim(sBRANv_fx = data_sim_RCP45$sBRANv_fx, sSSTsyd1 = data_sim_RCP45$sSSTsyd1, sChla_fx = data_sim_RCP45$sChla_fx, 
                 qt = data_sim_RCP45$qt)

data_sim_RCP45_juveniles <- data_sim_RCP45 %>%
  mutate(model = 'RCP45') %>%
  mutate(r_new = r_new) %>%
  filter(date >= as.Date('2020-01-01')) %>% 
  filter(date <= as.Date('2099-12-31')) %>% 
  # filter(date %in% c(as.Date('2020-01-01'):as.Date('2099-1-1'))) %>% 
  padr::thicken(interval  = '10 year', start_val = as.Date('2020-01-01'))


# .. Settlers RCP8.5 Run simulation ####
data_sim_RCP85 <- tibble(date = sstGBR_3_projRCP85_gbr$time,
                         sBRANv_fx = fx_env_mth$sBRANv_1,
                         sSSTsyd1 = sst_projRCP85_syd$sSST_1,
                         sChla_fx = fx_env_mth$sChla_1, 
                         qt = sstGBR_3_projRCP85_gbr$qt) %>% 
  filter(date %in% c(as.Date('2020-01-01'):as.Date('2099-12-31'))) 
data_sim_RCP85 <- na.exclude(data_sim_RCP85)

r_new <- MMI_sim(sBRANv_fx = data_sim_RCP85$sBRANv_fx, sSSTsyd1 = data_sim_RCP85$sSSTsyd1, sChla_fx = data_sim_RCP85$sChla_fx, 
                 qt = data_sim_RCP85$qt)

data_sim_RCP85_juveniles <- data_sim_RCP85 %>%
  mutate(model = 'RCP85') %>%
  mutate(r_new = r_new) %>%
  filter(date >= as.Date('2020-01-01')) %>% 
  filter(date <= as.Date('2099-12-31')) %>% 
  # filter(date %in% c(as.Date('2020-01-01'):as.Date('2099-1-1'))) %>% 
  padr::thicken(interval  = '10 year', start_val = as.Date('2020-01-01'))


# .. Merge and plot RCP 4.5 and 8.5 simulations ####

data_sim_RCPs_juveniles <- bind_rows(data_sim_RCP45_juveniles, data_sim_RCP85_juveniles)

# Plot

# png("r_sim_RCPs_abva_Juveniles_Ago2020.png", width = 5, height = 4, units = 'in', res = 600)
ggplot(data = data_sim_RCPs_juveniles, aes(x = as.factor(date_10_year), y = r_new, fill = model)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_boxplot(position=position_dodge(0.9)) + 
  scale_fill_manual(values = c('blue', 'red')) +
  geom_jitter(size = 0.5, position = position_jitterdodge(dodge.width = 0.9), alpha = 0.5) +
  scale_x_discrete(name = "Decade", labels = c('2020', '2030', '2040', '2050', '2060', '2070', '2080', '2090')) +
  scale_y_continuous(limits=c(-2.2, 2), breaks=seq(-2, 2, 1)) +
  theme_bw() +
  theme(axis.title.x=element_text(size=16, family="Arial", colour="black", vjust=-0.1),
        axis.title.y=element_text(size=16, family="Arial", colour="black", vjust=2),
        axis.text.x = element_text(size=12, colour="black", angle = 45, vjust = 1.2, hjust = 1.2),
        axis.text.y = element_text(size=12, colour="black"), 
        plot.margin = unit(c(2, 0.2, 2, 2), "line"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none') +
  ylab("Simulated population growth, r") 
# dev.off()


# .. Plot Number of months r > 0 vs Time ---------------------------------

## RPC 45
numMonth_posGr_a <- data_frame(group = 'observed', date = data_juveniles$date, r = data_juveniles$r)
numMonth_posGr_b <- data_frame(group = 'simulated', date = data_sim_RCP45_juveniles$date, 
                               r = data_sim_RCP45_juveniles$r_new)

numMonth_posGr_RCP45_juveniles <- bind_rows(numMonth_posGr_a, numMonth_posGr_b) %>% 
  filter(r > 0) %>%
  group_by(year = year(date)) %>%
  summarise(num = n()) 

numMonth_posGr_RCP45_juveniles <- data_frame(year = as.Date(as.character(numMonth_posGr_RCP45_juveniles$year), "%Y"),
                                             n = numMonth_posGr_RCP45_juveniles$num) %>%
  pad() %>% 
  mutate(n = replace_na(n, 0)) %>% mutate(model = 'RCP 4.5')

## RPC 85
numMonth_posGr_a <- data_frame(group = 'observed', date = data_juveniles$date, r = data_juveniles$r)
numMonth_posGr_b <- data_frame(group = 'simulated', date = data_sim_RCP85_juveniles$date, 
                               r = data_sim_RCP85_juveniles$r_new)

numMonth_posGr_RCP85_juveniles <- bind_rows(numMonth_posGr_a, numMonth_posGr_b) %>% 
  filter(r > 0) %>%
  group_by(year = year(date)) %>%
  summarise(num = n()) 

numMonth_posGr_RCP85_juveniles <- data_frame(year = as.Date(as.character(numMonth_posGr_RCP85_juveniles$year), "%Y"),
                                             n = numMonth_posGr_RCP85_juveniles$num) %>%
  pad() %>% 
  mutate(n = replace_na(n, 0)) %>% mutate(model = 'RCP 8.5')


numMonth_posGr_all_juveniles <- bind_rows(numMonth_posGr_RCP45_juveniles, numMonth_posGr_RCP85_juveniles) %>% 
  mutate(year = year(year))

# png("Months_Pos_r_sim_RCPs_abva_Juveniles_Ago2020.png", width = 5, height = 4, units = 'in', res = 600)
numMonth_posGr_all_juveniles %>%
  filter(!year %in% c(2017, 2018, 2019)) %>%  # Filter out unsampled years 
  ggplot(aes(x=year, y=n, colour = model, shape = model)) +
  geom_point(aes(size = model)) +
  geom_smooth() +
  scale_size_manual(values = c(2, 2)) +
  scale_shape_manual(values = c(19, 2)) +
  scale_colour_manual(values = c('blue', 'red')) +
  scale_x_continuous(limits=c(2000, 2100), breaks=seq(2000, 2100, 10)) +
  scale_y_continuous(limits=c(0, 13), breaks=seq(0, 12, 2)) +
  theme_bw() +
  theme(axis.title.x=element_text(size=16, family="Arial", colour="black", vjust=-0.1),
        axis.title.y=element_text(size=16, family="Arial", colour="black", vjust=2),
        axis.text.x = element_text(size=12, colour="black", angle = 45, vjust = 1.2, hjust = 1.2),
        axis.text.y = element_text(size=12, colour="black"), 
        plot.margin = unit(c(2, 0.2, 2, 2), "line"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = c(0.15, 0.85),
        legend.title = element_blank(),
        legend.text = element_text(size=12)) +
  ylab("Months per year with positive r") +
  xlab("Time") 
# dev.off()