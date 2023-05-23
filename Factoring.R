library(lavaan)
library(psy)
library(tidyverse)

# Factor analysis
m1a <- 'cultural_fact =~ america_customs + immig_restrict + immig_policy + syrian_refug + govt_help_black + jewft + moral_tolerance'
cult_fac <- cfa(m1a, data=anes2016, sampling.weights = "full_weights", std.lv = T)
fitmeasures(cult_fac, c('cfi', 'rmsea', 'rmsea.ci.upper', 'bic'))
summary(cult_fac, fit.measures=TRUE, standardized=TRUE)
cult_predicted <- as.data.frame(predict(cult_fac))

anes_cultural <- anes2016 %>% select(america_customs, immig_restrict, immig_policy, syrian_refug, govt_help_black, jewft, america_customs, moral_tolerance)
cronbach(anes_cultural)

m2a <- 'economic_fact =~ govt_outsource + immig_take_job + finance_worry + trade_increase + immig_good_econ + mexico_wall'
econ_fac <- cfa(m2a, data=anes2016, sampling.weights = "full_weights", std.lv = T)
fitmeasures(econ_fac, c('cfi', 'rmsea', 'rmsea.ci.upper', 'bic'))
summary(econ_fac, fit.measures=TRUE, standardized=TRUE)
econ_predicted <- as.data.frame(predict(econ_fac))

anes_economic <- anes2016 %>% select(govt_outsource, immig_take_job, finance_worry, trade_increase, immig_good_econ, mexico_wall)
cronbach(anes_economic)

m3a <- 'political_fact =~ gov_trust + strong_leader + polit_no_care + polit_trust + polit_no_say + polit_interest_rich + polit_num_corrupt'
polit_fac <- cfa(m3a, data=anes2016, sampling.weights = "full_weights", std.lv = T)
fitmeasures(polit_fac, c('cfi', 'rmsea', 'rmsea.ci.upper', 'bic'))
summary(polit_fac, fit.measures=TRUE, standardized=TRUE)
polit_predicted <- as.data.frame(predict(polit_fac))

anes_political <- anes2016 %>% select(gov_trust, strong_leader, polit_no_care, polit_trust, polit_no_say, polit_interest_rich, polit_num_corrupt)
cronbach(anes_political)


anes2016_fact <- 
  cbind(anes2016, cult_predicted, econ_predicted, polit_predicted) %>%
  mutate(
    cultural_fact = cultural_fact - floor(min(cultural_fact)),
    economic_fact = economic_fact - floor(min(economic_fact)),
    political_fact = political_fact - floor(min(political_fact)),
    
  ) %>%
  mutate(
    cultural_fact = cultural_fact / ceiling(max(cultural_fact)),
    economic_fact = economic_fact / ceiling(max(economic_fact)),
    political_fact = political_fact / ceiling(max(political_fact)),
  )


m1a2 <- 'cultural_fact =~ immig_restrict + immig_policy + govt_help_black + moral_tolerance'
cult_fac2 <- cfa(m1a2, data=anes2012, std.lv = T)
fitmeasures(cult_fac2, c('cfi', 'rmsea', 'rmsea.ci.upper', 'bic'))
summary(cult_fac2, fit.measures=TRUE, standardized=TRUE)

m2a2 <- 'economic_fact =~ govt_outsource + finance_worry + immig_take_job + imports_limit'
econ_fac2 <- cfa(m2a2, data=anes2012, std.lv = T)
fitmeasures(econ_fac2, c('cfi', 'rmsea', 'rmsea.ci.upper', 'bic'))
summary(econ_fac2, fit.measures=TRUE, standardized=TRUE)

m3a2 <- 'political_fact =~ a*polit_num_corrupt + a*polit_interest_few'
polit_fac2 <- cfa(m3a2, data=anes2012, std.lv=T)
fitmeasures(polit_fac2, c('cfi', 'rmsea', 'rmsea.ci.upper', 'bic'))
summary(polit_fac2, fit.measures=TRUE, standardized=TRUE)


cult_predicted2 <- as.data.frame(predict(cult_fac2))
econ_predicted2 <- as.data.frame(predict(econ_fac2))
polit_predicted2 <- as.data.frame(predict(polit_fac2))


anes2012_fact <- 
  cbind(anes2012, cult_predicted2, econ_predicted2, polit_predicted2) %>%
  mutate(
    cultural_fact = cultural_fact - floor(min(cultural_fact)),
    economic_fact = economic_fact - floor(min(economic_fact)),
    political_fact = political_fact - floor(min(political_fact)),
    
  ) %>%
  mutate(
    cultural_fact = cultural_fact / ceiling(max(cultural_fact)),
    economic_fact = economic_fact / ceiling(max(economic_fact)),
    political_fact = political_fact / ceiling(max(political_fact)),
  )
