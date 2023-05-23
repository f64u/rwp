library(tidyverse)
library(haven)

anes2016_raw <- 
  read_dta("~/hw/22/soscII/anes_timeseries_2016_dta/anes_timeseries_2016.dta")

anes2016 <- anes2016_raw %>%
  select(
    # meta
    full_weights = V160102,
    full_strata = V160201,
    full_psu = V160202,
    
    # demographics
    gender = V161342,
    age = V161267,
    income = V161361x,
    pid3 = V161155,
    pid7 = V161158x,
    party_strength = V161156,
    race = V161310x, # 1 is white; -2 is NA
    educ = V161270, # 1-16
    
    # presidential affect
    clintonft = V161086,
    trumpft = V161087,
    
    # vote choice
    primary_vote = V161021a, # 1-8; Donald is 4; Other republicans 5-8
    post_pres_vote = V162034a, # 1-5,7,9; 1-2 are important
    
    # culture issues
    moral_tolerance = V162209,   # 1-5, agree to disagree
    immig_policy = V161192,    # 1-4, felons to allow to stay
    syrian_refug = V161214x,   # 1-7, allow to oppose
    immig_restrict = V162157,  # 1-5, increase immigration to decrease immigration
    america_customs = V162274, # 1-4, important to not important
    jewft = V162108, # 0-100
    blmft = V162113, # 0-100
    whiteft = V162314, # 0-100
    blackft = V162312, # 0-100
    hispanicft = V162311, # 0-100
    govt_help_black = V161198, # 1-7, help to not help
    trad_family = V162210,       # 1-5, agree to disagree
    muslimft = V162106,         # 0-100
    
    # economy
    mexico_wall = V161196x,    # 1-7, favor to oppose
    immig_take_job = V162158,       # 1-4, likely to unlikely
    finance_worry = V162165,        # 1-5, worried to not worried
    trade_increase = V162175,       # 1 good 2 bad
    govt_outsource = V162177,       # 1 discourage, 2 encourage, 3 stay out
    immig_good_econ = V162268,      # 1-5, agree to disagree
    
    # policy affect
    strong_leader = V162263,  # 1-5, agree to disagree
    country_needs_strong_leader = V162170, # 1-5, agree to disagree
    gov_trust = V161215,      # 1-5, always to never
    polit_no_care = V162260,     # 1-5, agree to disagree
    polit_trust = V162261,    # 1-5, agree to disagree
    polit_interest_rich = V162265, # 1-5, agree to disagree
    polit_no_say = V162216,        # 1-5, agree to disagree
    polit_num_corrupt = V161218, # 1-5, all to none
    polit_white_influence = V162322, # 1-3, all to too little
  ) %>%
  
  filter(pid7 %in% 1:7 & pid3 %in% 1:3) %>%
  filter(gender %in% 1:3 & age %in% 18:90 & income %in% 1:28) %>%
  filter(race %in% 1:6 & educ %in% 1:16) %>%
  
  filter(clintonft %in% 0:100 & trumpft %in% 0:100) %>%
  
  filter(moral_tolerance %in% 1:5) %>%
  filter(syrian_refug %in% 1:7) %>%
  filter(immig_policy %in% 1:4) %>%
  filter(immig_restrict %in% 1:5) %>%
  filter(america_customs %in% 1:4) %>%
  filter(jewft %in% 0:100) %>% 
  filter(blmft %in% 0:100) %>%
  filter(whiteft %in% 0:100)  %>%
  filter(blackft %in% 0:100) %>%
  filter(hispanicft %in% 0:100) %>%
  #filter(trad_family %in% 1:5) %>%
  filter(govt_help_black %in% 1:7) %>%
  filter(muslimft %in% 0:100) %>%
  
  # filter(econ_state_lst_yr %in% 1:5) %>%
  # filter(econ_state_nxt_yr %in% 1:5) %>%
  # filter(unemployment_state %in% 1:5) %>%
  # filter(econ_since_2008 %in% 1:5) %>%
  filter(mexico_wall %in% 1:7) %>%
  filter(immig_take_job %in% 1:4) %>%
  filter(finance_worry %in% 1:5) %>%
  filter(trade_increase %in% 1:3) %>%
  filter(govt_outsource %in% 1:3) %>%
  filter(immig_good_econ %in% 1:5) %>%
  
  filter(strong_leader %in% 1:5) %>%
  filter(country_needs_strong_leader %in% 1:5) %>%
  filter(gov_trust %in% 1:5) %>%
  filter(polit_no_care %in% 1:5) %>%
  filter(polit_trust %in% 1:5) %>%
  filter(polit_interest_rich %in% 1:5) %>%
  filter(polit_no_say %in% 1:5) %>%
  filter(polit_num_corrupt %in% 1:5) %>%
  filter(polit_white_influence %in% 1:3) %>%
  
  
  mutate(
    pid3 = case_when(pid3 == 1 ~ -1, pid3 == 3 ~ 0, pid3 == 2 ~ 1),
    pid7 = (pid7 - 4) / 3,
    isfemale = as.numeric(gender == 2),
    iswhite = as.numeric(race == 1),
    post_pres_vote = post_pres_vote - 1,
    
    trump_primary_vote = case_when(
      primary_vote == 4 ~ 1,
      primary_vote %in% 5:8 ~ 0,
      T ~ -1,
    ),
    
    trumpft = trumpft / 100,
    clintonft = clintonft / 100,
    
    moral_tolerance = (moral_tolerance - 3) / 2,
    immig_policy = (immig_policy - 2.5) / -1.5,
    immig_restrict = (immig_restrict - 3) / 2,
    america_customs = (america_customs - 2.5) / -1.5,
    syrian_refug = (syrian_refug - 4) / 3,
    jewft = (jewft - 50) / -50,
    blmft = (blmft - 50) / -50,
    whiteft = (whiteft - 50) / 50,
    blackft = (blackft - 50) / -50,
    hispanicft = (hispanicft - 50) / -50,
    govt_help_black = (govt_help_black - 4) / 3,
    #trad_family = (trad_family - 3) / -2,
    muslimft = (muslimft - 50) / -50,
    
    mexico_wall = (mexico_wall - 4) / -3,
    immig_take_job = (immig_take_job - 2.5) / -1.5,
    finance_worry = (finance_worry - 3) / -2,
    trade_increase = case_when(
      trade_increase == 1 ~ -1,
      trade_increase == 3 ~ 0,
      trade_increase == 2 ~ 1
    ),
    govt_outsource = case_when(govt_outsource == 1 ~ 1,
                               govt_outsource == 3 ~ 0,
                               govt_outsource == 2 ~ -1),
    immig_good_econ = (immig_good_econ - 3) / 2,
    
    strong_leader = (strong_leader - 3) / -2,
    country_needs_strong_leader = (country_needs_strong_leader - 3) / -2,
    gov_trust = (gov_trust - 3) / 2,
    polit_no_care = (polit_no_care - 3) / -2,
    polit_trust = (polit_trust - 3) / 2,
    polit_interest_rich = (polit_interest_rich - 3) / -2,
    polit_no_say = (polit_no_say - 3) / -2,
    polit_num_corrupt = (polit_num_corrupt - 3) / -2,
    polit_white_influence = (polit_white_influence - 2) / 1,
  )


anes2012_raw <- 
  read_dta("~/hw/23/sosc/anes_timeseries_2012_dta/anes_timeseries_2012.dta")

anes2012 <- anes2012_raw %>%
  select(
    
    # demographics
    gender = gender_respondent_x,
    age = dem_age_r_x,
    income = inc_incgroup_pre,
    pid3 = prevote_regpty,
    race = dem_raceeth_x, # 1 is white; -2 is NA
    educ = dem_edu, # 1-16
    
    # presidential affect
    demft = ft_dpc,
    repft = ft_rpc,
    
    # vote choice
    primary_vote = prevote_primvwho, # 1-9; Romney is 1; Obama is 2
    post_pres_vote = presvote2012_x, # 1-5,7,9; 1-2 are important
    
    # moral
    trad_family = trad_famval,
    moral_tolerance = trad_tolerant,   # 1-5, agree to disagree
    immig_policy = immig_policy,    # 1-4, felons to allow to stay
    # syrian_refug = NA,   # 1-7, allow to oppose
    immig_restrict = immigpo_level,  # 1-5, increase immigration to decrease immigration
    govt_help_black = aidblack_self, # 1-7, help to not help
    
    # economy
    immig_take_job = immigpo_jobs,       # 1-4, likely to unlikely
    finance_worry = ecperil_worry,        # 1-5, worried to not worried
    #trade_increase = V162175,       # 1 good 2 bad
    govt_outsource = outsource_enc,       # 1 discourage, 2 encourage, 3 stay out
    #immig_good_econ = V162268,      # 1-5, agree to disagree
    imports_limit, # 1 favor, 2 oppose
    
    # policy affect
    #strong_leader = V162263,  # 1-5, agree to disagree
    #country_needs_strong_leader = V162170, # 1-5, agree to disagree
    #gov_trust = trustgov_trustgrev,      # 1-5, always to never
    #polit_no_care = V162260,     # 1-5, agree to disagree
    #polit_trust = V162261,    # 1-5, agree to disagree
    #polit_interest_rich = V162265, # 1-5, agree to disagree
    #polit_no_say = effic_saystd,        # 1-5, agree to disagree
    polit_num_corrupt = trustgov_corrpt, # 1-5, all to none
    polit_interest_few = trustgov_bigintrst, # 1 few big, 2 all people
    #polit_white_influence = V162322, # 1-3, all to too little
    
  ) %>%
  
  filter(pid3 %in% 1:3) %>%
  filter(gender %in% 1:2 & age %in% 18:90 & income %in% 1:28) %>%
  filter(race %in% 1:6 & educ %in% 1:16) %>%
  
  filter(demft %in% 0:100 & repft %in% 0:100) %>%
  
  filter(moral_tolerance %in% 1:5) %>%
  filter(immig_policy %in% 1:4) %>%
  filter(immig_restrict %in% 1:5) %>%
  filter(govt_help_black %in% 1:7) %>%
  #filter(trad_family %in% 1:4) %>%
  
  # filter(econ_state_lst_yr %in% 1:5) %>%
  # filter(econ_state_nxt_yr %in% 1:5) %>%
  # filter(unemployment_state %in% 1:5) %>%
  # filter(econ_since_2008 %in% 1:5) %>%
  filter(immig_take_job %in% 1:4) %>%
  filter(finance_worry %in% 1:5) %>%
  filter(govt_outsource %in% 1:3) %>%
  filter(imports_limit %in% 1:2 | imports_limit == -2) %>%
  
  #filter(gov_trust %in% 1:5) %>%
  #filter(polit_no_say %in% 1:5) %>%
  filter(polit_num_corrupt %in% 1:5) %>%
  filter(polit_interest_few %in% 1:2) %>%
  
  
  mutate(
    pid3 = case_when(pid3 == 1 ~ -1, pid3 == 3 ~ 0, pid3 == 2 ~ 1),
    isfemale = as.numeric(gender == 2),
    iswhite = as.numeric(race == 1),
    
    repft = repft / 100,
    demft = demft / 100,
    
    moral_tolerance = (moral_tolerance - 3) / 2,
    immig_policy = (immig_policy - 2.5) / -1.5,
    immig_restrict = (immig_restrict - 3) / 2,
    govt_help_black = (govt_help_black - 4) / 3,
    #trad_family = (trad_family - 3) / -2,
    
    imports_limit = case_when(
      imports_limit == -2 ~ 0,
      imports_limit == 1 ~ 1,
      imports_limit == 2 ~ -1,
    ),
    
    immig_take_job = (immig_take_job - 2.5) / -1.5,
    finance_worry = (finance_worry - 3) / -2,
    govt_outsource = case_when(govt_outsource == 1 ~ 1,
                               govt_outsource == 3 ~ 0,
                               govt_outsource == 2 ~ -1),
    
    polit_interest_few = (polit_interest_few - 1.5) / -0.5,
    polit_num_corrupt = (polit_num_corrupt - 3) / -2,
  )

