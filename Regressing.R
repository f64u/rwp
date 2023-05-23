library(stargazer)
library(ggthemes)

mod1 <- lm(voted_trump ~ cultural_fact + pid3 + isfemale + age + educ + iswhite, data = anes_voted)
mod2 <- lm(voted_trump ~ economic_fact + pid3 + isfemale + age + educ + iswhite, data = anes_voted)
mod3 <- lm(voted_trump ~ political_fact + pid3 + isfemale + age + educ + iswhite, data = anes_voted)
mod4 <- lm(voted_trump ~ cultural_fact + economic_fact + political_fact + pid3 + isfemale + age + educ + iswhite, data = anes_voted)

stargazer(mod1, mod2, mod3, mod4,
          type = "html", 
          omit.stat = c("adj.rsq", "f", "ser"),
          title = "Explaining Vote Choice for Trump in the 2016 election",
          dep.var.labels = c("Vote choice for Trump"),
          covariate.labels = c("Cultural Anxiety Index (0 least anxious; 1 most anxious)", 
                               "Economic Anxiety Index (0 least anxious; 1 most anxious)",
                               "Political Anxiety Index (0 least anxious; 1 most anxious)",
                               "Party ID (-1 Dem; 0 Ind; 1 Rep)", 
                               "Female",
                               "Age",
                               "Education",
                               "White"),
          #order=c("^trans$", "^is_2020$", "^pid7$", "^trans:is_2020$", "^gay_jobs_bi$", "^gay_jobs_bi:is_2020$", "^gay_adopt$", "^gay_adopt:is_2020$", "^age$", "^income$", "^female$"),
          out = "~/hw/23/sosc/assignment_table.html")

summary(lm(trumpft ~ cultural_fact + economic_fact + political_fact + pid3 + age + gender, data = anes2016_fact))

# dummies for who they vote in primaries: Trump, Rubio, Cruz, Kasich

anes_trump_primary_and_voted <- anes_voted %>% filter(trump_primary_vote == 1)
anes_ntrump_primary_and_voted <- anes_voted %>% filter(trump_primary_vote == 0)

prim_modt <-
  summary(lm(trumpft ~ cultural_fact + economic_fact + political_fact + pid3 + isfemale + age + educ + iswhite, data = anes_trump_primary_and_voted))
prim_modn <-
  summary(lm(trumpft ~ cultural_fact + economic_fact + political_fact + pid3 + isfemale + age + educ + iswhite, data = anes_ntrump_primary_and_voted))

coefst <- prim_modt$coefficients[2:4,1]
names(coefst) <- NULL

coefsn <- prim_modn$coefficients[2:4,1]
names(coefsn) <- NULL

set <- prim_modt$coefficients[2:4,2]
names(set) <- NULL

sen <- prim_modn$coefficients[2:4,2]
names(sen) <- NULL


corr_tb <- tibble(
  vote = c("C", "E", "P", "C", "E", "P"),
  pert = c(1.2, 2.8, 4.4, 1.8, 3.4, 5.0),
  type = c("T", "T", "T", "N", "N", "N"),
  coef = c(coefst, coefsn),
  se = c(set, sen),
  order = 1:6
)


corr_tb %>%
  ggplot(aes(x = coef, y = pert)) +
  geom_vline(xintercept = 0, color = "grey") +
  #geom_vline(xintercept = 1, color = "grey") +
  #geom_vline(xintercept = 0.2, color = "grey", linetype = 2) +
  #geom_vline(xintercept = 0.7, color = "grey", linetype = 2) +
  geom_errorbar(aes(xmin = coef - 1.96 * se, xmax = coef + 1.96 * se), 
                width = 0, color = "grey30") +
  geom_point(aes(x = coef, shape = type), size = 3, color = "#222222") + 
  scale_shape_manual(labels = c("Not Trump", "Trump"), name = "Republican Primary\nVote Choice", values = c(15, 17)) + 
  scale_x_continuous(breaks = c(0, .5, 1), 
                     labels = c("0", 
                                "50",
                                "100")) +
  scale_y_continuous(labels = c("Cultural Index",  "Economic Index", "Political Index"),
                     breaks = c(1.5, 3.1, 4.7)) +
  labs(x="Index β Coefficient for the Trump Feeling Thermometer", title = "", y="") +
  theme_few()



anes2012_voted <- anes2012_fact %>%
  filter(post_pres_vote %in% 1:2) %>%
  mutate(voted_romney =
           case_when(
             post_pres_vote == 2 ~ 1,
             T ~ 0))


mod12 <- lm(voted_romney ~ cultural_fact + pid3 + isfemale + age + educ + iswhite, data = anes2012_voted)
mod22 <- lm(voted_romney ~ economic_fact + pid3 + isfemale + age + educ + iswhite, data = anes2012_voted)
mod32 <- lm(voted_romney ~ political_fact + pid3 + isfemale + age + educ + iswhite, data = anes2012_voted)
mod42 <- lm(voted_romney ~ cultural_fact + economic_fact + political_fact + pid3 + isfemale + age + educ + iswhite, data = anes2012_voted)


coefs6 <- mod4$coefficients[2:4]
names(coefs6) <- NULL

coefs2 <- mod42$coefficients[2:4]
names(coefs2) <- NULL

se6 <- summary(mod4)$coefficients[2:4,2]
names(se6) <- NULL

se2 <- summary(mod42)$coefficients[2:4,2]
names(se2) <- NULL


corr_tb <- tibble(
  vote = c("C", "E", "P", "C", "E", "P"),
  pert = c(1.2, 2.8, 4.4, 1.8, 3.4, 5.0),
  type = c("T", "T", "T", "N", "N", "N"),
  coef = c(coefs6, coefs2),
  se = c(se6, se2),
  order = 1:6
)



corr_tb %>%
  ggplot(aes(x = coef, y = pert)) +
  geom_vline(xintercept = 0, color = "grey") +
  #geom_vline(xintercept = 1, color = "grey") +
  #geom_vline(xintercept = 0.2, color = "grey", linetype = 2) +
  #geom_vline(xintercept = 0.7, color = "grey", linetype = 2) +
  geom_errorbar(aes(xmin = coef - 1.96 * se, xmax = coef + 1.96 * se), 
                width = 0, color = "grey30") +
  geom_point(aes(x = coef, shape = type), size = 3, color = "#222222") + 
  scale_shape_manual(labels = c("Romney (2012)", "Trump (2016)"), name = "Republican Presidential\nCandidate", values = c(15, 17)) + 
  scale_x_continuous(breaks = c(0, .5, 1), 
                     labels = c("+0%", 
                                "+50%",
                                "+100%")) +
  scale_y_continuous(labels = c("Cultural Index",  "Economic Index", "Political Index"),
                     breaks = c(1.5, 3.1, 4.7)) +
  labs(x="Index β Coefficient for Vote Choice", title = "", y="") +
  theme_few()





