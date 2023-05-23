library(tidyverse)
library(ggthemes)

anes_voted <-
  anes2016_fact %>%
  filter(post_pres_vote %in% 0:4 | post_pres_vote == 9) %>%
  mutate(voted_trump =
           case_when(
             post_pres_vote == 1 ~ 1,
             T ~ 0)
  )


party_colors <- c("#187498", "#C7C6C1", "#EB5353")
party_labels <- c("Democratic", "Independent", "Republican")

ggplot(anes2016_fact, aes(cultural_fact, trumpft * 100, color = factor(pid3))) +
  geom_point(alpha = 0.6) +
  geom_smooth(formula = y~x, method = lm, se = F) +
  labs(x = "Cultural anxiety index",
       y = "Trump feeling thermometer",
       title = "The relationship between Trump feeling thermometer and\nthe cultural anxiety index") +
  scale_color_manual(values = party_colors, labels = party_labels, name = "Party") +
  ylim(0, 100) +
  theme_few()

ggplot(anes2016_fact, aes(economic_fact, trumpft * 100, color = factor(pid3))) +
  geom_point(alpha = 0.6) +
  geom_smooth(formula = y~x, method = lm, se = F) +
  labs(x = "Economic anxiety index",
       y = "Trump feeling thermometer",
       title = "The relationship between Trump feeling thermometer and\nthe economic anxiety index") +
  scale_color_manual(values = party_colors, labels = party_labels, name = "Party") +
  ylim(0, 100) +
  theme_few()

ggplot(anes2016_fact, aes(political_fact, trumpft * 100, color = factor(pid3))) +
  geom_point(alpha = 0.6) +
  geom_smooth(formula = y~x, method = lm, se = F) +
  labs(x = "Political anxiety index",
       y = "Trump feeling thermometer",
       title = "The relationship between Trump feeling thermometer and\nthe political anxiety index") +
  scale_color_manual(values = party_colors, labels = party_labels, name = "Party") +
  theme_few()


anes_voted %>% filter(post_pres_vote == 0 | post_pres_vote == 1) %>%
  ggplot(aes(x=factor(voted_trump), y=cultural_fact, fill=factor(voted_trump))) +
  labs(x = "Vote Choice",
       y = "Cultural Anxeity Index",
       title = "") +
  scale_fill_manual(values = c("#187498", "#EB5353"), labels = c("Not Trump", "Trump"), name = "Vote choice") +
  scale_x_discrete(labels = c("Clinton Voters", "Trump Voters")) +
  ylim(0, 1) +
  geom_boxplot() +
  guides(fill = "none") +
  theme_few()


anes_voted %>% filter(post_pres_vote == 0 | post_pres_vote == 1) %>%
  ggplot(aes(x=factor(voted_trump), y=economic_fact, fill=factor(voted_trump))) +
  labs(x = "Vote Choice",
       y = "Economic Anxiety Index",
       title = "") +
  scale_fill_manual(values = c("#187498", "#EB5353"), labels = c("Not Trump", "Trump"), name = "Vote choice") +
  scale_x_discrete(labels = c("Clinton Voters", "Trump Voters")) +
  ylim(0, 1) +
  geom_boxplot() +
  guides(fill = "none") +
  theme_few()


anes_voted %>% filter(post_pres_vote == 0 | post_pres_vote == 1) %>%
  ggplot(aes(x=factor(voted_trump), y=political_fact, fill=factor(voted_trump))) +
  labs(x = "Vote Choice",
       y = "Political Anxiety Index",
       title = "") +
  scale_fill_manual(values = c("#187498", "#EB5353"), labels = c("Not Trump", "Trump"), name = "Vote choice") +
  scale_x_discrete(labels = c("Clinton Voters", "Trump Voters")) +
  ylim(0, 1) +
  geom_boxplot() +
  guides(fill = "none") +
  theme_few()


anes_voted %>% filter(post_pres_vote == 0 | post_pres_vote == 1) %>%
  ggplot(aes(x=cultural_fact, fill=factor(voted_trump))) +
  labs(x = "Cultural Anxiety Index",
       y = "Number of Respondents",
       title = "") +
  scale_fill_manual(values = c("#187498", "#EB5353"), labels = c("Clinton", "Trump"), name = "Vote Choice") +
  geom_histogram(color="black", alpha = 0.8, position="identity") +
  xlim(0, 1) +
  theme_few()

anes_voted %>% filter(post_pres_vote == 0 | post_pres_vote == 1) %>%
  ggplot(aes(x=economic_fact, fill=factor(voted_trump))) +
  labs(x = "Economic Anxiety Index",
       y = "Number of Respondents",
       title = "") +
  scale_fill_manual(values = c("#187498", "#EB5353"), labels = c("Clinton", "Trump"), name = "Vote Choice") +
  geom_histogram(color="black", alpha = 0.8, position="identity") +
  xlim(0, 1) +
  theme_few()

anes_voted %>% filter(post_pres_vote == 0 | post_pres_vote == 1) %>%
  ggplot(aes(x=political_fact, fill=factor(voted_trump))) +
  labs(x = "Political Anxiety Index",
       y = "Number of Respondents",
       title = "") +
  scale_fill_manual(values = c("#187498", "#EB5353"), labels = c("Clinton", "Trump"), name = "Vote Choice") +
  geom_histogram(color="black", alpha = 0.8, position="identity") +
  xlim(0, 1) +
  theme_few()

anes_voted %>% filter(post_pres_vote == 0 | post_pres_vote == 1) %>%
  ggplot(aes(x=trumpft, fill=factor(voted_trump))) +
  labs(x = "Political anxiety index",
       y = "Number of respondents",
       title = "Histogram of political anxiety index by vote choice") +
  scale_fill_manual(values = c("#187498", "#EB5353"), labels = c("Not Trump", "Trump"), name = "Vote choice") +
  geom_density(color="black", alpha = 0.8, position="identity") +
  xlim(0, 1) +
  theme_bw()



ggplot(anes_voted %>% filter(primary_vote %in% c(1:9, -8, -1)),
       aes(x=factor(primary_vote),
           y=cultural_fact,
           fill=factor(primary_vote))) +
  labs(x = "Vote Choice",
       y = "Cultural anxiety index",
       title = "Distribution of the cultural anxiety index\nby vote choice in the primaries") +
  scale_x_discrete(labels=c("Don't know",
                            "Inapplicable",
                            "Clinton",
                            "Sanders",
                            "Another Democrat",
                            "Trump",
                            "Cruz",
                            "Kasich",
                            "Rubio",
                            "Another Republican",
                            "Other"),
                   guide = guide_axis(angle = -90),) +
  scale_fill_manual(
    values = c(
      "#ffffff",
      "#ffffff",
      "#187498",
      "#187498",
      "#187498",
      "#EB5353",
      "#EB5353",
      "#EB5353",
      "#EB5353",
      "#EB5353",
      "#C7C6C1"
    )
  ) +
  ylim(0, 1) +
  geom_boxplot() +
  guides(fill = "none") +
  theme_few()

anes_voted %>% filter(post_pres_vote == 0 | post_pres_vote == 1) %>%
  ggplot(aes(x = factor(voted_trump), fill = as.factor(pid3))) +
  geom_histogram(stat = "count") +
  labs(x = "Vote Choice", y = "Number of Respondents", title = "", fill = "Party") +
  scale_fill_manual(labels = c("Democrat", "Independent", "Republican"), values = party_colors) +
  scale_x_discrete(labels = c("Clinton", "Trump")) +
  theme_few()


anes2016_fact %>% filter(trump_primary_vote %in% 0:1) %>%
  ggplot(aes(x = factor(primary_vote))) +
  geom_histogram(stat = "count", fill = party_colors[3]) +
  labs(x = "Vote choice", y = "Number of respondents", title = "Republican primaries voters distribution for each candidate") +
  scale_fill_manual(labels = c("Democrat", "Independent", "Republican"), values = party_colors) +
  scale_x_discrete(labels = c("Donald Trump",
                              "Ted Cruz",
                              "John Kasich",
                              "Marco Rubio",
                              "Another Republican"),
                   guide = guide_axis(angle = -90)) +
  theme_bw()
