###########################################################################
### Item Nonresponse in ESS Round 9
### Author: Malte Grönemann
### Version: R 4.0.5 on Linux
###########################################################################

library(fixest)
library(tidyverse)
library(haven)


# import of the raw data and joining
# necessary to change encoding on Linux/OSX, remove if on Windows. 
# See https://haven.tidyverse.org/reference/read_dta.html#character-encoding
ESS9 <- full_join(read_dta("ESS9e03_1.dta", encoding = "latin1"), 
                  read_dta("ESS9INTe03.dta", encoding = "latin1"), 
                  by = c("idno", "cntry"))

# calculating the numbers of refusals and DK by interview
# See https://haven.tidyverse.org/articles/semantics.html#tagged-missing-values-1
# ESS coding: Not applicable -> a, Refusal -> b Don't know -> c, Not available -> d
count_refusals <- ESS9 %>% 
  select(nwspol:impfun) %>% # only substantial variables
  select(where(~ any(is_tagged_na(.x, tag = "a")) == FALSE)) %>% # excluding vars affected by routing
  mutate(across(.fns = ~ is_tagged_na(.x, tag = "b"))) %>%
  rowwise() %>%
  transmute(n_refusals = sum(c_across()))

count_dk <- ESS9 %>% 
  select(nwspol:impfun) %>%
  select(where(~ any(is_tagged_na(.x, tag = "a")) == FALSE)) %>%
  mutate(across(.fns = ~ is_tagged_na(.x, tag = "c"))) %>%
  rowwise() %>%
  transmute(n_dk = sum(c_across()))

# total numbers of DK and REF
sum(count_dk$n_dk)
sum(count_refusals$n_refusals)

# selecting and if necessary transforming variables for analysis
analysis <- ESS9 %>%
  transmute(`Age (in 10 years)` = agea / 10, # respondent
            `Female` = gndr - 1,
            `Education (ISCED)` = ifelse(eisced < 10, eisced, NA),
            `Ethnic Minority Member` = blgetmg * -1 + 2,
            `Member of discriminated Group` = dscrgrp * -1 + 2,
            `Political Interest` = (polintr-1) * -4/3 + 5,
            `People have Political Influence` = psppipla,
            `Health` = health * -1 + 6,
            `Generalised Trust` = ppltrst/2,
            `Duration of Interview (in 30 min)` = ifelse(inwtm < 180, inwtm/30, 180/30), # interview 
            `Interference of Interview` = preintf * -1 + 2,
            `Interview not in primary Language` = as.integer(lnghom1 != intlnga),
            `Gender Matching` = ifelse(intgndr < 5, as.numeric(intgndr == gndr), NA),
            `Respondent more than 10 years older` = ifelse(intagea < 200, as.numeric(agea - intagea > 10), NA),
            `Interviewer more than 10 years older` = ifelse(intagea < 200, as.numeric(intagea - agea > 10), NA),
            `Use of Showcards` = ifelse(resswcd < 6, (resswcd-1) * -2 + 5, NA), # explanations
            `Asked for Clarifications` = ifelse(resclq < 6, resclq, NA),
            `Reluctant to Answer` = ifelse(resrelq < 6, resrelq, NA),
            `Answered to Best Ability` = ifelse(resbab < 6, resbab, NA),
            `Understood Questions` = ifelse(resundq < 6, resundq, NA),
            `Interviewer` = paste(intnum, cntry, sep = ""), # FE
            n_dk = count_dk$n_dk, # DV
            n_refusals = count_refusals$n_refusals,
            n_total = n_dk + n_refusals)

colSums(is.na(analysis)) # number of missings by variable


# multivariate analysis
model <- fenegbin(data = analysis,
              fml =  c(n_dk, n_refusals, n_total) ~ 
                `Age (in 10 years)` + `Female` + `Education (ISCED)` + `Ethnic Minority Member` + 
                `Member of discriminated Group` + `Political Interest` + `People have Political Influence` +
                `Health` + `Generalised Trust` +
                `Duration of Interview (in 30 min)` + `Interference of Interview` + 
                `Interview not in primary Language` + `Gender Matching` +
                `Respondent more than 10 years older` + `Interviewer more than 10 years older` +
                `Use of Showcards` + `Asked for Clarifications` + `Reluctant to Answer` + 
                `Answered to Best Ability` + `Understood Questions` |
                `Interviewer`,
              se = "cluster")

setFixest_dict(c(n_dk = "DK", 
                 n_refusals = "Refusals",
                 n_total = "Total"))
etable(model, file = "results.tex", digits = 3, sdBelow = FALSE)

results <- bind_rows(bind_cols(model$n_dk$coeftable, y = "Don't know"),
                     bind_cols(model$n_refusals$coeftable, y = "Refusal"),
                     bind_cols(model$n_total$coeftable, y = "Total")) %>%
  rename(estimate = `Estimate`,
         se = `Std. Error`) %>%
  rownames_to_column(var = "var") %>%
  mutate(var = str_replace(var, "\\..*", ""),
         var = str_replace_all(var, "`", ""),
         var = factor(var, levels = unique(rev(results$var))),
         irr = exp(estimate),
         upper = exp(estimate + se * (-qnorm((1-0.95)/2))),
         lower = exp(estimate - se * (-qnorm((1-0.95)/2)))) %>%
  filter(var != "")

ggplot(results) +
  aes(y = var,
      x = irr,
      colour = y,
      shape = y) +
  geom_vline(xintercept = 1, 
             colour = "darkgrey") +
  geom_point(position = position_dodge(width = .5), 
             size = 2) +
  geom_linerange(aes(y = var, 
                     xmin = lower, 
                     xmax = upper),
                 lwd = .5, 
                 position = position_dodge(width = .6)) +
  facet_wrap(~y) +
  scale_color_manual(values = c("#E69F00", "#009E73", "#D55E00")) +
  scale_shape_manual(values = 15:17) +
  theme(legend.position = "bottom", 
        strip.background = element_blank(), strip.text = element_blank(),
        text = element_text(size = 15)) +
  labs(y = "", x = "Incidence Rate Ratios with 95% Confidence Intervals", 
       colour = "Dependent Variable", shape = "Dependent Variable")
ggsave("results.pdf", width = 22, height = 15, units = "cm")
