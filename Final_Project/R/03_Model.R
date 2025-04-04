#modeling

library(tidyverse)
library(easystats)
library(MASS)
library(ranger)
#install.packages("lme4")
library(lme4)
#install.packages("lmerTest")
library(lmerTest)
install.packages("MuMIn")
library(MuMIn)
library(broom.mixed)
#install.packages("sjPlot")
library(sjPlot)

df <- read_csv("./Data/Merged_DSR_Origin_Dataset.csv")

#remove NAs and set factors
df_clean <- df %>% drop_na(DSR) %>% drop_na(Origin) %>% 
  mutate(
    Origin = as.factor(Origin),
    Species = as.factor(Species),
    Season = as.factor(Season),
    Block = as.factor(Block),
    Rep = as.factor(Rep)
  )

mod1 <- 
  glm(data = df_clean,
      formula = DSR ~ Origin + Species + Block + Season + Rep)

mod2 <- 
  glm(data = df_clean,
      formula = DSR ~ Origin + Species) 

compare_performance(mod1, mod2)


#okay, those suck,
#let's try an LMER model
#this predicts DSR based on:
  #Fixed effects: Origin and species
  #Random effects: Season, Block, and Rep

#lmer model
model_lmer <- lmer(DSR ~ Origin + Species + (1|Season) + (1|Block) + (1|Rep), data = df_clean)

#test
# Refit model with lmerTest to get p-values
# These P values tell us wheather each origin or species effect is statistically important
model_lmer_test <- lmer(DSR ~ Origin + Species + (1|Season) + (1|Block) + (1|Rep), data = df_clean)

# See summary with p-values
#this model shows:
  #which origins impact DSR
  #which sp impact DSR
  #by how much
summary(model_lmer_test)

#what is the R squared value for this model
#Marginal R^2 how much is explained by fixed effects only
#Conditional R^2 = hw much variance is explained by fixed and random effects
r.squaredGLMM(model_lmer)

# Extract effects
fixed_effects <- broom.mixed::tidy(model_lmer, effects = "fixed")

#make a clean, tidy version of fixed effects
tab_model(model_lmer)

tab_model(model_lmer, file = "./Models/Model_Summary.html")


#make a plot showing the importance of Origin on DSR
#include Stars indicating P value
# Filter Origins only
fixed_effects_origin <- fixed_effects %>%
  filter(grepl("^Origin", term))

#remove origin prefix
fixed_effects_origin <- fixed_effects_origin %>%
  mutate(clean_term = sub("^Origin", "", term)) %>% # use sub, not gsub
  mutate(significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ ".",
    TRUE            ~ ""
  ))

m1 <- ggplot(fixed_effects_origin, aes(x = reorder(clean_term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  geom_text(aes(label = ifelse(p.value < 0.05, significance, "")), hjust = -0.5,  color = "red", size = 5)+
  coord_flip() +
  labs(title = "Effect of Origin on DSR",
       x = "Origin",
       y = "Effect on DSR") +
  theme_bw()

ggsave("./Plots/m1_origin.png", plot = m1, width = 10, height = 10, dpi = 300)

#make a similar plot but using species
fixed_effects_species <- fixed_effects %>%
  filter(grepl("^Species", term)) %>%
  mutate(clean_term = sub("^Species", "", term))

fixed_effects_species <- fixed_effects_species %>%
  mutate(significance = case_when(
    p.value < 0.001 ~ "***",
    p.value < 0.01  ~ "**",
    p.value < 0.05  ~ "*",
    p.value < 0.1   ~ ".",
    TRUE            ~ ""
  ))

m2 <- ggplot(fixed_effects_species, aes(x = reorder(clean_term, estimate), y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
  geom_text(aes(label = ifelse(p.value < 0.05, significance, "")), vjust = -0.25, color = "red", size = 5)+
  coord_flip() +
  labs(title = "Effect of Species on DSR",
       x = "Species",
       y = "Effect on DSR") +
  theme_bw()

ggsave("./Plots/m2_species.png", plot = m2, width = 10, height = 10, dpi = 300)
