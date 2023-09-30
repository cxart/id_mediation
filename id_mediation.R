##### CODE FOR MIXED-DESIGN MEDIATION #####
# 9.29.2023 - Carter Allen (atcarterallen@gmail.com)

# This code performs a mediation analysis,
# block-bootstrapping/resampling by participant
# rather than by row.

# Install/load packages

# add packages / install if necessary
packages<-list("tidyverse","sandwich", "boot")
for (i in packages){
  if(!require(i,character.only = TRUE)) {install.packages(i); require(i,character.only = TRUE)}
}

# prevent masking dplyr select function
select <- dplyr::select

# Function to resample, calculate indirect effect

indirectsaved = function(formula2, formula3, dataset, ids) {
  
  # get unique ids
  unique_ids <- unique(ids)
  
  # shuffle ids with replacement
  shuffled_ids <- sample(unique_ids, replace=TRUE)
  
  # create a new dataframe by appending rows for each shuffled id
  d <- dataset %>% 
    filter(id %in% shuffled_ids) %>%
    arrange(match(id, shuffled_ids))
  
  model2 = lm(formula2, data = d)
  model3 = lm(formula3, data = d)
  a = coef(model2)[2]
  b = coef(model3)[3]
  c = coef(model3)[2] # direct effect
  indirect = a * b
  total = c + indirect
  
  # return
  c(indirect, total, c)
  
}

##### EXAMPLE USE CASE #####

# example longform pivot for analysis

df <- fulldata %>% 
  mutate(
    id = 1:n()
  ) %>% 
  select(
    id,
    DV_A,
    DV_B,
    mediator_A,
    mediator_B
  ) %>% 
  pivot_longer( # 1 row per observation
    cols = c(DV_A, DV_B),
    names_to = "treat",
    values_to = "DV"
  ) %>% 
  mutate(
    mediator = ifelse( # create mediator var
      treat == "DV_A",
      mediator_A,
      mediator_B
    ),
    treat = ifelse( # contrast-code
      treat == "DV_A",
      -0.5, 0.5
    )
  ) %>% 
  select(
    -mediator_A,
    -mediator_B
  )

# view structure
df

# Direct and total effects (cluster SEs by ID)

mediator_model <- lm.cluster(DV2 ~ treat, data = df, cluster = "id")
summary(mediator_model)

outcome_model <- lm.cluster(DV1 ~ treat + DV2, data = df, cluster = "id")
summary(outcome_model)

# Block bootstrap mediation analysis

set.seed(12345)  # for reproducibility

# Calculate bootstrapped results

bootresults = boot(data = df,
                   statistic = indirectsaved,
                   formula2 = DV2 ~ treat,
                   formula3 = DV1 ~ treat + DV2,
                   R = 10000)

# Print results

# indirect (1), total (2), direct (3) effects
bootresults
boot.ci(bootresults,
        conf = .95,
        type = "norm")

# p-value
p_value = sum(bootresults$t[,1] <= 0) / length(bootresults$t[,1])
p_value

# prop mediated
prop_mediated <- bootresults$t[,1] / bootresults$t[,2]
mean(prop_mediated)

