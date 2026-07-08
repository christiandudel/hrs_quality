### Packages ###################################################################

  library(dtms)
  library(tidyverse)
  library(writexl)  
  library(margins)
  library(lme4)


### Load data ##################################################################

  load("Data/hrs_edited.Rda")


### Data handling (self-rated health) ##########################################

  # Get data right for regression, only recent waves, only working person years
  regdatshlt <- hrs |> filter(wave>=11 & 
                            race!="Other" &
                            workshlt %in% c("working/healthy",
                                             "working/unhealthy"))

  # Edit variables a bit
  regdatshlt$age2 <- regdatshlt$age^2
  regdatshlt$race <- as.factor(regdatshlt$race)
  regdatshlt$wave <- as.factor(regdatshlt$wave)
  regdatshlt$education <- as.factor(regdatshlt$education)
  regdatshlt$gender <- as.factor(regdatshlt$gender)
  regdatshlt$workshlt <- as.factor(regdatshlt$workshlt)

  # Rescaling age
  regdatshlt$age <- regdatshlt$age/50  
  regdatshlt$age2 <- regdatshlt$age2/2500
  
  # Only keep individuals with at least two observations
  keepids <- regdatshlt |> count(id) |> filter(n>1)
  regdatlimshlt <- regdatshlt |> filter(id%in%keepids$id)
  regdatlimshlt <- regdatlimshlt |> filter(!is.na(physical) & !is.na(stress) & !is.na(poverty))
  
  
### Data handling (depression) #################################################
  
  # Get data right for regression, only recent waves, only working person years
  regdatdepyr <- hrs |> filter(wave>=11 & 
                                race!="Other" &
                                 workdepyr %in% c("working/healthy",
                                                "working/unhealthy"))
  
  # Edit variables a bit
  regdatdepyr$age2 <- regdatdepyr$age^2
  regdatdepyr$race <- as.factor(regdatdepyr$race)
  regdatdepyr$wave <- as.factor(regdatdepyr$wave)
  regdatdepyr$education <- as.factor(regdatdepyr$education)
  regdatdepyr$gender <- as.factor(regdatdepyr$gender)
  regdatdepyr$workshlt <- as.factor(regdatdepyr$workdepyr)
  
  # Rescaling age
  regdatdepyr$age <- regdatdepyr$age/50  
  regdatdepyr$age2 <- regdatdepyr$age2/2500
  
  # Only keep individuals with at least two observations
  keepids <- regdatdepyr |> count(id) |> filter(n>1)
  regdatlimdepyr <- regdatdepyr |> filter(id%in%keepids$id)
  regdatlimdepyr <- regdatlimdepyr |> filter(!is.na(physical) & !is.na(stress) & !is.na(poverty))
  
  
### Data handling (cognition) #################################################
  
  # Get data right for regression, only recent waves, only working person years
  regdatcogd <- hrs |> filter(wave>=11 & 
                                 race!="Other" &
                                 workcogd %in% c("working/healthy",
                                                  "working/unhealthy"))
  
  # Edit variables a bit
  regdatcogd$age2 <- regdatcogd$age^2
  regdatcogd$race <- as.factor(regdatcogd$race)
  regdatcogd$wave <- as.factor(regdatcogd$wave)
  regdatcogd$education <- as.factor(regdatcogd$education)
  regdatcogd$gender <- as.factor(regdatcogd$gender)
  regdatcogd$workshlt <- as.factor(regdatcogd$workdepyr)
  
  # Rescaling age
  regdatcogd$age <- regdatcogd$age/50  
  regdatcogd$age2 <- regdatcogd$age2/2500
  
  # Only keep individuals with at least two observations
  keepids <- regdatcogd |> count(id) |> filter(n>1)
  regdatlimcogd <- regdatcogd |> filter(id%in%keepids$id)
  regdatlimcogd <- regdatlimcogd |> filter(!is.na(physical) & !is.na(stress) & !is.na(poverty))  

  
### Models: self-rated health ##################################################

  # Models
  physical_glmer <- glmer(physical ~ gender + race + education + age + age2 + wave + workshlt + (1|id),
                          family=binomial(),
                          data=regdatlimshlt)
  
  stress_glmer <- glmer(stress ~ gender + race + education + age + age2 + wave + workshlt + (1|id),
                        family=binomial(),
                        data=regdatlimshlt)
  
  poverty_glmer <- glmer(poverty ~ gender + race + education + age + age2 + wave + workshlt + (1|id),
                         family=binomial(),
                         data=regdatlimshlt)
  
  # Average marginal effect
  ame_shlt_physical <- margins(physical_glmer, type = "response",data=regdatlimshlt) |> summary()
  ame_shlt_stress <- margins(stress_glmer, type = "response",data=regdatlimshlt) |> summary()
  ame_shlt_poverty <- margins(poverty_glmer, type = "response",data=regdatlimshlt) |> summary()
  
  # Logit 
  odd_shlt_physical <- margins(physical_glmer, type = "link",data=regdatlimshlt) |> summary()
  odd_shlt_stress <- margins(stress_glmer, type = "link",data=regdatlimshlt) |> summary()
  odd_shlt_poverty <- margins(poverty_glmer, type = "link",data=regdatlimshlt) |> summary()
  

### Models: Depression #########################################################
  
  # Models
  physical_glmer <- glmer(physical ~ gender + race + education + age + age2 + wave + workdepyr + (1|id),
                          family=binomial(),
                          data=regdatlimdepyr)
  
  stress_glmer <- glmer(stress ~ gender + race + education + age + age2 + wave + workdepyr + (1|id),
                        family=binomial(),
                        data=regdatlimdepyr)
  
  poverty_glmer <- glmer(poverty ~ gender + race + education + age + age2 + wave + workdepyr + (1|id),
                         family=binomial(),
                         data=regdatlimdepyr)
  
  # Average marginal effect
  ame_depyr_physical <- margins(physical_glmer, type = "response",data=regdatlimdepyr) |> summary()
  ame_depyr_stress <- margins(stress_glmer, type = "response",data=regdatlimdepyr) |> summary()
  ame_depyr_poverty <- margins(poverty_glmer, type = "response",data=regdatlimdepyr) |> summary()
  
  # Logit 
  odd_depyr_physical <- margins(physical_glmer, type = "link",data=regdatlimdepyr) |> summary()
  odd_depyr_stress <- margins(stress_glmer, type = "link",data=regdatlimdepyr) |> summary()
  odd_depyr_poverty <- margins(poverty_glmer, type = "link",data=regdatlimdepyr) |> summary()
  
  
### Models: cognition ##########################################################  
  
  # Models
  physical_glmer <- glmer(physical ~ gender + race + education + age + age2 + wave + workcogd + (1|id),
                          family=binomial(),
                          data=regdatlimcogd)
  
  stress_glmer <- glmer(stress ~ gender + race + education + age + age2 + wave + workcogd + (1|id),
                        family=binomial(),
                        data=regdatlimcogd)
  
  poverty_glmer <- glmer(poverty ~ gender + race + education + age + age2 + wave + workcogd + (1|id),
                         family=binomial(),
                         data=regdatlimcogd)
  
  # Average marginal effect
  ame_cogd_physical <- margins(physical_glmer, type = "response",data=regdatlimcogd) |> summary()
  ame_cogd_stress <- margins(stress_glmer, type = "response",data=regdatlimcogd) |> summary()
  ame_cogd_poverty <- margins(poverty_glmer, type = "response",data=regdatlimcogd) |> summary()
  
  # Logit 
  odd_cogd_physical <- margins(physical_glmer, type = "link",data=regdatlimcogd) |> summary()
  odd_cogd_stress <- margins(stress_glmer, type = "link",data=regdatlimcogd) |> summary()
  odd_cogd_poverty <- margins(poverty_glmer, type = "link",data=regdatlimcogd) |> summary()
  
  
### Save results ###############################################################  
  
  save(file="Results/healthdef.rda",
       list=c("ame_shlt_physical","ame_shlt_stress","ame_shlt_poverty",
              "odd_shlt_physical","odd_shlt_stress","odd_shlt_poverty",
              "ame_depyr_physical","ame_depyr_stress","ame_depyr_poverty",
              "odd_depyr_physical","odd_depyr_stress","odd_depyr_poverty",
              "ame_cogd_physical","ame_cogd_stress","ame_cogd_poverty",
              "odd_cogd_physical","odd_cogd_stress","odd_cogd_poverty"))
  

