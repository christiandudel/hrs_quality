### Packages ###################################################################

  library(dtms)
  library(tidyverse)
  library(writexl)  
  library(margins)


### Load data ##################################################################

  load("Data/hrs_edited.Rda")


### Data handling ##############################################################

  # Get data right for regression, only recent waves, only working person years
  regdat <- hrs |> filter(wave>=11 & 
                            race!="Other" &
                            stateboth %in% c("working/healthy",
                                             "working/unhealthy"))
  
  # Edit variables a bit
  regdat$age2 <- regdat$age^2
  regdat$race <- as.factor(regdat$race)
  regdat$wave <- as.factor(regdat$wave)
  regdat$education <- as.factor(regdat$education)
  regdat$gender <- as.factor(regdat$gender)
  regdat$stateboth <- as.factor(regdat$stateboth)
  
  # Rescaling age
  regdat$age <- regdat$age/50  
  regdat$age2 <- regdat$age2/2500
  
  # Only keep individuals with at least two observations
  keepids <- regdat |> count(id) |> filter(n>1)
  regdatlim <- regdat |> filter(id%in%keepids$id)
  regdatlim <- regdatlim |> filter(!is.na(physical) & !is.na(stress) & !is.na(poverty))
  
  # Gender split
  regdatlim_m <- regdatlim |> filter(gender==1)
  regdatlim_f <- regdatlim |> filter(gender==2)


### Models (total population) ##################################################

  # Models
  physical_glm <- glm(physical ~ gender + race + education + age + age2 + wave + stateboth,
                      family=binomial(),
                      data=regdatlim)
  
  stress_glm <- glm(stress ~ gender + race + education + age + age2 + wave + stateboth,
                    family=binomial(),
                    data=regdatlim)
  
  poverty_glm <- glm(poverty ~ gender + race + education + age + age2 + wave + stateboth,
                    family=binomial(),
                    data=regdatlim)

  # Average marginal effect
  ame_pool_physical <- margins(physical_glm, type = "response",data=regdatlim) |> summary()
  ame_pool_stress <- margins(stress_glm, type = "response",data=regdatlim) |> summary()
  ame_pool_poverty <- margins(poverty_glm, type = "response",data=regdatlim) |> summary()
  
  # Logit 
  odd_pool_physical <- margins(physical_glm, type = "link",data=regdatlim) |> summary()
  odd_pool_stress <- margins(stress_glm, type = "link",data=regdatlim) |> summary()
  odd_pool_poverty <- margins(poverty_glm, type = "link",data=regdatlim) |> summary()


### Models (women) #############################################################  

  # Models
  physical_glm_f <- glm(physical ~ race + education + age + age2 + wave + stateboth ,
                            family=binomial(),
                            data=regdatlim_f)
  
  stress_glm_f <- glm(stress ~ race + education + age + age2 + wave + stateboth,
                          family=binomial(),
                          data=regdatlim_f)
  
  poverty_glm_f <- glm(poverty ~ race + education + age + age2 + wave + stateboth,
                           family=binomial(),
                           data=regdatlim_f)
  
  # Average marginal effect
  ame_pool_physical_f <- margins(physical_glm_f, type = "response",data=regdatlim_f) |> summary()
  ame_pool_stress_f <- margins(stress_glm_f, type = "response",data=regdatlim_f) |> summary()
  ame_pool_poverty_f <- margins(poverty_glm_f, type = "response",data=regdatlim_f) |> summary()
  
  # Logit 
  odd_pool_physical_f <- margins(physical_glm_f, type = "link",data=regdatlim_f) |> summary()
  odd_pool_stress_f <- margins(stress_glm_f, type = "link",data=regdatlim_f) |> summary()
  odd_pool_poverty_f <- margins(poverty_glm_f, type = "link",data=regdatlim_f) |> summary()


### Models (men) #############################################################  

  # Models
  physical_glm_m <- glm(physical ~ race + education + age + age2 + wave + stateboth,
                            family=binomial(),
                            data=regdatlim_m)
  
  stress_glm_m <- glm(stress ~ race + education + age + age2 + wave + stateboth,
                          family=binomial(),
                          data=regdatlim_m)
  
  poverty_glm_m <- glm(poverty ~ race + education + age + age2 + wave + stateboth,
                           family=binomial(),
                           data=regdatlim_m)
  
  # Average marginal effect
  ame_pool_physical_m <- margins(physical_glm_m, type = "response",data=regdatlim_m) |> summary()
  ame_pool_stress_m <- margins(stress_glm_m, type = "response",data=regdatlim_m) |> summary()
  ame_pool_poverty_m <- margins(poverty_glm_m, type = "response",data=regdatlim_m) |> summary()
  
  # Logit 
  odd_pool_physical_m <- margins(physical_glm_m, type = "link",data=regdatlim_m) |> summary()
  odd_pool_stress_m <- margins(stress_glm_m, type = "link",data=regdatlim_m) |> summary()
  odd_pool_poverty_m <- margins(poverty_glm_m, type = "link",data=regdatlim_m) |> summary()
  
  
### Save results ###############################################################  
  
  save(file="Results/pooled.rda",
       list=c("ame_pool_physical","ame_pool_stress","ame_pool_poverty",
              "odd_pool_physical","odd_pool_stress","odd_pool_poverty",
              "ame_pool_physical_m","ame_pool_stress_m","ame_pool_poverty_m",
              "odd_pool_physical_m","odd_pool_stress_m","odd_pool_poverty_m",
              "ame_pool_physical_f","ame_pool_stress_f","ame_pool_poverty_f",
              "odd_pool_physical_f","odd_pool_stress_f","odd_pool_poverty_f"))  
