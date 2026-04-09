### Packages ###################################################################

  library(dtms)
  library(tidyverse)
  library(writexl)  
  library(margins)
  library(lme4)


### Load data ##################################################################

  load("Data/hrs_edited.Rda")


### Data handling ##############################################################

  # Get data right for regression, only recent waves, only working person years
  regdat <- hrs |> filter(wave>=11 & wave<=14 &
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
  physical_glmer <- glmer(physical ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                          family=binomial(),
                          data=regdatlim)
  
  stress_glmer <- glmer(stress ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                        family=binomial(),
                        data=regdatlim)
  
  poverty_glmer <- glmer(poverty ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                         family=binomial(),
                         data=regdatlim)
  
  # Average marginal effect
  ame_covid_physical <- margins(physical_glmer, type = "response",data=regdatlim) |> summary()
  ame_covid_stress <- margins(stress_glmer, type = "response",data=regdatlim) |> summary()
  ame_covid_poverty <- margins(poverty_glmer, type = "response",data=regdatlim) |> summary()
  
  # Logit 
  odd_covid_physical <- margins(physical_glmer, type = "link",data=regdatlim) |> summary()
  odd_covid_stress <- margins(stress_glmer, type = "link",data=regdatlim) |> summary()
  odd_covid_poverty <- margins(poverty_glmer, type = "link",data=regdatlim) |> summary()

  
### Save results ###############################################################  
  
  save(file="Results/no_covid.rda",
       list=c("ame_covid_physical","ame_covid_stress","ame_covid_poverty",
              "odd_covid_physical","odd_covid_stress","odd_covid_poverty"))  
