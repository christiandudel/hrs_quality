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
  

### Models (total population) ##################################################

  # Models
  p100_glmer <- glmer(poverty ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                          family=binomial(),
                          data=regdatlim)
  
  p75_glmer <- glmer(poverty75 ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                      family=binomial(),
                      data=regdatlim)
  
  p125_glmer <- glmer(poverty125 ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                     family=binomial(),
                     data=regdatlim)
  
  p150_glmer <- glmer(poverty150 ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                      family=binomial(),
                      data=regdatlim)
  
  p200_glmer <- glmer(poverty200 ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                      family=binomial(),
                      data=regdatlim)
  
  # Average marginal effect
  ame_p100 <- margins(p100_glmer, type = "response",data=regdatlim) |> summary()
  ame_p75 <- margins(p75_glmer, type = "response",data=regdatlim) |> summary()
  ame_p125 <- margins(p125_glmer, type = "response",data=regdatlim) |> summary()
  ame_p150 <- margins(p150_glmer, type = "response",data=regdatlim) |> summary()
  ame_p200 <- margins(p200_glmer, type = "response",data=regdatlim) |> summary()

  # Logit 
  odd_p100 <- margins(p100_glmer, type = "link",data=regdatlim) |> summary()
  odd_p75 <- margins(p75_glmer, type = "link",data=regdatlim) |> summary()
  odd_p125 <- margins(p125_glmer, type = "link",data=regdatlim) |> summary()
  odd_p150 <- margins(p150_glmer, type = "link",data=regdatlim) |> summary()
  odd_p200 <- margins(p200_glmer, type = "link",data=regdatlim) |> summary()

  
### Save results ###############################################################  
  
  save(file="Results/poverty.rda",
       list=c("ame_p100","ame_p75","ame_p125","ame_p150","ame_p200",
              "odd_p100","odd_p75","odd_p125","odd_p150","odd_p200"))    