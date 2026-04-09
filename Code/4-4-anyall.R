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
  regdatlim <- regdatlim |> filter(!is.na(anybad) & !is.na(allbad) & 
                                   !is.na(pair1) & !is.na(pair2) & 
                                   !is.na(pair3))

  
### Models (total population) ##################################################

  # Models
  allbad_glmer <- glmer(allbad ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                          family=binomial(),
                          data=regdatlim)
  
  anybad_glmer <- glmer(anybad ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                        family=binomial(),
                        data=regdatlim)
  
  pair1_glmer <- glmer(pair1 ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                        family=binomial(),
                        data=regdatlim)
  
  pair2_glmer <- glmer(pair2 ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                       family=binomial(),
                       data=regdatlim)
  
  pair3_glmer <- glmer(pair3 ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                       family=binomial(),
                       data=regdatlim)
  
  # Average marginal effect
  ame_allbad <- margins(allbad_glmer, type = "response",data=regdatlim) |> summary()
  ame_anybad <- margins(anybad_glmer, type = "response",data=regdatlim) |> summary()
  ame_pair1 <- margins(pair1_glmer, type = "response",data=regdatlim) |> summary()
  ame_pair2 <- margins(pair2_glmer, type = "response",data=regdatlim) |> summary()
  ame_pair3 <- margins(pair3_glmer, type = "response",data=regdatlim) |> summary()
  
  # Logit 
  odd_allbad <- margins(allbad_glmer, type = "link",data=regdatlim) |> summary()
  odd_anybad <- margins(anybad_glmer, type = "link",data=regdatlim) |> summary()
  odd_pair1 <- margins(pair1_glmer, type = "link",data=regdatlim) |> summary()
  odd_pair2 <- margins(pair2_glmer, type = "link",data=regdatlim) |> summary()
  odd_pair3 <- margins(pair3_glmer, type = "link",data=regdatlim) |> summary()
  
  
### Save results ###############################################################  
  
  save(file="Results/anyall.rda",
       list=c("ame_allbad","ame_anybad","ame_pair1","ame_pair2","ame_pair3",
              "odd_allbad","odd_anybad","odd_pair1","odd_pair2","odd_pair3"))  
