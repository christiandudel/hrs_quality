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
  regdat <- hrs |> filter(wave>=11 & weight>0 &
                            race!="Other" &
                            stateboth %in% c("working/healthy",
                                             "working/unhealthy"))
  
  # Edit variables a bit
  regdat$age2 <- regdat$age^2
  regdat$race <- factor(regdat$race,levels=regdat$race |> unique())
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
  
  # Rescaling weights
  # https://cran.r-project.org/web/packages/WeMix/vignettes/Introduction_to_Mixed_Effects_Models_With_WeMix.pdf
  regdatlim <- regdatlim |> add_count(id,name="count") |> group_by(id) |>  
    mutate(weight=weight * (count/sum(weight))) |> 
    ungroup()
  
  
### SES ########################################################################
  
  ses100 <- glmer(eri ~ gender + race + education + age + age2 + wave + (1|id),
               family=binomial(),
               data=regdatlim[regdatlim$stateboth%in%"working/unhealthy",],
               weights=weight)
  
  ses75 <- glmer(eri75 ~ gender + race + education + age + age2 + wave + (1|id),
                  family=binomial(),
                  data=regdatlim[regdatlim$stateboth%in%"working/unhealthy",],
                  weights=weight)
  
  ses125 <- glmer(eri125 ~ gender + race + education + age + age2 + wave + (1|id),
                 family=binomial(),
                 data=regdatlim[regdatlim$stateboth%in%"working/unhealthy",],
                 weights=weight)
  
  ses150 <- glmer(eri150 ~ gender + race + education + age + age2 + wave + (1|id),
                  family=binomial(),
                  data=regdatlim[regdatlim$stateboth%in%"working/unhealthy",],
                  weights=weight)
  
  ses200 <- glmer(eri200 ~ gender + race + education + age + age2 + wave + (1|id),
                  family=binomial(),
                  data=regdatlim[regdatlim$stateboth%in%"working/unhealthy",],
                  weights=weight)
  
  ame_ses100 <- margins(ses100, type = "response",data=regdatlim[regdatlim$stateboth%in%"working/unhealthy",]) |> summary()
  ame_ses75 <- margins(ses75, type = "response",data=regdatlim[regdatlim$stateboth%in%"working/unhealthy",]) |> summary()
  ame_ses125 <- margins(ses125, type = "response",data=regdatlim[regdatlim$stateboth%in%"working/unhealthy",]) |> summary()
  ame_ses150 <- margins(ses150, type = "response",data=regdatlim[regdatlim$stateboth%in%"working/unhealthy",]) |> summary()
  ame_ses200 <- margins(ses200, type = "response",data=regdatlim[regdatlim$stateboth%in%"working/unhealthy",]) |> summary()
  
  
### Models (total population) ##################################################

  # Models
  p100_glmer <- glmer(eri ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                          family=binomial(),
                          data=regdatlim,
                          weights=weight)
  
  p75_glmer <- glmer(eri75 ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                      family=binomial(),
                      data=regdatlim,
                      weights=weight)
  
  p125_glmer <- glmer(eri125 ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                     family=binomial(),
                     data=regdatlim,
                     weights=weight)
  
  p150_glmer <- glmer(eri150 ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                      family=binomial(),
                      data=regdatlim,
                      weights=weight)
  
  p200_glmer <- glmer(eri200 ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                      family=binomial(),
                      data=regdatlim,
                      weights=weight)
  
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
       list=c("ame_ses100","ame_ses75","ame_ses125","ame_ses150","ame_ses200",
              "ame_p100","ame_p75","ame_p125","ame_p150","ame_p200",
              "odd_p100","odd_p75","odd_p125","odd_p150","odd_p200"))    