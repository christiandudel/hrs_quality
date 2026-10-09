### Packages ###################################################################

  library(dtms)
  library(tidyverse)
  library(writexl)  
  library(margins)


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
  
  
  # Gender split
  regdatlim_m <- regdatlim |> filter(gender==1)
  regdatlim_f <- regdatlim |> filter(gender==2)


### Effect race/ethnicity, education ###########################################
  
  ses <- glm(eri ~ gender + race + education + age + age2 + wave ,
               family=binomial(),
               data=regdatlim[regdatlim$stateboth%in%"working/unhealthy",])
  
  ame_pool_ses <- margins(ses, type = "response",data=regdatlim[regdatlim$stateboth%in%"working/unhealthy",]) |> summary()
  
  

### Models (total population) ##################################################

  # Models
  eri_glm <- glm(eri ~ gender + race + education + age + age2 + wave + stateboth,
                      family=binomial(),
                      data=regdatlim)
  
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
  ame_pool_eri <- margins(eri_glm, type = "response",data=regdatlim) |> summary()
  ame_pool_physical <- margins(physical_glm, type = "response",data=regdatlim) |> summary()
  ame_pool_stress <- margins(stress_glm, type = "response",data=regdatlim) |> summary()
  ame_pool_poverty <- margins(poverty_glm, type = "response",data=regdatlim) |> summary()
  
  # Logit 
  odd_pool_eri <- margins(eri_glm, type = "link",data=regdatlim) |> summary()
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

  
  
### Save results ###############################################################  
  
  save(file="Results/pooled.rda",
       list=c("ame_pool_ses",
              "ame_pool_eri","ame_pool_physical","ame_pool_stress","ame_pool_poverty",
              "odd_pool_eri","odd_pool_physical","odd_pool_stress","odd_pool_poverty"))  
