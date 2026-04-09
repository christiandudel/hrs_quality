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
  
  # Gender split
  regdatlim_m <- regdatlim |> filter(gender==1)
  regdatlim_f <- regdatlim |> filter(gender==2)
  
  # Gender & race/ethnicity split
  regdatlim_m_w <- regdatlim_m |> filter(race=="White")
  regdatlim_m_b <- regdatlim_m |> filter(race=="Black")
  regdatlim_m_h <- regdatlim_m |> filter(race=="Hispan")
  
  regdatlim_f_w <- regdatlim_f |> filter(race=="White")
  regdatlim_f_b <- regdatlim_f |> filter(race=="Black")
  regdatlim_f_h <- regdatlim_f |> filter(race=="Hispan")
  
  
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
  ame_physical <- margins(physical_glmer, type = "response",data=regdatlim) |> summary()
  ame_stress <- margins(stress_glmer, type = "response",data=regdatlim) |> summary()
  ame_poverty <- margins(poverty_glmer, type = "response",data=regdatlim) |> summary()
  
  # Logit 
  odd_physical <- margins(physical_glmer, type = "link",data=regdatlim) |> summary()
  odd_stress <- margins(stress_glmer, type = "link",data=regdatlim) |> summary()
  odd_poverty <- margins(poverty_glmer, type = "link",data=regdatlim) |> summary()
  

### Models (women) #############################################################  
  
  # Models
  physical_glmer_f <- glmer(physical ~ race + education + age + age2 + wave + stateboth + (1|id),
                          family=binomial(),
                          data=regdatlim_f)
  
  stress_glmer_f <- glmer(stress ~ race + education + age + age2 + wave + stateboth + (1|id),
                        family=binomial(),
                        data=regdatlim_f)
  
  poverty_glmer_f <- glmer(poverty ~ race + education + age + age2 + wave + stateboth + (1|id),
                         family=binomial(),
                         data=regdatlim_f)
  
  # Average marginal effect
  ame_physical_f <- margins(physical_glmer_f, type = "response",data=regdatlim_f) |> summary()
  ame_stress_f <- margins(stress_glmer_f, type = "response",data=regdatlim_f) |> summary()
  ame_poverty_f <- margins(poverty_glmer_f, type = "response",data=regdatlim_f) |> summary()
  
  # Logit 
  odd_physical_f <- margins(physical_glmer_f, type = "link",data=regdatlim_f) |> summary()
  odd_stress_f <- margins(stress_glmer_f, type = "link",data=regdatlim_f) |> summary()
  odd_poverty_f <- margins(poverty_glmer_f, type = "link",data=regdatlim_f) |> summary()
  
  
### Models (men) #############################################################  
  
  # Models
  physical_glmer_m <- glmer(physical ~ race + education + age + age2 + wave + stateboth + (1|id),
                            family=binomial(),
                            data=regdatlim_m)
  
  stress_glmer_m <- glmer(stress ~ race + education + age + age2 + wave + stateboth + (1|id),
                          family=binomial(),
                          data=regdatlim_m)
  
  poverty_glmer_m <- glmer(poverty ~ race + education + age + age2 + wave + stateboth + (1|id),
                           family=binomial(),
                           data=regdatlim_m)
  
  # Average marginal effect
  ame_physical_m <- margins(physical_glmer_m, type = "response",data=regdatlim_m) |> summary()
  ame_stress_m <- margins(stress_glmer_m, type = "response",data=regdatlim_m) |> summary()
  ame_poverty_m <- margins(poverty_glmer_m, type = "response",data=regdatlim_m) |> summary()
  
  # Logit 
  odd_physical_m <- margins(physical_glmer_m, type = "link",data=regdatlim_m) |> summary()
  odd_stress_m <- margins(stress_glmer_m, type = "link",data=regdatlim_m) |> summary()
  odd_poverty_m <- margins(poverty_glmer_m, type = "link",data=regdatlim_m) |> summary()   
  
  
### Models (women by race/ethnicity) ###########################################
  
  # Models
  physical_glmer_f_w <- glmer(physical ~ education + age + wave +  stateboth + (1|id),
                            family=binomial(),
                            data=regdatlim_f_w)
  
  physical_glmer_f_b <- glmer(physical ~ education + age + wave +  stateboth + (1|id),
                              family=binomial(),
                              data=regdatlim_f_b)
  
  physical_glmer_f_h <- glmer(physical ~ education + age + wave +  stateboth + (1|id),
                              family=binomial(),
                              data=regdatlim_f_h)
  
  stress_glmer_f_w <- glmer(stress ~  education + age + wave +  stateboth + (1|id),
                          family=binomial(),
                          data=regdatlim_f_w)
  
  stress_glmer_f_b <- glmer(stress ~  education + age + wave +  stateboth + (1|id),
                            family=binomial(),
                            data=regdatlim_f_b)
  
  stress_glmer_f_h <- glmer(stress ~  education + age + wave +  stateboth + (1|id),
                            family=binomial(),
                            data=regdatlim_f_h)
  
  poverty_glmer_f_w <- glmer(poverty ~ education + age + wave +  stateboth + (1|id),
                           family=binomial(),
                           data=regdatlim_f_w)
  
  poverty_glmer_f_b <- glmer(poverty ~ education + age + wave +  stateboth + (1|id),
                             family=binomial(),
                             data=regdatlim_f_b)
  
  poverty_glmer_f_h <- glmer(poverty ~ education + age + wave +  stateboth + (1|id),
                             family=binomial(),
                             data=regdatlim_f_h)
  
  # Average marginal effect
  ame_physical_f_w <- margins(physical_glmer_f_w, type = "response",data=regdatlim_f_w) |> summary()
  ame_physical_f_b <- margins(physical_glmer_f_b, type = "response",data=regdatlim_f_b) |> summary()
  ame_physical_f_h <- margins(physical_glmer_f_h, type = "response",data=regdatlim_f_h) |> summary()
  
  ame_stress_f_w <- margins(stress_glmer_f_w, type = "response",data=regdatlim_f_w) |> summary()
  ame_stress_f_b <- margins(stress_glmer_f_b, type = "response",data=regdatlim_f_b) |> summary()
  ame_stress_f_h <- margins(stress_glmer_f_h, type = "response",data=regdatlim_f_h) |> summary() 
  
  ame_poverty_f_w <- margins(poverty_glmer_f_w, type = "response",data=regdatlim_f_w) |> summary() 
  ame_poverty_f_b <- margins(poverty_glmer_f_b, type = "response",data=regdatlim_f_b) |> summary() 
  ame_poverty_f_h <- margins(poverty_glmer_f_h, type = "response",data=regdatlim_f_h) |> summary() 
  
  # Logit 
  odd_physical_f_w <- margins(physical_glmer_f_w, type = "link",data=regdatlim_f_w) |> summary() 
  odd_physical_f_b <- margins(physical_glmer_f_b, type = "link",data=regdatlim_f_b) |> summary() 
  odd_physical_f_h <- margins(physical_glmer_f_h, type = "link",data=regdatlim_f_h) |> summary() 
  
  odd_stress_f_w <- margins(stress_glmer_f_w, type = "link",data=regdatlim_f_w) |> summary() 
  odd_stress_f_b <- margins(stress_glmer_f_b, type = "link",data=regdatlim_f_b) |> summary() 
  odd_stress_f_h <- margins(stress_glmer_f_h, type = "link",data=regdatlim_f_h) |> summary() 
  
  odd_poverty_f_w <- margins(poverty_glmer_f_w, type = "link",data=regdatlim_f_w) |> summary() 
  odd_poverty_f_b <- margins(poverty_glmer_f_b, type = "link",data=regdatlim_f_b) |> summary()   
  odd_poverty_f_h <- margins(poverty_glmer_f_h, type = "link",data=regdatlim_f_h) |> summary()   

  
### Models (men by race/ethnicity) #############################################
  
  # Models
  physical_glmer_m_w <- glmer(physical ~ education + age + wave +  stateboth + (1|id),
                              family=binomial(),
                              data=regdatlim_m_w)
  
  physical_glmer_m_b <- glmer(physical ~ education + age + wave +  stateboth + (1|id),
                              family=binomial(),
                              data=regdatlim_m_b)
  
  physical_glmer_m_h <- glmer(physical ~ education + age + wave +  stateboth + (1|id),
                              family=binomial(),
                              data=regdatlim_m_h)
  
  stress_glmer_m_w <- glmer(stress ~  education + age + wave +  stateboth + (1|id),
                            family=binomial(),
                            data=regdatlim_m_w)
  
  stress_glmer_m_b <- glmer(stress ~  education + age + wave +  stateboth + (1|id),
                            family=binomial(),
                            data=regdatlim_m_b)
  
  stress_glmer_m_h <- glmer(stress ~  education + age + wave +  stateboth + (1|id),
                            family=binomial(),
                            data=regdatlim_m_h)
  
  poverty_glmer_m_w <- glmer(poverty ~ education + age + wave +  stateboth + (1|id),
                             family=binomial(),
                             data=regdatlim_m_w)
  
  poverty_glmer_m_b <- glmer(poverty ~ education + age + wave +  stateboth + (1|id),
                             family=binomial(),
                             data=regdatlim_m_b)
  
  poverty_glmer_m_h <- glmer(poverty ~ education + age + wave +  stateboth + (1|id),
                             family=binomial(),
                             data=regdatlim_m_h)
  
  # Average marginal effect
  ame_physical_m_w <- margins(physical_glmer_m_w, type = "response",data=regdatlim_m_w) |> summary() 
  ame_physical_m_b <- margins(physical_glmer_m_b, type = "response",data=regdatlim_m_b) |> summary() 
  ame_physical_m_h <- margins(physical_glmer_m_h, type = "response",data=regdatlim_m_h) |> summary() 
  
  ame_stress_m_w <- margins(stress_glmer_m_w, type = "response",data=regdatlim_m_w) |> summary() 
  ame_stress_m_b <- margins(stress_glmer_m_b, type = "response",data=regdatlim_m_b) |> summary() 
  ame_stress_m_h <- margins(stress_glmer_m_h, type = "response",data=regdatlim_m_h) |> summary() 
  
  ame_poverty_m_w <- margins(poverty_glmer_m_w, type = "response",data=regdatlim_m_w) |> summary() 
  ame_poverty_m_b <- margins(poverty_glmer_m_b, type = "response",data=regdatlim_m_b) |> summary() 
  ame_poverty_m_h <- margins(poverty_glmer_m_h, type = "response",data=regdatlim_m_h) |> summary() 
  
  # Logit 
  odd_physical_m_w <- margins(physical_glmer_m_w, type = "link",data=regdatlim_m_w) |> summary() 
  odd_physical_m_b <- margins(physical_glmer_m_b, type = "link",data=regdatlim_m_b) |> summary() 
  odd_physical_m_h <- margins(physical_glmer_m_h, type = "link",data=regdatlim_m_h) |> summary() 
  
  odd_stress_m_w <- margins(stress_glmer_m_w, type = "link",data=regdatlim_m_w) |> summary() 
  odd_stress_m_b <- margins(stress_glmer_m_b, type = "link",data=regdatlim_m_b) |> summary() 
  odd_stress_m_h <- margins(stress_glmer_m_h, type = "link",data=regdatlim_m_h) |> summary() 
  
  odd_poverty_m_w <- margins(poverty_glmer_m_w, type = "link",data=regdatlim_m_w) |> summary() 
  odd_poverty_m_b <- margins(poverty_glmer_m_b, type = "link",data=regdatlim_m_b) |> summary()   
  odd_poverty_m_h <- margins(poverty_glmer_m_h, type = "link",data=regdatlim_m_h) |> summary() 
  
  
### Save results ###############################################################  
  
  save(file="Results/main.rda",
       list=c("ame_physical","ame_stress","ame_poverty",
              "odd_physical","odd_stress","odd_poverty",
              "ame_physical_m","ame_stress_m","ame_poverty_m",
              "odd_physical_m","odd_stress_m","odd_poverty_m",
              "ame_physical_f","ame_stress_f","ame_poverty_f",
              "odd_physical_f","odd_stress_f","odd_poverty_f",
              "ame_physical_m_w","ame_stress_m_w","ame_poverty_m_w",
              "odd_physical_m_w","odd_stress_m_w","odd_poverty_m_w",
              "ame_physical_m_b","ame_stress_m_b","ame_poverty_m_b",
              "odd_physical_m_b","odd_stress_m_b","odd_poverty_m_b",
              "ame_physical_m_h","ame_stress_m_h","ame_poverty_m_h",
              "odd_physical_m_h","odd_stress_m_h","odd_poverty_m_h",
              "ame_physical_f_w","ame_stress_f_w","ame_poverty_f_w",
              "odd_physical_f_w","odd_stress_f_w","odd_poverty_f_w",
              "ame_physical_f_b","ame_stress_f_b","ame_poverty_f_b",
              "odd_physical_f_b","odd_stress_f_b","odd_poverty_f_b",
              "ame_physical_f_h","ame_stress_f_h","ame_poverty_f_h",
              "odd_physical_f_h","odd_stress_f_h","odd_poverty_f_h"))
  