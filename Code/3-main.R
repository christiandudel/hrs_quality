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
  
  
### Obs ########################################################################
  
  # Overall
  dim(regdatlim)
  regdatlim |> pull(id) |> unique() |> length()
  
  # Gender
  dim(regdatlim_m)
  dim(regdatlim_f)
  
  # Race/ethnicity
  dim(regdatlim_m_w) + dim(regdatlim_f_w)
  dim(regdatlim_m_b) + dim(regdatlim_f_b)
  dim(regdatlim_m_h) + dim(regdatlim_f_h)
  
  
### Effect race/ethnicity, education ###########################################
  
  ses <- glmer(eri ~ gender + race + education + age + age2 + wave + (1|id),
               family=binomial(),
               data=regdatlim[regdatlim$stateboth%in%"working/unhealthy",],
               weights=weight)
  
  ame_ses <- margins(ses, type = "response",data=regdatlim[regdatlim$stateboth%in%"working/unhealthy",]) |> summary()
  
  
### Models (total population) ##################################################

  # Models
  eri_glmer <- glmer(eri ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                     family=binomial(),
                     data=regdatlim,
                     weights=weight)
  
  physical_glmer <- glmer(physical ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                          family=binomial(),
                          data=regdatlim,
                          weights=weight)
  
  stress_glmer <- glmer(stress ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                        family=binomial(),
                        data=regdatlim,
                        weights=weight)
  
  poverty_glmer <- glmer(poverty ~ gender + race + education + age + age2 + wave + stateboth + (1|id),
                         family=binomial(),
                         data=regdatlim,
                         weights=weight)
  
  # Average marginal effect
  ame_eri <- margins(eri_glmer, type = "response",data=regdatlim) |> summary()
  ame_physical <- margins(physical_glmer, type = "response",data=regdatlim) |> summary()
  ame_stress <- margins(stress_glmer, type = "response",data=regdatlim) |> summary()
  ame_poverty <- margins(poverty_glmer, type = "response",data=regdatlim) |> summary()
  
  # Logit 
  odd_eri <- margins(eri_glmer, type = "link",data=regdatlim) |> summary()
  odd_physical <- margins(physical_glmer, type = "link",data=regdatlim) |> summary()
  odd_stress <- margins(stress_glmer, type = "link",data=regdatlim) |> summary()
  odd_poverty <- margins(poverty_glmer, type = "link",data=regdatlim) |> summary()
  

### Models (women) #############################################################  
  
  # Models
  eri_glmer_f <- glmer(eri ~ race + education + age + age2 + wave + stateboth + (1|id),
                       family=binomial(),
                       data=regdatlim_f,
                       weights=weight)
  
  physical_glmer_f <- glmer(physical ~ race + education + age + age2 + wave + stateboth + (1|id),
                            family=binomial(),
                            data=regdatlim_f,
                            weights=weight)
  
  stress_glmer_f <- glmer(stress ~ race + education + age + age2 + wave + stateboth + (1|id),
                        family=binomial(),
                        data=regdatlim_f,
                        weights=weight)
  
  poverty_glmer_f <- glmer(poverty ~ race + education + age + age2 + wave + stateboth + (1|id),
                         family=binomial(),
                         data=regdatlim_f,
                         weights=weight)
  
  # Average marginal effect
  ame_eri_f <- margins(eri_glmer_f, type = "response",data=regdatlim_f) |> summary()
  ame_physical_f <- margins(physical_glmer_f, type = "response",data=regdatlim_f) |> summary()
  ame_stress_f <- margins(stress_glmer_f, type = "response",data=regdatlim_f) |> summary()
  ame_poverty_f <- margins(poverty_glmer_f, type = "response",data=regdatlim_f) |> summary()
  
  # Logit 
  odd_eri_f <- margins(eri_glmer_f, type = "link",data=regdatlim_f) |> summary()
  odd_physical_f <- margins(physical_glmer_f, type = "link",data=regdatlim_f) |> summary()
  odd_stress_f <- margins(stress_glmer_f, type = "link",data=regdatlim_f) |> summary()
  odd_poverty_f <- margins(poverty_glmer_f, type = "link",data=regdatlim_f) |> summary()
  
  
### Models (men) #############################################################  
  
  # Models
  eri_glmer_m <- glmer(eri ~ race + education + age + age2 + wave + stateboth + (1|id),
                       family=binomial(),
                       data=regdatlim_m,
                       weights=weight)
  
  physical_glmer_m <- glmer(physical ~ race + education + age + age2 + wave + stateboth + (1|id),
                            family=binomial(),
                            data=regdatlim_m,
                            weights=weight)
  
  stress_glmer_m <- glmer(stress ~ race + education + age + age2 + wave + stateboth + (1|id),
                          family=binomial(),
                          data=regdatlim_m,
                          weights=weight)
  
  poverty_glmer_m <- glmer(poverty ~ race + education + age + age2 + wave + stateboth + (1|id),
                           family=binomial(),
                           data=regdatlim_m,
                           weights=weight)
  
  # Average marginal effect
  ame_eri_m <- margins(eri_glmer_m, type = "response",data=regdatlim_m) |> summary()
  ame_physical_m <- margins(physical_glmer_m, type = "response",data=regdatlim_m) |> summary()
  ame_stress_m <- margins(stress_glmer_m, type = "response",data=regdatlim_m) |> summary()
  ame_poverty_m <- margins(poverty_glmer_m, type = "response",data=regdatlim_m) |> summary()
  
  # Logit 
  odd_eri_m <- margins(eri_glmer_m, type = "link",data=regdatlim_m) |> summary()
  odd_physical_m <- margins(physical_glmer_m, type = "link",data=regdatlim_m) |> summary()
  odd_stress_m <- margins(stress_glmer_m, type = "link",data=regdatlim_m) |> summary()
  odd_poverty_m <- margins(poverty_glmer_m, type = "link",data=regdatlim_m) |> summary()   
  
  
### Models (women by race/ethnicity) ###########################################
  
  # Models
  eri_glmer_f_w <- glmer(eri ~ education + age + wave +  stateboth + (1|id),
                              family=binomial(),
                              data=regdatlim_f_w,
                              weights=weight)
  
  eri_glm_f_b <- glm(eri ~ education + age + wave +  stateboth,
                           family=binomial(),
                           data=regdatlim_f_b,
                           weights=weight)
  
  eri_glmer_f_h <- glmer(eri ~ education + age + wave +  stateboth + (1|id),
                              family=binomial(),
                              data=regdatlim_f_h,
                              weights=weight)
  
  physical_glmer_f_w <- glmer(physical ~ education + age + wave +  stateboth + (1|id),
                            family=binomial(),
                            data=regdatlim_f_w,
                            weights=weight)
  
  physical_glmer_f_b <- glmer(physical ~ education + age + wave +  stateboth + (1|id),
                              family=binomial(),
                              data=regdatlim_f_b,
                              weights=weight)
  
  physical_glmer_f_h <- glmer(physical ~ education + age + wave +  stateboth + (1|id),
                              family=binomial(),
                              data=regdatlim_f_h,
                              weights=weight)
  
  stress_glmer_f_w <- glmer(stress ~  education + age + wave +  stateboth + (1|id),
                          family=binomial(),
                          data=regdatlim_f_w,
                          weights=weight)
  
  stress_glmer_f_b <- glmer(stress ~  education + age + wave +  stateboth + (1|id),
                            family=binomial(),
                            data=regdatlim_f_b,
                            weights=weight)
  
  stress_glmer_f_h <- glmer(stress ~  education + age + wave +  stateboth + (1|id),
                            family=binomial(),
                            data=regdatlim_f_h,
                            weights=weight)
  
  poverty_glmer_f_w <- glmer(poverty ~ education + age + wave +  stateboth + (1|id),
                           family=binomial(),
                           data=regdatlim_f_w,
                           weights=weight)
  
  poverty_glmer_f_b <- glmer(poverty ~ education + age + wave +  stateboth + (1|id),
                             family=binomial(),
                             data=regdatlim_f_b,
                             weights=weight)
  
  poverty_glmer_f_h <- glmer(poverty ~ education + age + wave +  stateboth + (1|id),
                             family=binomial(),
                             data=regdatlim_f_h,
                             weights=weight)
  
  # Average marginal effect
  ame_eri_f_w <- margins(eri_glmer_f_w, type = "response",data=regdatlim_f_w) |> summary()
  ame_eri_f_b <- margins(eri_glm_f_b, type = "response",data=regdatlim_f_b) |> summary()
  ame_eri_f_h <- margins(eri_glmer_f_h, type = "response",data=regdatlim_f_h) |> summary()
  
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
  odd_eri_f_w <- margins(eri_glmer_f_w, type = "link",data=regdatlim_f_w) |> summary() 
  odd_eri_f_b <- margins(eri_glm_f_b, type = "link",data=regdatlim_f_b) |> summary() 
  odd_eri_f_h <- margins(eri_glmer_f_h, type = "link",data=regdatlim_f_h) |> summary() 
  
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
  eri_glmer_m_w <- glmer(eri ~ education + age + wave +  stateboth + (1|id),
                              family=binomial(),
                              data=regdatlim_m_w,
                              weights=weight)
  
  eri_glmer_m_b <- glmer(eri ~ education + age + wave +  stateboth + (1|id),
                              family=binomial(),
                              data=regdatlim_m_b,
                              weights=weight)
  
  eri_glmer_m_h <- glmer(eri ~ education + age + wave +  stateboth + (1|id),
                              family=binomial(),
                              data=regdatlim_m_h,
                              weights=weight)
  
  physical_glmer_m_w <- glmer(physical ~ education + age + wave +  stateboth + (1|id),
                              family=binomial(),
                              data=regdatlim_m_w,
                              weights=weight)
  
  physical_glmer_m_b <- glmer(physical ~ education + age + wave +  stateboth + (1|id),
                              family=binomial(),
                              data=regdatlim_m_b,
                              weights=weight)
  
  physical_glmer_m_h <- glmer(physical ~ education + age + wave +  stateboth + (1|id),
                              family=binomial(),
                              data=regdatlim_m_h,
                              weights=weight)
  
  stress_glmer_m_w <- glmer(stress ~  education + age + wave +  stateboth + (1|id),
                            family=binomial(),
                            data=regdatlim_m_w,
                            weights=weight)
  
  # stress_glmer_m_b <- glmer(stress ~  education + age + wave +  stateboth + (1|id),
  #                           family=binomial(),
  #                           data=regdatlim_m_b)
  
  stress_glm_m_b <- glm(stress ~  education + age + wave +  stateboth,
                        family=binomial(),
                        data=regdatlim_m_b,
                        weights=weight)
  
  
  stress_glmer_m_h <- glmer(stress ~  education + age + wave +  stateboth + (1|id),
                            family=binomial(),
                            data=regdatlim_m_h,
                            weights=weight)
  
  # poverty_glmer_m_w <- glmer(poverty ~ education + age + wave +  stateboth + (1|id),
  #                            family=binomial(),
  #                            data=regdatlim_m_w)
  poverty_glm_m_w <- glm(poverty ~ education + age + wave +  stateboth,
                         family=binomial(),
                         data=regdatlim_m_w,
                         weights=weight)
  
  
  poverty_glmer_m_b <- glmer(poverty ~ education + age + wave +  stateboth + (1|id),
                             family=binomial(),
                             data=regdatlim_m_b,
                             weights=weight)
  
  poverty_glmer_m_h <- glmer(poverty ~ education + age + wave +  stateboth + (1|id),
                             family=binomial(),
                             data=regdatlim_m_h,
                             weights=weight)
  
  # Average marginal effect
  ame_eri_m_w <- margins(eri_glmer_m_w, type = "response",data=regdatlim_m_w) |> summary() 
  ame_eri_m_b <- margins(eri_glmer_m_b, type = "response",data=regdatlim_m_b) |> summary() 
  ame_eri_m_h <- margins(eri_glmer_m_h, type = "response",data=regdatlim_m_h) |> summary() 
  
  ame_physical_m_w <- margins(physical_glmer_m_w, type = "response",data=regdatlim_m_w) |> summary() 
  ame_physical_m_b <- margins(physical_glmer_m_b, type = "response",data=regdatlim_m_b) |> summary() 
  ame_physical_m_h <- margins(physical_glmer_m_h, type = "response",data=regdatlim_m_h) |> summary() 
  
  ame_stress_m_w <- margins(stress_glmer_m_w, type = "response",data=regdatlim_m_w) |> summary() 
  ame_stress_m_b <- margins(stress_glm_m_b, type = "response",data=regdatlim_m_b) |> summary() 
  ame_stress_m_h <- margins(stress_glmer_m_h, type = "response",data=regdatlim_m_h) |> summary() 
  
  ame_poverty_m_w <- margins(poverty_glm_m_w, type = "response",data=regdatlim_m_w) |> summary() 
  ame_poverty_m_b <- margins(poverty_glmer_m_b, type = "response",data=regdatlim_m_b) |> summary() 
  ame_poverty_m_h <- margins(poverty_glmer_m_h, type = "response",data=regdatlim_m_h) |> summary() 
  
  # Logit 
  odd_eri_m_w <- margins(eri_glmer_m_w, type = "link",data=regdatlim_m_w) |> summary() 
  odd_eri_m_b <- margins(eri_glmer_m_b, type = "link",data=regdatlim_m_b) |> summary() 
  odd_eri_m_h <- margins(eri_glmer_m_h, type = "link",data=regdatlim_m_h) |> summary() 
  
  odd_physical_m_w <- margins(physical_glmer_m_w, type = "link",data=regdatlim_m_w) |> summary() 
  odd_physical_m_b <- margins(physical_glmer_m_b, type = "link",data=regdatlim_m_b) |> summary() 
  odd_physical_m_h <- margins(physical_glmer_m_h, type = "link",data=regdatlim_m_h) |> summary() 
  
  odd_stress_m_w <- margins(stress_glmer_m_w, type = "link",data=regdatlim_m_w) |> summary() 
  odd_stress_m_b <- margins(stress_glm_m_b, type = "link",data=regdatlim_m_b) |> summary() 
  odd_stress_m_h <- margins(stress_glmer_m_h, type = "link",data=regdatlim_m_h) |> summary() 
  
  odd_poverty_m_w <- margins(poverty_glm_m_w, type = "link",data=regdatlim_m_w) |> summary() 
  odd_poverty_m_b <- margins(poverty_glmer_m_b, type = "link",data=regdatlim_m_b) |> summary()   
  odd_poverty_m_h <- margins(poverty_glmer_m_h, type = "link",data=regdatlim_m_h) |> summary() 
  
  
### Save results ###############################################################  
  
  save(file="Results/main.rda",
       list=c("ame_ses",
              "ame_eri","ame_physical","ame_stress","ame_poverty",
              "odd_eri","odd_physical","odd_stress","odd_poverty",
              "ame_eri_m","ame_physical_m","ame_stress_m","ame_poverty_m",
              "odd_eri_m","odd_physical_m","odd_stress_m","odd_poverty_m",
              "ame_eri_f","ame_physical_f","ame_stress_f","ame_poverty_f",
              "odd_eri_f","odd_physical_f","odd_stress_f","odd_poverty_f",
              "ame_eri_m_w","ame_physical_m_w","ame_stress_m_w","ame_poverty_m_w",
              "odd_eri_m_w","odd_physical_m_w","odd_stress_m_w","odd_poverty_m_w",
              "ame_eri_m_b","ame_physical_m_b","ame_stress_m_b","ame_poverty_m_b",
              "odd_eri_m_b","odd_physical_m_b","odd_stress_m_b","odd_poverty_m_b",
              "ame_eri_m_h","ame_physical_m_h","ame_stress_m_h","ame_poverty_m_h",
              "odd_eri_m_h","odd_physical_m_h","odd_stress_m_h","odd_poverty_m_h",
              "ame_eri_f_w","ame_physical_f_w","ame_stress_f_w","ame_poverty_f_w",
              "odd_eri_f_w","odd_physical_f_w","odd_stress_f_w","odd_poverty_f_w",
              "ame_eri_f_b","ame_physical_f_b","ame_stress_f_b","ame_poverty_f_b",
              "odd_eri_f_b","odd_physical_f_b","odd_stress_f_b","odd_poverty_f_b",
              "ame_eri_f_h","ame_physical_f_h","ame_stress_f_h","ame_poverty_f_h",
              "odd_eri_f_h","odd_physical_f_h","odd_stress_f_h","odd_poverty_f_h"))
  