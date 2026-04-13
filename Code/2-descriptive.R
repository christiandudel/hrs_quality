### Packages ###################################################################

  library(tidyverse)


### Load data ##################################################################

  load("Data/hrs_edited.Rda")


### Editing ####################################################################

  hrs <- hrs |> mutate(gender=ifelse(gender==1,"Male",gender),
                       gender=ifelse(gender==2,"Female",gender))
  
  hrs <- hrs |> mutate(education=ifelse(education==0,"Less than HS",education),
                       education=ifelse(education==1,"High school",education),
                       education=ifelse(education==2,"(Some) college, university",education))

  hrs <- hrs |> mutate(healthywork=NA,
                       healthywork=ifelse(stateboth=="working/healthy",1,healthywork),
                       healthywork=ifelse(stateboth=="working/unhealthy",0,healthywork),
                       unhealthywork=NA,
                       unhealthywork=ifelse(stateboth=="working/healthy",0,unhealthywork),
                       unhealthywork=ifelse(stateboth=="working/unhealthy",1,unhealthywork))
  
  hrs$race <- factor(hrs$race,levels=c("White","Black","Hispan"))
  hrs$gender <- factor(hrs$gender,levels=c("Male","Female"))
  
  
### Prevalence healthy/unhealthy work and job quality ##########################


  descriptive_rg <- hrs |> filter(wave>=11 & race!="Other" & 
                                  stateboth %in% c("working/healthy","working/unhealthy")) |> 
                                  group_by(race,gender) |> summarize(mean(healthywork,na.rm=T),
                                                                     mean(unhealthywork,na.rm=T),
                                                                     mean(physical,na.rm=T),
                                                                     mean(stress,na.rm=T),
                                                                     mean(poverty,na.rm=T),
                                                                     mean(anybad,na.rm=T),
                                                                     mean(allbad,na.rm=T))
  
  names(descriptive_rg) <- c("Gender","Race",
                             "Healthy","Unhealthy",
                             "Physical","Stress","Poverty",
                             "Any","All")
  
  descriptive_t <- hrs |> filter(wave>=11 & race!="Other" & 
                                    stateboth %in% c("working/healthy","working/unhealthy")) |> 
                                       summarize(mean(healthywork,na.rm=T),
                                       mean(unhealthywork,na.rm=T),
                                       mean(physical,na.rm=T),
                                       mean(stress,na.rm=T),
                                       mean(poverty,na.rm=T),
                                       mean(anybad,na.rm=T),
                                       mean(allbad,na.rm=T))
  
  
  descriptive_t <- data.frame(c("Total","Total",descriptive_t))
  names(descriptive_t) <- names(descriptive_rg)
  
  descriptive <- rbind(descriptive_t,descriptive_rg)
  
  
### Prevalence job quality by work status ######################################

  # Descriptive
  quality_wu <- hrs |> filter(stateboth=="working/unhealthy" & wave>=11 & 
                                race!="Other") |> 
    summarise(mean(physical,na.rm=T),
              mean(stress,na.rm=T),
              mean(poverty,na.rm=T),
              mean(anybad,na.rm=T),
              mean(allbad,na.rm=T))
  
  quality_wh <- hrs |> filter(stateboth=="working/healthy" & wave>=11& 
                                race!="Other") |> 
    summarise(mean(physical,na.rm=T),
              mean(stress,na.rm=T),
              mean(poverty,na.rm=T),
              mean(anybad,na.rm=T),
              mean(allbad,na.rm=T))

  # Descriptive: gender
  quality_wu_g <- hrs |> filter(stateboth=="working/unhealthy" & wave>=11& 
                                  race!="Other") |> 
    group_by(gender) |> 
    summarise(mean(physical,na.rm=T),
              mean(stress,na.rm=T),
              mean(poverty,na.rm=T),
              mean(anybad,na.rm=T),
              mean(allbad,na.rm=T))
  
  quality_wh_g <- hrs |> filter(stateboth=="working/healthy" & wave>=11& 
                                  race!="Other") |> 
    group_by(gender) |> 
    summarise(mean(physical,na.rm=T),
              mean(stress,na.rm=T),
              mean(poverty,na.rm=T),
              mean(anybad,na.rm=T),
              mean(allbad,na.rm=T))


  # Descriptive: gender, race
  quality_wu_r <- hrs |> filter(stateboth=="working/unhealthy" & wave>=11& 
                                  race!="Other") |> 
    group_by(gender,race) |> 
    summarise(mean(physical,na.rm=T),
              mean(stress,na.rm=T),
              mean(poverty,na.rm=T),
              mean(anybad,na.rm=T),
              mean(allbad,na.rm=T))
  
  quality_wh_r <- hrs |> filter(stateboth=="working/healthy" & wave>=11& 
                                  race!="Other") |> 
    group_by(gender,race) |> 
    summarise(mean(physical,na.rm=T),
              mean(stress,na.rm=T),
              mean(poverty,na.rm=T),
              mean(anybad,na.rm=T),
              mean(allbad,na.rm=T))
  
  # Descriptive: gender, race, education
  quality_wu_e <- hrs |> filter(stateboth=="working/unhealthy" & wave>=11& 
                                  race!="Other") |> 
    group_by(gender,race,education) |> 
    summarise(mean(physical,na.rm=T),
              mean(stress,na.rm=T),
              mean(poverty,na.rm=T),
              mean(anybad,na.rm=T),
              mean(allbad,na.rm=T))
  
  quality_wh_e <- hrs |> filter(stateboth=="working/healthy" & wave>=11& 
                                  race!="Other") |> 
    group_by(gender,race,education) |> 
    summarise(mean(physical,na.rm=T),
              mean(stress,na.rm=T),
              mean(poverty,na.rm=T),
              mean(anybad,na.rm=T),
              mean(allbad,na.rm=T))

  
### Combine ####################################################################    
  
  # Total
  quality_wu$gender <- "Total"
  quality_wu$race <- "Total"
  quality_wu$education <- "Total"
  
  quality_wh$gender <- "Total"
  quality_wh$race <- "Total"
  quality_wh$education <- "Total"
  
  # By gender
  quality_wu_g$race <- "Total"
  quality_wu_g$education <- "Total"
  
  quality_wh_g$race <- "Total"
  quality_wh_g$education <- "Total"
  
  # By gender/race
  quality_wu_r$education <- "Total"
  quality_wh_r$education <- "Total"
  
  # Add work/health status everywhere
  quality_wu$state <- "Working unhealthy"
  quality_wh$state <- "Working healthy"
  
  quality_wu_g$state <- "Working unhealthy"
  quality_wh_g$state <- "Working healthy"
  
  quality_wu_r$state <- "Working unhealthy"
  quality_wh_r$state <- "Working healthy"
  
  quality_wu_e$state <- "Working unhealthy"
  quality_wh_e$state <- "Working healthy"
  
  # Big object
  quality <- rbind(quality_wh,quality_wu,
                   quality_wu_g,quality_wh_g,
                   quality_wu_r,quality_wh_r,
                   quality_wu_e,quality_wh_e)
  
  names(quality)[1:5] <- c("physical","stress","poverty","any","all")
  
  quality <- quality[,c(9,6:8,1:5)]
  
  
### Save #######################################################################
  
  save(list=c("descriptive","quality"),file="Results/descriptive_big.rda")
  