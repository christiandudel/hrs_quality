### Packages ###################################################################

  library(writexl)  
  library(tidyverse)


### Descriptive (Table 1) ######################################################

  # Load weighted results
  load(file="Results/descriptive_weighted.rda")

  # First part of table: prevalence
  table1 <- descriptivew |> select(Gender,Race,Healthy,Unhealthy,ERI)
  
  # Second part: conditional prevalence
  tmp <- qualityw |> filter(education=="Total") |> select(state,gender,race,eri)
  tmp <- tmp |> pivot_wider(id_cols=c(gender,race),names_from=state,values_from=eri)
  tmp <- tmp |> rename(Gender=gender,
                       Race=race)
  
  # Combine
  table1 <- left_join(table1,tmp,by=c("Gender","Race"))
  
  # Save
  write_xlsx(table1,path="Output/table1.xlsx")
  
  
### Main SES results (Table 2) #################################################
  
  # Load regression results
  load("Results/main.rda")
  
  # Results
  table2 <- ame_ses |> 
              filter(factor%in%c("education1","education2","raceBlack","raceHispan")) |> 
              select(factor,AME,p,lower,upper)
  
  # Save
  write_xlsx(table2,path="Output/table2.xlsx")

  
### Robustness checks SES ################################################
  
  # Load
  load("Results/no_covid.rda")
  load("Results/healthdef.rda")
  load("Results/pooled.rda")
  
  # Main results
  tmp <- ame_ses |> 
         filter(factor%in%c("raceBlack","raceHispan")) |> 
         select(factor,AME,p,lower,upper)
  
  # No covid 
  tmp_covid <- ame_covid_ses |> 
               filter(factor%in%c("raceBlack","raceHispan")) |> 
               select(factor,AME,p,lower,upper)
  
  # Health: depression
  tmp_depyr <- ame_depyr_ses |> 
               filter(factor%in%c("raceBlack","raceHispan")) |> 
               select(factor,AME,p,lower,upper)
  
  # Health: cognitive impairment
  tmp_cogd <- ame_cogd_ses |>
              filter(factor%in%c("raceBlack","raceHispan")) |>
              select(factor,AME,p,lower,upper)
  
  # Health: subjective health
  tmp_shlt <- ame_shlt_ses |> 
              filter(factor%in%c("raceBlack","raceHispan")) |> 
              select(factor,AME,p,lower,upper)
  
  # Pooled logit
  tmp_pool <- ame_pool_ses |> 
              filter(factor%in%c("raceBlack","raceHispan")) |> 
              select(factor,AME,p,lower,upper)
  
  # Combine
  tableS2 <- rbind(tmp,tmp_covid,tmp_depyr,tmp_cogd,tmp_shlt,tmp_pool)

  # Save
  write_xlsx(tableS2,path="Output/tableS1.xlsx")
  
  
  
### Robustness, second analysis, poverty #######################################
  
  # Variables to keep from summary object
  vars <- c("AME","SE","z","p","lower","upper")
  
  estimates <- rbind(
    ame_p100[ame_p100$factor=="statebothworking/unhealthy",vars],  
    ame_p75[ame_p75$factor=="statebothworking/unhealthy",vars],
    ame_p125[ame_p125$factor=="statebothworking/unhealthy",vars],
    ame_p150[ame_p150$factor=="statebothworking/unhealthy",vars],
    ame_p200[ame_p200$factor=="statebothworking/unhealthy",vars])
  
  # Combine 
  types <- c("100%","75%","125%","150%","200%")
  tableS1 <- cbind(types,estimates)
  
  # Save
  write_xlsx(tableS1,path="Output/tableS2.xlsx")  