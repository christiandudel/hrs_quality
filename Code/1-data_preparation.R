### Load Packages ##############################################################

  library(tidyverse)
  library(readstata13)


### Load data ##################################################################

  # rda file, makes reloading a lot faster
  rdafile <- "Data/hrs.Rda"
  
  if(!file.exists(rdafile)) { 

    # Data; can be obtained from https://hrs.isr.umich.edu
    dtafile <- "Data/randhrs1992_2022v1.dta"
    
    # Load
    hrs <- read.dta13(file=dtafile,
                      convert.factors=FALSE) 
  
    # Save
    save(hrs,file=rdafile)
  
  } else load(rdafile)
  

### Select variables ###########################################################

  # ID, gender, death/birth year, education
  hrs <- hrs |> select(hhidpn,ragender,radyear,rabyear,raeduc,rahispan,raracem,
                       # Wave status: Response indicator (1= in wave)
                       starts_with("inw"), 
                       # Interview status (5 & 6 = dead)
                       starts_with("r")&ends_with("iwstat"),
                       # Age in years at interview month
                       starts_with("r")&ends_with("agey_e")&!contains("respagey"),
                       # Sum of mobility difficulties
                       starts_with("r")&ends_with("mobila"),
                       # Sum of large muscle difficulties
                       starts_with("r")&ends_with("lgmusa"),
                       # Self reported health (1 = excellent, 5=very bad)
                       starts_with("r")&ends_with("shlt"),
                       # Depression symptoms (0=no, 1=yes)
                       starts_with("r")&ends_with("depyr"),
                       # Cognition 27
                       starts_with("r")&ends_with("cog27"),
                       # Labor force status
                       starts_with("r")&ends_with("lbrf")&!contains("inlbrf"),
                       # Current job requires physical effort
                       starts_with("r")&ends_with("jphys"),
                       # Current job involves lots of stress
                       starts_with("r")&ends_with("jstres"),
                       # Poverty (dummy)
                       starts_with("h")&ends_with("inpov"),
                       # Poverty threshold
                       starts_with("h")&ends_with("povthr"),
                       # Household income compared to poverty threshold
                       starts_with("h")&ends_with("povhhi"),
                       # Weights
                       starts_with("r")&ends_with("wtresp")
                       )


### Education/race #############################################################

  # Education, 3 levels, 0=low, 1=medium, 2=high
  hrs <- hrs |> mutate(education=recode_values(raeduc,
                                               c(1,2)~0,
                                               c(3,4)~1,
                                               5~2))
  
  # Race recode
  hrs <- hrs |> mutate(race=NA) |> 
    mutate(race=ifelse(raracem%in%1 & rahispan%in%0,"White",race),
           race=ifelse(raracem%in%2 & rahispan%in%0,"Black",race),
           race=ifelse(rahispan%in%1,"Hispan",race),
           race=ifelse(raracem%in%3 & rahispan%in%0,"Other",race),
           race=ifelse(raracem%in%3 & is.na(rahispan),"Other",race))
  
  
  # Drop if education/race is missing (573 individuals, negligible)
  dim(hrs)
  hrs |> filter(is.na(education) | is.na(race)) |> count()
  hrs <- hrs |> filter(!is.na(education) & !is.na(race))


### Rename vars for easier reshaping below #####################################

  # Wave status
  hrs <- hrs |> rename_with(~paste0("r",1:16,"inw"),starts_with("inw"))
  
  # Age
  hrs <- hrs |> rename_with(~paste0("r",1:16,"age"),ends_with("agey_e"))
  
  # Poverty variables
  hrs <- hrs |> rename_with(~paste0("r",1:16,"inpov"),ends_with("inpov"))
  hrs <- hrs |> rename_with(~paste0("r",1:16,"povthr"),ends_with("povthr"))
  hrs <- hrs |> rename_with(~paste0("r",1:16,"povhhi"),ends_with("povhhi"))
  
  
  # Cognition
  hrs <- hrs |> rename_with(~paste0("r",3:15,"cog"),ends_with("cog27"))

  # Empty vars for reshaping later (required by reshape function)
  hrs$r1mobila <- NA
  hrs$r1lgmusa <- NA
  hrs$r1depyr <- NA
  hrs$r2depyr <- NA
  hrs$r1cog <- NA
  hrs$r2cog <- NA
  hrs$r16cog <- NA

  # Change format of time varying variables (not a great solution, but works)
  hrsnames <- str_split_fixed(names(hrs),"r[[:digit:]]{1,2}",2)
  hrsnames <- apply(hrsnames,1,function(x) {paste0(x,collapse="")})
  hrsnumbers <- parse_number(names(hrs)) # Warning, expected
  hrswhich <- !is.na(hrsnumbers)
  hrsnames[hrswhich] <- paste(hrsnames[hrswhich],hrsnumbers[hrswhich],sep="_")
  names(hrs) <- hrsnames


### Reshape ####################################################################

  # Get names of longitudinal vars and their ordering right 
  repvars <- grepl("_",names(hrs))   
  repvars <- names(hrs)[repvars]
  repvars <- unique(unlist(lapply(strsplit(repvars,split="_"),function(x)x[1])))
  repvars <- paste(rep(repvars, each = length(1:16)), 1:16, sep = "_")
  
  # Reshape (pivot_longer is just not intuitive to me, sorry)
  hrs <- reshape(data=as.data.frame(hrs),
                 direction="long",
                 varying=repvars,
                 sep="_",
                 idvar="hhidpn",
                 #times=1:15,
                 timevar="wave")
  
  # Sort 
  hrs <- hrs |> arrange(hhidpn,wave)
  
  # Drop people after death, and when not (yet) in wave
  hrs <- hrs |> filter(iwstat%in%c(1,5))


### Age ########################################################################

  # Age is missing in the year of death, add
  hrs <- hrs |> mutate(age=ifelse(iwstat==5,radyear-rabyear,age))
  
  # Age is still missing for a few people with unknown birth year and/or unknown 
  # year of death; for the latter, we impute year of death as mid-interval,
  # and generate age based on that
  hrs <- hrs |> mutate(toedit=ifelse(is.na(radyear) & !is.na(rabyear) & iwstat==5 & is.na(age),1,0),
                       radyear=case_when(
                         toedit==1 & wave==2~1993,
                         toedit==1 & wave==3~1995,
                         toedit==1 & wave==4~1997,
                         toedit==1 & wave==5~1999,
                         toedit==1 & wave==6~2001,
                         toedit==1 & wave==7~2003,
                         toedit==1 & wave==8~2005,
                         toedit==1 & wave==9~2007,
                         toedit==1 & wave==10~2009,
                         toedit==1 & wave==11~2011,
                         toedit==1 & wave==12~2013,
                         toedit==1 & wave==13~2015,
                         toedit==1 & wave==14~2017,
                         toedit==1 & wave==15~2019,
                         toedit==1 & wave==16~2021,
                         .default=radyear
                       ),
                       age=ifelse(iwstat==5&is.na(age),radyear-rabyear,age))
  
  # Drop if age is missing
  hrs <- hrs |> filter(!is.na(age))


### Work, disability, health (separate) ########################################
  
  # Pension age
  hrs <- hrs |> mutate(state_pension=ifelse(rabyear<=1942,65,
                                           ifelse(rabyear>1942 & rabyear<=1959,66,
                                                   ifelse(rabyear>=1960,67,NA))))
  
  
  # Employment (slightly more detailed/simplified)
  hrs <- hrs |> mutate(workstatus=recode_values(
                          lbrf,
                          1:2~"working",
                          3  ~"unemployed",
                          4:5~"retired",
                          6:7~"inactive"),
                        workstatus=ifelse(lbrf%in%6:7&age>=state_pension,"retired",workstatus),
                        worksimple=recode_values(
                          workstatus,
                          c("unemployed","inactive")~"not working",
                          default=workstatus)) 
  
  # Mobility, large muscle, both combined
  hrs <- hrs |> mutate(mobility=recode_values(mobila,
                                      0~0,
                                      1:5~1),
                       muscle=recode_values(lgmusa,
                                       0~0,
                                       1:4~1),
                       both=ifelse(mobility%in%1 | muscle %in%1, 1, 0))
  
  # Cognition: dummy
  threshold <- quantile(hrs$cog,prob=0.25,na.rm=T)
  hrs <- hrs |> mutate(cogd=recode_values(cog,
                                          NA~NA,
                                          0:threshold~1,
                                          (threshold+1):27~0))
  
  # Get missings right
  hrs <- hrs |> mutate(both=ifelse(is.na(mobility)|is.na(muscle), NA, both))
  
  
### Work & health (combined) ###################################################
  
  # Work and limitations
  hrs <- hrs |> mutate(workboth=NA,
                       workboth=ifelse(worksimple%in%"working" & both%in%0,"working/healthy",workboth),
                       workboth=ifelse(worksimple%in%"working" & both%in%1,"working/unhealthy",workboth),
                       workboth=ifelse(worksimple%in%"retired" & both%in%0,"retired/healthy",workboth),
                       workboth=ifelse(worksimple%in%"retired" & both%in%1,"retired/unhealthy",workboth),
                       workboth=ifelse(worksimple%in%"not working" & !is.na(both) ,"not working",workboth))


### State variables (including death) ##########################################
  
  # State using limitation
  hrs <- hrs |> mutate(stateboth=NA,
                       stateboth=ifelse(iwstat==1,workboth,stateboth),
                       stateboth=ifelse(iwstat==5,"dead",stateboth))
  
  # Missing person-years
  table(hrs$stateboth,useNA="always") |> prop.table()
  
  
### Self-rated health & depression #############################################
  
  # Self-rated health
  hrs <- hrs |> mutate(workshlt=NA,
                       workshlt=ifelse(worksimple%in%"working" & shlt%in%3:5,"working/healthy",workshlt),
                       workshlt=ifelse(worksimple%in%"working" & shlt%in%1:2,"working/unhealthy",workshlt),
                       workshlt=ifelse(worksimple%in%"retired" & shlt%in%3:5,"retired/healthy",workshlt),
                       workshlt=ifelse(worksimple%in%"retired" & shlt%in%1:2,"retired/unhealthy",workshlt),
                       workshlt=ifelse(worksimple%in%"not working" & !is.na(shlt) ,"not working",workshlt),
                       workshlt=ifelse(iwstat==5,"dead",workshlt))
  
  # Depression
  hrs <- hrs |> mutate(workdepyr=NA,
                       workdepyr=ifelse(worksimple%in%"working" & depyr%in%0,"working/healthy",workdepyr),
                       workdepyr=ifelse(worksimple%in%"working" & depyr%in%1,"working/unhealthy",workdepyr),
                       workdepyr=ifelse(worksimple%in%"retired" & depyr%in%0,"retired/healthy",workdepyr),
                       workdepyr=ifelse(worksimple%in%"retired" & depyr%in%1,"retired/unhealthy",workdepyr),
                       workdepyr=ifelse(worksimple%in%"not working" & !is.na(cogd) ,"not working",workdepyr),
                       workdepyr=ifelse(iwstat==5,"dead",workdepyr))
  
  # Cognition
  hrs <- hrs |> mutate(workcogd=NA,
                       workcogd=ifelse(worksimple%in%"working" & cogd%in%0,"working/healthy",workcogd),
                       workcogd=ifelse(worksimple%in%"working" & cogd%in%1,"working/unhealthy",workcogd),
                       workcogd=ifelse(worksimple%in%"retired" & cogd%in%0,"retired/healthy",workcogd),
                       workcogd=ifelse(worksimple%in%"retired" & cogd%in%1,"retired/unhealthy",workcogd),
                       workcogd=ifelse(worksimple%in%"not working" & !is.na(cogd) ,"not working",workcogd),
                       workcogd=ifelse(iwstat==5,"dead",workcogd))
  

### Recode physical, stress, poverty ###########################################
  
  # Physical (1=yes,0 =no)
  hrs <- hrs |> mutate(physical=NA,
                       physical=ifelse(jphys%in%1:2,1,physical),
                       physical=ifelse(jphys%in%3:4,0,physical))
  
  # Stress (1=yes, 0=no)
  hrs <- hrs |> mutate(stress=NA,
                       stress=ifelse(jstres%in%1:2,1,stress),
                       stress=ifelse(jstres%in%3:4,0,stress))
  
  # Poverty (1=yes, 0=no)
  hrs <- hrs |> mutate(poverty=NA,
                       poverty=ifelse(inpov%in%1,1,poverty),
                       poverty=ifelse(inpov%in%0,0,poverty))
  
  # Poverty alternatives (75% of threshold, 125%, 150%, 200%)
  hrs <- hrs |> mutate(poverty75=NA,
                       poverty75=ifelse(povhhi<povthr*0.75,1,poverty75),
                       poverty75=ifelse(povhhi>=povthr*0.75,0,poverty75))
  
  hrs <- hrs |> mutate(poverty125=NA,
                       poverty125=ifelse(povhhi<povthr*1.25,1,poverty125),
                       poverty125=ifelse(povhhi>=povthr*1.25,0,poverty125))
  
  hrs <- hrs |> mutate(poverty150=NA,
                       poverty150=ifelse(povhhi<povthr*1.5,1,poverty150),
                       poverty150=ifelse(povhhi>=povthr*1.5,0,poverty150))
  
  hrs <- hrs |> mutate(poverty200=NA,
                       poverty200=ifelse(povhhi<povthr*2,1,poverty200),
                       poverty200=ifelse(povhhi>=povthr*2,0,poverty200))

  # Any (1=yes, 0=no)
  hrs <- hrs |> mutate(anybad=NA,
                       anybad=ifelse(physical%in%1 | stress%in%1 | poverty%in%1,1,anybad),
                       anybad=ifelse(physical%in%0 & stress%in%0 & poverty%in%0,0,anybad),
                       anybad=ifelse(is.na(physical)|is.na(stress)|is.na(poverty),NA,anybad))
  
  # All
  hrs <- hrs |> mutate(allbad=NA,
                       allbad=ifelse(physical%in%1 & stress%in%1 & poverty%in%1,1,allbad),
                       allbad=ifelse(physical%in%0 | stress%in%0 | poverty%in%0,0,allbad),
                       allbad=ifelse(is.na(physical)|is.na(stress)|is.na(poverty),NA,allbad))
  
  # Pairwise combinations: phyiscal & stress (poverty could be true or false or missing)
  hrs <- hrs |> mutate(pair1=NA,
                       pair1=ifelse(physical%in%1 & stress%in%1,1,anybad),
                       pair1=ifelse(physical%in%0 & stress%in%0,0,anybad),
                       pair1=ifelse(is.na(physical)|is.na(stress),NA,pair1))
  
  # Pairwise combinations: phyiscal & poverty (stress could be true or false or missing)
  hrs <- hrs |> mutate(pair2=NA,
                       pair2=ifelse(physical%in%1 & poverty%in%1,1,anybad),
                       pair2=ifelse(physical%in%0 & poverty%in%0,0,anybad),
                       pair2=ifelse(is.na(physical)|is.na(poverty),NA,pair2))
  
  # Pairwise combinations: stress & poverty (physical could be true or false or missing)
  hrs <- hrs |> mutate(pair3=NA,
                       pair3=ifelse(stress%in%1 & poverty%in%1,1,anybad),
                       pair3=ifelse(stress%in%0 & poverty%in%0,0,anybad),
                       pair3=ifelse(is.na(stress)|is.na(poverty),NA,pair3))
  
  
### Limit data #################################################################

  # Limit variables
  hrs <- hrs |> select(hhidpn,ragender,race,education,wave,age,
                       stateboth,workshlt,workdepyr,workcogd,shlt,depyr,cogd,
                       stress,physical,poverty,anybad,allbad,pair1,pair2,pair3,
                       poverty75,poverty125,poverty150,poverty200,
                       wtresp)

  # Rename
  hrs <- hrs |> rename('gender'='ragender',
                       'id'='hhidpn',
                       'weight'='wtresp')
  
  # Drop observations below age 50 (partners)
  hrs <- hrs |> filter(age>=50)


### Saving #####################################################################

  save(hrs,file="Data/hrs_edited.Rda")

