### Tidy patient data.
tidy_patient <- function(data, pars){
   
   # Process medical condition parss.
   d_ht = 'Neither'; othr = 0
   
   if('Other risk factors' %in% pars$med){
      othr=1
   }
   
   if( ('Hypertension' %in% pars$med) & !('Diabetes' %in% pars$med) ){
      d_ht = 'Htn'
   } else if( !('Hypertension' %in% pars$med) & ('Diabetes' %in% pars$med) ){
      d_ht = 'Diabetes'
   } else if( ('Hypertension' %in% pars$med) & ('Diabetes' %in% pars$med)  ){
      d_ht = 'Both'
   }
   
   # Process race/ethnicity parss.
   d_eth = ifelse(pars$eth=="Black (non-Hispanic)",  "Black NonHisp",
                  ifelse(pars$eth=="Hispanic", "Hispanic", 
                         ifelse(pars$eth=="Other", "Other", "White NonHisp")))
   
   # Subset data based on parss.
   subset = data[which(
      data$agegrp == pars$agegrp &
         data$diab_htn == d_ht &
         data$otherRisk == othr &
         data$momEthnicity == d_eth &
         data$wtgainQ == gsub(".*\\[","\\[",pars$wtgainQ) &
         data$birthwtQ == gsub(".*\\[","\\[",pars$birthwtQ) &
         data$multiparous == pars$multiparous & 
         data$male == pars$male
   ),]
}

### Generate set of similar patients to the parss.
tidy_similar_patients <- function(data, pars){
   
   # Process medical condition parss.
   d_ht = 'Neither'; othr = 0
   
   if('Other risk factors' %in% pars$med){
      othr=1
   }
   
   if( ('Hypertension' %in% pars$med) & !('Diabetes' %in% pars$med) ){
      d_ht = 'Htn'
   } else if( !('Hypertension' %in% pars$med) & ('Diabetes' %in% pars$med) ){
      d_ht = 'Diabetes'
   } else if( ('Hypertension' %in% pars$med) & ('Diabetes' %in% pars$med)  ){
      d_ht = 'Both'
   }
   
   # Process race/ethnicity parss.
   d_eth = ifelse(pars$eth=="Black (non-Hispanic)",  "Black NonHisp",
                  ifelse(pars$eth=="Hispanic", "Hispanic", 
                         ifelse(pars$eth=="Other", "Other", "White NonHisp")))
   
   # Subset data based on parss.
   subset = data[which(
      data$agegrp == pars$agegrp &
         data$diab_htn == d_ht &
         data$otherRisk == othr &
         data$momEthnicity == d_eth &
         data$multiparous == pars$multiparous & 
         data$male == pars$male
   ),]
   
   return(subset)
}