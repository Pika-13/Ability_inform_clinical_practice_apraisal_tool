#Author: Joana RR
#Starting date: Feb 9, 2026
#Last update date: 
#--------------------------------------------
#adding table for the pilot phase
#--------------------------------------------
#install packages if not installed
list.of.packages <- c("tibble" ,"tidyverse", "dplyr", "data.table", "colorspace", "readxl", "haven",
                      "ggpubr", "ggbeeswarm", "scales", "gtsummary", 
                      "data.table", "flextable", "countrycode", "presize")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#loading packages from library
lapply(c("tibble" ,"tidyverse", "dplyr", "data.table", "colorspace", "readxl", "haven",
         "ggpubr", "ggbeeswarm", "scales", "gtsummary", 
         "data.table", "flextable", "countrycode", "presize"), 
       require,character.only = TRUE)
#--------------------------------------------
#read csv from covidence
database_pilot_v0 <- read_xlsx("Final_phase_Article/database/Pilot_87_database.xlsx") %>%
  janitor::clean_names() %>%
  select(article_id, author, title,doi,
         rct_aim_trial,
         rct_design_aim,
         rct_primary_endpoint,
         rct_sample_size,
         pretrial_document_type,
         protocol_aim_trial,
         protocol_design_aim,
         protocol_primary_endpoint,
         protocol_sample_size,
         rct_clinical_discussion_interpretation,
         final_publication_significant_results)

#add columns
database_pilot_v1 <- database_pilot_v0 %>%
  mutate(
    key= as.character(article_id),
    authors = as.character(author),
    title = as.character(title),
    pretrial_document_type = factor(pretrial_document_type,levels=c(1,2,3,999)), #0=published protocol; 1=unpublished protocol; 2=registry only; 999=not mentioned
    RCT_aim_trial = factor(rct_aim_trial,levels = c(0:2,999)), #0=sup; 1=non-inf; 2=equi; 999=not mentioned
    protocol_aim_trial = factor(protocol_aim_trial,levels = c(0:2,999)), #0=sup; 1=non-inf; 2=equi; 999=not mentioned
    RCT_design_aim = factor(rct_design_aim,levels=c(0:2,999)),
    protocol_design_aim = factor(protocol_design_aim,levels=c(0:2,999)),
    RCT_primary_endpoint = as.character(rct_primary_endpoint),
    protocol_primary_endpoint = as.character(protocol_primary_endpoint),
    RCT_sample_size = factor(rct_sample_size,levels=c(0:1,999)),
    protocol_sample_size = factor(protocol_sample_size,levels=c(0:1,999)),
    RCT_clinical_discussion_interpretation = factor(rct_clinical_discussion_interpretation,levels=c(0,1,999)),
    final_publication_significant_results = factor(final_publication_significant_results,levels=c(0,1,999)),
    condordance_aim = case_when(
      RCT_aim_trial!=999 & RCT_aim_trial == protocol_aim_trial ~ 1, #rct and protocol report an aim (SAME aim)
      RCT_aim_trial!=999 & (is.na(protocol_aim_trial) |protocol_aim_trial ==999) ~ 1, #rct reports an aim, protocol doesn't report any aim
      RCT_aim_trial!=999 & protocol_aim_trial !=999 & RCT_aim_trial != protocol_aim_trial  ~ 2, #rct and protocol report an aim (DISCORDANT aim)
      RCT_aim_trial==999 & protocol_aim_trial != 999 ~ 1, #rct doesn't report any aim, protocol reports an aim
      RCT_aim_trial==999 & (is.na(protocol_aim_trial) |protocol_aim_trial == 999) ~ 0) #rct and protocol don't report any aim
  ) %>% 
  mutate(condordance_aim = factor(condordance_aim,levels=c(0,1,2))) %>% 
  mutate(consensus_aim_trial = case_when(
    RCT_aim_trial==0 &  protocol_aim_trial==0 ~ 0, #both report superiority
    RCT_aim_trial==0 &  (is.na(protocol_aim_trial) | protocol_aim_trial==999) ~0, #rct reports superiority, protocol doesn't report any aim
    RCT_aim_trial==999 &   protocol_aim_trial==0 ~0, #rct doesn't report any aim, protocol reports superiority
    RCT_aim_trial==1 &   protocol_aim_trial==1 ~ 1, #both report superiority
    RCT_aim_trial==1 & (is.na(protocol_aim_trial) |protocol_aim_trial==999) ~1, #rct reports superiority, protocol doesn't report any aim
    RCT_aim_trial==999 &   protocol_aim_trial==1 ~1, #rct doesn't report any aim, protocol reports superiority
    RCT_aim_trial==2 &   protocol_aim_trial==2 ~ 2, #both report equivalence
    RCT_aim_trial==2 &   (is.na(protocol_aim_trial) |protocol_aim_trial==999) ~2, #rct reports equivalence, protocol doesn't report any aim
    RCT_aim_trial==999 &   protocol_aim_trial==2 ~2, #rct doesn't report any aim, protocol reports equivalence
    RCT_aim_trial==999 &   (is.na(protocol_aim_trial) |protocol_aim_trial==999) ~ 999, #rct and protocol don't report any aim& 
    RCT_aim_trial!=999 & protocol_aim_trial !=999 & RCT_aim_trial != protocol_aim_trial  ~ 3 #rct and protocol report an aim (DISCORDANT aim)
  )) %>% 
  mutate(consensus_aim_trial = factor(consensus_aim_trial,levels=c(0,1,2,3,999))) %>% 
  mutate(consensus_design_aim = case_when(
    RCT_design_aim==0 &   protocol_design_aim==0 ~ 0, #both have superiority aim
    RCT_design_aim==0 &   (is.na(protocol_design_aim) |protocol_design_aim==999) ~0, #rct has superiority aim, protocol don't have design aim
    RCT_design_aim==999 &   protocol_design_aim==0 ~0, #rct don't have design aim, protocol has superiority aim
    RCT_design_aim==1 &   protocol_design_aim==1 ~ 1, #both have superiority aim
    RCT_design_aim==1 &   (is.na(protocol_design_aim) |protocol_design_aim==999) ~1, #rct has superiority aim, protocol don't have design aim
    RCT_design_aim==999 &   protocol_design_aim==1 ~1, #rct doesn't have any aim, protocol has superiority aim
    RCT_design_aim==2 &   protocol_design_aim==2 ~ 2, #both have equivalence aim
    RCT_design_aim==2 &   (is.na(protocol_design_aim) |protocol_design_aim==999) ~2, #rct has equivalence aim, protocol doesn't have any aim
    RCT_design_aim==999 &   protocol_design_aim==2 ~2, #rct don't have design aim, protocol has equivalence aim
    RCT_design_aim==999 &   (is.na(protocol_design_aim) |protocol_design_aim==999) ~ 999, #rct and protocol don't have design aim& 
    RCT_design_aim!=999 & protocol_design_aim !=999 & RCT_design_aim != protocol_design_aim  ~ 3 #rct and protocol have a discordant aim
  )) %>%
  mutate(consensus_design_aim = factor(consensus_design_aim,levels=c(0,1,2,3,999))) %>% 
  mutate(condordance_design_aim = case_when(
    consensus_aim_trial==999 & consensus_design_aim==999 ~999, #NA: no reported aim, no design aim
    consensus_aim_trial==999 & consensus_design_aim==0 ~999, #NA:no reported aim
    consensus_aim_trial==999 & consensus_design_aim==1 ~999, #NA:no reported aim
    consensus_aim_trial==999 & consensus_design_aim==2 ~999, #NA:no reported aim
    consensus_aim_trial==999 & consensus_design_aim==3 ~3, #discordant design aim
    consensus_aim_trial==0 & consensus_design_aim==999 ~999, #NA:reported aim sup, no design aim
    consensus_aim_trial==0 & consensus_design_aim==0 ~1, #reported aim sup equal to design aim sup
    consensus_aim_trial==0 & consensus_design_aim==1 ~0, #discordant: reported aim sup, design aim non-inf
    consensus_aim_trial==0 & consensus_design_aim==2 ~0, #discordant: reported aim sup, design aim equiv
    consensus_aim_trial==0 & consensus_design_aim==3 ~3, #discordant design aim
    consensus_aim_trial==1 & consensus_design_aim==999 ~999, #NA:reported aim noninf, no design aim
    consensus_aim_trial==1 & consensus_design_aim==0 ~0, #discordant: reported aim noninf, design aim sup
    consensus_aim_trial==1 & consensus_design_aim==1 ~1, #reported aim noninf equal to design aim non-inf
    consensus_aim_trial==1 & consensus_design_aim==2 ~0, #discordant: reported aim noninf, design aim equiv
    consensus_aim_trial==1 & consensus_design_aim==3 ~3, #discordant design aim
    consensus_aim_trial==2 & consensus_design_aim==999 ~999, #NA:reported aim equiv, no design aim
    consensus_aim_trial==2 & consensus_design_aim==0 ~0, #discordant: reported aim equiv, design aim sup
    consensus_aim_trial==2 & consensus_design_aim==1 ~0, #discordant: reported aim equiv, design aim non-inf
    consensus_aim_trial==2 & consensus_design_aim==2 ~1, #reported aim equiv equal to design aim equiv
    consensus_aim_trial==2 & consensus_design_aim==3 ~3, #discordant design aim
    consensus_aim_trial==3 ~3 #discordant reported aim or design aim
  ))  %>%  
  mutate(condordance_design_aim = factor(condordance_design_aim,levels=c(0,1,2,3,999))) %>%
  mutate(condordance_primary_endpoint= case_when(
    RCT_primary_endpoint!=999 & RCT_primary_endpoint == protocol_primary_endpoint ~ 1, #rct and protocol report 1 ep (SAME 1 ep)
    RCT_primary_endpoint!=999  & (is.na(protocol_primary_endpoint) |protocol_primary_endpoint==999) ~ 1, #rct reports 1 ep, protocol doesn't report 1 ep
    RCT_primary_endpoint!=999  & protocol_primary_endpoint!=999 & RCT_primary_endpoint != protocol_primary_endpoint  ~ 2, #rct and protocol report 1 ep (DISCORDANT)
    RCT_primary_endpoint==999  & protocol_primary_endpoint!=999 ~ 1, #rct doesn't report 1 ep, protocol reports 1 ep
    RCT_primary_endpoint==999 & (is.na(protocol_primary_endpoint) |protocol_primary_endpoint==999) ~ 0)) %>% #rct and protocol don't report 1 ep
  mutate(condordance_primary_endpoint = factor(condordance_primary_endpoint,levels=c(0,1,2))) %>% 
  mutate(condordance_sample_size = case_when(
    RCT_sample_size == 1 & protocol_sample_size ==1 ~ 1, #clinical relevance considered in any
    RCT_sample_size== 1  & protocol_sample_size == 0 ~ 2, #discordance: rct considers clinical signif, not the protocol
    RCT_sample_size== 1  & (is.na(protocol_sample_size) |protocol_sample_size == 999)  ~ 1, #clinical relevance considered in any
    RCT_sample_size == 0 & protocol_sample_size ==1 ~ 2, #discordance: protocol considers clinical signif, not the rct
    RCT_sample_size== 0  & protocol_sample_size == 0 ~ 0, #clinical relevance not considered
    RCT_sample_size== 0  & (is.na(protocol_sample_size) |protocol_sample_size == 999)  ~ 0, #clinical relevance not considered
    RCT_sample_size == 999 & protocol_sample_size ==1 ~ 1, #clinical relevance considered in any
    RCT_sample_size== 999  & protocol_sample_size == 0 ~ 0, #clinical relevance not considered
    RCT_sample_size== 999  & (is.na(protocol_sample_size) |protocol_sample_size == 999)  ~ 999) #NA
  )%>% 
  mutate(condordance_sample_size = factor(condordance_sample_size,levels=c(0,1,2,999))
         #,pretrial_documentation_type
  ) %>% 
  mutate(outcome = case_when(condordance_aim==999 &
                               condordance_design_aim==1 & 
                               condordance_primary_endpoint==1 &
                               condordance_sample_size==1 &
                               RCT_clinical_discussion_interpretation==1 ~ 1,
                             condordance_aim==1 &
                               condordance_design_aim==999 & 
                               condordance_primary_endpoint==1 &
                               condordance_sample_size==1 &
                               RCT_clinical_discussion_interpretation==1 ~ 1,
                             condordance_aim==1 &
                               condordance_design_aim==1 & 
                               condordance_primary_endpoint==1 &
                               condordance_sample_size==1 &
                               RCT_clinical_discussion_interpretation==1 ~ 1,
                             TRUE ~ 0)) %>% 
  mutate(outcome = factor(outcome,levels=c(1,0))) %>% 
  select(key, authors, title, 
         #year,
         doi,
         pretrial_document_type,
         RCT_aim_trial,
         protocol_aim_trial,
         RCT_design_aim,
         protocol_design_aim,
         RCT_primary_endpoint,
         protocol_primary_endpoint,
         RCT_sample_size,
         protocol_sample_size,
         RCT_clinical_discussion_interpretation,
         final_publication_significant_results,
         condordance_aim,
         consensus_aim_trial,
         consensus_design_aim,
         condordance_design_aim,
         condordance_primary_endpoint,
         condordance_sample_size,
         outcome)
#save excel
write_csv(database_pilot_v1, "Final_phase_Article/database/database_pilot_v1.csv")
write_xlsx(database_pilot_v1, "Final_phase_Article/database/database_pilot_v1.xlsx")
