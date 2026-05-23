#Author: Joana RR
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
#step 1 combining data pilot and final
#step 2 generating results table with 5 criteria

#step 1 combining data pilot and final
#read tidied datasets
database_pilot_v2 <- readr::read_csv("Final_phase_Article/database/database_pilot_v1.csv") %>% 
  mutate(key = article_id) %>% 
  dplyr::select(key, 
         outcome,
         condordance_aim,
         condordance_primary_endpoint,
         condordance_design_aim,
         condordance_sample_size,
         RCT_clinical_discussion_interpretation,
         final_publication_significant_results,
         pretrial_document_type) %>% 
  add_column(phase_pilot=1)

database_v2 <- read_csv("Final_phase_Article/database/database_vapril27.csv") %>% 
  dplyr::select(key, 
                outcome,
                condordance_aim,
                condordance_primary_endpoint,
                condordance_design_aim,
                condordance_sample_size,
                RCT_clinical_discussion_interpretation,
                final_publication_significant_results,
                pretrial_document_type) %>% 
  add_column(phase_pilot=0)

 #combine datasets 
total_87_167 <- rbind(database_pilot_v2, database_v2)
 
total_87_167_v1 <- total_87_167 %>% 
  mutate(outcome = factor(outcome,levels=c(1,0,999),
                          labels =c("Yes","No","999")),
         condordance_aim = factor(condordance_aim,levels=c(1,0,2,999),
                                  labels =c("Yes ", "No ", "Discrepancy between pretrial document and final RCT", "Not applicable")),
         condordance_primary_endpoint= factor(condordance_primary_endpoint,levels=c(1,0,2, 999),
                                              labels =c("Yes", "No", "Discrepancy between pretrial document and final RCT",
                                                        "Not applicable")),
         condordance_design_aim= factor(condordance_design_aim,levels=c(1,0,3,999), 
                                        labels =c("Yes", "No",
                                                  "Discrepancy between pretrial document and final RCT",
                                                  "Not applicable")),
         condordance_sample_size= factor(condordance_sample_size,levels=c(1,0,2,999),
                                         labels =c("Yes", "No","Discrepancy between pretrial document and final RCT", "Not applicable")),
         RCT_clinical_discussion_interpretation= factor(RCT_clinical_discussion_interpretation,
                                                        levels=c(1,0,999),
                                                        labels =c("Yes", "No", "Not applicable")),
         final_publication_significant_results=factor(final_publication_significant_results,
                                                      levels=c(1,0,999),
                                                      labels =c("Yes", "No", "Not applicable")),
         pretrial_document_type = factor(pretrial_document_type,levels=c(1,2,3,999),
                                         labels =c("Published protocol", "Unpublished protocol", "Registry only", "No document")),
         phase_pilot = factor(phase_pilot, levels=c(1,0), labels=c("Pilot", "Final") )
  )


#specifying factor variables
#ensure factor variables are factors
discrete_variables <- c(
  "outcome",
  "condordance_aim",
  "condordance_primary_endpoint",
  "condordance_design_aim",
  "condordance_sample_size",
  "RCT_clinical_discussion_interpretation",
  "final_publication_significant_results",
  "pretrial_document_type", 
  "phase_pilot")
total_87_167_v1[, discrete_variables] <- lapply(
  total_87_167_v1[, discrete_variables], factor)

#create labels
labels_table_1 <- list(
  "outcome"~"RCTs fulfilling four criteria",
  "condordance_primary_endpoint"~"Reported specific primary outcome",
  #  "condordance_aim"~"Study hypothesis specified",
  "condordance_design_aim"~"Trial design aligned with reported trial hypothesis",
  "condordance_sample_size"~"Sample size calculation based on clinical relevance",
  "RCT_clinical_discussion_interpretation"~"Results interpreted based on clinical relevance"
#  "phase_pilot" ~ "Phase of data collection"
  #,"final_publication_significant_results"~"Statistical significance achieved for primary endpoint"
  #,pretrial_document_type ~ "Pretrial documentation type"
)

#create tbl_summary no comparison
summary_table_1 <- total_87_167_v1 %>%
dplyr::select(
    outcome,
    condordance_primary_endpoint,
    #    condordance_aim,
    condordance_design_aim,
    condordance_sample_size,
    RCT_clinical_discussion_interpretation,
    phase_pilot
    #,final_publication_results
    #,pretrial_documentation_type
  ) %>%
  tbl_summary(
    by=phase_pilot,
    label = labels_table_1,
    statistic = all_categorical() ~ "{n} ({p}%)" ,
    missing = "no",
    digits = list(
      all_categorical() ~ c(0,1,0))
  ) %>% 
  as_flex_table() %>% 
  add_footer_lines(values = "Results are expressed in numbers (percentage(%))" ) %>% 
  add_footer_lines(values ="Abbreviations: RCT, Randomized controlled trial"
  ) %>% 
  set_caption("Table 1. Assessment of ability of surgical RCTs to meaningfully inform clinical practice")
#save
save_as_docx(summary_table_1, path = "Final_phase_Article/output/tables/Table_1_Assessment_surgical_RCTs_clinical_practice_april27correction.docx")

#create tbl_summary 2 comparing RCTs with registry documentation/unpublished protocol/published protocol
summary_table <- 
  

