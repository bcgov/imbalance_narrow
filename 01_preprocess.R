library(tidyverse)
library(here)
library(vroom)
library(janitor)
library(readxl)
library(conflicted)
library(qdapRegex)
conflicts_prefer(dplyr::filter)
#constants-------------------
letters <- "[:alpha:]"
#functions
fix_data <- function(vctr){
  vctr|>
    str_remove_all(",")|>
    as.numeric()
}
# get and process the data
cip_noc_long <- vroom::vroom(here("data","cip_2_noc_canada.csv"), skip = 13)[-1,]
colnames(cip_noc_long)[1] <- "field_of_study"
cip_noc_long <- cip_noc_long|>
  remove_empty("cols")|>
  pivot_longer(cols=-field_of_study, names_to = "NOC", values_to = "value")|>
  mutate(value=as.numeric(str_remove_all(value,",")),
         CIP=str_sub(field_of_study, 1, 5),
         field_of_study=str_sub(field_of_study, 7))|>
  filter(value>0)

cip_noc_long|>
  group_by(NOC)|>
  mutate(prop=value/sum(value))|>
  filter(prop>.01)|>
  mutate(greater_than_one=n())|>
  slice_max(prop, n=1, with_ties = FALSE)|>
  mutate(prop=round(prop, 3),
         reciprocal=1/greater_than_one,
         index=prop^.5*reciprocal^.5,
         narrowness=case_when(index>.4 ~ "very narrow",
                              index>.3 ~ "narrow",
                              TRUE ~ "broad"
         ))|>
  arrange(desc(index))|>
  select(`For those with this occupation`=NOC,
         `the most common field of study was`=field_of_study,
         `(with this proportion)`=prop,
         `Furthermore, there were paths greater than 1%`=greater_than_one,
         `resulting in a narrowness index is`=index,
         `and grouping`=narrowness)|>
  write_rds(here("out","narrowness.rds"))

cip_noc_long|>
  write_rds(here("out","cip_noc_long.rds"))

readxl::read_excel(here("data","job_openings_occupation.xlsx"), skip = 3)|>
  remove_constant()|>
  filter(NOC!="#T",
         Variable=="Job Openings",
         `Geographic Area`=="British Columbia"
  )|>
  select(-Variable, -`Geographic Area`)|>
  mutate(NOC=str_remove(NOC,"#"))|>
  unite("NOC", NOC, Description, sep=" ")|>
  pivot_longer(cols = starts_with("2"))|>
  group_by(NOC)|>
  summarize(mean_job_openings=mean(value, na.rm = TRUE))|>
  write_rds(here("out","mean_jo.rds"))

read_excel(here("data", "3710023501_PSE grads by CIP4 and status of students.xlsx"))|>
  clean_names()|>
  select(year=ref_date, field_of_study, status_of_student_in_canada, graduates=value)|>
  filter(status_of_student_in_canada %in% c("Canadian students", "International students"))|>
  mutate(CIP=ex_between(field_of_study,"[","]"),
         CIP=str_remove_all(CIP, letters),
         field_of_study=word(field_of_study, sep="\\["))|>
  group_by(CIP, status_of_student_in_canada, field_of_study)|>
  summarize(mean_grads=mean(graduates))|>
  group_by(CIP, field_of_study)|>
  pivot_wider(names_from = status_of_student_in_canada, values_from = mean_grads)|>
  write_rds(here("out","raw_cip.rds"))

read_excel(here("data","Construction Trades occupations.xlsx"))|>
  clean_names()|>
  mutate(NOC=paste(str_sub(noc, 2), description, sep=" "))|>
  select(NOC)|>
  write_rds(here("out","construction.rds"))


