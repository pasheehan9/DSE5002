library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(lubridate)

-------------------------
##Loading in Salary Data
-------------------------
  
salary <- read.csv("C:/DSE5002/R Project/patricksheehan.module05RProject.csv"
  ,stringsAsFactors=FALSE
  )

----------------------------
## Finding Unique Job titles
----------------------------

unique_job_titles <- unique(salary$job_title)

print(unique_job_titles)

------------------------------------
##Removing Data From Large Companies 
------------------------------------

salary <- salary[salary$company_size != 'L', ]

-----------------------------------
##Removing Freelance and Part Time
-----------------------------------
  
median_df <-salary[salary$employment_type %in% c('CT','FT'), ]


median_df <-median_df %>%
  mutate(foreign_residence = median_df$employee_residence != 'US')

--------------------------------
##Adding in median salary column
--------------------------------
  
median_df <-median_df %>%
  group_by(experience_level, employment_type, foreign_residence) %>%
  mutate(median_salary = median(salary_in_usd))


-----------------------------------------
##Creating Experience Level Vector to Order
-----------------------------------------
  
level_order <- c('EN', "MI", "SE", 'EX') 

--------------------------------------------------
  ##Chart to display US v Foreign Median Salaries
--------------------------------------------------

ggplot(median_df)+
  geom_point((aes( x= factor(experience_level, level = level_order), y= median_salary, color = foreign_residence, size = 2))) +
  facet_wrap(.~employment_type) +
  scale_x_discrete(level_order) +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,450000))+
  labs(x= "Experience Level", y = "Median Salary in USD", title = "Median Salary for Contract/Fulltime Workers Grouped By Residence")  +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_size_continuous(guide = "none")

--------------------------------------------------
##Creating Data frame for Data Scientist in the US
--------------------------------------------------

us_data_scientist <- salary[salary$employee_residence == 'US', ]

##Narrowing to only Senior Level

us_data_scientist_senior <- us_data_scientist[us_data_scientist$experience_level == 'SE', ]

##Narrowing to only Data Scientists

us_data_scientist_senior_title <- us_data_scientist_senior[us_data_scientist_senior$job_title == 'Data Scientist', ]

##Creating Data Frames for on-site vs remote

us_data_scientist_senior_title_on_site <- us_data_scientist_senior_title[us_data_scientist_senior_title$remote_ratio == 0, ]

us_data_scientist_senior_title_remote <-us_data_scientist_senior_title[us_data_scientist_senior_title$remote_ratio != 0, ]


percentile_75_on_site <- quantile(us_data_scientist_senior_title_on_site$salary_in_usd, .75)
percentile_50_on_site <- quantile(us_data_scientist_senior_title_on_site$salary_in_usd, .50)

percentile_75_remote <- quantile(us_data_scientist_senior_title_remote$salary_in_usd, .75)
percentile_50_remote <- quantile(us_data_scientist_senior_title_remote$salary_in_usd, .50)

-----------------------------------------------
##Creating Data frame for Foreign Data Engineer
-----------------------------------------------
  
foreign_data_engineer <- salary[salary$employee_residence != 'US', ]

##Narrowing to only Senior Level

foreign_data_engineer_mid <- foreign_data_engineer[foreign_data_engineer$experience_level == 'MI', ]

##Narrowing to only Data Scientists

foreign_data_engineer_mid_title <- foreign_data_engineer_mid[foreign_data_engineer_mid$job_title == 'Data Engineer', ]

##Creating Data Frames for remote

foreign_data_engineer_senior_title_remote <-foreign_data_engineer_mid_title[foreign_data_engineer_mid_title$remote_ratio == 100, ]

percentile_75_remote_engineer <- quantile(foreign_data_engineer_senior_title_remote$salary_in_usd, .75)
percentile_50_remote_engineer <- quantile(foreign_data_engineer_senior_title_remote$salary_in_usd, .50)
percentile_25_remote_engineer <- quantile(foreign_data_engineer_senior_title_remote$salary_in_usd, .25)
-------------------------------------
##Show Percentiles of Data Scientist
-------------------------------------

ggplot(us_data_scientist_senior_title) +
  geom_boxplot(aes(x=job_title, y= salary_in_usd)) +
  scale_y_continuous(labels = scales::dollar_format(), breaks = c(0,100000,130000,145200,172500,210000, 250000)) +
  facet_grid(.~remote_ratio) +
  labs(x= "Job Title", y = "Salary in USD", title = "Salary for Senior Level US Data Scientists") 

----------------------------------------
##Show Box Plot of Foreign Data Engineer
----------------------------------------
  
ggplot(foreign_data_engineer_senior_title_remote) +
  geom_boxplot(aes(x=job_title, y= salary_in_usd)) +
  scale_y_continuous(labels = scales::dollar_format(), breaks = c(0,40000,60658,76940,87932,240000)) +
  facet_grid(.~remote_ratio) +
  labs(x= "Job Title", y = "Salary in USD", title = "Salary for Foreign Mid-Level Data Engineer") 


-------------------------------------
##Show Percentiles of Data Analyst
-------------------------------------
  
ggplot(us_data_scientist_senior_title) +
  geom_boxplot(aes(x=job_title, y= salary_in_usd)) +
  scale_y_continuous(labels = scales::dollar_format(), breaks = c(0,100000,130000,145200,172500,210000, 250000)) +
  facet_grid(.~remote_ratio) +
  labs(x= "Job Title", y = "Salary in USD", title = "Salary for Senior Level US Data Scientists") 







--------------------------------------------
##Show Change in Salary of Data Scientists
--------------------------------------------
all_data_scientists <- salary[salary$job_title == 'Data Scientist', ]

all_data_scientists <- all_data_scientists[all_data_scientists$employment_type %in% c('CT','FT'),]

all_data_scientists<- all_data_scientists %>%
  group_by(work_year, experience_level) %>%
  mutate(median_salary = median(salary_in_usd))
  
ggplot(all_data_scientists) +
  geom_line(aes(x = work_year, y = median_salary, color = experience_level, group = experience_level)) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_x_continuous(breaks = c(2020,2021,2022)) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x= "Work Year", y = "Median Salary in USD", title = "Median Data Scientist Salaries Over Time") 

--------------------------------------------
##Show Change in Salary of Data Engineers
--------------------------------------------
all_data_engineers <- salary[salary$job_title == 'Data Engineer', ]

all_data_engineers <- all_data_engineers[all_data_scientists$employment_type %in% c('CT','FT'),]

all_data_engineers<- all_data_engineers %>%
  group_by(work_year, experience_level) %>%
  mutate(median_salary = median(salary_in_usd))

ggplot(all_data_engineers) +
  geom_line(aes(x = work_year, y = median_salary, color = experience_level, group = experience_level)) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_x_continuous(breaks = c(2020,2021,2022)) +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,150000),breaks = c(0, 50000,78526,100000,130000,150000)) +
  labs(x= "Work Year", y = "Median Salary in USD", title = "Median Data Engineer Salaries Over Time") 

--------------------------------------------
##Show Change in Salary of Data Analysts
---------------------------------------------
all_data_analysts <- salary[salary$job_title == 'Data Analyst', ]

all_data_analysts <- all_data_analysts[all_data_analysts$employment_type %in% c('CT','FT'),]

all_data_analysts<- all_data_analysts %>%
  group_by(work_year, experience_level) %>%
  mutate(median_salary = median(salary_in_usd))

ggplot(all_data_analysts) +
  geom_line(aes(x = work_year, y = median_salary, color = experience_level, group = experience_level)) +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_x_continuous(breaks = c(2020,2021,2022)) +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,150000)) +
  labs(x= "Work Year", y = "Median Salary in USD", title = "Median Data Analyst Salaries Over Time") 

