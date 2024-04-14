----------------------
##Loading Packages in
----------------------

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

-------------------------------------------------------------------------------------
##Removing Freelance and Part Time Employees for foreign v US Median Salary Analysis
-------------------------------------------------------------------------------------
  
foreign_median_df <-salary[salary$employment_type %in% c('CT','FT'), ]

----------------------------------------------------
##Adding in a Column to show foreign vs US Residency
-----------------------------------------------------
  
foreign_median_df <-foreign_median_df %>%
  mutate(foreign_residence = foreign_median_df$employee_residence != 'US')

--------------------------------
##Adding in median salary column
--------------------------------
  
foreign_median_df <-foreign_median_df %>%
  group_by(experience_level, employment_type, foreign_residence) %>%
  mutate(median_salary = median(salary_in_usd))

------------------------------------------------------------------------------------------
##Making Experience Level/Remote Ratio/Foreign Residence more Readable in both Data frames
------------------------------------------------------------------------------------------
salary <- salary %>%
  mutate(experience_level = recode(experience_level, EN = 'Entry Level', MI = 'Mid-level', SE =  'Senior Level', EX = 'Executive Level' ))

salary$remote_ratio = factor(salary$remote_ratio, 
                     levels = c(0, 50, 100), 
                     labels = c("On-Site", "Hybrid", "Remote"))

foreign_median_df <- foreign_median_df %>%
  mutate(experience_level = recode(experience_level, EN = 'Entry Level', MI = 'Mid-level', SE =  'Senior Level', EX = 'Executive Level' ))


foreign_median_df$remote_ratio = factor(foreign_median_df$remote_ratio, 
                      levels = c(0, 50, 100), 
                      labels = c("On-Site", "Hybrid", "Remote"))


foreign_median_df$foreign_residence = factor(foreign_median_df$foreign_residence,
                      levels = c("TRUE","FALSE"),
                      labels = c("Foreign","US"))


------------------------------------------------
##Recoding employment type for display purposes
------------------------------------------------

foreign_median_df <- foreign_median_df %>%
  mutate(employment_type = recode(employment_type, FT = 'Full Time', CT = 'Contract', FL =  'Freelance' ))
  
------------------------------------------------------
##Creating Experience Level Vector to Order for charts
------------------------------------------------------
  
level_order <- c('Entry Level', "Mid-level", "Senior Level", 'Executive Level') 

--------------------------------------------------
##Chart to display US v Foreign Median Salaries
--------------------------------------------------

ggplot(foreign_median_df)+
  geom_point((aes( x= factor(experience_level, level = level_order), y= median_salary, color = foreign_residence, size = 2))) +
  facet_grid(.~employment_type) +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0,450000), breaks = c(0,50000,100000,150000,200000,250000,300000,350000,400000,450000))+
  labs(x= "Experience Level", y = "Median Salary in USD", title = "Median Salary for US/Foreign Data Science Roles ",color = "Residence")  +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_size_continuous(guide = "none")

---------------------------------------------------------------------------------------
##Creating Data frame to show change in Salary of selected Data Science Roles over time
---------------------------------------------------------------------------------------
  
all_data_scientists <- salary[salary$job_title %in% c('Data Scientist','Data Analyst','Data Engineer'), ]

##Narrowing to only contract/Full time

all_data_scientists <- all_data_scientists[all_data_scientists$employment_type %in% c('CT','FT'),]

##Narrowing to only mid/senior level employees

all_data_scientists <- all_data_scientists[all_data_scientists$experience_level %in% c('Senior Level','Mid-level'),]

##Adding in median salary column to dataframe grouped by work year, experience level, and job title

all_data_scientists<- all_data_scientists %>%
  group_by(work_year, experience_level, job_title) %>%
  mutate(median_salary = median(salary_in_usd))

----------------------------------------------------
## Graph of Changing Salaries over time by job title
----------------------------------------------------
  
ggplot(all_data_scientists) +
  geom_line(aes(x = work_year, y = median_salary, color = job_title, group = job_title)) +
  theme(axis.text.x = element_text(angle = 45)) +
  facet_wrap(.~experience_level) +
  scale_x_continuous(breaks = c(2020,2021,2022)) +
  scale_y_continuous(labels = scales::dollar_format(),breaks = c(0,25000,50000,75000,100000,125000,150000),limits = c(0,150000)) +
  labs(x= "Work Year", y = "Median Salary in USD", title = "Median Data Science Salaries Over Time",color = 'Job Title') 

--------------------------------------------------
##Creating Data frame for Data Scientist in the US
--------------------------------------------------

us_data_scientist <- salary[salary$employee_residence == 'US', ]

##Narrowing to only Senior Level

us_data_scientist_senior <- us_data_scientist[us_data_scientist$experience_level == 'Senior Level', ]

##Narrowing to only Data Scientists

us_data_scientist_senior_title <- us_data_scientist_senior[us_data_scientist_senior$job_title == 'Data Scientist', ]

##Creating Data Frames for on-site vs remote

us_data_scientist_senior_title_on_site <- us_data_scientist_senior_title[us_data_scientist_senior_title$remote_ratio == 'On-Site', ]

us_data_scientist_senior_title_remote <-us_data_scientist_senior_title[us_data_scientist_senior_title$remote_ratio == 'Remote', ]

--------------------------------------------------------------------------------------------
##Finding Percentiles for breakdown on Salaries in USD for remote vs on site Data Scientists
--------------------------------------------------------------------------------------------

percentile_75_on_site <- quantile(us_data_scientist_senior_title_on_site$salary_in_usd, .75)
percentile_50_on_site <- quantile(us_data_scientist_senior_title_on_site$salary_in_usd, .50)
percentile_25_on_site <- quantile(us_data_scientist_senior_title_on_site$salary_in_usd, .25)

percentile_75_remote <- quantile(us_data_scientist_senior_title_remote$salary_in_usd, .75)
percentile_50_remote <- quantile(us_data_scientist_senior_title_remote$salary_in_usd, .50)
percentile_25_remote <- quantile(us_data_scientist_senior_title_remote$salary_in_usd, .25)

-----------------------------------------------------------------
##Box Plot Showing Percentiles of salary in USD of Data Scientist
-----------------------------------------------------------------
  
ggplot(us_data_scientist_senior_title) +
  geom_boxplot(aes(x=job_title, y= salary_in_usd, fill = remote_ratio)) +
  scale_y_continuous(labels = scales::dollar_format(), breaks = c(0,106763,130000,145200,172500,210000,250000)) +
  facet_grid(.~remote_ratio) +
  labs(x= "Job Title", y = "Salary in USD", title = "Box Plot of Salary for Senior Level US Data Scientists", fill = "Remote or On-Site") 

-----------------------------------------------
##Creating Data frame for Foreign Data Engineer
-----------------------------------------------
  
foreign_data_engineer <- salary[salary$employee_residence != 'US', ]

##Narrowing to only Mid-level

foreign_data_engineer_mid <- foreign_data_engineer[foreign_data_engineer$experience_level == 'Mid-level', ]

##Narrowing to only Data Engineers

foreign_data_engineer_mid_title <- foreign_data_engineer_mid[foreign_data_engineer_mid$job_title == 'Data Engineer', ]

##Creating Data Frames for remote

foreign_data_engineer_senior_title_remote <-foreign_data_engineer_mid_title[foreign_data_engineer_mid_title$remote_ratio == 'Remote', ]

--------------------------------------------------------------------------------------------
##Finding Percentiles for breakdown on Salaries in USD for remote and foreign Data Engineers
--------------------------------------------------------------------------------------------

percentile_75_remote_engineer <- quantile(foreign_data_engineer_senior_title_remote$salary_in_usd, .75)
percentile_50_remote_engineer <- quantile(foreign_data_engineer_senior_title_remote$salary_in_usd, .50)
percentile_25_remote_engineer <- quantile(foreign_data_engineer_senior_title_remote$salary_in_usd, .25)

------------------------------------------------------------------------
##Box Plot Showing Percentiles of salary in USD of Foreign Data Engineer
------------------------------------------------------------------------
  
ggplot(foreign_data_engineer_senior_title_remote) +
  geom_boxplot(aes(x=job_title, y= salary_in_usd,fill = job_title)) +
  scale_y_continuous(labels = scales::dollar_format(), breaks = c(0,25000,50000,60658,76940,87932,100000,125000,150000)) +
  facet_grid(.~remote_ratio) +
  labs(x= "Job Title", y = "Salary in USD", title = "Box Plot of Salary for Foreign Mid-Level Data Engineer", fill = "Job Title") 

----------------------------------------------
##Creating Data Frame for Foreign Data Analyst
----------------------------------------------
  
foreign_data_analyst <- salary[salary$employee_residence != 'US', ]

##Narrowing to only Mid-level

foreign_data_analyst_mid <- foreign_data_analyst[foreign_data_analyst$experience_level == 'Mid-level', ]

##Narrowing to only Data Analysts

foreign_data_analyst_mid_title <- foreign_data_analyst_mid[foreign_data_analyst_mid$job_title == 'Data Analyst', ]

##Creating Data Frames for remote

foreign_data_analyst_title_remote <-foreign_data_analyst_mid_title[foreign_data_analyst_mid_title$remote_ratio == 'Remote', ]

--------------------------------------------------------------------------------------------
##Finding Percentiles for breakdown on Salaries in USD for remote and foreign Data Analysts
--------------------------------------------------------------------------------------------

percentile_75_remote_analyst <- quantile(foreign_data_analyst_title_remote$salary_in_usd, .75)
percentile_50_remote_analyst <- quantile(foreign_data_analyst_title_remote$salary_in_usd, .50)
percentile_25_remote_analyst <- quantile(foreign_data_analyst_title_remote$salary_in_usd, .25)
 
------------------------------------------------------------------------
##Box Plot Showing Percentiles of salary in USD for Foreign Data Analyst
------------------------------------------------------------------------
  
ggplot(foreign_data_analyst_title_remote) +
  geom_boxplot(aes(x=job_title, y= salary_in_usd,fill = job_title)) +
  scale_y_continuous(labels = scales::dollar_format(), breaks = c(0,25000,32974,39263,43966,50000)) +
  facet_grid(.~remote_ratio) +
  labs(x= "Job Title", y = "Salary in USD", title = "Box Plot of Salary for Foreign Mid-Level Data Analyst", fill = "Job Title") 
