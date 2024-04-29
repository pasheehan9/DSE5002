# -*- coding: utf-8 -*-
"""
Created on Sun Apr 28 12:10:52 2024

@author: pashe
"""
# Importing Libraries
import pandas as pd
import numpy as np



#Reading in and renaming different data sets

cost_of_living_df = pd.read_csv('Week_8/psheehan.module08PythonProject.cost_of_living.csv')


fyi_salary_df = pd.read_csv('Week_8/psheehan.module08PythonProject.Levels_Fyi_Salary_Data.csv')


salary_df = pd.read_csv('Week_8/psheehan.module08PythonProject.ds_salaries.csv')


country_codes_df = pd.read_excel('Python Project/country_codes.xlsx')



#Seperating out Country into a new column to Join later 


cost_of_living_df['Country'] = cost_of_living_df['City'].str.rsplit(', ').str[-1]



#Renaming Country Names to be able to fully left join the two data framse


country_codes_df['Country'] = country_codes_df['Country'].replace('United States of America (the)', 'United States')

country_codes_df['Country'] = country_codes_df['Country'].replace('Bahamas (the)', 'Bahamas')

country_codes_df['Country'] = country_codes_df['Country'].replace('United Kingdom of Great Britain and Northern Ireland (the)', 'United Kingdom')

country_codes_df['Country'] = country_codes_df['Country'].replace('Russian Federation (the)', 'Russia')

country_codes_df['Country'] = country_codes_df['Country'].replace('Netherlands (the)', 'Netherlands')

country_codes_df['Country'] = country_codes_df['Country'].replace('United Arab Emirates (the)', 'United Arab Emirates')

country_codes_df['Country'] = country_codes_df['Country'].replace('Trinidad and Tobago', 'Trinidad And Tobago')

country_codes_df['Country'] = country_codes_df['Country'].replace('Philippines (the)', 'Philippines')

country_codes_df['Country'] = country_codes_df['Country'].replace('Viet Nam', 'Vietnam')

country_codes_df['Country'] = country_codes_df['Country'].replace('Bosnia and Herzegovina', 'Bosnia And Herzegovina')

country_codes_df['Country'] = country_codes_df['Country'].replace('Bolivia (Plurinational State of)', 'Bolivia')

country_codes_df['Country'] = country_codes_df['Country'].replace('Syrian Arab Republic', 'Syria')

country_codes_df['Country'] = country_codes_df['Country'].replace('Tanzania, United Republic of', 'Tanzania')
                                                                  
country_codes_df['Country'] = country_codes_df['Country'].replace('Iran (Islamic Republic of)', 'Iran')

country_codes_df['Country'] = country_codes_df['Country'].replace('Moldova (the Republic of)', 'Moldova')

country_codes_df['Country'] = country_codes_df['Country'].replace('Czechia', 'Czech Republic')

country_codes_df['Country'] = country_codes_df['Country'].replace('Taiwan (Province of China)', 'Taiwan')

country_codes_df['Country'] = country_codes_df['Country'].replace('Venezuela (Bolivarian Republic of)', 'Venezuela')

country_codes_df['Country'] = country_codes_df['Country'].replace('Dominican Republic (the)', 'Dominican Republic')


#Splitting the location column to prepare to join to the merges salaries data scientist dataframe

fyi_salary_df['Country'] = fyi_salary_df['location'].str.rsplit(', ').str[-1]




#Creating a function to return United States so that I can join Country Codes for

def country_transformation(Country):
    if len(Country) == 2:
        return 'United States'
    else:
        return Country

fyi_salary_df['Country'] = fyi_salary_df['Country'].apply(country_transformation)



#Narrowing down to my demographic

fyi_salary_df_male = fyi_salary_df[fyi_salary_df['gender'] == 'Male']

fyi_salary_df_male_white = fyi_salary_df_male[fyi_salary_df_male['Race'] == 'White']

fyi_salary_df_male_white_male_bachelor_degree = fyi_salary_df_male_white[fyi_salary_df_male_white['Education'] == "Bachelor's Degree"]

fyi_salary_df_male_white_male_bachelor_degree_data_scientist = fyi_salary_df_male_white_male_bachelor_degree[fyi_salary_df_male_white_male_bachelor_degree['title'] == "Data Scientist"]

#Joining Country Codes to the cost of living dataframe

codes_living_df = pd.merge(cost_of_living_df, country_codes_df, how = 'left', on= 'Country')

#Renaming alpha two code to merge salary and cost of living on employee residence

codes_living_df = codes_living_df.rename(columns = {'Alpha-2 code' : 'employee_residence'})


salaries_merged_df = pd.merge(codes_living_df,salary_df, how = 'left', on = 'employee_residence')

#Narrowing to Data Scientist 

salaries_merged_data_scientist = salaries_merged_df[salaries_merged_df['job_title'] == 'Data Scientist']


#Joining My demographic group to country codes

codes_demogaphic_df = pd.merge(fyi_salary_df_male_white_male_bachelor_degree_data_scientist, country_codes_df, how = 'left', on= 'Country')

#Concatening all data to use as my working dataset


working_df = pd.concat([salaries_merged_data_scientist,codes_demogaphic_df])


