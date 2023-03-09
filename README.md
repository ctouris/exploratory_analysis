# exploratory_analysis
Exploratory data analysis on population data for several countries.
The code analyzes population data from a CSV file downloaded from the United Nations website. 
It starts by loading several libraries such as tidyverse, dplyr, VIM, lattice, and mice. Then, it reads the CSV file from the internet and stores it in the data object. After that, it changes the column names and removes some rows that represent global areas. Next, it converts some columns to numeric and removes rows that contain missing values.

The code then creates a boxplot of the variable "Population mid-year estimates (millions)" for the year 2005, for all countries in the world. It also removes outliers from this boxplot.

The next part of the code creates a new data frame with data for six countries: Uruguay, Argentina, Spain, South Africa, Australia, and Portugal. It creates a boxplot for each country and removes outliers. Finally, it creates a new data frame with data for population mid-year estimates for males in these six countries and creates a boxplot for this data.
