


# Use this cell to begin your analysis, and add as many as you would like!
library(readr)
library(data.table)
library(readxl)
library(dplyr)
library(stringr)

wastestats <- read_csv("datasets/wastestats.csv")
wastestats <- wastestats %>% 
filter(waste_type %in% 
       c("Glass", "Plastics", "Ferrous Metals", "Non-ferrous Metals", "Ferrous metal", "Non-ferrous metal", "Plastic", "Non-ferrous metals") 
      & year > 2014)

waste_18_19 <-  read.csv("datasets/2018_2019_waste.csv")
names(waste_18_19)[names(waste_18_19) == "Waste.Type"] <- "waste_type"
names(waste_18_19)[names(waste_18_19) == "Total.Generated...000.tonnes."] <- "total_waste_generated"
names(waste_18_19)[names(waste_18_19) == "Total.Recycled...000.tonnes."] <- "total_waste_recycle_tonne"
waste_18_19 <- waste_18_19 %>% 
filter(waste_type %in% c("Glass", "Plastics", "Ferrous Metal", "Non-Ferrous Metal"))

saved <- read_csv("datasets/energy_saved.csv", skip = 2)

# Extracted the amount saved per metric ton by item:
plastic_rate <- as.numeric(str_replace(saved[2,2], "Kwh", ""))
glass_rate <- as.numeric(str_replace(saved[2,3], "Kwh", ""))
ferrous_metal_rate <- as.numeric(str_replace(saved[2,4], "Kwh", ""))
nonferrous_metal_rate <- as.numeric(str_replace(saved[2,5], "Kwh", ""))

# Made separate tables for each year:
annual_energy_savings <- data.frame(year = c(2015:2019), total_energy_saved = c(0,0,0,0,0))
wastestats_2015 <- wastestats %>% filter(year == 2015)
wastestats_2016 <- wastestats %>% filter(year == 2016)
wastestats_2017 <- wastestats %>% filter(year == 2017)
wastestats_2018 <- waste_18_19 %>% filter(Year == 2018)
wastestats_2019 <- waste_18_19 %>% filter(Year == 2019)

# Took each rcycled total, multiplied by the rate fro each item, and added by year:
total_2015 = (wastestats_2015[1,]$total_waste_recycled_tonne * plastic_rate) + 
             (wastestats_2015[2,]$total_waste_recycled_tonne * ferrous_metal_rate) +
             (wastestats_2015[3,]$total_waste_recycled_tonne * nonferrous_metal_rate) +
             (wastestats_2015[4,]$total_waste_recycled_tonne * glass_rate)
total_2016 = (wastestats_2016[1,]$total_waste_recycled_tonne * plastic_rate) + 
             (wastestats_2016[2,]$total_waste_recycled_tonne * ferrous_metal_rate) +
             (wastestats_2016[3,]$total_waste_recycled_tonne * nonferrous_metal_rate) +
             (wastestats_2016[4,]$total_waste_recycled_tonne * glass_rate)
total_2017 = (wastestats_2017[1,]$total_waste_recycled_tonne * ferrous_metal_rate) + 
             (wastestats_2017[2,]$total_waste_recycled_tonne * nonferrous_metal_rate) +
             (wastestats_2017[3,]$total_waste_recycled_tonne * glass_rate) +
             (wastestats_2017[4,]$total_waste_recycled_tonne * plastic_rate)
total_2018 = (wastestats_2018[1,]$total_waste_recycle_tonne * ferrous_metal_rate) + 
             (wastestats_2018[2,]$total_waste_recycle_tonne * plastic_rate) +
             (wastestats_2018[3,]$total_waste_recycle_tonne * nonferrous_metal_rate) +
             (wastestats_2018[4,]$total_waste_recycle_tonne * glass_rate)
total_2018 = total_2018 * 1000

total_2019 = (wastestats_2019[1,]$total_waste_recycle_tonne * ferrous_metal_rate) + 
             (wastestats_2019[2,]$total_waste_recycle_tonne * plastic_rate) +
             (wastestats_2019[3,]$total_waste_recycle_tonne * nonferrous_metal_rate) +
             (wastestats_2019[4,]$total_waste_recycle_tonne * glass_rate)
total_2019 = total_2019 * 1000

annual_energy_savings[1,]$total_energy_saved = total_2015
annual_energy_savings[2,]$total_energy_saved = total_2016
annual_energy_savings[3,]$total_energy_saved = total_2017
annual_energy_savings[4,]$total_energy_saved = total_2018
annual_energy_savings[5,]$total_energy_saved = total_2019
annual_energy_savings

# These packages need to be loaded in the first @tests cell
library(testthat) 
library(IRkernel.testthat)
library(stringr)

# https://instructor-support.datacamp.com/en/articles/4544008-writing-project-tests-guided-and-unguided-r-and-python
# The @solution should pass the tests
# The purpose of the tests is to try to catch common errors and
# to give the student a hint on how to resolve these errors

# There are two tests in this cell. The first one tests that the
# correct package was loaded. The second one tests that the
# correct data were read in.

test_saved <- function(df1, df2){
    return(as.numeric(data.frame(df1)$total_energy_saved) == 
                     as.numeric(df2$total_energy_saved))
}


year <- c(2015, 2016, 2017, 2018, 2019)
total <- c(3435929000, 2554433400, 2470596000, 2698130000, 2765440000)
test_solution <- data.frame('year' = year, 'total_energy_saved' = total)

run_tests({    
    test_that("The solution has been provided.", {
        expect_true(exists('annual_energy_savings'), 
            info = "Have you assigned your answer to a data frame named `annual_energy_savings`?")
        expect_s3_class(annual_energy_savings, "data.frame")
    })
    test_that("The answer column exists.", {
        expect_true('total_energy_saved' %in% colnames(data.frame(annual_energy_savings)),
            info = "Your data frame is missing the required columns!")
    })
    test_that("The year column exists.", {
        expect_true('year' %in% colnames(data.frame(annual_energy_savings)),
            info = "Your data frame is missing the required columns!")
    })
    test_that("The total energy saved column is equal", {
        expect_true(isTRUE(all.equal(as.numeric(data.frame(annual_energy_savings)$total_energy_saved), 
                     as.numeric(test_solution$total_energy_saved))),
            info = "Your submitted data frame does not contain the correct values!")
    })
    test_that("The year column is equal", {
        expect_true(isTRUE(all.equal(str_extract(data.frame(annual_energy_savings)$year, 
                                                 "(\\d{4})"), str_extract(test_solution$year, "(\\d{4})"))),
            info = "Your submitted data frame does not contain the correct values!")
    })
})
