library(here)

source(here("R/create_Table1.R"))

# NHANES ------------------------------------------------------------------
nhanes1 <- generate_nhanes_Table1("2017Mar2020")
sink(here("results", "tables", "Table1_NHANES_2017Mar2020.txt"))
print(nhanes1)
sink()

nhanes2 <- generate_nhanes_Table1("20212023")
sink(here("results", "tables", "Table1_NHANES_20212023.txt"))
print(nhanes2)
sink()

# BRFSS -------------------------------------------------------------------
brfss17 <- generate_brfss_Table1(2017)
sink(here("results", "tables", "Table1_BRFSS_2017.txt"))
print(brfss17)
sink()

brfss19 <- generate_brfss_Table1(2019)
sink(here("results", "tables", "Table1_BRFSS_2019.txt"))
print(brfss19)
sink()

brfss21 <- generate_brfss_Table1(2021)
sink(here("results", "tables", "Table1_BRFSS_2021.txt"))
print(brfss21)
sink()

brfss23 <- generate_brfss_Table1(2023)
sink(here("results", "tables", "Table1_BRFSS_2023.txt"))
print(brfss23)
sink()

# Pursuant ----------------------------------------------------------------
pursuant17 <- generate_pursuant_Table1(2017, 2020)
sink(here("results", "tables", "Table1_Pursuant_20172020.txt"))
print(pursuant17)
sink()

pursuant21 <- generate_pursuant_Table1(2021, 2024)
sink(here("results", "tables", "Table1_Pursuant_20212024.txt"))
print(pursuant21)
sink()

pursuantAll <- generate_pursuant_Table1(2017, 2024)
sink(here("results", "tables", "Table1_Pursuant_20172024.txt"))
print(pursuantAll)
sink()

pursuantExcluded <- generate_pursuant_Table1_exclude(2017, 2024)
sink(here("results", "tables", "Table1_PursuantExcluded_20172024.txt"))
print(pursuantExcluded)
sink()