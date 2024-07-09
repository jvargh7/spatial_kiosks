library(arrow)
library(data.table)
library(dplyr)
library(here)

dt   <- open_dataset(here("data", "kiosk-data-parquet-cleaned"),format = "parquet") 

taskmap <- dt |>
  count(state, age_group, gender, year_range) |>
  arrange(desc(n)) |>
  data.table() |>
  collect()

taskmap[, ID := 1:.N]

LOWER_LIMIT <- 200000

map <- taskmap[n < LOWER_LIMIT]

reverse_sum    <- cumsum(map[order(n),n])
reverse_sum_dt <- data.table(ID = rev(map[,ID]), n = reverse_sum)

N <- reverse_sum_dt[, max(ID)]

# index
start <- N

group_num <- 1
for(i in 1:nrow(map)){
  row <- map[i,]
  n_left <- LOWER_LIMIT - row[,n]
  
  end <- reverse_sum_dt[ID %in% start:1, ][n >= n_left, max(ID)]
  
  group <- start:end
  
  map[ID %in% c(row[,ID], group), grp := group_num]
  start <- end-1
  group_num <- group_num + 1
}

# Specific group finder algorithm
# Input: a data.table with columns for the group variables and 'n'. 
# Output: the same data.table but with a new "group" column identifying the SLURM array task ID. 
#
# 1. Compute the cumulative sum of `n` in reverse order. Refer to this table as rev_dt. 
# For each row
# 2. Retrieve `n` for that row. If it is greater than 500K, it is its own group.
# 3. Let 'i' be the starting point in rev_dt we are allowed to access for making new groups. 
# 4. If a row's `n` is less than 500k, start from 'i' in rev_dt and go all the way to wherever the 'n' value added to the row 'n'
# exceeds 500000. If it does, make the group. 
# Reset i to the end of the previous operation. 
# 5. Keep going with the loop until 1) there are no more rows left. 

# Generate task map to fit within constraints of SLURM cluster array job size
# 1. Group by state/age group/gender/year_range
# 2. Order by (n). 
# 3. Require any group to be at least 500,000 rows long
# 4. If groups have less than 500,000, group them together until they reach 500K (sum(n)) and give them a group identifier.

# Need to stay under 1000 jobs for array
# Largest N allowed is 1.5 million
# Let's enforce smallest N allowed is 500K