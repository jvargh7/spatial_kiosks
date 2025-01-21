#!/bin/bash

# Define the years for which you want to download the files
years=(2017 2018 2019 2020 2021 2022 2023)

# Base URL for the BRFSS data files
base_url="https://www.cdc.gov/brfss/annual_data"

# Loop through each year and download the respective ASCII zip file
for year in "${years[@]}"; do
  # Construct the URL for each year's ASCII zip file
  url="$base_url/$year/files/LLCP${year}ASC.zip"
  
  # Set the destination file path for downloading the zip file
  zipfile="/projects/waller/spatial_kiosks/data/raw/brfss/BRFSS_${year}.zip"
  
  # Download the zip file
  echo "Downloading BRFSS ${year} data..."
  wget -O "$zipfile" "$url"
  
  # Check if the download was successful
  if [ $? -eq 0 ]; then
    # Unzip the file into a folder named BRFSS_<year>
    echo "Unzipping BRFSS ${year} data..."
    unzip -o "$zipfile" -d "/projects/waller/spatial_kiosks/data/raw/brfss/ascii"

    # Remove the zip file after unzipping
    rm "$zipfile"
    
    echo "BRFSS ${year} data downloaded and unzipped."
  else
    echo "Failed to download BRFSS ${year} data."
  fi
done

# Verify that the files have been unzipped
echo "Download and extraction complete!"