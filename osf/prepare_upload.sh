#!/bin/bash

# Get the current date in the format of YYYY-MM-DD
current_date=$(date +%Y-%m-%d)

# Read the file line by line
while IFS=, read -r name url
do
  # Append the current_date to the name
  new_filename="to_upload/${name}-${current_date}.zip"

  # Download and rename the zip file
  wget -O "$new_filename" "$url"
done < repos.txt
