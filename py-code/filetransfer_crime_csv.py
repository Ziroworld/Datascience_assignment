import os
import shutil

source_dir = r"/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/0assignment/data_2024-07-9/crime data"
destination_dir = r"/home/rohan-manandhar/College Every Semester Data/Semester 4/Data_Science/0assignment/data_2024-07-9/merged_crime_data"

if not os.path.exists(destination_dir):
    os.makedirs(destination_dir)

for root, dirs, files in os.walk(source_dir):
    for file in files:
        if file.endswith(".csv"):
            source_file = os.path.join(root, file)
            destination_file = os.path.join(destination_dir, file)
            shutil.move(source_file, destination_file)
            print(f"Moved: {source_file} to {destination_file}")

print("All CSV files have been moved successfully.")
