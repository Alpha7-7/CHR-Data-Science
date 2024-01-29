import os
import pandas as pd
import zipfile

# Replace 'your_folder_path' with the path to the folder containing the zipped files
folder_path = 'Data'

# List all files in the folder
files = os.listdir(folder_path)

# Extract zipped files
for file in files:
    file_path = os.path.join(folder_path, file)
    
    # Check if the file is a zip file
    if file.endswith('.zip'):
        # Create a folder to extract the contents into (replace 'extracted' with your desired folder name)
        extract_folder = os.path.join(folder_path, 'extracted')
        os.makedirs(extract_folder, exist_ok=True)
        
        # Extract the contents of the zip file
        with zipfile.ZipFile(file_path, 'r') as zip_ref:
            zip_ref.extractall(extract_folder)
            
        print(f"Extracted contents from {file} to {extract_folder}")
