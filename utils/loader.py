# Class for loading images from a directory

import os
import re

class Loader:
    
    def __init__(self):
        self.base_path = None
        self.directories = [d for d in os.listdir(self.base_path) if os.path.isdir(os.path.join(self.base_path, d))]
                
    def extract_number(self, filename):
        """
        Extracts the first sequence of digits from a given filename.
        """
        match = re.search(r"\d+", filename)
        return int(match.group()) if match else 0
    
    def fetch_data(self, study_path, sample_idx):
        """
        This function is the bread and butter of the sample class. It pulls your data according to the desired study and the desired sample.
        """
        base_path = study_path + sample_idx
        # ex. D:\copyRaw\Rabbit_AGuIX or D:\copyRaw\Phantom_XeGd
        directories = [d for d in os.listdir(base_path) if os.path.isdir(os.path.join(base_path, d))]
        #directories.sort()
        directories.sort(key=self.extract_number)

        print(f"Available directories:")
        for i, directory in enumerate(directories):
            print(f"{i}: {directory}")

        #create a timepoint for each unique study in the base path
        for t, timepoint  in enumerate(directories):
            self.add_timepoint(base_path, directories, t)