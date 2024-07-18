# No Vote, No Voice: TN Disenfranchisement 2024

### How to Install and Run the Project
1. Make sure you have git installed (Learn how to do that [here](https://github.com/git-guides/install-git)).  
2. Install R Studio (Learn how to do that [here](https://posit.co/download/rstudio-desktop/)).  
3. Open R Studio.  

4. Clone our GitHub repository!  
      1. Go to the Project tab in the upper right corner RStudio.  
      2. Click New Project -> Version Control -> Git  
      3. Enter the URL of our repository (https://github.com/sewaneedata/TN-disenfranchisement-2024/tree/main) and specify the directory where you want to clone the repository.  
      4. Click Create Project!  

5. Install the following dependencies using the install.packages() function. (You can also just run our "packages" script.)  
    - sf
    - tmap
    - tidycensus
    - tidyverse
    - ggthemes
    - tufte
    - remotes
    - sjPlot
    - rnaturalearthhires

### File Descriptions
- `.gitignore` - This file specifies which files should not be pushed to GitHub.
- `Poster graphs.R` - This script contains the findings we included in our DataFest Poster and DataFest slides.
- `TN-disenfranchisement-2024.Rproj` - This file stores the project settings.
- `exploratory.R` - Run this script first. This script merges and scopes out the data we found.
- `install_required_packages.R` - This file installs all of the necessary packages to run all of our other scripts.
- `visualizations.R` - This script contains our visualizations of the data. 

### Credits
##### Project Partners
- [Sewanee DataLab,](https://new.sewanee.edu/sewanee-datalab/) a Data Science for Social Justice Summer Fellowship Program
- [Free Hearts,](https://freeheartsorg.com/) a Tennessee state-wide organization founded by formerly incarcerated women that provides support, education, and advocacy for families impacted by incarceration
- Free the Vote Coalition, a a state-wide grassroots network anchored by Free Hearts
- [Dr. Sekou Franklin,](https://sekoufranklin.com/) a Professor in the Department of Political Science at Middle Tennessee State University (MTSU)

##### Project Contributors
- [Joseph Thomas III,](https://github.com/JosephDataN) Jackson State University C'26
- [Kayla Ahrndt,](https://github.com/kayla-ahrndt/) Belmont University C'25
- [Khalilah Karriem,](https://github.com/kkarriemk1234) Jackson State University C'26
- [Santana Etchison,](https://github.com/santanaetch) The University of the South C'27
- [Adri Silva,](https://github.com/adri-elle-silva) The University of the South C'24

### Notes
1. All the required data is in the data folder.
2. For some datasets, our team did prep work in Excel. This allowed us to webscrape (automatically convert data from websites into manipulateable datasets) and quickly reformat data. We then renamed and cleaned the data in .csvs files. You can find these cleaned csvs files with the rest of our datasets in our data folder.    
3. You may need a census api key (request one [here](https://api.census.gov/data/key_signup.html)) to access the census data. Then, learn more about using your api key with the tidycensus package [here](https://walker-data.com/census-r/an-introduction-to-tidycensus.html).  

