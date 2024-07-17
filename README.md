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
6. Create a folder on your computer for data.  

7. Access our datasets [here]() and download them to your data folder.   
      1. For some datasets, our team did prep work in Excel. This allowed us to webscrape (automatically convert data from websites into manipulateable datasets) and quickly reformat data. We then renamed and cleaned the data in .csvs files. You can find these cleaned csvs files with the rest of our datasets in our data folder.    
      2. You may need a census api key (request one [here](https://api.census.gov/data/key_signup.html)) to access the census data. Then, learn more about using your api key with the tidycensus package [here](https://walker-data.com/census-r/an-introduction-to-tidycensus.html).  

### Project Organization 
  - We created a dedicated folder for our project. Within this folder, we organize subfolders for data (data/), scripts (scripts/),and documentation.  
  - We created a separate environment using R to manage dependencies and ensure reproducibility.
  - The team wrote modular scripts/functions that performed specific tasks (data cleaning, analysis, visualization, etc.). We added comments within our code to explain logic, functions, and complex operations. 
  - We ensured proper handling of data (importing, cleaning, transformation) within scripts. Additionally, we stored raw data in data/raw/ and cleaned data in data/processed/.
  - Visualizations: Use libraries like ggplot2, geom_bar,and geom_histrogram (R) for creating plots. Save plots in outputs/ folder.
  - Add New Data

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
