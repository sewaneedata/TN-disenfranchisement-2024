# No Vote, No Voice: TN Disenfranchisement 2024

### 1. How to Install and Run the Project
1. Make sure you have git installed (Learn how to do that [here](https://github.com/git-guides/install-git)).
2. Clone the repository (Learn how to do that [here](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository)).
3. Install R Studio (Learn how to do that [here](https://posit.co/download/rstudio-desktop/)). 
4. Open R Studio.
5. Install these dependencies using package managers such as install.packages().
    - sf
    - rnaturalearth
    - remotes
    - rnaturalearthhires
    - rnaturalearthdata
    - tmap
    - osmdata
    - tidycensus
    - dplyr
    - readr
    - tidyr
    - ggplot2
    - sjPlot

### 3. How to Use the Project 

### 4. Credits
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
Project Organization:  
  - Folder Structure: Created a dedicated folder for our project. Within this folder, we organize subfolders for data (data/), scripts (scripts/),and documentation.  
  - Environment Setup: Created a separate environment using R to manage dependencies and ensure reproducibility.
  - Script Organization: The team wrote modular scripts/functions that performed specific tasks (data cleaning, analysis, visualization, etc.). Add comments within our code to explain logic, functions, and complex operations. Using R to comment and document functions.
  - Data Handling: Ensured proper handling of data (importing, cleaning, transformation) within scripts. Store raw data in data/raw/ and cleaned data in data/processed/.
  - Visualizations: Use libraries like ggplot2, geom_bar,and geom_histrogram (R) for creating plots. Save plots in outputs/ folder.
  - Add New Data