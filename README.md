<!---
This README uses Markdown syntax, though it is saved in .txt format. For details on Markdown syntax, see <https://daringfireball.net/projects/markdown/>.
--->

# Reference Information
Filename: README.md
Author: Winer Daniel Reyes-Corral
Date created: 2022-11-29
Date modified: 2023-01-16

# Dataset Version
Version: 1.0.0
Date: 2023-01-16
DOI: [https://doi.org/10.5061/dryad.h70rxwdnz](https://doi.org/10.5061/dryad.h70rxwdnz)

# Dataset Title:
"Phenotypic divergence of traits that mediate antagonistic and mutualistic interactions between island and continental populations of the tropical plant, Tribulus cistoides (Zygophyllaceae)"

# Dataset metadata

## Funding sources
- Canada Research Chairs
- Connaught Fund
- NSERC CREATE in Biodiversity Ecosystem Services and Sustainability (BESS)
- Natural Sciences and Engineering Research Council of Canada
- E.W.R. Steacie Memorial Fund

## Dates and locations
- Field data collected between 2015 - 2019 once a year
- Location: Galapagos Islands, Ecuador (see publication for more details).

# Data and File Overview

## Summary:
- Number of files: 4.
- File size: 1.48 MB
- File formats: .csv

## Main datasets:

### Tribulus_mericarp_data_clean.csv
- Description: This dataset includes the individual measurements of mericarp traits. This is the main dataset used for the mericarp analysis.
- Format: .csv
- Size: 1.08 MB
- Dimensions: 5085 rows x 31 columns

### Tribulus_flower_data_clean.csv
- Description: This dataset includes the indivdual information of flower traits. This is the main dataset for the flower analysis.
- Format: .csv
- Size: 204 KB
- Dimensions: 773 rows x 29 columns

## Additional datasets:
To further support our results we performed a series of additionaly analysis. Here we include two datasets that were used for these supporting analysis. (See Appendices in manuscript)
### mericarp_means.csv
- Description: This dataset estimates the mean mean of mericarps from locations grouped by the ID column.
- Format: .csv
- Size: 104 KB
- Dimensions: 316 rows x 45 columns
 
### mericarp_ind.csv
- Description: Our sampling efforts were focused on two locations which made our sample size umbalanced. We estimated the means grouped by the ID column for Florida and Galapagos, then used the individual measurements for the other locations. This dataset was used for the PCA of the mean analysis.
- Format: .csv
- Size: 100 KB
- Dimensions: 562 rows x 21 columns

# Variables:
Some of these variables are shared across datasets. Here we describe each varable in the same order that appears on each dataset. We decided to divide this section into **Grouping variables**, mainly factors that helped that allowed to make comparisons to our data. Then **Dataset specific variables**, corresponds to each particular dataset
## Grouping Variables:
**Shared variables with the mericarp and flower dataset.**
- ind_num: Number. Indvidual number. Unique ID for each mericarp.
- ID: Factor. Grouping variable that groups samples per herbarium voucher (when applicable) or field site (when field collected). See methods in the publication for more details.
- Herb_name: Factor. Grouping variable that identifies the different herbarium sources used in the dataset.
- Herbarium: Factor. Grouping variable with 2 categories, "Herbarium" and "Natural Population". This variable was used to test the effect of using herbarium samples together with field collected samples.
- Herb_number: Unique identifier. Voucher number for each herbarium collection.
- year_collected: Factor. Year when the sample was collected, either herbarium or field collected samples.
- continent: Factor. Grouping variable that groups data into continent location: "Africa", "North_Central_America", and "South America"
- mainland_island: Factor. Grouping variable that groups data by origin: "island" if they come from an archipelago or "continent" if they come from the mainland. (Variable used for model 1 in analysis. See methods for defitions of island used in the study)
- galapagos_other: Factor. Grouping variable with 2 categories that classifies Island samples into samples from the Galapagos Islands or Other island systems. NAs in this case represent continental samples. (Variable used for model 2 in analysis).
- island_group: Factor. Grouping variable with categories that correspond to specific island systems. NAs represent continental samples. (12 categories)
- galapagos_island: Factor. Grouping variable with categories that represent specific islands in Galapagos. (19 categories)
- finch_beak: Factor. Binomial category that represents the presence (1) or absence (0) or large beak Darwin's finches on islands in Galapagos. Large beak finches have and advantage to predate Tribulus mericarps compared to medium or small sized beak finches (See Study System section in the Methods). Large beak finches are _Geospiza magnirostris_ and _Geospiza conirostris_.
## Specific dataset variables

### Mericarp Dataset Variables:
- mericarp_num: Number. Unique identifier for mericarps grouped by ID.
- Linear mericarp measurements: Length (mm), width (mm), depth (mm), spine_length (mm), tip_distance (mm). See Methods for descriptions of how these measurements were taken. A spine length of 0, means that the mericarp in question don't present spines at all. NAs for spine length mean that the spine could not be measured, it was either incomplete or missing.
- spine_num: Factor, number of spines found in a mericarp (0 - 6). NAs for this variable mean that the spines were incomplete or missing.
- upper_spines: Factor. Binomial category that represents the presence (1) or absence (0) of upper mericarps. NAs for this variable is for mericarps were we could not determine if the absence of spines was natural or it was just missing or broken. (See methods for description of upper and lower mericarps)
- lower_spines: Factor. Binomial category that represents the presence (1) or absence (0) of lower spines. NAs for this variable is for mericarps were we could not determine if the absence of spines was natural or it was just missing or broken. 

### Flower Dataset Variables:
- flower_num: Number. Unique identifier for flowers grouped by ID.
- petal_length: Continous (mm). Linear measurements of flower size.
- province_state: Factor. Provinve or state were sample was collected.
- scale: Measurement of the photographed scale to calibrate petal length.
- collection.date: Estimated date of collection in voucher.
- collector: Person who collected the voucher.
- collection.no: Voucher number
- Coordinate.notes: These are comments about the latitute and longitude measurements.
- Measured_note: Comments from the person measuring petal length.

## Notes and Bioclimate variables
**These are shared between the mericarp and flower datasets**
- country: Factor, categorical variable that groups the data per specfici country.
- location: Notes were the samples were specficially located. Based on herbarium notes or field annotations
- notes: additional comments
- latitude and longitude: Coordinates were the samples were collected. It uses the decimal degrees (DD).
- Location_notes: These are comments about the latitute and longitude measurements.
- Temp (Bio_1): Biocliamtic variable it represents the Annual Mean Temperature. BIO1 in https://worldclim.org/data/bioclim.html (See Methods for the source of this data). 
- Temp_S (Bio_4): Temperature Seasonality (standard deviation x 100). BIO4 in https://worldclim.org/data/bioclim.html
- Prec (Bio_12): Annual Precipitation. BIO12 in https://worldclim.org/data/bioclim.html
- varP (Bio_15): Precipitation seasonality (Coefficient of Variation). BIO15 in https://worldclim.org/data/bioclim.html.

# GitHub configuration
The scripts of the analysis are in a github repository: https://github.com/Winer-DanielR/Tribulus-mericarp-morphology

The repository contains folders for data and scripts. The data folder is the same datasets described above, with additional datasets per trait.
The scripts folder contains all the scripts used for analysis. Each analysis has the similar structure but it has been divided per branch.

## Scripts
The scripts are numbered and should be executed in order. Some scripts were aditional analysis that were not included in the paper. 
Each script contains a description of the analysis and objective for that script.

## Branches
Each branch corresponds to a particualr analysis. 
- The master branch is the main analysis of the paper.
- The Galapagos_Mainland_Analysis is a supplemental analysis were we compare Galapagos samples with the continent
- The Mainland_Other_Analysis is a supplemental analysis were we compare Other Islands systems with the continent
- Mean_Galapagos_Florida_Analysis is a supplemental analysis were we took the mean of Galapagos and Florida samples to account for unbalanced sampling (See methods of the paper for details)
- Removal_Africa_Analysis is a supplemental analysis were we removed samples from island systems that account for individual plants.


## Data
The data folder is divided into two subfolders, _Raw_ and _Processed_. Raw data is all the data collected from the field. The processed folder contained any changes to the data, data wrangling or corrections (NA cleaning or summary stats) that may be used for analysis. That is the ready-to-go datasets. 

## Output
Contains any results summary, usually PDFs for the results, figures, or JASP files (for quick data exploration).

## Scripts
Contains all the R scripts of the project. These scripts are numbered and are organized into sections. Each script has its description in the title and a brief introduction of what the code does. Here you will also find the R markdown file used to produce the outputs.
