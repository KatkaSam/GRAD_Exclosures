# GRAD_Exclosures
Exclosures from along Mt. Wilhelm elevational gradient

#----------------------------------------------------------#
# A. Dataset in inputs information  -----
#----------------------------------------------------------#

FOR ALL DATASETS
There are 2 experiments

VAC = the main one, conducted at 8 study sites, with 4 treatments on each of them except 3200 and 3700 m
Tretments in VAC are - CN1 = control, ANT = ants excluded, VER = vertebrates included, ALL = vertebrates and ants excluded
The experiment VAC took 2x 6 months, and it was set and then collected after wet season (A_wet) and then after dry season (B_dry)

BBC experiment was conducted on 4 sites, with 3 treatments at each of them
Treatments are  CN2 = control, BIR = birds excluded, BAT = bats excluded

SampleID can be used as sample identifier matcchable across the datasets
Plant_name should be use wherever the identity of plant species is needed.
As each sapling was surveyed twice in the VAC experiment, each sapling has its "Code" and the specific sampling event is named "SampleID"

FOR INDIVIDUAL DATASETS
Arthropod_induviduals_20210213 
-> consists of data from all individual insects as they were collected from the complete experiment. Each line is an individual. 
27 samples were empty samples, with no insect in them, these were excluded form the dataset and notes about them are in "Empty samples"
In total, there was 32314 individuals observed. 
This dataset is used for the analyses of body sizes

Arthropod_abundances_20210313
Is the summary (pivot table) from the arthropods individuals, including also a column with the abundances of all arthropods per m2 of foliage. 
This column (TotalAbundnace) is to be used in the main analyses of the effect of the treatment on all arthropods.
Similar columns would have to be calculated if interest in the effect on other guilds.

Herbivory_TotalLeafArea_20210314
Includes the data for the analysis of herbivorous damage - "Herbivory"
And includes the column where total leaf area of the sapling is noted, and should be used everytime we need to calcualte abundances of arthropods per m2 
(i.e. this is the leaf area from which the arthropdos were collected)
