This folder contains example R code pipeline showing how the Shorebird Collective cleans and harmonizes shorebird tracking datasets. This code is referenced in a preprint of a paper describing our work and is currently available here: https://www.biorxiv.org/content/10.1101/2024.01.30.576574v3. There are three folders of code: "code" contains the main data processing pipeline, "figure_code" contains coded need to create some figures used in the pipeline, and "functions" contains code for custom functions used in the pipeline.

The code was built using R Version 4.4.0 and following the Movebank Attribute Dictionary (https://www.movebank.org/cms/movebank-content/movebank-attribute-dictionary) as of 2024-06-01. 

Sensitive information and study-specific filtering steps are not included in this example for privacy reasons. Some examples include single tags deployed on multiple birds but not separated in Movebank, tags tested prior or after deployment on a bird, data stored in the same Movebank study with different ownership and terms of use, etc. 

Code sharing remains relatively uncommon in Ecology (Maitner et al. 2024, DOI: 10.1002/ece3.70030). Please cite our paper and this Github Repository to credit our work if you use this code.
