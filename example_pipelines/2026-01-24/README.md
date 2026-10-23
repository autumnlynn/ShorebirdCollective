This folder contains example R code pipeline showing how the Shorebird Collective cleans and harmonizes shorebird tracking datasets and underlies a data version: 2026-01-24. This pipeline is an improvement on the code used in the published paper available open access here: https://doi.org/10.1111/cobi.70194. Main improvements to the pipeline include reducing script length into functions.  

There are three folders of code: "code" contains the main data processing pipeline and "functions" contains code for custom functions used in the pipeline.

The code was built using R Version 4.5.2 and following the Movebank Attribute Dictionary (https://www.movebank.org/cms/movebank-content/movebank-attribute-dictionary) as of 2026-01-24.

Sensitive information and study-specific filtering steps are not included in this example for privacy reasons. Some examples include single tags deployed on multiple birds but not separated in Movebank, tags tested prior or after deployment on a bird, data stored in the same Movebank study with different ownership and terms of use, etc.

Code sharing remains relatively uncommon in Ecology (Maitner et al. 2024, DOI: 10.1002/ece3.70030). Please cite our paper and this Github Repository (see CITATION.cff) to credit our work if you use this code.
