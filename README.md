READ ME FILE

Hello, this is a simple README file for the biol_835AQ project.

Overview
This database contains dietary observation records organized in a wide-format table. Each row represents one observation unit identified by a unique specimen or sample code, and the columns record the sampling context, the focal fish species, and the quantified occurrence of prey or ingested material categories.


The attached raw_data.csv file contains 729 rows and 56 columns. The first three columns are metadata fields (sampling_area, unique_identifier, and species_name), and the remaining 53 columns correspond to prey, organic material, or non-prey categories recorded in each observation.


File structure
The table is structured as follows:

sampling_area: Sampling stratum or collection category, with values such as PI, PII, PV, and Perene.


unique_identifier: A unique code for each record or specimen, for example CRUCH#16A1P215112601.


species_name: Serrapinnus heterodon, Serrapinnus piaba, Compsura heterura, or Phenacogaster calverti.

Remaining columns: Quantitative entries for individual diet-item categories such as daphniidae, chironomidae, diptera, filamentous_algae, seed, and several fragment or substrate classes.


Biological and non-biological categories
Most columns after species_name represent potential prey or ingested materials. These include arthropods, zooplankton, algae, eggs, plant material, and fragments, but the table also includes non-prey or ambiguous material categories such as clay, substrate, shiny_frag_rock, translucid_frag_rock, black_frag_rock, and aglutinaded_rock_frag.


Because the raw dataset contains both biological prey items and non-prey material, downstream analysis should use a grouping file when the goal is to summarize true prey composition. This will prevent rocks, sediment, and other incidental material from being interpreted as prey.


Taxonomic coverage
The species_name column includes four focal fish taxa in the attached file: Serrapinnus heterodon, Serrapinnus piaba, Compsura heterura, and Phenacogaster calverti.


Data values
The prey-category columns are stored as numeric values, with many zeros indicating absence of a given item in a record. Some rows contain NA values across all diet-item columns, which likely indicate missing dietary information rather than confirmed absence.


For this reason, analyses should distinguish among three situations: observed absence (0), measured positive values (>0), and missing data (NA). Treating NA as zero would likely bias summaries of diet composition.

PS: I know that every script kinda does the cleaning part again, this is a feature not a defect, I just wanted to make sure that the data is clean and ready for analysis, and that the scripts are self-contained and can be run independently, with the goal of exporting it as a shiny app in the future. I also wanted to make sure that the data is clean and ready for analysis, and that the scripts are self-contained and can be run independently, with the goal of exporting it as a shiny app at the end. 

Thnak you.
