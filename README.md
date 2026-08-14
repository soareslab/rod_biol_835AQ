# READ ME FILE

## This README file provides documentation for the project biol_835AQ, which focuses instructions of how to use the tool develped during this course.


### Project Overview
Guidelines, to be able to use this tool, your dataset should de structured in a way that the first 6 columns are the following:
1) sampling_area
2) season
3) unique_id
4) sex
5) species_name
6 and on) diet_items represented in volume

#### Example of a dataset structure: 

| sampling_area | season | unique_id | sex | species_name | diet_item_1 | diet_item_2 |
|---------------|--------|-----------|-----|--------------|-------------|-------------|
| Area_1 | Spring | 001 | M | Geophagus_brasiliensis | 10 | 5 |
| Area_1 | Spring | 002 | F | Astyanax_fasciatus | 8 | 12 |
| Area_1 | Spring | 003 | M | Prochilodus_lineatus | 15 | 7 |
| Area_2 | Summer | 004 | M | Prochilodus_lineatus | 15 | 7 |
| Area_2 | Summer | 005 | F | Geophagus_brasiliensis | 6 | 9 |
| Area_2 | Summer | 006 | F | Geophagus_brasiliensis | 6 | 9 |
| Area_3 | Fall | 007 | F | Prochilodus_lineatus | 9 | 11 |
| Area_3 | Fall | 008 | M | Astyanax_fasciatus | 12 | 4 |
| Area_3 | Fall | 009 | F | Prochilodus_lineatus | 9 | 11 |

### Logic of the script:
Four functions plus one wrapper:

validate_diet_data() returns a validation report, not just pass/fail.

clean_diet_data() returns cleaned data plus a cleaning log.

fo_summary() performs only the analysis.

run_diet_pipeline() orchestrates the steps and returns everything in one object.

Optionally, print_diet_report() or summarize_diet_report() gives a human-readable summary.



The develop of this script and functions comes from the need of easing the process of fish diet data analysis.
When working with fish diet data, researchers often face challenges in organizing and analyzing the information effectively. This project aims to provide a streamlined approach to handling such data, making the analysis more efficient and accessible.
We come from the understanding that scientists often deal with large datasets, and manually processing this information can be time-consuming and error-prone. By automating certain aspects of the analysis, we hope to save researchers valuable time and reduce the likelihood of mistakes.
Discovering patterns and trends in fish diet data can lead to valuable insights into ecological dynamics, species interactions, and environmental impacts. Our tool is designed to facilitate this process, enabling researchers to extract meaningful information from their datasets with ease.
The skill of programming and data analysis is becoming increasingly important in the field of biology. By providing a user-friendly tool, we aim to empower researchers with the ability to leverage computational techniques for their studies, ultimately advancing our understanding of aquatic ecosystems.

The ultimate goal is to develop a package able to be a one-stop solution for fish diet data analysis, providing a comprehensive set of functions and tools that can be easily integrated into existing workflows. We envision this package as a valuable resource for researchers in the field, enabling them to conduct their analyses more efficiently and effectively.
Trying to be widely used, we are working on making the package available on CRAN, which will allow users to easily install and access the tool. Additionally, we are committed to providing thorough documentation and support to ensure that users can make the most of the package's capabilities.
One key aspect of developing a tool such as this one is to have able to deal with different types of data formats and structures. We recognize that researchers may encounter diverse datasets, and our package is designed to accommodate various input formats, ensuring flexibility and usability across different research contexts. But also being grounded in not overwhelming the user with too many options, we are working on providing a set of default parameters and settings that can be easily adjusted based on the specific needs of the analysis.
A tool that has too many uses is not likely a good tool, remaining specific enough and still being able to solve the problem at hand is a challenge we are working on. We are committed to striking the right balance between functionality and simplicity, ensuring that our package remains focused on its core purpose while still offering valuable features for users.
