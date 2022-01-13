# Early life adversity and structural plasticity

Version 0.1.0

Structural changes of early life adversity in rodents: a systematic review and meta-analysis. 
For information about this project, see https://osf.io/9gru2/

To reproduce the results and figures for the analysis, you need to run the file src/analysis.rmd . This generates and HTML file will all information required. For this file to run, you need: 
- to install dependencies. Most dependencies will install automatically, with exception of 1) knitr, 2) metaforest and 3) caret. All these are on CRAN, so you can simply install them as "install.packges('name_package')
- to copy the data in the correct folders. Specifically, you should have two data files (data/processed/data_for_analysis.RDS and data/temp/structural_plasticity_complete.RDS). These files are the RDS counterpart of the .csv files available at our depository (https://osf.io/9gru2/)


To run the risk of bias assessment, you need to:
- run src/rob_df.R
- run src/figure_rob.R


If you encounter any problem, feel free to get in touch!


## Project organization

```
.
├── .gitignore
├── CITATION.md
├── LICENSE.md
├── README.md
├── requirements.txt
├── bin                <- Compiled and external code, ignored by git (PG)
│   └── external       <- Any external source code, ignored by git (RO)
├── config             <- Configuration files (HW)
├── data               <- All project data, ignored by git
│   ├── processed      <- The final, canonical data sets for modeling. (PG)
│   ├── raw            <- The original, immutable data dump. (RO)
│   └── temp           <- Intermediate data that has been transformed. (PG)
├── docs               <- Documentation notebook for users (HW)
│   ├── manuscript     <- Manuscript source, e.g., LaTeX, Markdown, etc. (HW)
│   └── reports        <- Other project reports and notebooks (e.g. Jupyter, .Rmd) (HW)
├── results
│   ├── figures        <- Figures for the manuscript or reports (PG)
│   └── output         <- Other output for the manuscript or reports (PG)
└── src                <- Source code for this project (HW)

```


## License

This project is licensed under the terms of the [MIT License](/LICENSE.md)

## Citation

Please [cite this project as described here](/CITATION.md).
