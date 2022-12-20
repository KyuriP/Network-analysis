===============================================================================
## README FILE FOR THE FINAL INDIVIDUAL REPORT
## COURSE: NETWORK ANALYSIS 2022
## AUTHOR: KYURI PARK
## STUDENT ID: 12183881
## THIS README PROVIDES THE DESCRIPTION ON THE STRUCTURE OF "CriticalReflection_Park_12183881.zip" FILE.
===============================================================================

===============================================================================
## CONTENTS
===============================================================================

The folder structure is as follows:

```
├── CriticalReflection_Park_12183881.Rmd
├── CriticalReflection_Park_12183881.pdf
├── Individual_report.Rproj
├── README_KP
├── apa.csl
├── code
│   ├── R
│   │   ├── CCD_fnc.R
│   │   ├── data_generating_fnc.R
│   │   ├── dsep_fnc.R
│   │   ├── equivset_fnc.R
│   │   ├── eval_metric_fnc.R
│   │   ├── plot_fnc.R
│   │   ├── searchAM_KP_fnc.R
│   │   └── variation_fnc.R
│   ├── empircal_example.R
│   ├── simulation.R
│   └── variation_DCG.R
├── data
│   ├── McNally.csv
│   ├── equiv4p.RData
│   ├── equiv4pL2.RData
│   ├── equiv4pL3.RData
│   ├── equiv4p_high.RData
│   ├── equiv5p.RData
│   ├── equiv5p_high.RData
│   ├── equiv6p.RData
│   └── equiv6p_high.RData
├── img
│   ├── CCDsummary.png
│   ├── deg_variation.pdf
│   ├── density_variation.pdf
│   └── logouva.png
└── references.bib
```
===============================================================================
## REPORT
- `CriticalReflection_Park_12183881.pdf` is the main report file.
- `CriticalReflection_Park_12183881.Rmd` renders the report.
===============================================================================
## CODE
- `code` folder contains all the code used to create the report.
- Three R scripts containing the code for creating figures in the report.
  - `simulation.R`: code to simulate the models.
  - `variation_DCG.R`: code to create the density plots (Figure 2) and degree centrality plots (Figure 3) in the report.
  - `empircal_example.R`: code to create the PAG (Figure 4) and GGM (Figure 5) on empirical data.
- `R` folder contains all necessary functions to run all the analysis for the report.
===============================================================================
## DATA
- All equivalence classes of DCGs from each simulated model saved in `.Rdata`.
- One example empirical data `McNally.csv`.
===============================================================================
## INCIDENTALS
- `img`: img folder contains all necessary image files to render the report.
- `references.bib`: BibTex for references cited in the report.
- `apa.csl`: csl file to format the references as per APA guidelines.
- `README_KP`: current file describing the content of the zip file.
===============================================================================
## RECOMMENDATION
1. Open the `Individual_report.Rproj`.
2. Find the `CriticalReflection_Park_12183881.pdf` report file.
3. Check the `code` folder for any code used to perform the analysis presented in the report.

In case of any questions, please contact Kyuri Park (kyuri.park@student.uva.nl).

===============================================================================

