---

#### README FILE FOR THE FINAL PROJECT
#### COURSE: NETWORK ANALYSIS 2022
#### AUTHOR: KYURI PARK & SHANNON DICKSON

#### THIS README PROVIDES THE DESCRIPTION ON THE STRUCTURE OF "FinalProject_Group9.zip" FILE.

---
## INTRODUCTION

Use this as a guide for understanding the folder structure within our project.


## CONTENTS


```
├── NA_finalproject.Rproj
├── R
│   ├── CCD_fnc.R
│   ├── data_generating_fnc.R
│   ├── dsep_fnc.R
│   ├── equivset_fnc.R
│   ├── eval_metric_fnc.R
│   ├── plot_fnc.R
│   ├── searchAM_KP_fnc.R
│   └── simulation_study.R
├── data
│   ├── equiv4p.RData
│   ├── equiv4p_high.RData
│   ├── equiv5p.RData
│   ├── equiv5p_high.RData
│   ├── equiv6p.RData
│   └── equiv6p_high.RData
├── poster_group9.pdf
├── supplementary_material
│   ├── apa.csl
│   ├── img
│   │   ├── CCDsummary.png
│   │   └── truemodels.png
│   ├── references.bib
│   ├── rsconnect
│   │   └── documents
│   │       └── supplementary.Rmd
│   │           └── rpubs.com
│   │               └── rpubs
│   │                   ├── Document.dcf
│   │                   └── Publish Document.dcf
│   ├── style.css
│   ├── supplementary.Rmd
│   └── supplementary.html
└── README.md
```



## DATA


In the `data` folder you can find the equivalent sets of directed cyclic graphs (DCG) for each of the simulation conditions saved as `Rdata` format. 
This is done in order to save the potentially long running time of the provided code (as computing the equivalent sets of DCG using `semiequiv_dcg` function takes a while).



## CODE


In the `R` folder you can find the following:

 - `simulation_study.R`, which contains all of the code you need to reproduce our simulation study.

 - Remaining `.R` files are supporting functions required to run `simulation_study.R`.



## SUPPLEMENTARY MATERIALS


In the `supplementary_materials` folder, you can find a complete overview of our study. 

- `supplementary.html` presents the detailed descriptions on our study, including the background, analytic strategies, results, and conclusion. This is intended to accompany our poster.

- `supplementary.Rmd` is the markdown document that produces the `.html`.



## POSTER


The file `poster_group9.pdf` is the poster for our project. 



## RECOMMENDATION
We recommend first checking our poster along with the supplementary material.  
For those who are interested in reproducing the analyses, check `simulation_study.R` and run the code step by step as described. Make sure that the working directory is correctly set (you can easily open the `.Rproj` and access all the files, which automatically ensures to have the correct working directory).

---
In case of any questions, please contact us (kyuri.park@student.uva.nl).

