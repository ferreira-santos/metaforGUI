# metaforGUI
## Description
The `metaforGUI` R package provides a Graphical User Interface (GUI) for the excellent [`metafor`](http://www.metafor-project.org/) meta-analysis R package by [Wolfgang Viechtbauer](http://www.wvbauer.com/), supporting basic functionality to run a simple meta-analysis (load dataset, select variables, run analysis). It was primarily developed as a teaching resource for teaching fundamental meta-analytic principles and procedures to students that are not necessarily proficient with R (while at the same time introducing them to the R environment without first having to undertake a steep learning curve).

## Status
In early development (working, but still under testing).

Development version may be installed within R using the `devtools` R package. Use the following code:

```R
install.packages("devtools") # if needed
library("devtools")
install_github("ferreira-santos/metaforGUI")
```

Some manual installation steps may be required (namely, installing RGtk2).

## Tutorial

To run `metaforGUI` from the R command line type the following code and then look for the `metaforGUI` window:
```R
library(metaforGUI)
metaforGUI()
```

### Loading data

The `metaforGUI` package requires you to load your data into R to conduct the meta-analysis. This can be done manually in R, and any variables you define will show up in the variable browser in `metaforGUI`. The package also allows you to load a dataset in CSV format (Comma-Separated Values) that you can prepare in a spreadsheet software.

There are some requirements for this to work:
- File must be saved in CSV format (you can "save as..." CSV from virtually any other modern spreadsheet).
- First row must contain variable names (one variable per column).
- Each other row should contain a study or a contrast to be entered into the meta-analysis.
- A typical dataset will contain a column for each of the following variables: study names (optional), effect sizes, effect size variances or standard errors. You may find and example dataset CSV-file in the package folder ("example_dataset.csv") that you can use as a template.

Note: different Country and Language settings define the CSV files differently. The package will attempt to load your dataset according to your computer's regional/locale settings.

### Defining variables for meta-analysis

After loading your data, you need to specify which variables contain study names (an optional step, as the meta-analysis will run without this information), effect sizes, and effect size variances/SEs. All these variables should have the same number of data points (i.e., each effect size requires a corresponding variance or SE) and they must be correctly ordered. The package will detect some problems with the data (like differences in the number of data points) but will try to run the analysis on any data you provide it with, so if there are errors in the data, the results will be produced nonetheless. This is sometimes called the _garbage-in-garbage-out_ principle that generally applies to any statistical analysis.

### Defining output folder and results

The output of metaforGUI will be one or more files summarizing the results of the meta-analysis. These files will be written on the current Working Directory (WD) defined in R. You may change the WD manually in R or via the "Change Folder" button on the GUI.

The main results file will be a simple text file "metaforGUI_Output.txt" containing (1) a description of the data entered itnto the analysis, (2) the results of the meta-analysis itself and of (3) Egger's regression test for publication bias, as well as (4) a list of any additional files produced, and (5) information on how to cite the software used (`R`, `metafor`, and `metaforGUI`).

The additional files that may be produced are PDFs of the meta-analysis forest and funnel plots, and a .RData file with the resulting `metafor` data structure. This data file can be loaded into R by using the `load()` function.

Currently, metaforGUI will ovewrite previous results files if they exist in the Output Folder. Please make sure to rename files you wish to save before running another analysis or copying them to another folder.

## About

The `metaforGUI` package was developed by Fernando Ferreira-Santos as an Open Source software ([MIT License](https://github.com/ferreira-santos/metaforGUI/blob/master/LICENSE)) intended for scientific research and teaching. Citation information can be found in the outputs, or viewed in R by typing `citation(package="metaforGUI")`. Any issues or bugs that arise while using the software may be reported [here](https://github.com/ferreira-santos/metaforGUI/issues).
