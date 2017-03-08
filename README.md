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

## Tutorial (under development)

### Loading data

The `metaforGUI` package requires you to load your data into R to conduct the meta-analysis. This can be done manually in R, and any variables you define will show up in the variable browser in `metaforGUI`. The package also allows you to load a dataset in CSV format (Comma-Separated Values) that you can prepare in a spreadsheet software.

There are some requirements for this to work:
- File must be saved in CSV format (you can "save as..." CSV from Microsoft(r) Excel or virtually any other modern spreadsheet)
- First row must contain variable names
- Each other row should contain a study or a contrast to be entered into the meta-analysis

You may find and example dataset CSV-file in the package folder ("example_dataset.csv") that you can use as a template.

Note: different Country and Language setting define the CSV files differently. The package will attempt to load your dataset according to your computer's regional settings. In some countries the decimal place is defined with ".", in others with "," [more details on this to be added].


## 
