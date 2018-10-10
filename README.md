
# Making Research Reproducible

\doublespacing
\clearpage


## Author

Philipp Homan <phoman1 at northwell dot edu>


## Getting Started

This repository contains all the data and analysis code to reproduce the
manuscript Making Research Reproducible. These instructions describe how to obtain a copy
of the project up and running on your local machine for reproducing the
analysis described in the manuscript. The repository contains a Makefile
which reflects the dependencies of the analysis; analysis, figures and
manuscript can be produced by simply typing 'make' from the Unix command
line.


### Prerequisites

All analyses were conducted with the R software 
R version 3.3.2 (2016-10-31). Mixed models were estimated
using the lme4 library, Python 2.7.13 and pysurfer (0.7) were also used. The full session info under R can be found at
the end of this file


## Installing

Clone the repository or download the zip file.


## Running the analysis

Change to the represearch directory and run 'make analysis'.


## Producing the figures

Change to the represearch directory and run 'make figures'. The figures can then
be found in output/figures.


## Producing the manuscript

Change to the represearch directory and run 'make manuscript'. The manuscript
will be in src/.


## Built With

Ubuntu 17.04 on emacs
25.1.1 and org-mode
9.1.9.


## Session info

    R version 3.3.2 (2016-10-31)
    Platform: x86_64-pc-linux-gnu (64-bit)
    Running under: Ubuntu 17.04
    
    locale:
     [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
     [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
     [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
     [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
     [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
    
    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

