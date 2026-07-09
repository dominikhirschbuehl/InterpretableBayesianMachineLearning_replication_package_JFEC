The folder 00\_JFEC\_\[AcceptedPaper] contain codes and data to replicate the results, figures and table of the paper "Interpretable Bayesian machine learning for assessing the effects of climate news shocks on firm-level returns"

Note: LSEG Datastream data is propietary and cannot be shared.

\---



Creation date: July 9th, 2026



Contact information for the replication package:



Dominik Hirschbuehl (Dominik.Hirschbuehl@ec.europa.eu)



Luca Tiozzo Pezzoli (Luca.Tiozzo-Pezzoli@uib.es)



Luca Barbaglia (luca.barbaglia@ec.europa.eu)





## \---> Folder "Data/"

> "Data/INPUT/"

> "DNA\_sentiment\_US.csv": Raw news dataset from Dow Jones Factiva. For each entry, it includes the date, media source, topic, full text/chunks, and sentiment scores calculated following the procedure in Barbaglia et al. (2023).

	> "USFiveFactors\_2022\_incl\_ins.csv": Raw financial data retrieved from LSEG Datastream, paired with the Fama-French 5 factors and momentum factor, CO2 emission data and net sales from LSEG Datastream. 


> "Data/OUTPUT/": 

> "Financial\_Data\_2015D-2022D.RData" produced after having runned the 0\_CLEANING.R code in the "Code" folder.


\---

## \---> Folder "Code"

> "FUNCTIONS.R": Core script containing the BART functions and priors utilized for model estimation. Detailed methodology can be found on pages 3–10 of the paper. 

> "0\_CLEANING.R" 

> Cleans "USFiveFactors\_2022\_incl\_ins.csv" and map firms to NAICS sectors 
	> Constructs climate news shock indicators from DNA\_sentiment\_US.csv
	> Generate effect modifiers
	> Saves the final compiled data as a structured R list (DATA) containing: Excess returns, Fama-French factors + Momentum, Effect modifiers, and Text indices. Outputs directly to "Data/OUTPUT/"


> "1\_ESTIMATION.R": Loads the DATA list from "Data/OUTPUT/" and sources "FUNCTIONS.R". Winsorizes and demeans stock returns, then runs two distinct BART estimation blocks: one considering the climate change index and another regarding natural disaster shocks index. Saves output as "RESULTS\_CC\_ND\_20000.RData" to the "Estimations/" directory.

> "2\_GRAPHS\_\&\_TABLES.R": Processes "RESULTS\_CC\_ND\_20000.RData" alongside the text data and cleaned financial data to generate the main text figures and tables.
>>>>>>\[HERE WE MISS THE TABLE AND FIGURE FOR THE ASSET ALLOCATION SECTION]<<<<<<

\---

## \---> Folder "Estimations"

Stores the final model outputs. Contains "RESULTS\_CC\_ND\_20000.RData" once "1\_ESTIMATION.R" completes successfully.

\---

## \---> Folder "Graphs"

Target export directory for all figures produced by running "2\_GRAPHS\_\&\_TABLES.R".



\---

## \---> HOW TO REPLICATE

At the begining you should have in your main directory the two folder "Code" and "Data" with the main codes and the raw database. Whe you run "0\_CLEANING.R" the folders "Estimations" and "Graphs" are automatically generated.

Run:

(1) "0\_CLEANING.R"
(2) "1\_ESTIMATIONS.R"
(3) "2\_GRAPHS\_\&\_TABLES.R"

