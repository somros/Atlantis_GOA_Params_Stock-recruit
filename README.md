# Stock-recruit relations of fish groups in Atlantis GOA

This code performs a series of calculations related to recruitment of fish species in Atlantis GOA, specifically to obtain the $\alpha$ and $\beta$ parameters for the Beverton-Holt stock recruit relation used in Atlantis GOA for fish. There are four main parts to the workflow:

1. The script `maturity_ogives.R` constructs maturity ogives based on stock assessment model output for Tier 3 species in the GOA. For other species, ogives are built symmetrically around age at 50% maturity.
2. The script `spr.R` calculates spawners per recruit based on maturity ogives and life-history. These will be used with R<sub>0</sub> (our $\alpha$ from assessments) to get SSB<sub>0</sub>, which will be used to obtain $\beta$. Note that SSB<sub>0</sub> is not equal to biomass in 1990 for the initial conditions.
3. The script `FSPB_calculator.R` converts maturity ogives (annual) to `FSPB_XXX` values for `biol.prm`.
4. For the $\beta$ parameter of the BH relationship, we extract steepness $h$ from global databases. This is in `h_and_R0.Rmd`.

See scripts for details. Also detail on stock-recruitment calculations is written up [here](https://docs.google.com/document/d/1PoBXgp-o9bzAr2H9njh-K5XT6a4kvhiheB7uB5eLH1M/edit#) (in prep. - access available upon request). 

