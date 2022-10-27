# Stock-recruit relations of fish groups in Atlantis GOA

This code performs a series of calculations related to recruitment of fish species in Atlantis GOA, specifically to obtain the $\alpha$ and $\beta$ parameters for the Beverton-Holt stock recruit relation used in Atlantis GOA for fish. There are four main parts to the workflow:

1. The script `maturity_ogives.R` constructs maturity ogives based on stock assessment model output for Tier 3 species in the GOA. For other species, ogives are built symmetrically around age at 50% maturity.
2. The script `spr.R` calculates spawners per recruit based on maturity ogives and life-history. These will be used with R<sub>0</sub> (our $\alpha$ from assessments) to get SSB<sub>0</sub>.
3. The script `FSPB_calculator.R`onvert maturity ogives (annual) to `FSPB_XXX` values for `biol.prm`.
4. For the $\beta$ parameter of the BH relationship, we extract steepness $h$ from global databases. This is in `h_and_R0.Rmd`.

See scripts for details.

