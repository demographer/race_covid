library(data.table)

source("./standardization_functions.R")

## Census Bureau county-level estimates
census_filename = "~/Downloads/cc-est2017-alldata.csv"
x_cdc = c(0, seq(5, 85, 10))
all_states = state.name
Nijk = get_Nijk(state_name = all_states, age_grouping = x_cdc, census_filename = census_filename)
Nijk.dt <- as.data.table(Nijk)
fwrite(Nijk.dt, "./data/Nijk.csv")

## Code to reconstuct Nijk
reconstruct_Nijk = function(Nijk.dt)
{
    ## get racehisp in canonical order
    Nijk.dt[, racehisp := factor(racehisp, levels = c("w", "b", "a", "i", "h" ,"o"))]
    ## order labels for age
    Nijk.dt[, x := factor(x, levels = c("[0,5)",
                                        "[5,15)",
                                        "[15,25)",
                                        "[25,35)",
                                        "[35,45)",
                                        "[45,55)",
                                        "[55,65)",
                                        "[65,75)",
                                        "[75,85)",
                                        "[85,199]"))]
    ## create array
    Nijk = Nijk.dt[, xtabs(N ~ x + county_state + racehisp)]
    return(Nijk)
}

## test to make sure recreated array is same as original
if (0) {
Nijk_orig = Nijk
rm(Nijk, Nijk.dt)

Nijk.dt <- fread("Nijk.csv")
Nijk.dt[, racehisp := factor(racehisp, levels = c("w", "b", "a", "i", "h" ,"o"))]
Nijk.dt[, x := factor(x, levels = c("[0,5)",
                  "[5,15)",
                  "[15,25)",
                  "[25,35)",
                  "[35,45)",
                  "[45,55)",
                  "[55,65)",
                  "[65,75)",
                  "[75,85)",
                  "[85,199]"))]
Nijk = Nijk.dt[, xtabs(N ~ x + county_state + racehisp)]
sum(Nijk_orig != Nijk)

}
