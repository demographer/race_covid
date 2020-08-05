## Create deaths by race and state from "Weekly_State-Specific_Data_Updates__06_17" file from CDC

clean_cdc_race_file <- function(cdc_race_filename)
{
    dt_race = fread(cdc_race_filename)
    setnames(dt_race, old = c("Non-Hispanic White",
                              "Non-Hispanic Black or African American",
                              "Non-Hispanic American Indian or Alaska Native",
                              "Non-Hispanic Asian",
                              "Hispanic or Latino",
                              "Other"),
             new = c("w", "b", "i", "a", "h", "o"))
    dt_race[grepl("^Distribution of COV", Indicator),
            Indicator := "covid_death_perc"]
    dt_race[Indicator == "Weighted distribution of population (%)",
            Indicator := "weighted_pop_perc"]
    dt_race[Indicator == "Unweighted distribution of population (%)",
            Indicator := "unweighted_pop_perc"]
    ## re-order races
    dt_race <- dt_race[, .(Indicator, State, w, b, a, i, h, o)]

    ## fix New York<sup>5</sup> --> "New York State"
    ##     dt_race[State == "New York<sup>5</sup>", State := "New York State"]
    dt_race[State == "New York", State := "New York State"]

    ## replace NA with zero (don't do this because they're not zero, they're suppressed)
##     dt_race[is.na(dt_race)] <- 0

    ## keep only covid_death_perc
    return(dt_race)
}

cdc_race_filename = "../source_data/Provisional_Death_Counts_for_Coronavirus_Disease_Weekly_State-Specific_Data_Updates__06_17.csv"
foo = clean_cdc_race_file(cdc_race_filename)

clean_cdc_race.dt = foo[Indicator == "covid_death_perc"]

## combine NYS and NYC
ny_weights = c("NYC" = .44, "NYS" = .56)
ny_row = data.table(Indicator = "covid_death_perc",
           State = "New York",
           ny_weights["NYC"] * clean_cdc_race.dt[State == "New York City", .(w,b,a,i,h,o)] +
           ny_weights["NYS"] * clean_cdc_race.dt[State == "New York State", .(w,b,a,i,h,o)])
clean_cdc_race_ny.dt = rbind(clean_cdc_race.dt, ny_row)
clean_cdc_race_ny.dt[grepl("New York", State)]
clean_cdc_race_out.dt = clean_cdc_race_ny.dt[ !(State %in% c("New York City", "New York State"))][order(State)]
fwrite(clean_cdc_race_out.dt, "./prov_4.csv")
