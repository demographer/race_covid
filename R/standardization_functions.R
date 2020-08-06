## cdc_county_filename = "~/.../provisional_COVID-19_death_counts_by_county_may_9.csv"
## census_filename = "~/.../cc-est2018-alldata.csv.zip"
## cdc_race_filename = "~/.../provisional_death_counts_for_COVID-19__weekly_state_updates_may_9.csv"

reconstruct_Nijk <- function(Nijk.dt)
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


get_Nijk <- function(state_name, age_grouping = seq(0, 80, 10), census_filename)
{
    ## read in data and keep only most recent year, dropping age totals
    dt <- fread(census_filename, encoding = 'UTF-8')
    dt <- dt[YEAR == 10 & AGEGRP != 0] ## 2017

    ## we want white, black, asian, am indian, other, hisp
    ## define other as Native Hawaiian (NA) and 2+ races (NHTOM , HTOM)
    dt[, O_MALE   :=  NA_MALE   + NHTOM_MALE   + HTOM_MALE] # other
    dt[, O_FEMALE :=  NA_FEMALE + NHTOM_FEMALE + HTOM_FEMALE]

    dt <- dt[, .(STNAME,
                 CTYNAME,
                 YEAR,
                 AGEGRP,
                 TOT_POP,
                 NHWA_MALE, ## not hispanic white alone
                 NHWA_FEMALE,
                 NHBA_MALE, ## black
                 NHBA_FEMALE,
                 NHAA_MALE, ## asian
                 NHAA_FEMALE,
                 NHIA_MALE, ## am indian
                 NHIA_FEMALE, ## am indian
                 H_MALE, ## Hispanic
                 H_FEMALE,
                 O_MALE,
                 O_FEMALE)]
    ## combine sexes
    dt[, w := NHWA_MALE + NHWA_FEMALE]
    dt[, b := NHBA_MALE + NHBA_FEMALE]
    dt[, a := NHAA_MALE + NHAA_FEMALE]
    dt[, h := H_MALE + H_FEMALE]
    dt[, i := NHIA_MALE + NHIA_FEMALE]
    dt[, o := O_MALE + O_FEMALE]

    ## check if we're missing anyone
    dt[, TOT_POP_HAT := w + b + a + i + h + o]
    ## print(dt[, .(TOT_POP_HAT, TOT_POP)])

    ## convert age groups to x, start of age group
    dt[AGEGRP == 1, x := 0]
    dt[AGEGRP == 2, x := 5]
    dt[AGEGRP == 3, x := 10]
    dt[AGEGRP == 4, x := 15]
    dt[AGEGRP == 5, x := 20]
    dt[AGEGRP == 6, x := 25]
    dt[AGEGRP == 7, x := 30]
    dt[AGEGRP == 8, x := 35]
    dt[AGEGRP == 9, x := 40]
    dt[AGEGRP == 10, x := 45]
    dt[AGEGRP == 11, x := 50]
    dt[AGEGRP == 12, x := 55]
    dt[AGEGRP == 13, x := 60]
    dt[AGEGRP == 14, x := 65]
    dt[AGEGRP == 15, x := 70]
    dt[AGEGRP == 16, x := 75]
    dt[AGEGRP == 17, x := 80]
    dt[AGEGRP == 18, x := 85]

    ## reshape the data so that we have
    ## state, county
    my.dt <- dt[STNAME %in% state_name,]
    long.dt <- melt(my.dt,
                    id.vars = c("STNAME", "CTYNAME", "x"),
                    measure.vars = c("w", "b", "a", "i", "h", "o"))
    setnames(long.dt, old = "variable", new = "racehisp")
    ## create unique county code county_state
    long.dt[, county_state := paste0(CTYNAME, ", ", STNAME)]

    ## create appropriate age groups
    long.dt[, x := cut(x, breaks = c(age_grouping, 199), include.lowest = T, right = FALSE)]

    ## fix Dona Ana County New Mexico (Spanish ~n)
    long.dt[, county_state := gsub("\xf1", "n", county_state)]
    ## long.dt[grepl("Ana County$", county_state, useBytes = TRUE),
    ##         county_state == "Dona Ana County, New Mexico"]

    ## cross-tab
    Nijk = long.dt[, xtabs(value ~ x + county_state + racehisp)]
    return(Nijk)
}

 get_Dj <- function(state_name, cdc_county_filename)
 {
## state_name = "Michigan"

##     cdc_county_filename = cdc_county_filename

## convert name to abb for state, for selecting within dt_county
state_abb = state.abb[match(state_name, state.name)]

    ## counts of deaths for counties > 100 deaths
    dt_county = fread(cdc_county_filename)
    names(dt_county) <- c("date", "first_week", "last_week", "State", "County", "FIPS",
                          "deaths_covid", "death_all_cause")
    ## do "Lane County, Oregon" format for county_state in dt_county
    ## 1) convert abbrev "OR" --> "Oregon"
    my_dt_county <- dt_county[State %in% state_abb,]
    my_dt_county[, state := state.name[match(State, state.abb)]]
    ## note this omits District of Columbia, which is NA for "state"
    my_dt_county = my_dt_county[!is.na(state)]
    ## 2) create county_state
    my_dt_county[, county_state := paste0(County, ", ", state)]
    my_county = names(table(my_dt_county$county_state))
    Dj = my_dt_county$deaths_covid
    names(Dj) = my_dt_county$county_state
    return(Dj)
}

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
    dt_race[State == "New York<sup>5</sup>", State := "New York State"]

    ## replace NA with zero (don't do this because they're not zero, they're suppressed)
##     dt_race[is.na(dt_race)] <- 0

    ## keep only covid_death_perc
    return(dt_race)
}

get_perc_Dk <- function(cleaned_cdc_race_filename, place.name)
{


    ## counts of deaths by race for place ("Oregon" or "United States")
    ## or "New York", or "New York City" [ note : "New York State" is NY - NYC ]
    dt_race = fread(cleaned_cdc_race_filename)

    perc_Dk <- dt_race[State %in% place.name & Indicator == "covid_death_perc",
                       .(w, b, a, i, h, o)]

    perc_Dk = unlist(perc_Dk)
    return(perc_Dk)
}




## get_perc_Dk <- function(cdc_race_filename, place.name,
##                         k_names = c("w", "b", "a", "i", "h", "o"))
## {
##     ## counts of deaths by race for place ("Oregon" or "United States")
##     dt_race = fread(cdc_race_filename)
##     setnames(dt_race, old = c("Non-Hispanic White",
##                               "Non-Hispanic Black or African American",
##                               "Non-Hispanic American Indian or Alaska Native",
##                               "Non-Hispanic Asian",
##                               "Hispanic or Latino",
##                               "Other"),
##              new = c("w", "b", "i", "a", "h", "o"))
##     dt_race[Indicator == "Distribution of COVID deaths (%)",
##             Indicator := "covid_death_perc"]
##     dt_race[Indicator == "Weighted distribution of population (%)",
##             Indicator := "weighted_pop_perc"]
##     dt_race[Indicator == "Unweighted distribution of population (%)",
##             Indicator := "unweighted_pop_perc"]
##     ## re-order races
##     dt_race <- dt_race[, ..k_names] ## super weird syntax for using a vector of names

##     ## fix New York<sup>5</sup> --> "New York Notcity"
##     dt_race[State == "New York<sup>5</sup>", State := "New York State"]

##     ## keep only covid_death_perc
##     return(dt_race)
##     ## get D_k for place.name (e.g., "United States", "Oregon", "New York City")
##     foo <- dt_race[State %in% place.name & Indicator == "covid_death_perc",]
##     perc_Dk = foo[, ..k_names] ## super weird syntax for using a vector of names
##     perc_Dk = unlist(perc_Dk)
##     return(perc_Dk)
## }

get_Di <- function(cdc_age_filename, age_grouping = seq(5, 85, 10))
{

    dt_age = dt_age[grep("year", Indicator), c(4, 7)]
    names(dt_age) <- c("Indicator", "nDx_string")
    ##     dt_age[, x := c(0, 1, seq(5, 85, 10))]
    dt_age[, x := c(0, 1, age_grouping)]
    dt_age[, nDx := as.numeric(gsub(",", "", nDx_string))]
    ## combine lowest age group to get 5 year
    nDx = c(sum(dt_age$nDx[1:2]), dt_age$nDx[-(1:2)])
    names(nDx) = c(0, age_grouping)
##    ndx = prop.table(nDx)
    return(nDx)
}


## ## get_Nijk


## ## get Nijk for entire USA
## source("get_Nijk_3.R")
## x_cdc = c(0, seq(5, 85, 10))
## Nijk = get_Nijk(state.name, age_grouping = x_cdc)
## ## get Nijk for counties with more than 100 deaths
## #########
## ## Nijk ##
## #########
## my_Nijk = Nijk[,names(D_j),]
## jj <- dimnames(my_Nijk)[[2]] ## order of counties
## kk <- dimnames(my_Nijk)[[3]] ## order of races
## D_j <- D_j[jj] ## make sure order of counties is rigth



## #########
## ## D_i ##
## #########

## D_i = nDx
## N_i = apply(Nijk, 1, sum) ## note we do entire US

## #########
## ## M_i ##
## #########

## M_i = D_i / N_i ## entire country
## plot(names(M_i), M_i, log = 'y', type = 'o')


## #########
## ## M_j ##
## #########


## N_j = apply(my_Nijk, 2, sum) ## note: just for counties > 100 deaths
## N_j = N_j[jj]
## D_j = D_j[jj]
## M_j = D_j / N_j

## #########
## ## M_k ##
## #########

## ## Note: D_k are adjusted to scale to the counties > 100 deaths
## N_k = apply(my_Nijk, 3, sum) ## note: just for counties > 100 deaths
## N_k <- N_k[kk]
## D_k = D_k[kk]
## M_k = D_k / N_k


## ################################
## ## Get my_Nik, my_Njk, my_Nij ##
## ################################

## my_Nik = apply(my_Nijk, c(1,3), sum)
## my_Njk = apply(my_Nijk, c(2,3), sum)
## my_Nij = apply(my_Nijk, c(1,2), sum)


get_R_k = function(D_k, my_Nijk)
{
    my_Nk = apply(my_Nijk, 3, sum)
    Mk = D_k / my_Nk
    R_k = Mk/Mk["w"]
    return(R_k)
}


get_R_k_i = function(M_i, my_Nijk, D_k)
{
    my_Nik = apply(my_Nijk, c(1,3), sum)
    stan_D_k_i = rbind(M_i) %*% my_Nik
    stan_D_k_i = stan_D_k_i[1,] ## turn to vector
    ## SMR
    SMR_k_i = D_k / stan_D_k_i
    ## Relative to whites
    R_k_i = SMR_k_i / SMR_k_i["w"]
    return(R_k_i)
}



get_R_k_j = function(M_j, my_Nijk, D_k)
{
    ## make sure j indices match
    my_counties = dimnames(my_Nijk)[[2]]
    M_j = M_j[my_counties]

    my_Njk = apply(my_Nijk, c(2,3), sum)
    stan_D_k_j = rbind(M_j) %*% my_Njk
    stan_D_k_j = stan_D_k_j[1,] ## turn to vector
    ## SMR
    SMR_k_j = D_k / stan_D_k_j
    ## Relative to whites
    R_k_j = SMR_k_j / SMR_k_j["w"]
    return(R_k_j)
}


get_R_k_ij =
function(M_i, D_j, my_Nijk, D_k)
{

     ## make sure j indices match
     my_counties = dimnames(my_Nijk)[[2]]
     D_j = D_j[my_counties]

    ## M_i = Mi
    ## D_j = Dj
    ## D_k = Dk
    my_Nij = apply(my_Nijk, c(1,2), sum)
    ## get M_ij
    deaths_by_place_from_age_standard = rbind(M_i) %*% my_Nij
    stan_D_j_i = deaths_by_place_from_age_standard
    stan_D_j_i = stan_D_j_i[1,]
    alpha_j = D_j / (rbind(M_i) %*% my_Nij)
    ##
    M_ij = cbind(M_i) %*% rbind(alpha_j)
    ##
D_hat_w = sum(M_ij * my_Nijk[,,"w"])
D_hat_b = sum(M_ij * my_Nijk[,,"b"])
D_hat_a = sum(M_ij * my_Nijk[,,"a"])
D_hat_i = sum(M_ij * my_Nijk[,,"i"])
D_hat_h = sum(M_ij * my_Nijk[,,"h"])
D_hat_o = sum(M_ij * my_Nijk[,,"o"])
stan_D_k_ij = c("w" = D_hat_w,
                "b" = D_hat_b,
                "a" = D_hat_a,
                "i" = D_hat_i,
                "h" = D_hat_h,
                "o" = D_hat_o)
SMR_k_ij = D_k / stan_D_k_ij
## Relative to whites
R_k_ij = SMR_k_ij / SMR_k_ij["w"]
return(R_k_ij)
}
