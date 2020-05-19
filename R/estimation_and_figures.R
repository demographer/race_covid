## Estimation and Figures for Indirect Standardization of COVID19 deaths by age and place
library(data.table)
source("./standardization_functions.R")

produce_figures = TRUE

#################################################
## Get exposure information from census (Nijk) ##
#################################################
Nijk.dt <- fread("../data/Nijk.csv")
Nijk = reconstruct_Nijk(Nijk.dt)


##################################################################
## Get marginal distributions of death by age, county, and race ##
##################################################################

## Deaths by age (Di)
Di.df = read.csv("../data/deaths_by_age_Di.csv", header = T, comment.char = "#")
Di = Di.df$nDx
## also get age grouping (this is CDC 0, 5, 15, 25, ..., 85+)
x_cdc = Di.df$x




## Deaths by county (Dj)
cdc_county_filename = "../data/Provisional_COVID-19_Death_Counts_in_the_United_States_by_County (5).csv"
Dj = get_Dj(state_name = state.name, cdc_county_filename) ## state.name is all 50 states, but not DC




## Deaths by race for states and nation (DJk)
perc_DJk = fread("../data/clean_cdc_race_may_13.csv")

########################
## National estimates ##
########################

## census counts for observed counties
my_counties = names(Dj)
my_Nijk = Nijk[,my_counties,]

print("Number of counties")
print(length(my_counties))
## [1] 365
print("Share of US pop covered by these counties")
print(sum(my_Nijk)/sum(Nijk))
## [1] 0.6226889
## sum(my_Nijk)
## [1] 203626968

## Death rates by age (Mi)
## Note: sum(Di) > sum(Dj) because of dropping of < 10
## sum(Di)
## [1] 54861
## sum(Dj)
## [1] 52385
Ni_my_counties = apply(my_Nijk, 1, sum)
Mi = Di / Ni_my_counties

## Death rates by county (Mj)
Nj_my_counties = apply(my_Nijk, 2, sum)
Mj = Dj/Nj_my_counties



## Dk for nation
cleaned_cdc_race_filename = "../data/clean_cdc_race_may_13.csv"
perc_Dk_usa = get_perc_Dk(cleaned_cdc_race_filename, place.name = "United States")
## ok function works, and it now uses updated file
## scale percentages by sum of county deaths
Dk = perc_Dk_usa / 100 * sum(Dj)
## reorder, using Nijk
Dk = Dk[dimnames(Nijk)[[3]] ]


########################
## National estimates ##
########################

## Relative risks of mortality, crude and various standardized
R_k = get_R_k(D_k = Dk, my_Nijk = my_Nijk)
R_k_i = get_R_k_i(M_i = Mi, my_Nijk = my_Nijk, D_k = Dk)
R_k_j = get_R_k_j(M_j = Mj, my_Nijk = my_Nijk, D_k = Dk)
R_k_ij = get_R_k_ij(M_i = Mi, D_j = Dj, my_Nijk = my_Nijk, D_k = Dk)



#####################
## State estimates ##
#####################

## get list of states that have enough data to do standardization
states_with_race_reports.dt <- fread(cleaned_cdc_race_filename)
tt <- table(states_with_race_reports.dt$State)
my_state_names = names(tt)



get_standardization_by_state_fun <- function(state_name, place.name = state_name,
                                             Mj,
                                             Mi,
                                             Dj,
                                             my_Nijk,
                                             cleaned_cdc_race_filename)
{
    ## state_name = "Michigan"
    ## place.name = state_name
    my_counties = dimnames(my_Nijk)[[2]]
    these_counties = grep(paste0(", ", state_name, "$"), my_counties, value = TRUE)

    ## these_Nijk
    these_Nijk <- my_Nijk[, these_counties,]
    k_names = dimnames(Nijk)[[3]]

    ## Dk -- for this state_name
    print(place.name)
    this_perc_Dk = get_perc_Dk(cleaned_cdc_race_filename, place.name=place.name)
     ## ok function works, and it now uses updated file
     ## scale percentages by sum of county deaths
    Dk = this_perc_Dk / 100 * sum(Dj)
    Dk[is.na(Dk)] = 0
     ## reorder, using Nijk
    Dk = Dk[dimnames(Nijk)[[3]] ]

    ## Mj
    these_Mj = Mj[these_counties]
    ## Dj
    these_Dj = Dj[these_counties]
     ##
    R_k = get_R_k(D_k = Dk, my_Nijk = these_Nijk)
    R_k_i = get_R_k_i(M_i = Mi, my_Nijk = these_Nijk, D_k = Dk)
    R_k_j = get_R_k_j(M_j = these_Mj, my_Nijk = these_Nijk, D_k = Dk)
    R_k_ij = get_R_k_ij(M_i = Mi, D_j = these_Dj, my_Nijk = these_Nijk, D_k = Dk)



    ## get approximate SE for R_k_ij, R_k, R_k_i
    ## SE(Rk) ~ Rk * sqrt( (1 / Dk) + (1 / Dw) )
    print(Dk)
    se_R_k_ij = rep(NA, length(Dk))
    if (sum(Dk, na.rm = TRUE) > 1) {
        se_R_k_ij = R_k_ij * sqrt( (1/ Dk) + (1/Dk["w"]))
    }

    R_k_mat = rbind(R_k = R_k,
                    R_k_i = R_k_i,
                    R_k_j = R_k_j,
                    R_k_ij = R_k_ij,
                    se = se_R_k_ij)


    out = R_k_mat
    return(out)
}


cleaned_cdc_race_filename = "../data/clean_cdc_race_may_13.csv"

tmp = get_standardization_by_state_fun(state_name = "California",
                                  Mj = Mj,
                                  Mi = Mi,
                                  Dj = Dj,
                                  my_Nijk = my_Nijk,
                                  cleaned_cdc_race_filename = cleaned_cdc_race_filename)



state_results_array <- array(NA, dim = c(nrow(tmp), ncol(tmp), length(my_state_names)))
dimnames(state_results_array) = list(rownames(tmp), colnames(tmp), my_state_names)
for (i in 1:length(my_state_names))
{
    this_state = my_state_names[i]
    Astan = get_standardization_by_state_fun(state_name = this_state,
                                       Mj = Mj,
                                       Mi = Mi,
                                       Dj = Dj,
                                       my_Nijk = my_Nijk,
                                       cleaned_cdc_race_filename = cleaned_cdc_race_filename)
    state_results_array[,,i] <- Astan
}


###################
## state figures ##
###################

## try with CI
if(produce_figures)
    pdf("../figures/state.pdf", height = 10, width = 8)
par(mfrow = c(2,1))
black_R_k_ij.vec <- state_results_array["R_k_ij", "b",]
se.vec <- state_results_array["se", "b",]
black_R_k_ij.vec[black_R_k_ij.vec == 0] <- NA
o <- order(black_R_k_ij.vec, na.last = NA)
dotchart(black_R_k_ij.vec[o], xlim = c(0, 4), cex = .9, pch = 19)
segments(x0 = black_R_k_ij.vec[o] - 2*se.vec[o],
         x1 = black_R_k_ij.vec[o] + 2*se.vec[o],
         y0 = seq(se.vec[o]),
         col = "black",
         lwd = 2)
points(x = black_R_k_ij.vec[o],
       y = seq(se.vec[o]),
       col = "blue",
       cex = .9, pch = 19)
abline(v = 1)
abline(v = 1.3, lty = 2)
title("(a) African American")
##
hisp_R_k_ij.vec <- state_results_array["R_k_ij", "h",]
hisp_R_k_ij.vec[hisp_R_k_ij.vec == 0] <- NA
se.vec <- state_results_array["se", "h",]
o <- order(hisp_R_k_ij.vec, na.last = NA)
dotchart(hisp_R_k_ij.vec[o], xlim = c(0, 4), cex = .9, pch = 19)
segments(x0 = hisp_R_k_ij.vec[o] - 2*se.vec[o],
         x1 = hisp_R_k_ij.vec[o] + 2*se.vec[o],
         y0 = seq(se.vec[o]),
         col = "black",
         lwd = 2)
points(x = hisp_R_k_ij.vec[o],
       y = seq(se.vec[o]),
       col = "red",
       cex = .9, pch = 19)
abline(v = 1)
abline(v = .7, lty = 2)
title("(b) Hispanic")
if(produce_figures)
{
    dev.off()
    system("open ../figures/state.pdf")
}

#####################
## National figure #
#####################

result.mat <- round(rbind("Crude Rates" = R_k, "Place-standardized" =  R_k_j, "Age-standardized" = R_k_i,
                 "Age-and-place\n standardized" = R_k_ij),2)
colnames(result.mat) = c("White", "Black", "Asian", "i", "Hispanic", "o")
my_result.mat <- t(result.mat[, c("Asian", "Hispanic", "Black")])
if(produce_figures)
    pdf("../figures/usa_all.pdf", width = 12, height = 9)
par(mfrow = c(1,1))
dotchart(my_result.mat,
         color = c("black", "red", "blue"),
         cex = 1.3,
         pch = 19)
abline(v =1)
## add segments and labels
## (there's some reversing of order and some insertion of blank lines needed here)
A = my_result.mat[,4:1]
y_val = seq(A) + (col(A) - 1) * 2
text(A, y_val, A, pos = 3, col = c("black", "red", "blue"))
segments(x0 = 1, x1 = A,
         y0 = y_val,
         col = c("black", "red", "blue"))
title("Covid-19 mortality relative to Whites", cex.main = 2)
if(produce_figures)
{    dev.off()
    system("open ../figures/usa_all.pdf")
}



