#***************************************************************************************************#
#MODULE NAME:         mvs002_sim_mort_port_P2
#MODULE DESCRIPTION:  CODE TO CREATE A SIMULATED MORTGAGE PORTFOLIO OVER A LONG TIME PERIOD - part2
#
#AUTHOR:              MATT SALISBURY
#DATE CREATED:        28TH APRIL 2019
#***************************************************************************************************#

#0. SET OUT FUNCTIONS REQUIRED FOR CALIBRATION ROUTINE ----

#multinomial coefficient function

fn_multicoeff <- function(input_matrix) {
  
  ln_multi_coeffs <- rep(0, dim(input_matrix)[[1]])
  
  for(k in 1:dim(input_matrix)[[1]]) {
    
    tmp <- input_matrix[k,]
    if(sum(tmp) == 0) {
      ln_multi_coeffs[k] <- 0
    } else {
      num <- sum(log(sum(tmp):1))
      denom <- 0
      
      for(i in 1:length(tmp[tmp > 0])) {
        denom <- denom + sum(log(tmp[tmp > 0][i]:1))
      }
      ln_multi_coeffs[k] <- num - denom
    }
  }
  ln_multi_coeffs
}


#log likelihood function

fn_log_lik <- function(gam, av_mat = score_av_mat, Z = z, obs = trans[1:17, 1:17,]) {
  
  t_sum <- 0

  cum_av_mat <- t(round(apply(X = av_mat, MARGIN = 1, FUN = cumsum),2))
  thresh <- cbind(rep(-10, dim(cum_av_mat)[[1]]), qnorm(cum_av_mat))
  #thresh <- cbind(rep(-10, dim(cum_av_mat)[[1]]), qnorm(cum_av_mat)*sqrt(1 - gam))
  thresh[thresh > 10] <- 10
  thresh[thresh < -10] <- -10
  lower <- thresh[,-dim(thresh)[[2]]]
  upper <- thresh[,-1]
  
    for (t in 1:dim(obs)[[3]]) {
    ln_multi_coef <- fn_multicoeff(obs[,,t])
    int1 <- pnorm((upper + sqrt(gam)*Z[t])/sqrt(1 - gam)) - pnorm((lower + sqrt(gam)*Z[t])/sqrt(1 - gam))
    int1[int1 == 0] <- 1e20
    int2 <- ln_multi_coef + obs[,,t] * log(int1)
    t_sum <- t_sum + sum(int2)
  }
  outp <- (-1) * t_sum
  outp
}

#bottom up portfolio default rate calculation - choose log odds adjustment to hit portfolio level default rate target

fn_bottom_up_port_dr_calc <- function(adj, a_start = ln_odds_coeff[[1]], b = b_year, ead_dist = ead_dist_prior_year_end, target = port_dr_td) {
  adj_lnodds <- (adj + a_start) + b * (1:max(ead_dist[,1]))
  adj_dr <- 1/(1 + exp(adj_lnodds))
  bu_port_dr <- sum(ead_dist[,2] * adj_dr)/sum(ead_dist[,2])
  miss <- abs(bu_port_dr - target)
  miss
}

#1. IMPORT REQUIRED DATA ----

#1a. External series

file_location = "C:\\SARC code\\Inputs\\"
SVR_hist = read.csv(paste(file_location, "int_rate_table.csv", sep=""), header = TRUE, sep =",")
HPI_GRW_hist = read.csv(paste(file_location, "HPI_grw.csv", sep=""), header = TRUE, sep =",")
DISP_INCOME_GRW_hist = read.csv(paste(file_location, "DISP_INCOME_GRW.csv", sep=""), header = TRUE, sep =",")
BEH_MAT_PROFILE = read.csv(paste(file_location, "BEHAVE_MAT_PROFILE.csv", sep=""), header = TRUE, sep =",") 
DRS_PRS = read.csv(paste(file_location, "drs and prs.csv", sep=""), header = TRUE, sep =",") 

#1b. Already simulated payment history

#load outstanding balances at end of each year, 
setwd('C:\\SARC code\\Outputs')

save_time_ref <- '20190504_163301'

load(paste0('year_end_balances_',  save_time_ref, '.RData'))
load(paste0('orig_month_',  save_time_ref, '.RData'))
load(paste0('closed_month_',  save_time_ref, '.RData'))
load(paste0('loan_at_app_',  save_time_ref, '.RData'))

#2. INITIALISE REQUIRED VARIABLES -----

obs_start_year <- 1980 #first observation is at the end of 1980, so first year of default flows is end 80 to end 81, i.e 1981
orig_year <- as.numeric(substring(orig_month,1,4))
closed_year <- as.numeric(substring(closed_month, 1, 4))
n <- dim(ann_end_bal)[[1]]
min_year <- min(orig_year)
max_year <- max(DRS_PRS[,1])
num_years <- max_year - min_year + 1
num_grades <- 17
rho <- rep(0.08522503, n) # asset correlation
tau <- rep(0.7, n) # correlation of idiosyncratic element of score with idiosyncratic element of distance to default
gamma <- rep(0.1, n) # correlation of score process with systematic element of distance to default
theta <- rep(0.8, n) # autocorrelation parameter of idiosyncratic element of distance to default
lr_trans_mat <- matrix(0, num_grades, num_grades)
eps <- matrix(0, n, num_years)
landa <- matrix(0, n, num_years)
psi <- matrix(0, n, num_years)
x <- matrix(0, n, num_years)
s <- matrix(0, n, num_years)
grade_app <- rep(0, n)
grade <- matrix(100, n, num_years)
def <- matrix(0, n, num_years)
near_def <- matrix(0, n, num_years)
grade_lookup <- as.data.frame(cbind(1:num_grades, num_grades:1))
mig_mat <- array(data = 0, dim = c(num_grades, num_grades, num_years))
dti <-  matrix(0, n, num_years)
income <- matrix(0, n, num_years)
dtv <-  matrix(0, n, num_years)
val <- matrix(0, n, num_years)
cor_dti_grade <- 0.6
cor_ltv_grade <- 0.8
sd_ltv <- 0.3
mean_ltv <- 0.55
max_ltv <- 0.95
sd_dti <- 0.5
mean_dti <- 3.5
near_def_mult <- 1.5 #number of times greater than the default rate the flow rate into 'near default' state is

#3. GENERATE SYSTEMIC VARIABLE AND TRANSITION THRESHOLDS ----

#3a. Systemic variable from long run default rate

var_ext <- var(qnorm(DRS_PRS[,2]))
cor_ext <- var_ext/(1+var_ext)
lrdr_ext <- mean(DRS_PRS[,2])
z <- (qnorm(lrdr_ext) - qnorm(DRS_PRS[,2])*sqrt(1 - cor_ext))/sqrt(cor_ext)
check <- pnorm((qnorm(lrdr_ext) - sqrt(cor_ext)*z)/sqrt(1-cor_ext))
print(cbind(DRS_PRS[,2], check))
min_z_year <-  min(DRS_PRS[,1])
z <- c(rep(0, min_z_year - min_year), z)
#z <- seq(from = -2, to = 2, length.out = 36)
z_years <- c(min_year:max(DRS_PRS[,1]))

#4.  SET DEFAULT POINTS, NEAR DEFAULT POINTS, MIGRATION THRESHOLDS AND DISTRIBUTION OF NEW BUSINESS ----

#4a. Proportion of new business in each rating grade

cum_prop <- c(pnorm(qnorm(pgamma(seq(from = 1, to = 7, length.out = (num_grades - 1)), shape = 2.5))),1)
cum_prop <- c(0, 0, cum_prop[-c(length(cum_prop), length(cum_prop) - 1)])
cum_prop <- cum_prop/cum_prop[14]
cum_prop[15:17] <- 1

#4b. Generate default points

ln_odds_exts <- c(log(1/0.12 - 1), log(1/0.003 - 1))
ln_odds_coeff <- lm(ln_odds_exts ~ c(num_grades, 1))$coefficients
ln_odds <- ln_odds_coeff[1] + ln_odds_coeff[2]*seq(1:num_grades)
ttc_pds <- 1/(exp(ln_odds) + 1)
dp <- qnorm(ttc_pds)
sum(c(cum_prop[1], (cum_prop[-1] - cum_prop[-length(cum_prop)]))*ttc_pds)
lrdr_ext

#4c. Generate 'near default' state points

ttc_p_near_d <- 2 * ttc_pds
near_dp <- qnorm(ttc_p_near_d)

#4d.  Generate transition thresholds - matrix of num_grades x num_grades
#nb: aimed to create an upgrade bias in a neutral economy.  Aim is to reflect reducing risk (other things being equal) as time on book increases

bias_scalar <- 0.6
diag_val <- 0.55
one_off_diag_val_upgrade <- 0.15
one_off_diag_val_downgrade <- one_off_diag_val_upgrade * bias_scalar
two_off_diag_val_upgrade <- 0.06
two_off_diag_val_downgrade <- two_off_diag_val_upgrade * bias_scalar

prob_mass_to_spread <- 1 - diag_val - (1+bias_scalar)*one_off_diag_val_upgrade - (1+bias_scalar)*two_off_diag_val_upgrade
prob_mass_up <- prob_mass_to_spread * 0.6
prob_mass_down <- prob_mass_to_spread - prob_mass_up

step_size_down <- (-3 - qnorm(prob_mass_down))/5
tmp<- rep(0,6)
tmp[1] <- -3
for(i in 2:6) {tmp[i] <- tmp[i-1]-step_size_down}

step_size_up <- (3 - qnorm(1 - prob_mass_up))/5
tmp2<- rep(0,6)
tmp2[1] <- 3
for(i in 2:6) {tmp2[i] <- tmp2[i-1]-step_size_up}
tmp2<- c(10,tmp2)

lower_end_probs <- pnorm(tmp)
upper_end_probs <-pnorm(sort(tmp2))
cum_probs <- c(lower_end_probs, max(lower_end_probs)+two_off_diag_val_downgrade, max(lower_end_probs)+two_off_diag_val_downgrade+one_off_diag_val_downgrade)
cum_probs <- c(cum_probs, max(cum_probs)+diag_val, max(cum_probs)+diag_val+one_off_diag_val_upgrade)
cum_probs <- c(cum_probs, upper_end_probs)
mid_row <- rev(c(cum_probs[1], cum_probs[-1] - cum_probs[-length(cum_probs)]))

lr_trans_mat <- matrix(0, num_grades, num_grades)
lr_trans_mat[9,] <- mid_row
row_i <- mid_row
for(i in 8:1) {
  row_i <- c(row_i[-1],row_i[length(row_i)]*0.66)
  lr_trans_mat[i,] <- row_i
}
row_i <- mid_row
for(i in 10:17) {
  row_i <- c(row_i[1]*0.66,row_i[-length(row_i)])
  lr_trans_mat[i,] <- row_i
}
lr_trans_mat <- lr_trans_mat/rowSums(lr_trans_mat)
round(lr_trans_mat,3)
rowSums(lr_trans_mat)

cum_lr_trans <- lr_trans_mat
for (i in 2:dim(cum_lr_trans)[[2]]) {cum_lr_trans[,i] <- cum_lr_trans[,i-1]+lr_trans_mat[,i]}
round(cum_lr_trans,3)
trans_thresh <- cbind(rep(-10, num_grades), qnorm(round(cum_lr_trans,9)))
trans_thresh[trans_thresh > 10] <- 10
round(trans_thresh, 3)

#5. GENERATE RATING GRADE, DEBT TO INCOME AND LOAN TO VALUE AT APPLICATION ----

#5a. Rating grade at app
set.seed(20190512)
tmp <- runif(n)
grade_app <- findInterval(tmp, cum_prop)+1
table(grade_app)

norm_dist_ranking_var <- pnorm(tmp)

#5b. Debt to Income at app

set.seed(20190512)
stand_dti <- sqrt(cor_dti_grade) * norm_dist_ranking_var + sqrt(1 - cor_dti_grade) * rnorm(n)
dti_app <- stand_dti * sd_dti + mean_dti 
hist(dti_app)
income_app <- loan_at_app / dti_app

#5c. Loan to Value at app

set.seed(20190512)
stand_ltv <- sqrt(cor_ltv_grade) * norm_dist_ranking_var + sqrt(1 - cor_ltv_grade) * rnorm(n)
ltv_app <- stand_ltv * sd_ltv + mean_ltv
ltv_app[ltv_app > max_ltv] <- max_ltv
hist(ltv_app)
val_app <- loan_at_app / ltv_app

#6. GENERATE DEFAULT RATE AND RATING GRADES VARIABLES AT APPLICATION ----

#6a. Distance to default variable
#set.seed(20190512)
eps <- matrix(data = rnorm(n*num_years), nrow = n, ncol = num_years, byrow = FALSE)
x <- sqrt(rho) * matrix(data = rep(z, n), nrow = n, ncol = num_years, byrow = TRUE) + sqrt(1 - rho) * eps

#6b. Distance to migration variable
#set.seed(20190512)
psi <- matrix(data = rnorm(n*num_years), nrow = n, ncol = num_years, byrow = FALSE)
landa <- sqrt(tau) * eps + sqrt(1 - tau) * psi
s <- sqrt(gamma) * matrix(data = rep(z, n), nrow = n, ncol = max_year - min_year + 1, byrow = TRUE) + sqrt(1 - gamma) * landa

#7.  GENERATE VALUATION, INCOME, RATING GRADE, NEAR DEFAULT STATUS INDICATOR, DEFAULT STATUS INDICATOR IN EACH PERIOD ----

#7a. Generate changes in income (standardised)
#set.seed(20190512)
stand_delta_I <- sqrt(cor_dti_grade) * s + matrix(data = rnorm(n*num_years), nrow = n, ncol = num_years, byrow = TRUE)

for(year in min_year:max_year) {
  ind <- rep(0,n)
  ind[year > orig_year] <- 1
  grade[year == orig_year, year - 1980 + 1] <- grade_app[year == orig_year]
  income[year == orig_year, year - 1980 + 1] <- income_app[year == orig_year]
  val[year == orig_year, year - 1980 + 1] <- val_app[year == orig_year]
  #7.b update income and valuation in each period
  if(sum(ind) > 0) {
    income[ind == 1,year - min_year +1] <- (stand_delta_I[ind == 1,year - min_year +1] * 0.2 * income[ind == 1,year - min_year] + income[ind == 1,year - min_year]) * (DISP_INCOME_GRW_hist[match(orig_year[ind == 1], DISP_INCOME_GRW_hist[, 1]),year - 1972 + 2]+1)
    val[ind == 1,year - min_year + 1] <- val_app[ind == 1] * (HPI_GRW_hist[match(orig_year[ind == 1], HPI_GRW_hist[, 1]), year - 1953 + 2]+1)
  }

  #7.c overwrite grades where default or near default has occured
  if(year > min_year) {
    port_dr_td <- pnorm((qnorm(lrdr_ext) - sqrt(cor_ext)*z[year - 1980 + 1])/sqrt(1-cor_ext))
    print(port_dr_td)
    b_year <- ln_odds_coeff[2] + 0 * z[year - 1980 + 1]
    ead_dist_prior_year_end <- matrix(data = 0, nrow = num_grades, ncol = 2)
    ead_dist_prior_year_end[,1] <- 1:num_grades
    #tmp <- aggregate(ann_end_bal[ind == 1, year - 1980] ~ grade[ind == 1, year - 1980], FUN = sum)
    tmp <- aggregate(ann_end_bal[ind == 1, year - 1980] ~ grade[ind == 1, year - 1980], FUN = length)
    tmp <- tmp[tmp[,1] > 0 & tmp[,1] <= num_grades,]
    ead_dist_prior_year_end[match(tmp[,1], c(1:num_grades)),2] <- tmp[,2]
    
    int_logodds <- optimise(fn_bottom_up_port_dr_calc, c(-10, 10))$minimum
    adj_ln_odds <- (ln_odds_coeff[[1]] + int_logodds) + b_year*seq(1:num_grades)
    adj_dr_year <- 1/(1 + exp(adj_ln_odds))
    adj_dr_year_lookup <- cbind(c(0,1:18), c(0, adj_dr_year, 1))
    #print(sum(ead_dist_prior_year_end[-18,2]*adj_dr_year_lookup[-c(1,19),2])/sum(ead_dist_prior_year_end[-18,2]))
    
    adj_near_dr_year <- near_def_mult * adj_dr_year
    adj_near_dr_year_lookup <- cbind(c(0,1:18), c(0, adj_near_dr_year, 1))

    for(g in 1:(num_grades)) {
      num_in_grade <- length(grade[ind == 1, year - 1980][grade[ind == 1, year - 1980] == g])
      #set.seed(20190512)
      def[ind == 1, year - 1980 + 1][grade[ind == 1, year - 1980] == g] <- rbinom(n = num_in_grade, size = 1, prob = adj_dr_year_lookup[match(g, adj_dr_year_lookup[,1]),2])
      #set.seed(20190602)
      near_def[ind == 1, year - 1980 + 1][grade[ind == 1, year - 1980] == g] <- rbinom(n = num_in_grade, size = 1, prob = adj_near_dr_year_lookup[match(g, adj_near_dr_year_lookup[,1]),2])
      #7d. Rating Grade at each period, before near default or default considered
      if(sum(ind) > 0 & length(grade[ind == 1, year - 1980 + 1][grade[ind == 1, year - 1980] == g]) != 0) {
        grade[ind == 1 & grade[, year - 1980] == g & def[, year - 1980 + 1] != 1, year - 1980 + 1] <- findInterval(-1*s[ind == 1 & grade[, year - 1980] == g & def[, year - 1980 + 1] != 1, year - 1980 + 1], trans_thresh[g,])
      }
    }

#    grade[ind == 1, year - 1980 + 1][near_def[ind == 1, year - 1980 + 1] == 1] <- num_grades
    grade[ind == 1 & def[,year - 1980 + 1] == 1, year - 1980 + 1] <- num_grades + 1
    grade[ind == 1 & def[,year - 1980] == 1, year - 1980 + 1] <- num_grades + 1
  }
}

#7.e calculate debt to value position in each period
val[val == 0] <- -1
dtv <- ann_end_bal / val
dtv[val == -1] <- 0
val[val == -1] <- 0

#7.f calculate debt to income position in each period
income[income == 0] <- -1
dti <- ann_end_bal / income
dti[income == -1] <- 0
income[income == -1] <- 0

#8. CREATE AND SAVE FINAL DATASET - NB; WILL BE LARGE ----

#8a. Remove all objects except final set required
dataset_elements <- c("ann_end_bal", "grade", "dti", "dtv", "near_def", "orig_month", "closed_month")
port_data <- c("DRS_PRS")

sim_dataset_names <- c()
for(i in 1:(length(dataset_elements)-2)) {sim_dataset_names <- c(sim_dataset_names, paste0(dataset_elements[i], "_", c(min_year:max_year)))}
sim_dataset_names <- c(sim_dataset_names, "orig_month", "closed_month")
rm_list = ls()[-match(c(dataset_elements, port_data, "dataset_elements","port_data", "sim_dataset_names"), ls())]
#rm(list = rm_list)

#8b. Create dataset
sim_dataset <- get(dataset_elements[1])
rm(list = dataset_elements[1])
for (i in 2:length(dataset_elements)) {
  sim_dataset <- cbind(sim_dataset, get(dataset_elements[i]))
  #rm(list = dataset_elements[i])
  }
sim_dataset <- as.data.frame(sim_dataset)
names(sim_dataset) <- sim_dataset_names

#8c. Save dataset, including time stamp
save(sim_dataset, file = paste0('sim_dataset_', format(Sys.time(), '%Y%m%d')))

#9. CALIBRATION ROUTINE - PREP DATA AND RUN CALIBRATION ALGORITHMS ----

#9a. Generate transition matrix for each year
trans <- array(data = 0, dim = c(19,19,35))
for(year in 1981:2015) {
  select_rows <- (1:dim(sim_dataset)[[1]])[(sim_dataset[,match(paste0('grade_', year-1), names(sim_dataset))] + sim_dataset[,match(paste0('grade_', year), names(sim_dataset))])>0]
  tmp <- sim_dataset[select_rows, c(grep(year - 1, names(sim_dataset)), grep(year, names(sim_dataset)))]
  trans[,,year-1980] <- table(factor(tmp[,match(paste0('grade_', year-1), names(tmp))], levels = c(1:18,100)), factor(tmp[,match(paste0('grade_', year), names(tmp))], levels = c(1:18,100)))
}

#9b. Calculate portfolio level default rate and cci (z)

num <- rep(0, dim(trans)[[3]])
denom <- rep(0, dim(trans)[[3]])
port_dr <- rep(0, dim(trans)[[3]])

for(i in 1:dim(trans)[[3]]) {
  num[i] <- sum(trans[1:17,18,i])
  denom[i] <- sum(rowSums(trans[1:17,1:18,i]))
  port_dr[i] <- num[i]/denom[i]
}

rho_est <- var(qnorm(port_dr))/(1 + var(qnorm(port_dr)))
DP <- qnorm(mean(port_dr))
z <- (DP - qnorm(port_dr)*sqrt(1 - rho_est))/sqrt(rho_est)
#z <- seq(from = -2, to = 2, length.out = 36)[-1]
#9c. Create estimated long run average matrix and transition thresholds

trans_pc_gb <- trans[1:num_grades,1:num_grades,]
for(i in 1:35) {
  trans_pc_gb[,,i] <- trans_pc_gb[,,i]/rowSums(trans_pc_gb[,,i])
  trans_pc_gb[,,i][is.na(trans_pc_gb[,,i])]<-0
}

cum_mat <- trans_pc_gb[,,8]
for(i in 9:dim(trans_pc_gb)[[3]]) {
  cum_mat <- cum_mat + trans_pc_gb[,,i]
}
#score_av_mat <- cum_mat[1:17,1:17]/rowSums(cum_mat[1:17,1:17])
score_av_mat <- cum_mat/(35-7)
score_av_mat <- score_av_mat/rowSums(score_av_mat)
cum_score_av_mat <- t(round(apply(X = score_av_mat, MARGIN = 1, FUN = cumsum),9))
#cum_score_av_mat <- 1 - cum_score_av_mat

#9d. Minimise the log liklihod function with respect to gamma

x <- optimise(fn_log_lik, c(0,1))

#9e. Minimise the chi-squared statistic for the percentage migration matrices

res <- matrix(data = 0, nrow = 100, ncol = 2)
res <- as.data.frame(res)
names(res) <- c("gamma", "chi_sq")

obs_pc <- array(data = 0, dim = c(17,17,35))
for(i in 1:dim(trans)[[3]]) {obs_pc[,,i] <- trans[1:17,1:17,i]/rowSums(trans[1:17,1:17,i])}
obs_pc[is.na(obs_pc)] <- 0

for(j in 1:100) {
  gam <- (j-1)/100
  expect_pc <- array(data = 0, dim = c(17,17,35))
  #thresh <- cbind(rep(-10, dim(cum_score_av_mat)[[1]]), qnorm(cum_score_av_mat)*sqrt(1 - gam))
  thresh <- cbind(rep(-10, dim(cum_score_av_mat)[[1]]), qnorm(cum_score_av_mat))
  thresh[thresh > 10] <- 10
  thresh[thresh < -10] <- -10
  upper_thresh <- thresh[,-1]
  lower_thresh <- thresh[,-dim(thresh)[[2]]]

  for(i in 1:dim(expect_pc)[[3]]){
    expect_pc[,,i] <- pnorm((upper_thresh + sqrt(gam)*z[i])/sqrt(1 - gam)) - pnorm((lower_thresh + sqrt(gam)*z[i])/sqrt(1 - gam))
  }
  res[j,1] <- gam
  res[j,2] <- sum((obs_pc[,,-1] - expect_pc[,,-1])^2)
}

res[res[,2] == min(res[,2]), ]
x

#10. Fit log odds parameters

dr <- matrix(data = NA, nrow = num_grades, ncol = num_years - 1)
for(i in 1:(num_years - 1)) {
  dr[,i] <- trans[1:17,18,i]/rowSums(trans[1:17,,i])  
}

ln_odds_obs <- log(1/dr -1)




##################################################################################################################
#END                                                                                                             #
##################################################################################################################

