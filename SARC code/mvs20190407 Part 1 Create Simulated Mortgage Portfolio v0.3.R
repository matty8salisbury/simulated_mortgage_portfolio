#***************************************************************************************************#
#MODULE NAME:         mvs001_sim_mort_port
#MODULE DESCRIPTION:  CODE TO CREATE A SIMULATED MORTGAGE PORTFOLIO OVER A LONG TIME PERIOD
#
#AUTHOR:              MATT SALISBURY
#DATE CREATED:        7TH APRIL 2019
#***************************************************************************************************#

#1. INITIALISE REQUIRED OBJECTS ----

#SET WORKING DIRECTORY

memory.size(16000)
setwd('C:\\SARC code\\Inputs')

#import historic uk trend information

SVR_hist = read.csv("int_rate_table.csv", header = TRUE, sep =",")
HPI_GRW_hist = read.csv("HPI_grw.csv", header = TRUE, sep =",")
DISP_INCOME_GRW_hist = read.csv("DISP_INCOME_GRW.csv", header = TRUE, sep =",")
BEH_MAT_PROFILE = read.csv("BEHAVE_MAT_PROFILE.csv", header = TRUE, sep =",") 
DRS_PRS = read.csv("drs and prs.csv", header = TRUE, sep =",") 
base_rate_hist = read.csv("Bank_Rate.csv", header = TRUE, sep =",")
rates_hist = read.csv("rates.csv", header = TRUE, sep =",")
names(rates_hist)[c(2,3,4)] <- c('uk_banks_base', 'bank_rate', 'rev_rate')

tmp <- as.numeric(substring(as.character(rates_hist$Date),8,9))
tmp[tmp > 20] <- tmp[tmp > 20]+1900
tmp[tmp < 20] <- tmp[tmp < 20]+2000
rates_hist$Date <- paste0(substring(as.character(rates_hist$Date),1,7), tmp)

rates_hist$Date2 <- as.Date(as.character(rates_hist$Date), format = '%d %b %Y')
rates_hist <- rates_hist[order(rates_hist$Date2),]
rates_hist$period <- as.numeric(format(rates_hist$Date2, '%Y')) * 100 + as.numeric(format(rates_hist$Date2, '%m'))

sapply(rates_hist, class)
rates_hist$uk_banks_base <- as.numeric(as.character(rates_hist$uk_banks_base))
rates_hist$rev_rate <- as.numeric(as.character(rates_hist$rev_rate)) 
sapply(rates_hist, class)

plot(rates_hist$bank_rate, type = 'l', col = 2)
lines(rates_hist$rev_rate)
rates_hist$sprd<-rates_hist$rev_rate - rates_hist$bank_rate
plot(rates_hist$sprd, type = 'l')
backcast_spread <- mean(rates_hist$sprd[rates_hist$period < 200810], na.rm = T)
rates_hist$rev_rate[is.na(rates_hist$rev_rate) == T] <- rates_hist$bank_rate[is.na(rates_hist$rev_rate) == T] + backcast_spread 
rates_hist$uk_banks_base <- rates_hist$uk_banks_base/100
rates_hist$bank_rate <- rates_hist$bank_rate/100
rates_hist$rev_rate <- rates_hist$rev_rate/100

#Initialise key variables: Month, time period, remaining contractual term, closed indicator, interest rate at start of period, discount rate at start of period,
#loan amounts outstanding, interest paid in period to date, principal repaid in period to date, annuity rate

#To estimate size of book and number of accounts, take 1.5% of mortgage market wtih average balance of ?123k.  Market size at 2015 of ?1,111,512m (i.e. ?1.1trillion). Churning 1.5% a month.
#no. total accounts = (0.015 * 1,111,512 / 0.123) * (1 + 0.015 * 430) = 1009849
#no. accounts open in any single period = (0.015 * 1,111,512 / 0.123) = 135550
#n <- 1000
#start_n <- 135

#n <- 1009849 #number of customers ever on the book 
#start_n <- 135550
red_fac <- 4  #reduction factor on size of portfolio in order to produce managable dataset size - ideally this would be 1
n <- 1009849/red_fac #number of customers ever on the book 
start_n <- 135550 / red_fac

nb_rate <- 0.015

obs_month <- c()  #Month of observation
for(t in 1980:2015) {obs_month <- c(obs_month, rep(t, 12)*100 + c(1:12))} 
obs_month <- obs_month[-c(1:2)]

time_period <- seq(1:length(obs_month))             #time period
rem_cont_term <- matrix(0, n, length(time_period))  #remaining contractual term at the start of the period
closed_ind <- matrix(0, n, length(time_period))     #closed indicator at the start of the period
end_bal <- matrix(0, n, length(time_period))      #loan amounts outstanding at the end of the period

rev_rate <- rep(0, length(time_period))             #interest rate during period
disc_rate <- rep(0, length(time_period))            #discount rate during period

int_paid <- c()       #interest paid during period
prin_paid <- c()      #principal repaid during period to date
ann_rate <- c()       #annuity rate at start of period

sd_amt_outs_pc_mean <- 0.33                         #standard deviation of outstanding balances expressed as a % of mean

#2. SET BALANCE SHEET SIZE IN EACH PERIOD ----

#import size of UK mortgage market (amounts outstanding, source Bank of England)

boe_amt_outs <- read.csv('amounts_outstanding_lending_sec_dwell_qrtly.csv')
names(boe_amt_outs)[2] <- 'amt_outs_dwell'
tmp <- as.numeric(substring(as.character(boe_amt_outs$Date),8,9))
tmp[tmp > 20] <- tmp[tmp > 20]+1900
tmp[tmp < 20] <- tmp[tmp < 20]+2000
boe_amt_outs$Date <- paste0(substring(as.character(boe_amt_outs$Date),1,7), tmp)

boe_amt_outs$Date2 <- as.Date(as.character(boe_amt_outs$Date), format = '%d %b %Y')
boe_amt_outs <- boe_amt_outs[order(boe_amt_outs$Date2),]
boe_amt_outs$period <- as.numeric(format(boe_amt_outs$Date2, '%Y')) * 100 + as.numeric(format(boe_amt_outs$Date2, '%m'))
boe_amt_outs$ln_amt_outs_dwell <- log(boe_amt_outs$amt_outs_dwell)

int <- lm(ln_amt_outs_dwell ~ period, data = boe_amt_outs)$coefficients[1]
slope <- lm(ln_amt_outs_dwell ~ period, data = boe_amt_outs)$coefficients[2]
plot(boe_amt_outs$period, boe_amt_outs$ln_amt_outs_dwell)
lines(boe_amt_outs$period, boe_amt_outs$period*slope+int, col = 2)

bs_size <- as.data.frame(cbind(obs_month, rep(0,length(obs_month))))
names(bs_size)[[2]] <- 'amt_outs'
tmp <- match(bs_size$obs_month, boe_amt_outs$period)
bs_size$amt_outs[is.na(tmp) == F] <- boe_amt_outs$amt_outs_dwell[tmp[is.na(tmp) == F]]
num_mths <- dim(bs_size)[[1]]
for(i in (1:(floor(num_mths/3)))*3-2) {
  bs_size$amt_outs[i+1] <- bs_size$amt_outs[i]+1/3*(bs_size$amt_outs[i+3]-bs_size$amt_outs[i])
  bs_size$amt_outs[i+2] <- bs_size$amt_outs[i]+2/3*(bs_size$amt_outs[i+3]-bs_size$amt_outs[i])
  }
bs_size$amt_outs <- bs_size$amt_outs*0.015*1000000 / red_fac     #simulated lender has a market share of 1.5%.  Also convert units from millions to ?

#define probability of closing due to early repayment in each month after origination conditional on open at the start of the month, for the next 50 years

p_close_erp_lookup <- c(rep(0.015,24), rep(0.015, 24), rep(0.015, 12*(25-4)))

#3. START LOOP THROUGH MONTHS ----

start_loop_time <- format(Sys.time(), '%X')

for (i in 1:num_mths) {
    #3.1 For first period generate key items - closed indicator, remaining contractual maturity and balance at end of period ----
  if(i == 1) {
    closed_ind[,i] <- 1
    closed_ind[1:start_n,i] <- 0
    rem_cont_term[1:start_n,i] <- rep(25*12, start_n)  #remaining contractual term at the start of the period
    set.seed(i)
    end_bal[1:start_n,i] <- round(rnorm(start_n, mean = bs_size$amt_outs[match(obs_month[i], bs_size$obs_month)]/start_n, sd = sd_amt_outs_pc_mean * bs_size$amt_outs[match(obs_month[i], bs_size$obs_month)]/start_n),0)      #loan amounts outstanding at the end of the period
    end_bal[1:start_n,i][end_bal[1:start_n,i] < 0] <- 0
    cum_num_acs <- start_n
  } else {
    rem_cont_term[,i] <- rem_cont_term[,i-1] - 1
    #3.2  Set closed indicator at the end of the period and calculate new buiness coming on at start of the month ----
    #close those reaching contractual maturity in period and those that have not yet been opened
    closed_ind[,i] <- closed_ind[,i-1]
    closed_ind[rem_cont_term[,i] <= 0, i] <- 1
    #close those repaying early in period
    num_open_in_period <- length(closed_ind[closed_ind[,i] == 0,i])
    tob <- rep(25 * 12, num_open_in_period) - rem_cont_term[closed_ind[,i] == 0,i-1] #time on book at the start of the month
    p_close_erp_flow <- p_close_erp_lookup[tob + 1]
    closed_ind[closed_ind[,i] == 0,i][runif(num_open_in_period) < p_close_erp_flow] <- 1
    num_open_in_period <- length(closed_ind[closed_ind[,i] == 0,i])

    #3.3  Set interest rate as in force at the start of the month and calculate discount rate and annuity rate ----
    rev_rate[i] <- rates_hist$rev_rate[match(obs_month[i], rates_hist$period)]                #customer rate during period (aer)
    monthly_rev_rate <- (1 + rev_rate[i])^(1/12) - 1                                          #monthly customer rate during period
    monthly_disc_rate <- 1/(1 + monthly_rev_rate)                                             #monthly discount rate during period
    ann_rate <- (1 - monthly_disc_rate^rem_cont_term[closed_ind[,i] == 0,i])/monthly_rev_rate #annuity rate at start of period
    
    #3.4  Calculate interest and principal paid in the month and balance at end of the month for those not closing ----
    int_paid <- round(monthly_rev_rate * end_bal[closed_ind[,i] == 0,i-1],2)                  #interest paid during period
    prin_paid <- round((1/ann_rate - monthly_rev_rate) * end_bal[closed_ind[,i] == 0,i-1],2)  #principal repaid during period to date
    end_bal[closed_ind[,i] == 0,i] <- end_bal[closed_ind[,i] == 0,i-1] - prin_paid            #outstanding balance at period end
    
    #3.5  Calculate size of new business and write new business on ----
    new_bus <- bs_size$amt_outs[match(obs_month[i], bs_size$obs_month)] - sum(end_bal[closed_ind[,i] == 0,i])
    num_new_bus <- round(nb_rate * start_n,0)
    if((cum_num_acs + num_new_bus) > n) {num_new_bus <- max(0, n - cum_num_acs - num_new_bus)}
    if(num_new_bus != 0) {
      print(num_open_in_period + num_new_bus)
      av_bal <- round(new_bus / num_new_bus, 0)
      new_bus_pos <- as.numeric((cum_num_acs + 1):(cum_num_acs + num_new_bus))
      closed_ind[new_bus_pos, i] <- 0
      rem_cont_term[new_bus_pos, i] <- 25*12
      print(c(i, num_new_bus, av_bal, sum(is.na(round(rnorm(num_new_bus, mean = av_bal, sd = sd_amt_outs_pc_mean * av_bal), 0)))))
      set.seed(i)
      end_bal[new_bus_pos, i] <- round(rnorm(num_new_bus, mean = av_bal, sd = sd_amt_outs_pc_mean * av_bal), 0)      #loan amounts outstanding at the end of the period
      end_bal[new_bus_pos, i][end_bal[new_bus_pos, i] < 0] <- 0
      #print(summary(end_bal[(cum_num_acs + 1):(cum_num_acs + num_new_bus), i]))
      
      cum_num_acs <- cum_num_acs + num_new_bus
    }
    
  }
}

end_loop_time <- format(Sys.time(), '%X')
print(paste0("Start loop through years: ", start_loop_time))
print(paste0("End loop through years: ", end_loop_time))  

#4. SAVE OUTPUT ----

setwd('C:\\Users\\Matt\\Documents\\SARC code\\Outputs')

#4.1 save monthly data ----
save_time <- Sys.time()
file_ref <- paste0('period_end_balances_', substring(save_time,1,4), substring(save_time,6,7), substring(save_time,9,10), '_', substring(save_time,12,13),substring(save_time,15,16),substring(save_time,18,19), '.RData')
save(end_bal, file = file_ref)

file_ref <- paste0('closed_ind', substring(save_time,1,4), substring(save_time,6,7), substring(save_time,9,10), '_', substring(save_time,12,13),substring(save_time,15,16),substring(save_time,18,19), '.RData')
save(closed_ind, file = file_ref)

file_ref <- paste0('rem_cont_term', substring(save_time,1,4), substring(save_time,6,7), substring(save_time,9,10), '_', substring(save_time,12,13),substring(save_time,15,16),substring(save_time,18,19), '.RData')
save(rem_cont_term, file = file_ref)

#4.1 save annual data ----

#rm(list =c("rem_cont_term"))
keep_ind <- rep(0, length(obs_month))
keep_ind[as.numeric(substring(obs_month, 5,6)) == 12] <- 1
ann_end_bal <- end_bal[,keep_ind == 1]
ann_closed_ind <- closed_ind[,keep_ind == 1]
ann_rem_cont_term <- rem_cont_term[,keep_ind == 1]


orig_ref <- max.col(closed_ind[,-dim(closed_ind)[[2]]] - closed_ind[,-1]) + 1 
orig_month <- obs_month[orig_ref]
orig_month[1:start_n] <- obs_month[1]

closed_ref <- max.col((closed_ind[,-dim(closed_ind)[[2]]] - closed_ind[,-1])*-1) + 1 
closed_month <- obs_month[closed_ref]

loan_at_app <- apply(end_bal, 1, max)
rm(list =c("end_bal", "closed_ind"))

file_ref <- paste0('year_end_balances_', substring(save_time,1,4), substring(save_time,6,7), substring(save_time,9,10), '_', substring(save_time,12,13),substring(save_time,15,16),substring(save_time,18,19), '.RData')
save(ann_end_bal, file = file_ref)

file_ref <- paste0('year_end_closed_ind_', substring(save_time,1,4), substring(save_time,6,7), substring(save_time,9,10), '_', substring(save_time,12,13),substring(save_time,15,16),substring(save_time,18,19), '.RData')
save(ann_closed_ind, file = file_ref)

file_ref <- paste0('ann_rem_cont_term_', substring(save_time,1,4), substring(save_time,6,7), substring(save_time,9,10), '_', substring(save_time,12,13),substring(save_time,15,16),substring(save_time,18,19), '.RData')
save(ann_rem_cont_term, file = file_ref)

file_ref <- paste0('orig_month_', substring(save_time,1,4), substring(save_time,6,7), substring(save_time,9,10), '_', substring(save_time,12,13),substring(save_time,15,16),substring(save_time,18,19), '.RData')
save(orig_month, file = file_ref)

file_ref <- paste0('closed_month_', substring(save_time,1,4), substring(save_time,6,7), substring(save_time,9,10), '_', substring(save_time,12,13),substring(save_time,15,16),substring(save_time,18,19), '.RData')
save(closed_month, file = file_ref)

file_ref <- paste0('loan_at_app_', substring(save_time,1,4), substring(save_time,6,7), substring(save_time,9,10), '_', substring(save_time,12,13),substring(save_time,15,16),substring(save_time,18,19), '.RData')
save(loan_at_app, file = file_ref)

rm(list = ls())