
library(chron)
library(tidyr)
library(dplyr)
library(xlsx)
library(writexl)
library(scales)
library(magrittr)
library(sqldf)
library(ggplot2)

----------------------------------- Data Cleaning & Manipulation----------------------------

## Load "loan.csv" file into R console.

loan_data <- read.delim("loan.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

## The following columns will be removed as part of data cleaning process;

## collections_12_mths_ex_med   ## It contains 0 and NA
## mths_since_last_major_derog   ## It contains only NA
## policy_code                   ## It contains only 1
## application_type              ## it contains only “INDIVIDUAL”
## annual_inc_joint              ## It contains all NA
## dti_joint                     ## It contains only NA
## verification_status_joint     ## It contains only NA
## acc_now_delinq                ## It contains only 0
## tot_coll_amt                  ## It contains only NA
## tot_cur_bal                   ## It contains only NA
## open_acc_6m                   ## It contains only NA
## open_il_6m                    ## It contains only NA
## open_il_24m                    ## It contains only NA
## mths_since_rcnt_il             ## It contains only NA
## total_bal_il                   ## It contains only NA
## il_util                        ## It contains only NA
## open_rv_12m                    ## It contains only NA
## open_rv_24m                    ## It contains only NA
## max_bal_bc                    ## It contains only NA
## all_util                       ## It contains only NA
## total_rev_hi_lim               ## It contains only NA
## inq_fi                         ## It contains only NA
## total_cu_tl                    ## It contains only NA
## inq_last_12m                   ## It contains only NA
## acc_open_past_24mths           ## It contains only NA
## avg_cur_bal                    ## It contains only NA
## bc_open_to_buy                 ## It contains only NA
## bc_util                        ## It contains only NA
## loan_data$chargeoff_within_12_mths     ## It contains only NA & 0
## delinq_amnt                     ## It contains only 0
## mo_sin_old_il_acct              ## It contains only NA
## mo_sin_old_rev_tl_op            ## It contains only NA
## mo_sin_rcnt_rev_tl_op           ## It contains only NA
## mo_sin_rcnt_tl                  ## It contains only NA
## mort_acc                        ## It contains only NA
## mths_since_recent_bc            ## It contains only NA
## mths_since_recent_bc_dlq        ## It contains only NA
## mths_since_recent_inq           ## It contains only NA
## mths_since_recent_revol_delinq  ## It contains only NA
## num_accts_ever_120_pd           ## It contains only NA
## num_actv_bc_tl                  ## It contains only NA
## num_actv_rev_tl                 ## It contains only NA
## num_bc_sats                     ## It contains only NA
## num_bc_tl                       ## It contains only NA
## num_il_tl                       ## It contains only NA
## num_op_rev_tl                   ## It contains only NA
## num_rev_accts                   ## It contains only NA
## num_rev_tl_bal_gt_0             ## It contains only NA
## num_sats                        ## It contains only NA
## num_tl_120dpd_2m                ## It contains only NA
## pymnt_plan                      ## It contains only n
## initial_list_status             ## It contains only f
## open_il_12m                     ## It contains only NA
## num_tl_30dpd                    ## It contains only NA
## num_tl_90g_dpd_24m              ## It contains only NA
## num_tl_op_past_12m              ## It contains only NA
## pct_tl_nvr_dlq                  ## It contains only NA
## percent_bc_gt_75                ## It contains only NA
## tot_hi_cred_lim                 ## It contains only NA
## total_bal_ex_mort               ## It contains only NA
## total_bc_limit                  ## It contains only NA
## total_il_high_credit_limit       ## It contains only NA

loan_data_01 <- subset(loan_data, select=-c(collections_12_mths_ex_med, mths_since_last_major_derog, policy_code, application_type, annual_inc_joint, dti_joint, verification_status_joint, acc_now_delinq, tot_coll_amt, tot_cur_bal, open_acc_6m, open_il_6m, open_il_24m, mths_since_rcnt_il, total_bal_il, il_util, open_rv_12m, open_rv_24m, max_bal_bc, all_util, total_rev_hi_lim, inq_fi, total_cu_tl, inq_last_12m, acc_open_past_24mths, avg_cur_bal, bc_open_to_buy, bc_util, delinq_amnt, mo_sin_old_il_acct, mo_sin_old_rev_tl_op, mo_sin_rcnt_rev_tl_op, mo_sin_rcnt_tl, mort_acc, mths_since_recent_bc, mths_since_recent_bc_dlq, mths_since_recent_inq, mths_since_recent_revol_delinq, num_accts_ever_120_pd, num_actv_bc_tl, num_actv_rev_tl, num_bc_sats, num_bc_tl, num_il_tl, num_op_rev_tl, num_rev_accts, num_rev_tl_bal_gt_0, num_sats, num_tl_120dpd_2m, pymnt_plan, initial_list_status, open_il_12m, num_tl_30dpd, num_tl_90g_dpd_24m, num_tl_op_past_12m, pct_tl_nvr_dlq, percent_bc_gt_75, tot_hi_cred_lim, total_bal_ex_mort, total_bc_limit, total_il_high_credit_limit, url, chargeoff_within_12_mths))    


## Split term column into ‘term_in_month’ & ‘remove’ column;
loan_data_02 <- separate(loan_data_01, term, into = c("remove", "term_in_month"), sep = " ")
## Remove ‘remove’ column from dataset;
loan_data_02 <- subset(loan_data_02, select=-c(remove))

loan_data_03 <- loan_data_02

## Remove % symbol from int_rate column;
loan_data_03$int_rate <- as.character(sub("%", "", loan_data_03$int_rate))

loan_data_04 <- loan_data_03

## Replace < 1 year as 0 year in emp_length column;
loan_data_04$emp_length <- as.character(sub("< 1 year", "0 year", loan_data_04$emp_length))

## Remove year from emp_length column;
loan_data_04$emp_length <- as.character(sub("year", "", loan_data_04$emp_length))

## Remove 's' from emp_length column; 
loan_data_04$emp_length <- as.character(sub("s", "", loan_data_04$emp_length))

## Remove '+' sign from emp_length column;

loan_data_04$emp_length <- as.character(sub("\\+", "", loan_data_04$emp_length))

loan_data_05 <- loan_data_04

## Convert Dec-11 to proper date format in column ‘issue_d’;

loan_data_05$issue_d <- as.Date(paste("01-", loan_data_05$issue_d, sep = ""), format = "%d-%b-%y")

loan_data_06 <- loan_data_05

## Remove 'xx' from zip_code column;

loan_data_06$zip_code <- as.character(sub("xx", "", loan_data_06$zip_code))

## Convert date format from Dec-11 to y-m-d;

loan_data_06$earliest_cr_line <- as.Date(paste("01-", loan_data_06$earliest_cr_line, sep = ""), format = "%d-%b-%y")

## Remove percentage % symbol from revol_util column;
loan_data_06$revol_util <- as.character(sub("%", "", loan_data_06$revol_util))

## Convert date format from Dec-11 to y-m-d for columns last_pymnt_d & next_pymnt_d

loan_data_06$last_pymnt_d <- as.Date(paste("01-", loan_data_06$last_pymnt_d, sep = ""), format = "%d-%b-%y")

loan_data_07 <- loan_data_06

loan_data_07$next_pymnt_d <- as.Date(paste("01-", loan_data_07$next_pymnt_d, sep = ""), format = "%d-%b-%y")

## split issue_d column into three column issue_year, issue_month, remove.

loan_data_07 <- separate(loan_data_07, issue_d, into = c("issue_year", "issue_month", "remove"), sep = "-")

## remove 'remove' unwanted column from dataset containing only '01'

loan_data_07 <- subset(loan_data_07, select=-c(remove))

loan_masterdata <- loan_data_07

summary(loan_masterdata)

## Plot 1: Frequency Distribution of log (loan_amnt)
ggplot(loan_masterdata, aes(x= log(loan_amnt))) + geom_histogram()+ stat_bin(bins = 40)

## Plot 2: Frequency distribution of log(instalment)
ggplot(loan_masterdata, aes(x= log(installment))) + geom_histogram()+ stat_bin(bins = 40)

## Plot 3: Frequency Distribution of log(annual_inc)
ggplot(loan_masterdata, aes(x= log(annual_inc))) + geom_histogram()+ stat_bin(bins = 40)

## Plot 4: Frequency Distribution of Debt to income ratio.
ggplot(loan_masterdata, aes(x= dti)) + geom_histogram()+ stat_bin(bins = 40)

## Plot 5: Loan amount vs interest rate 
loan_masterdata$int_rate <- as.numeric(loan_masterdata$int_rate)
ggplot(aes(x= log(loan_amnt), y= log(int_rate)), data= loan_masterdata)+geom_point(shape=1) + geom_smooth()

## Plot 6: loan amount vs applicant income

ggplot(aes(x= log(loan_amnt), y= log(annual_inc)), data= loan_masterdata)+geom_point(shape=1) + geom_smooth()

## Plot 7: Understanding total charged off loans

ggplot(loan_masterdata, aes(x = loan_status, fill= loan_status)) + geom_bar() + geom_bar(position = position_dodge(width=NULL)) + geom_text(stat="count",aes(label=..count..),position = position_dodge(width = .99), size = 3,vjust=-.4) + xlab("Loan Status")+ylab("Number of Loan Applicant")+ggtitle(" Fig 1: Shows the total charged off loan ")

## Plot 8: Loan status vs employee years of experience.
ggplot(loan_masterdata, aes(x = loan_status, fill= loan_status)) + geom_bar()+ facet_wrap(~ emp_length)


## Plot 9: Loan issuance year of distribution
ggplot(loan_masterdata, aes(x = issue_year, fill= loan_status)) + geom_bar() + geom_bar(position = position_dodge(width=NULL)) + geom_text(stat="count",aes(label=..count..),position = position_dodge(width = .99), size = 3,vjust=-.4) + xlab("loan Issued Year")+ylab("Number of Loan Applicants")+ggtitle(" loan Issuance Year of Distribution ")

## Plot 10: Loan term vs Loan status
ggplot(loan_masterdata, aes(x = term_in_month, fill= loan_status)) + geom_bar() + geom_bar(position = position_dodge(width=NULL)) + geom_text(stat="count",aes(label=..count..),position = position_dodge(width = .99), size = 3,vjust=-.4) + xlab("Loan term in months")+ylab("Number of loan applicants")+ggtitle(" Fig 2: Loan term and its status ")

## Plot 11: Loan approval was made for purpose;
ggplot(loan_masterdata, aes(x = loan_status, fill= purpose))+ geom_bar()

## Plot 12: Loan approval vs customer income source was verified
ggplot(loan_masterdata, aes(x = verification_status, fill=loan_status))+ geom_bar()

## Plot 13: Home ownership vs loan status
ggplot(loan_masterdata, aes(x = home_ownership, fill=loan_status))+ geom_bar()

## Plot 14: Debt-To-Income Ratio vs Applicants annual income
ggplot(loan_masterdata, aes(x= log(annual_inc) , y= log(dti), color= loan_status)) + geom_point(shape= 3) + scale_x_discrete(name = "Applicant Annual Income") + scale_y_continuous(name = "Debt to Income ratio") + ggtitle("Debt-To-Income Ratio") + geom_smooth()

## Plot 15: Loan issuance year of distribution grade wise

ggplot(loan_masterdata, aes(x = grade, fill= loan_status)) + geom_bar(position = "stack") + geom_bar(position = position_dodge(width=NULL)) + xlab("loan grading done by LC")+ylab("Number of Loan Applicants")+ ggtitle(" loan issuance year of distribution grade wise") + facet_grid(~ issue_year)

## Plot 16: Address State

ggplot(loan_masterdata, aes(x = addr_state, fill= loan_status)) + geom_bar(position = "stack" , na.rm = TRUE)


