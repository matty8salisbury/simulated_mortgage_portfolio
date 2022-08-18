# simulated_mortgage_portfolio
Repo with R code to generate a simulated UK retail mortgage portfolio from 1980 to 2015

The initial intention was to give a suitable dataset for account level stress testing investigation and demo.
The simulated portfolio evolves through time, with records for any account on the book at each year end recorded in a single final dataset.
Fields simulated and retained are those account level fields typically of primary intesting in stress testing default flows (e.g. credit score, Loan to Value, Debt to Value, Debt to Income, Near Default state - such as in arrears, Default flag).

Many extensions could be made, such as recording monthly snapshots rather annual or increasing the fields and targeting LGD related variables, such as behaviour post default.

Two parts to the code.  
Part 1 generates the payment and balance history through time.
Part 2 generates the account level variables, deals with the correlation between them and also simulates the default and near default outcomes before combining to produce a single panel dataset of year end snapshots.

TO RUN:
clone the repo directly into the C drive
Run in R, first part 1 and then part 2 concurrently in the same session

