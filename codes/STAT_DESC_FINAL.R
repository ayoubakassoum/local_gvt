#######################################################
#     DETERMINANTS OF LOCAL DEPARTMENTS EFFICIENCY    #
#              DEA CONDITIONAL FRAMEWORK              #
#           PART 2: DESCRIPTIVE STATISTICS            #
#######################################################

library(xtable)


# DATA ---------------------------------------------------
rm(list = ls())
load ('dpt_data.RData')


# DESCRIPTIVE STATISTICS (2007 TO 2018) ---------------
des_stat <- function(x) c(Min = min(x), Max = max(x), Mean = mean(x), CV = sd(x)/mean(x))


var1 <- cbind(x_final, y1_final, y2_final, y3_final, y4_final, y5_final, y6_final, y7_final)
var1_desc <- t(apply(var1, 2, des_stat))

rownames(var1_desc) <- c('TOT_EXPEN (2015 million euro)', 'ROAD (kilometers)','PUB_PUPILS (No. of pupils)', 'BENEF_RSA/RMI (No. of beneficiaries)', 'BENEF_APA (No. of beneficiaries)', 'BENEF_ASE (No. of beneficiaries)', 'BENEF_PCH (No. of beneficiaries)', 'TOT_POP (No. of inhabitant)')
                         

write.csv(var1_desc, "stat_global.csv")

xtable(var1_desc, caption = 'Descriptive statistics of original input and outputs included in the analysis', label = 'T1', booktabs = TRUE, digits = 4)






# DESCRIPTIVE STATISTICS (YEAR BY YEAR) ------------------
year <- rep(2007:2019, each = 92)

var2 <- cbind(x_final, y1_final, y2_final, y3_final, y4_final, y5_final, y6_final, y7_final, year)
var2_desc <- t(aggregate(var2[,-ncol(var2)], by=list(var2[, ncol(var2)]), mean))


colnames(var2_desc) <- 2007:2019
var2_desc <- var2_desc[-1, ]


rownames(var2_desc) <- c('TOT_EXPEN (2015 million euro)', 'ROAD (kilometers)','PUB_PUPILS (No. of pupils)', 'BENEF_RSA/RMI (No. of beneficiaries)', 'BENEF_APA (No. of beneficiaries)', 'BENEF_ASE (No. of beneficiaries)', 'BENEF_PCH (No. of beneficiaries)', 'TOT_POP (No. of inhabitant)')


write.csv(round(var2_desc, 2), "stat_year.csv")

xtable(var2_desc[, 1:4], caption = 'Averages from 2007 to 2010', label = 'A1', booktabs = TRUE, digits = 2)

xtable(var2_desc[, 5:8], caption = 'Averages from 2011 to 2014', label = 'A2', booktabs = TRUE, digits = 2)

xtable(var2_desc[, 9:13], caption = 'Averages from 2015 to 2019', label = 'A3', booktabs = TRUE, digits = 2)








# DESCRIPTIVE STATISTICS COMPOSITE INDEX (2007 TO 2018) ---------------
var3_desc <- as.data.frame(des_stat(y_composite_final))
colnames(var3_desc) <- c('COMP_INDEX (Unitless)')
                         
xtable(var3_desc, caption = 'Descriptive statistics of the composite index', label = 'T3', booktabs = TRUE, digits = 4)






# DESCRIPTIVE STATISTICS COMPOSITE INDEX (YEAR BY YEAR) ------------------
year <- rep(2007:2019, each = 92)
var4 <- cbind(y_composite_final, year)

var4_desc <- aggregate(var4[,-ncol(var4)], by=list(var4[, ncol(var4)]), mean)
colnames(var4_desc) <-c('YEAR', 'COMP_INDEX (Unitless)')
                       
xtable(var4_desc, caption = 'Average values of the composite index', label = 'B2', booktabs = TRUE, digit = 4)








# DESCRIPTIVE STATISTICS Z VARIABLES (2007 TO 2019) ---------------
var5 <- cbind(debt_final, dens_final, income_final,  unempl_final)
var5_desc <- t(apply(var5, 2, des_stat))

rownames(var5_desc) <- c('DEBT (2005 euro/inhabitant)', 'DENSITY (inhabitant/km2)', 'INCOME (2005 euro/inhabitant)', 'UNEMPLY (%)')

xtable(var5_desc, caption = 'Descriptive statistics of contextual variables included in the analysis', label = 'T4', booktabs = TRUE, digits = 4)







# DESCRIPTIVE STATISTICS OF Z VARIABLES (YEAR BY YEAR) ------------------
year <- rep(2007:2019, each = 92)

var6 <- cbind(debt_final, dens_final, income_final,  unempl_final, year)
var6_desc <- t(aggregate(var6[,-ncol(var6)], by=list(var6[, ncol(var6)]), mean))

colnames(var6_desc) <- 2007:2019
var6_desc <- var6_desc[-1, ]

rownames(var6_desc) <- c('DEBT (2005 euro/inhabitant)', 'DENSITY (inhabitant/km2)', 'INCOME (2005 euro/inhabitant)', 'UNEMPLY (%)')


xtable(var6_desc[, 1:4], caption = 'Averages from 2007 to 2010', label = 'C1', booktabs = TRUE, digits = 2)

xtable(var6_desc[, 5:8], caption = 'Averages from 2011 to 2014', label = 'C2', booktabs = TRUE, digits = 2)

xtable(var6_desc[, 9:13], caption = 'Averages from 2015 to 2019', label = 'C3', booktabs = TRUE, digits = 2)








# DESCRIPTIVE STATISTICS EXPENDITURE VARIABLES (2007 TO 2018) ---------------
var <- cbind(x_fct_final, x_inv_final, x_final)
var_desc <- t(apply(var, 2, des_stat))

rownames(var_desc) <- c('OPE_EXPEN (2015 million euro)', 'INV_EXPEN (2015 million euro)', 'TOT_EXPEN (2015 million euro)')

xtable(var_desc, caption = 'Descriptive statistics of expenditure variables', label = 'T9', booktabs = TRUE, digits = 4)







# DESCRIPTIVE STATISTICS OF EXPENDITURE VARIABLES (YEAR BY YEAR) ------------------
year <- rep(2007:2019, each = 92)

var6 <- cbind(x_fct_final, x_inv_final, x_final, x_inv_final/x_final, year)
var6_desc <- t(aggregate(var6[,-ncol(var6)], by=list(var6[, ncol(var6)]), mean))

colnames(var6_desc) <- 2007:2019
var6_desc <- t(var6_desc[-1, ])

colnames(var6_desc) <- c('OPE_EXPEN (2015 million euro)', 'INV_EXPEN (2015 million euro)', 'TOT_EXPEN (2015 million euro)', 'Ratio INV_EXPEN/TOT_EXPEN')


xtable(var6_desc, caption = 'Averages of expenditure variables from 2007 to 2010', label = 'E1', booktabs = TRUE, digits = 4)






# DESCRIPTIVE STATISTICS OF VARIABLES BY SIZE ------------------
var <- cbind(x_fct_final, x_inv_final, x_final, y1_final, y2_final, y3_final, y4_final, y5_final, y6_final, debt_final, dens_final, income_final, unempl_final)
size <- data.frame(var, pop_final)

size$quant <- cut(size[,14], quantile(size[,14], probs = c(0, 0.25, 0.5, 0.75, 1)), include.lowest = TRUE, labels = 1:4)

size$quant  <- as.numeric(size$quant)
mat_size    <- aggregate(size, by=list(size[,15]), mean)
mat_size    <- mat_size[, c(-1, -16)]         

rownames(mat_size) <- c('Extra small', 'Small','Large', 'Extra large')
colnames(mat_size) <- c('OPE_EXPEN', 'INV_EXPEN', 'TOT_EXPEN', 'ROAD','PUB_PUPILS', 'BENEF_RSA/RMI', 'BENEF_APA', 'BENEF_ASE', 'BENEF_PCH', 'DEBT', 'DENSITY', 'INCOME', 'UNEMPLY', 'TOT_POP')


write.csv(t(round(mat_size, 4)), "variables_size.csv")
























