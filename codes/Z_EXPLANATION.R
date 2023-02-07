#######################################################
#     DETERMINANTS OF LOCAL DEPARTMENTS EFFICIENCY    #
#              DEA CONDITIONAL FRAMEWORK              #
#            PART 4: EXPLANING EFFICIENCY             #
#######################################################

library(np)
library(xtable)


# DATA ---------------------------------------------------
rm(list = ls())
load ('uncon_con_eff_TIME_CONTINUOUS.RData')


# EFFECT OF Z --------------------------------------------

COND_TO_UNCOND <- COND_EFF_FDH/UNCOND_EFF_FDH

n <- length(COND_TO_UNCOND)
data.train <- data.frame(COND_TO_UNCOND, Z_USED)


bw <- npregbw(COND_TO_UNCOND ~ debt_final+dens_final+income_final+unempl_final+year+factor(pol_col_final), bwmethod = "cv.ls", regtype="ll", ckertype="epanechnikov", ukertype = "aitchisonaitken", data = data.train)



model_ll <- npreg(bws=bw, gradients = TRUE)
dat <- plot(model_ll, plot.errors.method="bootstrap", plot.behavior = "plot-data", common.scale = F, neval = 1500)






jpeg("effects.jpeg",
     units="in",
     width = 16, 
     height =14,
     res = 300,
     pointsize=13)

par(mfrow=c(3,2),mai=c(0.8, 1, 0.2, 0.5))



# DEBT
DEBT <- data.frame(x= dat$r1$eval[, 1], y = dat$r1$mean, ci_low = dat$r1$mean + dat$r1$merr[, 1], ci_up = dat$r1$mean + dat$r1$merr[, 2])


Q <- quantile(data.train$debt_final, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(data.train$debt_final)
up  <- min(Q[2]+1.5*iqr, max(data.train$debt_final)) 
low <- max(0, Q[1]-1.5*iqr) 



DEBT_FINAL <- DEBT[DEBT$x >= low & DEBT$x <= up,]

matplot(DEBT_FINAL$x, DEBT_FINAL[, c("ci_low", "y", "ci_up")], type="l", pch = 1, lty = c(2,1,2), cex=0.5, ylab = "Cond./Uncond.", xlab = "Debt", col = 'black', ylim = c(0.8, 1.5))



# DENS
DENS <- data.frame(x= dat$r2$eval[, 2], y = dat$r2$mean, ci_low = dat$r2$mean + dat$r2$merr[, 1], ci_up = dat$r2$mean + dat$r2$merr[, 2])


Q <- quantile(data.train$dens_final, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(data.train$dens_final)
up  <- min(Q[2]+1.5*iqr, max(data.train$dens_final)) 
low <- max(0, Q[1]-1.5*iqr) 



DENS_FINAL <- DENS[DENS$x >= low & DENS$x <= up,]

matplot(DENS_FINAL$x, DENS_FINAL[, c("ci_low", "y", "ci_up")], type="l", pch = 1, lty = c(2,1,2), cex=0.5, ylab = "Cond./Uncond.", xlab = "Density", col = 'black', , ylim = c(0.8, 1.5))






# INCOME
INCOME <- data.frame(x= dat$r3$eval[, 3], y = dat$r3$mean, ci_low = dat$r3$mean + dat$r3$merr[, 1], ci_up = dat$r3$mean + dat$r3$merr[, 2])


Q <- quantile(data.train$income_final, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(data.train$income_final)
up  <- min(Q[2]+1.5*iqr, max(data.train$income_final)) 
low <- max(0, Q[1]-1.5*iqr) 



INCOME_FINAL <- INCOME[INCOME$x >= low & INCOME$x <= up,]

matplot(INCOME_FINAL$x, INCOME_FINAL[, c("ci_low", "y", "ci_up")], type="l", pch = 1, lty = c(2,1,2), cex=0.5, ylab = "Cond./Uncond.", xlab = "Income", col =  'black',  ylim = c(0.8, 1.5))





# UNEMPL
UNEMPL <- data.frame(x= dat$r4$eval[, 4], y = dat$r4$mean, ci_low = dat$r4$mean + dat$r4$merr[, 1], ci_up = dat$r4$mean + dat$r4$merr[, 2])



matplot(UNEMPL$x, UNEMPL[, c("ci_low", "y", "ci_up")], type="l", pch = 1, lty = c(2,1,2), cex=0.5, ylab = "Cond./Uncond.", xlab = "Unemployment", col =  'black', ylim = c(0.8, 1.5))





# YEAR
YEAR <- data.frame(x= dat$r5$eval[, 5], y = dat$r5$mean, ci_low = dat$r5$mean + dat$r5$merr[, 1], ci_up = dat$r5$mean + dat$r5$merr[, 2])



matplot(YEAR$x, YEAR[, c("ci_low", "y", "ci_up")], type="l", pch = 1, lty = c(2,1,2), cex=0.5, ylab = "Cond./Uncond.", xlab = "Year", col =  'black',  ylim = c(0.8, 1.5))





# POL_COL
POL <- data.frame(x= dat$r6$eval[, 6], y = dat$r6$mean, ci_low = dat$r6$mean + dat$r6$merr[, 1], ci_up = dat$r6$mean + dat$r6$merr[, 2])


x = 1:3
y = as.vector(POL$y)
yl = as.vector(POL$ci_low)
yu = as.vector(POL$ci_up)
plot.new()
plot.window(xlim = c(0.5, 3.5), ylim =  c(1.1, 1.15),  ylab = "Cond./Uncond.", xlab = "POL_COL")
arrows(x, yl, x, yu, code = 3, angle = 90, length = .125, lty = 2)
points(x, y, pch = 20, cex = 1.5)
axis(1, at = 1:3, labels = 1:3)
axis(2, las = 1)
title(ylab = "Cond./Uncond.", xlab = "Political color")
box()

dev.off()




#######################################################
# THIS CODE CAN BE USED FOR M = 1 AND INPUT
# SPECIFIC EFFICIENCIES
#######################################################


