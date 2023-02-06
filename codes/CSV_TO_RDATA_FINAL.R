#######################################################
#     DETERMINANTS OF LOCAL DEPARTMENTS EFFICIENCY    #
#              DEA CONDITIONAL FRAMEWORK              #
#                    PART 1: DATA                     #
#######################################################

library(stringr)
library(FactoMineR)
library(factoextra)
library(xtable)


## OUTPUTS + REMOVE LYON
# ORIGINAL DATA
y1 <- read.csv('Voirie des dpts O.csv', dec = '.')
y2 <- read.csv('Effectifs des collèges publics O.csv', dec = '.')
y3 <- read.csv('Bénéficiaires RMI-RSA O.csv', dec = '.')
y4 <- read.csv('Bénéficiaires APA O.csv', dec = '.')

y5 <- read.csv('Bénéficiaires ASE O.csv', dec = '.')
y6 <- read.csv('Bénéficiaires PCH O.csv', dec = '.')
y7 <- read.csv('Population totale des dpts O-VE.csv', dec = '.')


y1 <- y1[-68, -c(1:2)]
y2 <- round(y2[-68, -c(1:2)])
y3 <- round(y3[-68, -c(1:2)])
y4 <- round(y4[-68, -c(1:2)])

y5 <- round(y5[-68, -c(1:2)])
y6 <- round(y6[-68, -c(1:2)])

y7 <- round(y7[-68, -c(1:2)])
y7$X2019    <- y7$X2018




y1_final <- as.vector(as.matrix(y1))
y2_final <- as.vector(as.matrix(y2))
y3_final <- as.vector(as.matrix(y3))
y4_final <- as.vector(as.matrix(y4))
y5_final <- as.vector(as.matrix(y5))
y6_final <- as.vector(as.matrix(y6))
y7_final <- as.vector(as.matrix(y7))



# CORRELATION + COMPOSITE INDEX
Y <- cbind(y1_final, y2_final, y3_final, y4_final, y5_final, y6_final, y7_final)
cor_matrix <- round(cor(Y), 2)


names <- c('ROAD', 'PUB_PUPILS', 'BENEF_RSA/RMI', 'BENEF_APA', 'BENEF_ASE ', 'BENEF_PCH', 'TOT_POP')
rownames(cor_matrix) <- names
colnames(cor_matrix) <- names

write.csv(cor_matrix, "cor_matrix.csv")

xtable(cor_matrix, caption = 'Correlation matrix', label = 'A1', booktabs = TRUE)



res.pca <- PCA(Y[,-1], scale.unit = TRUE, ncp = 5, graph = FALSE)

eig.val <- get_eigenvalue(res.pca)
colnames(eig.val) <- c('Total', 'Variance (%)', 'Aggregate (%)')
rownames(eig.val) <- c('Component 1', 'Component 2', 'Component 3', 'Component 4', 'Component 5', 'Component 6')


write.csv(round(eig.val[-6,], 4), "eigen_val.csv")

xtable(round(eig.val[-6,], 4), caption = 'Principal component analysis – explained variance', label = 'A2', booktabs = TRUE)



ind <- get_pca_ind(res.pca)
y_composite <- as.vector(ind$coord[,1])
y_composite_final <- y_composite - min(y_composite)



var <- get_pca_var(res.pca)
cor <- var$cor
co2 <- var$cos2

colnames(cor) <- c('Comp.1', 'Comp.2', 'Comp.3', 'Comp.4', 'Comp.5')
colnames(co2) <- c('Comp.1', 'Comp.2', 'Comp.3', 'Comp.4', 'Comp.5')

rownames(cor) <- c('PUB_PUPILS', 'BENEF_RSA/RMI', 'BENEF_APA', 'BENEF_ASE ', 'BENEF_PCH', 'TOT_POP')
rownames(co2) <- c('PUB_PUPILS', 'BENEF_RSA/RMI', 'BENEF_APA', 'BENEF_ASE ', 'BENEF_PCH', 'TOT_POP')

write.csv(round(cor, 4), "cor.csv")
write.csv(round(co2, 4), "co2.csv")

xtable(round(cor, 4), caption = 'Correlation between original variables and Components', label = 'A8', booktabs = TRUE, file="cor.docx")





## INPUTS + REMOVE LYON
x     <- read.csv('Dépenses totales des dpts I.csv', dec = '.')
x_inv <- read.csv('Dépenses dinv des dpts I.csv', dec = '.')
x_fct <- read.csv('Dépenses de fct des dpts I.csv', dec = '.')

x     <- x[-68, -c(1:2)]
x_inv <- x_inv[-68, -c(1:2)]
x_fct <- x_fct[-68, -c(1:2)]




# DEFLATION OF INPUTS BY IPC BASE 2015 (INSEE)
ipc      <- read.csv('valeurs_mensuelles_v1.csv', dec = '.')
ipc$year <- str_sub(ipc$period, 1, 4) 

ipc_mean <- aggregate(x = ipc$index, by = list(ipc$year), FUN = mean)
ipc_mean <- ipc_mean[ipc_mean$Group.1 %in% c(2007:2019), 2]/100


x_d     <- as.matrix(x)%*%diag(1/ipc_mean)
x_inv_d <- as.matrix(x_inv)%*%diag(1/ipc_mean)
x_fct_d <- as.matrix(x_fct)%*%diag(1/ipc_mean)



x_final     <- as.vector(as.matrix(x_d))
x_inv_final <- as.vector(as.matrix(x_inv_d))
x_fct_final <- as.vector(as.matrix(x_fct_d))







## Z VARIABLES + REMOVE LYON + RECODING
pop          <- read.csv('Population totale des dpts O-VE.csv', dec = '.')
pop$X2019    <- pop$X2018
pop          <- pop[-68, -c(1:2)]
pop_final    <- as.vector(as.matrix(pop))


z1           <- read.csv('Taux de chômage VE.csv', dec = '.')
unempl       <- z1[-68, -c(1:2)]
unempl_final <- as.vector(as.matrix(unempl))


z2           <- read.csv('Revenu fiscal foyers fiscaux VE.csv', dec = '.')
z2$X2019[48] <- z2$X2018[48]
income       <-  z2[-68, -c(1:2)]/pop
income_d     <- as.matrix(income)%*%diag(1/ipc_mean)
income_final <- as.vector(as.matrix(income_d))

z3           <- read.csv('Densité de la population.csv', dec = '.')
dens         <- z3[-68, -c(1:2)]
dens_final   <- as.vector(as.matrix(dens))
                       
z4          <- read.csv('Dette des départements.csv', dec = '.')
z4$X2006    <- z4$X2007
debt        <- z4[-68, -c(1:2)]
debt_d      <- as.matrix(debt)%*%diag(1/ipc_mean)
debt_final  <- as.vector(as.matrix(debt_d))



z5             <- read.csv('Couleur politique dpts.csv', dec = '.')
pol_col         <- z5[-68, -c(1:2)]
pol_col_final   <- as.vector(as.matrix(pol_col))




## DPTS NAMES
dpt_names <- read.csv('Voirie des dpts O.csv', dec = '.')[-68, 1]



save.image('dpt_data.RData')

