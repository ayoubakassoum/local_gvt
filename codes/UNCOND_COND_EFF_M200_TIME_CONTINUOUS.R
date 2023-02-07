#######################################################
#     DETERMINANTS OF LOCAL DEPARTMENTS EFFICIENCY    #
#              DEA CONDITIONAL FRAMEWORK              #
#  PART 3: UNCONDITIONAL AND CONDITIONAL EFFICIENCY   #
#######################################################

library(lpSolve)
library(np)
library(xtable)


# DATA ---------------------------------------------------
rm(list = ls())
load ('dpt_data.RData')



# UNCONDITIONAL EFFICENCY (INPUT ORIENTED DEA ORDER M) ---
n <- length(x_final)

B = 2000
orderm = 200


Y_USED <- data.frame(y1_final, y_composite_final)
X_USED <- data.frame(x_final)

s <- ncol(Y_USED)
m <- ncol(X_USED)

MAT1 <- cbind(Y_USED, X_USED)
colnames(MAT1) <- c('y1', 'y2',  'x')
    
EF_FDH  <- matrix(NA, ncol = 1, nrow = n)

    
    
for (j in 1:n){

        print(j)
        CI <- MAT1[j,] 
        MAT <- MAT1[MAT1$y1 >= MAT1$y1[j] & MAT1$y2 >= MAT1$y2[j],]

        EFF_REPL <- matrix(NA, nrow = B)

      
        for (l in 1:B){
            
            MAT_DRAW <- MAT[sample(nrow(MAT), size = orderm, replace = TRUE),]

            DF   <- cbind(MAT_DRAW, L = rep(1, nrow(MAT_DRAW)))
            VEC  <- c(rep(0, s), -CI[(s+1):(s+m)], 0)
            names(VEC) <- colnames(DF)
            DF_NEW <- rbind(DF, VEC)
  
            f.obj <- t(c(rep(0, nrow(MAT_DRAW)), 1))
            f.con <- t(DF_NEW)
            f.dir <- c(rep (">=", s), rep("<=", m), "==")
            f.rhs <- c( t(CI[1:s]), rep(0, m), 1)  
  
            E1  <- lp ("min", f.obj, f.con, f.dir, f.rhs, int.vec = 1:orderm)
            EFF_REPL[l] <- E1$objval
  
  
      }


        EF_FDH[j] <- mean(EFF_REPL)

      }




UNCOND_EFF_FDH <- EF_FDH






# DMU-SPECIFIC BANDWITH ESTIMATION
n <- length(x_final)

Y_USED <- data.frame(y1_final, y_composite_final)
X_USED <- data.frame(x_final)


s <- ncol(Y_USED)
m <- ncol(X_USED)
  
MAT1 <- cbind(Y_USED, X_USED)
colnames(MAT1) <- c('y1', 'y2', 'x')
    

pol_col_final[pol_col_final == 'droite'] <- 1
pol_col_final[pol_col_final == 'gauche'] <- 2
pol_col_final[pol_col_final == 'centre'] <- 3



    
unordered  <- pol_col_final
continuous <- cbind(debt_final, dens_final, income_final, unempl_final, year = rep(2007:2019, each = 92) )


bw_cx <- matrix(NA, nrow=n, length(unordered[1]) + length(continuous[1,]))



flag_median <- (MAT1$y1 >= median(MAT1$y1) & MAT1$y2 >= median(MAT1$y2))

flag_x    <- subset(X_USED,subset=flag_median,drop=TRUE)
flag_c    <- subset(continuous,subset=flag_median,drop=TRUE)
flag_u    <- subset(unordered,subset=flag_median,drop=TRUE)
flag_y    <- subset(Y_USED,subset=flag_median,drop=TRUE)

data_fr <- data.frame(factor(flag_u), flag_c)




bw <- npcdensbw(ydat=flag_x, xdat=data_fr, cykertype="epanechnikov", cxkertype="epanechnikov", uxkertype = "aitchisonaitken", bwmethod="cv.ls") 

bwfull <- bw$xbw

 


for (i in 1:n){
    
    print(i)
     
    flag      <-  (MAT1$y1 >= MAT1$y1[i] & MAT1$y2 >= MAT1$y2[i])
       
       flag_x1    <- subset(X_USED,subset=flag,drop=TRUE)
       flag_c1    <- subset(continuous,subset=flag,drop=TRUE)
       flag_u1    <- subset(unordered,subset=flag,drop=TRUE)
       flag_y1    <- subset(Y_USED,subset=flag,drop=TRUE)


       if((length(flag_x1)) > 10){

       data_fr1 <- data.frame(factor(flag_u1), flag_c1)
      
       bw1 <- npcdensbw(ydat=flag_x1, xdat=data_fr1, cykertype="epanechnikov", cxkertype="epanechnikov",  uxkertype = "aitchisonaitken", bwmethod="cv.ls") 



       bw_cx[i,] <- bw1$xbw

       } else {

           bw_cx[i,] <- bwfull

           }
  
   }



save.image('bandwidth_TIME_CONTINUOUS.RData')











# CONDITIONAL EFFICENCY (INPUT ORIENTED FDH ORDER M) --------------------------------
n <- length(x_final)

B = 2000
orderm = 200


Y_USED <- data.frame(y1_final, y_composite_final)
X_USED <- data.frame(x_final)


s <- ncol(Y_USED)
m <- ncol(X_USED)
  
MAT1 <- cbind(Y_USED, X_USED)
colnames(MAT1) <- c('y1', 'y2',  'x')


Z_USED <- data.frame(factor(unordered), continuous)

# FDH
EF_FDH  <- matrix(NA, ncol = 1, nrow = n)

    for (j in 1:n){

        print(j)

        tdata <- Z_USED[j,]

        den.k <- npksum(bws=t(bw_cx[j,]), txdat=tdata, exdat=Z_USED, cykertype ="epanechnikov", cxkertype = "epanechnikov", uxkertype = "aitchisonaitken", return.kernel.weights=TRUE)

        kerz  <- as.matrix(as.vector(den.k$kw))


        kern_prob <- kerz

        PROB1 <- cbind(Y_USED,  K = kern_prob)
        colnames(PROB1) <- c('y1', 'y2',  'K')
        
        
        CI <- MAT1[j,] 
        MAT <- MAT1[MAT1$y1 >= MAT1$y1[j] & MAT1$y2 >= MAT1$y2[j], ]

        PROB <- PROB1[PROB1$y1 >= PROB1$y1[j] & PROB1$y2 >= PROB1$y2[j], ][,3]
        
        EFF_REPL <- matrix(NA, nrow = B)

      
        for (l in 1:B){
            
            MAT_DRAW <- MAT[sample(nrow(MAT), prob = PROB, size = orderm, replace = TRUE),]

            DF   <- cbind(MAT_DRAW, L = rep(1, nrow(MAT_DRAW)))
            VEC  <- c(rep(0, s), -CI[(s+1):(s+m)], 0)
            names(VEC) <- colnames(DF)
            DF_NEW <- rbind(DF, VEC)
  
            f.obj <- t(c(rep(0, nrow(MAT_DRAW)), 1))
            f.con <- t(DF_NEW)
            f.dir <- c(rep (">=", s), rep("<=", m), "==")
            f.rhs <- c( t(CI[1:s]), rep(0, m), 1)  
  
            E1  <- lp ("min", f.obj, f.con, f.dir, f.rhs, int.vec = 1:orderm)
            EFF_REPL[l] <- E1$objval
  
  
      }


        EF_FDH[j] <- mean(EFF_REPL)

      }



COND_EFF_FDH <- EF_FDH


save.image('uncon_con_eff_TIME_CONTINUOUS.RData')


########################################################
# THE CODE CAN BE STRAITGHTFORWARDLY ADAPTED TO ESTIMATE 
# EFFICIENCY FOR M=1 OR FOR INPUT SPECIFIC EFFICIENCIES 
# FOR INSTANCE FOR M=1, orderm (LINES 22 & 178) SHOULD
# BE 1 INSTEAD OF 200  
########################################################


