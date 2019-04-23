#Parametric Regression
matrix_coef <- matrix(NA, nrow = 2, ncol = 11)

model1 <- lm(myoutcomenext ~ above, data = indiv, subset = abs(difshare) <= 0.5)
matrix_coef[1, 1] <- model1$coefficients[2]
pvalue <- summary(model1)
matrix_coef[2, 1] <- pvalue$coefficients[2, 4]

model2 <- lm(myoutcomenext ~ above + X1, data = indiv, subset = abs(difshare) <= 0.5)
matrix_coef[1, 2] <- model2$coefficients[2]
pvalue <- summary(model2)
matrix_coef[2, 2] <- pvalue$coefficients[2, 4]

model3 <- lm(myoutcomenext ~ above + X1 + X4, data = indiv, subset = abs(difshare) <= 0.5)
matrix_coef[1, 3] <- model3$coefficients[2]
pvalue <- summary(model3)
matrix_coef[2, 3] <- pvalue$coefficients[2, 4]

model4 <- lm(myoutcomenext ~ above + X1 + X2, data = indiv, subset = abs(difshare) <= 0.5)
matrix_coef[1, 4] <- model4$coefficients[2]
pvalue <- summary(model4)
matrix_coef[2, 4] <- pvalue$coefficients[2, 4]

model5 <- lm(myoutcomenext ~ above + X1 + X2 + X4 + X5, data = indiv, 
            subset = abs(difshare) <= 0.5)
matrix_coef[1, 5] <- model5$coefficients[2]
pvalue <- summary(model5)
matrix_coef[2, 5] <- pvalue$coefficients[2, 4]

model6 <- lm(myoutcomenext ~ above + X1 + X2 + X3, data = indiv, 
            subset = abs(difshare) <= 0.5)
matrix_coef[1, 6] <- model6$coefficients[2]
pvalue <- summary(model6)
matrix_coef[2, 6] <- pvalue$coefficients[2, 4]

model7 <- lm(myoutcomenext ~ above + X1 + X2 + X3 + X4 + X5 + X6, data = indiv, 
            subset = abs(difshare) <= 0.5)
matrix_coef[1, 7] <- model7$coefficients[2]
pvalue <- summary(model7)
matrix_coef[2, 7] <- pvalue$coefficients[2, 4]

matrix_coef