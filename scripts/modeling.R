#Fit the Logistic Regression Model
#predicting injury occurrence based on match congestion
injury_model <- glm(is_injured ~ is_congested, data = final_model_data, family = "binomial")

#View the statistical summary (coefficients, standard errors, p-values)
summary(injury_model)

#Calculate the Odds Ratios
#exponentiate the coefficients to interpret them as odds
exp(coef(injury_model))

