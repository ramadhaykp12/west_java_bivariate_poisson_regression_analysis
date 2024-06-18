install.packages("VGAM")
library("VGAM")

dataset <- read.csv("5 varx.csv")

# Model Penuh ivariate Poisson regression
model_penuh <- vglm(cbind(Y1, Y2) ~ X1 + X2 + X3 + X4 + X5, family = negbinomial(), data=dataset)
summary(model_penuh)


# Model sederhana
model_sederhana <- vglm(cbind(Y1, Y2) ~ X2 + X3 + X4 + X5, family = negbinomial(), data = dataset)
summary(model_sederhana)

# Menghitung Log Likelihood pada model penuh dan model sederhana
logLik_full <- logLik(model_penuh)
logLik_saturated <- logLik(model_sederhana)

# Menghitung Likelihood Ratio (LR) 
LR <- 2 * (logLik_full - logLik_saturated)

# Degrees of freedom
df <- length(coef(model_penuh)) - length(coef(model_sederhana))

# Menghitung p-value dengan distribusi chi-square 
p_value <- 1 - pchisq(LR, df = df)


# Menyimpan hasil dalam bentuk dataframe
lrt_results <- data.frame(
  Model = c("Full Model", "Saturated Model"),
  LogLikelihood = c(logLik_full, logLik_saturated),
  LR_Statistic = LR_statistic,
  DF = df,
  P_Value = p_value
)

# Lihat hasil
print("Likelihood Ratio Test Results:")
print(lrt_results)


# If H0 is rejected, proceed to Wald test
if(p_value < 0.05) {
  # Extract coefficients and covariance matrix
  coef_model <- coef(model_penuh)
  vcov_model <- vcov(model_penuh)

# Menghitung uji wald and p-value untuk setiap koefisien
wald_stat <- (coef_model / sqrt(diag(vcov_model)))^2
wald_p_value <- 1 - pchisq(wald_stat, df = 1)  # 1 degree of freedom for Poisson model

# menyimpan hasil
wald_results <- data.frame(
  Predictor = names(coef_model),
  Coefficient = coef_model,
  Wald_Statistic = wald_stat,
  Wald_P_Value = wald_p_value
)

# Lihat hasil
# Print Wald test results
print("Wald Test Results:")
print(wald_results)
} else {
  print("H0 is not rejected. No need to proceed with Wald test.")
}
