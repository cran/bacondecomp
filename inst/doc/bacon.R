## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----castle, fig.width = 7----------------------------------------------------
library(bacondecomp)

df_bacon <- bacon(l_homicide ~ post,
                  data = bacondecomp::castle,
                  id_var = "state",
                  time_var = "year")
coef_bacon <- sum(df_bacon$estimate * df_bacon$weight)
print(paste("Weighted sum of decomposition =", round(coef_bacon, 4)))

fit_tw <- lm(l_homicide ~ post + factor(state) + factor(year), 
             data = bacondecomp::castle)
print(paste("Two-way FE estimate =", round(fit_tw$coefficients[2], 4)))


## ----plot, fig.width = 6, fig.height = 4, fig.align = 'center'----------------
library(ggplot2)

ggplot(df_bacon) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point()

## ----cont---------------------------------------------------------------------
ret_bacon <- bacon(l_homicide ~ post + l_pop + l_income, 
                   data = bacondecomp::castle,
                   id_var = "state",
                   time_var = "year")
beta_hat_w <- ret_bacon$beta_hat_w
beta_hat_b <- weighted.mean(ret_bacon$two_by_twos$estimate, 
                            ret_bacon$two_by_twos$weight)
Omega <- ret_bacon$Omega
bacon_coef_cont <- Omega*beta_hat_w + (1 - Omega)*beta_hat_b
print(paste("Weighted sum of decomposition =", round(bacon_coef_cont, 4)))

two_way_fe_cont <- lm(l_homicide ~ post + l_pop + l_income + factor(state) + 
                        factor(year), 
                      data = bacondecomp::castle)
two_way_fe_coef_cont <- two_way_fe_cont$coefficients["post"]
print(paste("Two way FE estimate =", round(two_way_fe_coef_cont, 4)))

## ----plot2, fig.width = 6, fig.height = 4, fig.align = 'center'---------------
ggplot(ret_bacon$two_by_twos) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate") +
  geom_point()

