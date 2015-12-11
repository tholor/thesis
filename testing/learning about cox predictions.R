
## learning about predictions in cox models

aml_data = aml
aml_data[13,1]=6
cox1 = coxph(Surv(time,status)~x, data=aml_data, ties="breslow")
temp=data.frame(x=levels(aml$x))
#survfit produces exp(-cum baseline hazard) for a population with mean of all variables
summary(survfit(cox1), data =aml_data)
surv = exp(-basehaz(cox1)$hazard)
# manual calculation of the same value at t= 5: fails so far 
beta_x = coef(cox1)
mean_x = mean(as.numeric(aml$x)-1)
breslow_est = 1/(sum(exp(beta_x*as.numeric(aml_data$x)-1)))
exp(-breslow_est)
#if newdata is given -> offset for the particular value is included in survival prediction
summary(survfit(cox1, newdata = temp))
plot(survfit(cox1))
#predict produces "offset" to mean-population: (x_i -mean(x))*beta_x
predict(cox1, newdata = list (x="Maintained"),type="lp")
beta_x*(as.numeric(aml$x)-1-mean_x)
#or relative risk to mean population when type =risk (=> also exp(lp))
predict(cox1, newdata = list (x="Maintained"),type="risk")
beta_x = coef(cox1)
mean_x = mean(as.numeric(aml_data$x)-1)
risk_mean = exp(mean_x*beta_x)
risk_1234 = exp(beta_x*(as.numeric(aml_data$x)-1))
risk_1234 /risk_mean
(0-mean_x)*beta_x
0.8401^exp(-0.477669)
