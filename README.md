# Summary
Code for the Cox Model and log. regression used in my paper ["Exploring Dynamic Risk Prediction for Dialysis Patients"](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5333314/) 

The java code for the Dynamic Bayesian Network can be found here: https://github.com/tholor/dbn


# Context  
Despite substantial advances in the treatment of end-stage renal disease, mortality of hemodialysis patients remains high. Several models exist that predict mortality for this population and identify patients at risk. However, they mostly focus on patients at a particular stage of dialysis treatment, such as start of dialysis, and only use the most recent patient data. Generalization of such models for predictions in later periods can be challenging since disease characteristics change over time and the evolution of biomarkers is not adequately incorporated. In this research, we explore dynamic methods which allow updates of initial predictions when patients progress in time and new data is observed. We compare a Dynamic Bayesian Network (DBN) to regularized logistic regression models and a Cox model with landmarking. Our preliminary results indicate that the DBN achieves satisfactory performance for short term prediction horizons, but needs further refinement and parameter tuning for longer horizons.

# Methods 
- Cox Model with landmarking (implemented in R)  
- Logistic Regression (implemented in R)
