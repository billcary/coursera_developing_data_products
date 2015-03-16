Predicting the Compressive Strength of Concrete
========================================================
author: Bill Cary
date: March 22, 2015

Application Overview
========================================================

- Course: Coursera Developing Data Products (March 2015)
- Purpose: Predict the compressive strength of a concrete sample
- Source Data: Concrete dataset provided by the UCI Machine Learning Repository
- Prediction Algorithm: Random Forest
- Shiny App: https://billcary.shinyapps.io/concrete_strength/

How do you use it?
========================================================

- Select the desired concrete parameters using the sliders on the sidebar panel
- The predicted compressive strenth (and associated confidence interval) for the concrete will automatically update based on the new parameters
- The main panel will also disply the chosen parameters in both tabular and chart format.

Random Forest Model Summary
========================================================




```r
print(model.rf)
```

```

Call:
 randomForest(formula = CompressiveStrength ~ ., data = training,      xtest = testing[, 1:8], ytest = testing$CompressiveStrength,      mtry = 6, ntree = 750, importance = TRUE, keep.forest = TRUE) 
               Type of random forest: regression
                     Number of trees: 750
No. of variables tried at each split: 6

          Mean of squared residuals: 23.24835
                    % Var explained: 91.44
                       Test set MSE: 31.72
                    % Var explained: 89.72
```

What it IS and IS NOT
========================================================

- It *IS* intended to demonstrate the capabilities of Shiny for the rapid deployment of web-based analytical tools
- It *IS NOT* an attempt to develop the optimal predictive model for concrete compressive strength

Citations
========================================================

* I-Cheng Yeh, "Modeling of strength of high performance concrete using artificial neural networks," Cement and Concrete Research, Vol. 28, No. 12, pp. 1797-1808 (1998). 
* Data Set: https://archive.ics.uci.edu/ml/datasets/Concrete+Compressive+Strength
