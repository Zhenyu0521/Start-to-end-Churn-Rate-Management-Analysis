# Intuit-Churn-Rate-Management-Analysis-Experiment-Design

### Hi, there!
### This is a churn rate management analysis for intuit Quickbook upgrades(the data has been disguised for academic purpose)

See the more specific report from this [html](report/intuit.html)

## Data Summary: 
* 75,000 oberservations
* 14 features:
    * id, zip, zip_bins,sex,bizflag,
    * Number of orders,Total $ ordered,Time (in months) since last orders,
    * Sincepurch:Time (in months) since original (not upgrade) Quickbooks purchase
    * Version: version of software
    * Owntaxprod: whether they have purchased products from Intuit
    * Upgraded: whether upgrated or not 
    * Res1: Response to wave-1 mailing (“Yes” if responded else “No”)
    * Training:  70/30 split, 1 for training sample, 0 for validation sample

## Data explore and feature engineering

* Exam relationships between predictors and response variable, one by one
* Add interactions between features
* Add new features to distinguish special location by using zip code

## Models:

* Equential RFM
* Logistic Regression
* Naive Bayes
* Neural Network

## Train, Validation and Test

* Train and validation are split among all 75,000 overservations
* Test data comes from another dataset

## Model Selections

* Rule of Thumbs: Estimated profits based on profit/cost info.
* Others: models' AUC, accuracy.

## Overfitting test

All the model's performance on train / validation set are compared to choose the best model and best parameters(eg: the layers of Neural Network)


![model result_2]("pics/model_comparison_2.png")

![model result]("pics/model_comparison.png")
