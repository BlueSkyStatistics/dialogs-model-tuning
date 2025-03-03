/**
  * This file is protected by copyright (c) 2023-2025 by BlueSky Statistics, LLC.
  * All rights reserved. The copy, modification, or distribution of this file is not
  * allowed without the prior written permission from BlueSky Statistics, LLC.
 */


var localization = {
    en: {
        title: "Build a tuned model with k-Fold cross validation",
        navigation: "K-Fold Cross Validation",
        modelname: "Enter name for tuned model",
        folds: "Enter the number of folds",
        dependent: "Variable to predict",
        levelOfInterest: "When the variable to predict has 2 levels, specify the level of interest. The confusion matrix and related statistics are displayed with the specified level of interest as the reference",
        independent: "Independent variables",
        label1: "SOME MODELS OBJECT TO MISSING VALUES OR DEPENDENT VARIABLES THAT ARE FACTORS. FOR HANDLING MISSING VALUES, SEE [ VARIABLES > MISSING VALUES ] AND FOR DUMMY CODING FACTOR VARIABLES, SEE [ VARIABLES > COMPUTE > DUMMY CODE ]. Click ? icon on top right of dialog for details.",
        ModelSelection: "Select a model category and then the type of model",
        help: {
            title: "Build a tuned model with k-Fold cross validation",
            r_help: "help(train, package=caret)",
            body: `
<b>Description</b></br>
K-Fold Cross Validation uses the trainControl and train functions within the caret package to create a tuned/optimized model.<br/>
A tuned model is built by resampling results across tuning parameters. The results of the tuning are displayed below the tables at the bottom of the output<br/>
Once the tuned model is built, we make predictions using the tuned model against the training dataset (used to build the model) and display the confusion matrix and related statistics.<br/>
NOTE: see URL at https://www.blueskystatistics.com/Articles.asp?ID=330 for details on models that require dependent variables to be dummy coded and corresponding requirements for dependent variables supported. Also some models do not support missing values, if there are errors displayed when building the model, remove missing values.<br/>
The tuned model is stored in the object of class train with a name that you specified in the textbox control.<br/>
The parameter values selected for the final tuned model are displayed at the bottom in the output.<br/>
In the case with predictors with 2 levels, you have the option to select the reference level/level of interest.<br/>
The confusion matrix and related statistics are created using the specified level of interest.<br/>
<code> 
train_control <- caret::trainControl(method = "cv", number = Enter no of folds)<br/>
#General syntax<br/>
tunedModel <- caret::train(x = sample data to train the model, y = dependent variable, trControl = train_control, method = "the model to use", prob.model = TRUE )<br/>
#Sample populated syntax<br/>
tunedModel <- caret::train(as.data.frame(TrainData), dependentVariable, trControl = train_control, method = "adaboost", preProcess = NULL, prob.model = TRUE )<br/>
</code>
We generate a confusion matrix and model accuracy statistics for the tuned model. This is done as follows
<ul>
<li>
We generate predicted values from the training dataset by calling the predict function on the optimized model of class train (Internally predict.train is the function in the caret package that R calls)<br/>
<code> 
predictedValues = predict(tunedModel returned by the train function)<br/>
</code> <br/>
</li>
<li>
We compute the accuracy statistics from the confusion matrix generated as below<br/>
<code> 
caret::confusionMatrix (predictions = predictedValues, reference = model dependent variable)<br/>
</code> 
</li>
</ul>
You can use the final tuned/optimized model to score a dataset. To do so, follow the steps below<br/>
1.  Go to the dataset you want to score. Note: The variable names in the dataset to be scored (a.k.a. independent variables) must match the variable names you used in the dataset to originally create the tuned model. If not an error will display. This error message will display under the diagnostic tests section on the Model scoring dialog, see point 2 below. <br/>
2.	Open the Model Scoring dialog under Model Evaluation>Predict>Model Scoring. <br/>
3.	Select a model you want to use to score the dataset. You can filter the models you built by class. <br/>
4.	Once you have selected the model, specify a prefix to use to store the predicted values. You can optionally save confidence intervals for the predictions and generate a confusion matrix. You must specify a prefix for the variable that contains the predictions/scores. The predictions and predicted probabilities where applicable are stored as new variables at the end of the dataset. For example, if the prefix specified is AA, the predictions are stored in a variable AA_original dependent variable name_Predictions, the predicted probabilities are stored in a variable AA_Level1_PredictedProbs. <br/>
5.  Click on the Run button to score the dataset. <br/>
<b>Package</b></br>
caret</br>
<b>Help</b></br>
For detailed help click on the R icon on the top right hand side of this dialog or run the following command help(train, package ='caret') in the R Editor window
`}
    }
}

class kFoldCrossValidation extends baseModal {
    constructor() {
        var config = {
            id: "kFoldCrossValidation",
            label: localization.en.title,
            modalType: "two",
            RCode: `
require(caret)
{{selected.modelname | safe}}=character(0)
local (
{
 #get short name for  selected model 
 #modelshortname = getModelInitials("{{modelselected}}")
 #load package for selected model like nb , rf 
 modelselected = "{{selected.combokid | safe}}" 
 success = loadReqModelPackage(modelselected )
 if(success)
 {
  if ("tbl" %in% class({{dataset.name}}))
  {
  TrainData <- {{dataset.name}}[,c({{selected.independent | safe}})]
  TrainClasses  <- {{dataset.name}}[[c({{selected.dependent | safe}})]]
  }
  else
  {
  TrainData <- {{dataset.name}}[,c({{selected.independent | safe}})]
  TrainClasses  <- {{dataset.name}}[,c({{selected.dependent | safe}})]
  }
  # define training control
 train_control <- caret::trainControl(method="cv", number={{selected.folds | safe}})
 #We pass the parameter prob.model=TRUE to get predicted probabilities for svm, this is required for factor/ordinal and string and not numeric
  if ( (modelselected == "svmLinear" || modelselected == "svmRadial" ||  modelselected == "svmPoly"  ) && (class( {{dataset.name}}[,c({{selected.dependent | safe}})]) =="ordinal" ||class( {{dataset.name}}[,c({{selected.dependent | safe}})]) =="factor" || class( {{dataset.name}}[,c({{selected.dependent | safe}})]) =="character" ))
  {
    .GlobalEnv\${{selected.modelname | safe}} <- caret::train(as.data.frame(TrainData) ,TrainClasses , trControl=train_control, method= "{{selected.combokid | safe}}",preProcess=NULL, prob.model=TRUE )
  }
  else
  {
  .GlobalEnv\${{selected.modelname | safe}} <- caret::train(as.data.frame(TrainData) ,TrainClasses , trControl=train_control, method= "{{selected.combokid | safe}}" )
  }
#We generate the confusion matrix and accuract statistics only for dependent variables of class factor, character and ordered
 if (class(TrainClasses) =="factor" || class(TrainClasses) =="ordered" || class(TrainClasses) =="character")
 {
 if (!is.null(.GlobalEnv\${{selected.modelname | safe}}))
 {
   #Compute predicted values
   predictedValues =predict(.GlobalEnv\${{selected.modelname | safe}})
   if (class(TrainClasses) =="character")
   {
    TrainClasses =as.factor(TrainClasses)
   }
   BSkyConfusionMatrixTrain (predictions =predictedValues, reference =TrainClasses, levelOfInterest = "{{selected.levelOfInterest | safe}}")
  }
  }
  ## save dependent independent var with model for scoring
  attr(.GlobalEnv\${{selected.modelname | safe}},"classDepVar")= class({{dataset.name}}[, c({{selected.dependent | safe}})])
   attr(.GlobalEnv\${{selected.modelname | safe}},"depVarSample")= sample({{dataset.name}}[, c({{selected.dependent | safe}})], size = 2, replace = TRUE)
   attr(.GlobalEnv\${{selected.modelname | safe}},"depvar")="{{selected.dependent | safe}}"
   attr(.GlobalEnv\${{selected.modelname | safe}},"indepvar")="{{selected.independent | safe}}"
   # summarize results
   print(.GlobalEnv\${{selected.modelname | safe}} )
}
  else
  {
   cat("There was an error when training the model")
  }
}
)
                `
        }
        var objects = {
            modelname: {
                el: new input(config, {
                    no: 'modelname',
                    label: localization.en.modelname,
                    placeholder: "",
                    extraction: "NoPrefix|UseComma",
                    required: true,
                    type: "character",
                    overwrite: "dataset"
                })
            },
            folds: {
                el: new input(config, {
                    no: 'folds',
                    label: localization.en.folds,
                    placeholder: "",
                    allow_spaces:true,
                    extraction: "NoPrefix|UseComma",
                    required: true,
                    type: "numeric"
                })
            },
            content_var: { el: new srcVariableList(config, {action: "move"}) },
            dependent: {
                el: new dstVariable(config, {
                    label: localization.en.dependent,
                    no: "dependent",
                    filter: "String|Numeric|Date|Logical|Ordinal|Nominal|Scale",
                    extraction: "NoPrefix|UseComma|Enclosed",
                    required: true,
                    onselect_r: { levelOfInterest: "bivariateLevels(datasetName=c('{{dataset.name}}'),dependentVariable='{{value}}')" }
                }), r: ['{{ var | safe}}']
            },
            independent: {
                el: new dstVariableList(config, {
                    label: localization.en.independent,
                    no: "independent",
                    filter: "String|Numeric|Date|Logical|Ordinal|Nominal|Scale",
                    extraction: "NoPrefix|UseComma|Enclosed",
                    required: true,
                }), r: ['{{ var | safe}}']
            },
            label1: { el: new labelVar(config, { label: localization.en.label1, style:"mt-3",h: 6 }) },
            ModelType: {
                el: new comboBoxWithChilderen(config, {
                    no: 'ModelSelection',
                    nochild: 'combokid',
                    label: localization.en.ModelSelection,
                    multiple: false,
                    required: true,
                    extraction: "NoPrefix|UseComma",
                    options: [
                        { "name": "Adaboost Classification Trees", "value": ["adaboost"] },
                        { "name": "Bagged Logic Regression", "value": ["logicBag"] },
                        { "name": "Bayesian Ridge Regression", "value": ["bridge"] },
                        { "name": "Boosted Trees", "value": ["gbm", "xgbTree", "C5.0"] },
                        { "name": "Decision Trees", "value": ["C5.0Tree", "ctree", "rpart"] },
                        { "name": "K Nearest Neighbhors", "value": ["knn"] },
                        { "name": "Linear Regression", "value": ["lm", "lmStepAIC"] },
                        { "name": "Logistic Regression", "value": ["glm", "glmnet"] },
                        { "name": "Multi-variate Adaptive Regression Spline", "value": ["earth"] },
                        { "name": "Naive Bayes", "value": ["nb"] },
                        { "name": "Neural Network", "value": ["nnet", "neuralnet", "dnn", "mlp",] },
                        { "name": "Random Forest", "value": ["rf", "cforest", "ranger",] },
                        { "name": "Robust Linear Regression", "value": ["rlm", "xgbTree", "C5.0"] },
                        { "name": "Support Vector Machines", "value": ["svmLinear", "svmRadial", "svmPoly"] },
                    ]
                })
            },
            levelOfInterest: {
                el: new comboBox(config, {
                    no: 'levelOfInterest',
                    label: localization.en.levelOfInterest,
                    multiple: false,
                    extraction: "NoPrefix|UseComma",
                    options: [],
                    default: ""
                })
            },
        }
        const content = {
            head: [objects.modelname.el.content, objects.folds.el.content],
            left: [objects.content_var.el.content],
            right: [objects.dependent.el.content, objects.independent.el.content,],
            bottom: [objects.levelOfInterest.el.content, objects.label1.el.content, objects.ModelType.el.content],
            nav: {
                name: localization.en.navigation,
                icon: "icon-k_fold",
                modal: config.id
            }
        }
        super(config, objects, content);
        this.help = localization.en.help;
    }
}
module.exports.item = new kFoldCrossValidation().render()