/**
  * This file is protected by copyright (c) 2023-2025 by BlueSky Statistics, LLC.
  * All rights reserved. The copy, modification, or distribution of this file is not
  * allowed without the prior written permission from BlueSky Statistics, LLC.
 */




class repkFoldCrossValidation extends baseModal {
    static dialogId = 'repkFoldCrossValidation'
    static t = baseModal.makeT(repkFoldCrossValidation.dialogId)

    constructor() {
        var config = {
            id: repkFoldCrossValidation.dialogId,
            label: repkFoldCrossValidation.t('title'),
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
 train_control <- caret::trainControl(method="repeatedcv", number={{selected.folds | safe}}, repeats={{selected.repeats | safe}})
  # train the model
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
   BSkyConfusionMatrixTrain (predictions = predictedValues, reference = TrainClasses, levelOfInterest = "{{selected.levelOfInterest | safe}}")
  }
  }
  ## save dependent independent var with model for scoring
  attr(.GlobalEnv\${{selected.modelname | safe}},"classDepVar")= class({{dataset.name}}[, c({{selected.dependent | safe}})])
  attr(.GlobalEnv\${{selected.modelname | safe}},"depVarSample")= sample({{dataset.name}}[, c({{selected.dependent | safe}})], size = 2, replace = TRUE)
  attr(.GlobalEnv\${{selected.modelname | safe}},"depvar")="{{selected.dependent | safe}}"
  attr(.GlobalEnv\${{selected.modelname | safe}},"indepvar")="c({{selected.independent | safe}})"
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
                    label: repkFoldCrossValidation.t('modelname'),
                    placeholder: "",
                    extraction: "NoPrefix|UseComma",
                    required: true,
                    type: "character",
                    overwrite: "dataset"
                })
            },
            label2: { el: new labelVar(config, { label: repkFoldCrossValidation.t('label2'),style:"mt-3", h: 6 }) },
            folds: {
                el: new input(config, {
                    no: 'folds',
                    allow_spaces:true,
                    label: repkFoldCrossValidation.t('folds'),
                    placeholder: "",
                    extraction: "NoPrefix|UseComma",
                    required: true,
                    allow_spaces:true,
                    type: "numeric"
                })
            },
            repeats: {
                el: new input(config, {
                    no: 'repeats',
                    allow_spaces:true,
                    label: repkFoldCrossValidation.t('repeats'),
                    placeholder: "",
                    extraction: "TextAsIs",
                    required: true,
                    type: "numeric"
                })
            },
            content_var: { el: new srcVariableList(config, {action: "move"}) },
            dependent: {
                el: new dstVariable(config, {
                    label: repkFoldCrossValidation.t('dependent'),
                    no: "dependent",
                    filter: "String|Numeric|Date|Logical|Ordinal|Nominal|Scale",
                    extraction: "NoPrefix|UseComma|Enclosed",
                    required: true,
                    onselect_r: { levelOfInterest: "bivariateLevels(datasetName=c('{{dataset.name}}'),dependentVariable='{{value}}')" }
                }), r: ['{{ var | safe}}']
            },
            independent: {
                el: new dstVariableList(config, {
                    label: repkFoldCrossValidation.t('independent'),
                    no: "independent",
                    filter: "String|Numeric|Date|Logical|Ordinal|Nominal|Scale",
                    extraction: "NoPrefix|UseComma|Enclosed",
                    required: true,
                }), r: ['{{ var | safe}}']
            },
            label1: { el: new labelVar(config, { label: repkFoldCrossValidation.t('label1'), h: 6 }) },
            ModelType: {
                el: new comboBoxWithChilderen(config, {
                    no: 'ModelSelection',
                    nochild: 'combokid',
                    label: repkFoldCrossValidation.t('ModelSelection'),
                    multiple: false,
                    required:true,
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
                        { "name": "Neural Network", "value": ["nnet", "neuralnet", "dnn", "mlp"] },
                        { "name": "Random Forest", "value": ["rf", "cforest", "ranger",] },
                        { "name": "Robust Linear Regression", "value": ["rlm", "xgbTree", "C5.0"] },
                        { "name": "Support Vector Machines", "value": ["svmLinear", "svmRadial", "svmPoly"] },
                    ]
                })
            },
            levelOfInterest: {
                el: new comboBox(config, {
                    no: 'levelOfInterest',
                    label: repkFoldCrossValidation.t('levelOfInterest'),
                    multiple: false,
                    extraction: "NoPrefix|UseComma",
                    options: [],
                    default: ""
                })
            },
        }
        const content = {
            head: [objects.modelname.el.content, objects.label2.el.content, objects.folds.el.content, objects.repeats.el.content,],
            left: [objects.content_var.el.content],
            right: [objects.dependent.el.content, objects.independent.el.content,],
            bottom: [objects.levelOfInterest.el.content,objects.label1.el.content, objects.ModelType.el.content],
            nav: {
                name: repkFoldCrossValidation.t('navigation'),
                icon: "icon-k_fold_repeated",
                modal: config.id
            }
        }
        super(config, objects, content);
        
        this.help = {
            title: repkFoldCrossValidation.t('help.title'),
            r_help: repkFoldCrossValidation.t('help.r_help'),  //r_help: "help(data,package='utils')",
            body: repkFoldCrossValidation.t('help.body')
        }
;
    }
}

module.exports = {
    render: () => new repkFoldCrossValidation().render()
}
