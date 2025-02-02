
class bootstrapResampling extends baseModal {
    static dialogId = 'bootstrapResampling'
    static t = baseModal.makeT(bootstrapResampling.dialogId)

    constructor() {
        var config = {
            id: bootstrapResampling.dialogId,
            label: bootstrapResampling.t('title'),
            modalType: "two",
            RCode: `
require(caret)
{{selected.modelname | safe}}=character(0)
local (
{
 #load package for selected model like nb , rf 
 modelselected = "{{selected.combokid | safe}}"    
 success = loadReqModelPackage(modelselected)
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
  train_control <- caret::trainControl(method="boot", number={{selected.iterator | safe}})
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
                    label: bootstrapResampling.t('modelname'),
                    placeholder: "",
                    extraction: "NoPrefix|UseComma",
                    required: true,
                    type: "character",
                    overwrite: "dataset"
                })
            },
            iterator: {
                el: new input(config, {
                    no: 'iterator',
                    label: bootstrapResampling.t('iterator'),
                    placeholder: "",
                    extraction: "NoPrefix|UseComma",
                    allow_spaces: true,
                    required: true,
                    type: "numeric"
                })
            },
            content_var: { el: new srcVariableList(config, { action: "move" }) },
            dependent: {
                el: new dstVariable(config, {
                    label: bootstrapResampling.t('dependent'),
                    no: "dependent",
                    filter: "String|Numeric|Date|Logical|Ordinal|Nominal|Scale",
                    extraction: "NoPrefix|UseComma|Enclosed",
                    required: true,
                    onselect_r: { levelOfInterest: "bivariateLevels(datasetName=c('{{dataset.name}}'),dependentVariable='{{value}}')" }
                }), r: ['{{ var | safe}}']
            },
            independent: {
                el: new dstVariableList(config, {
                    label: bootstrapResampling.t('independent'),
                    no: "independent",
                    filter: "String|Numeric|Date|Logical|Ordinal|Nominal|Scale",
                    extraction: "NoPrefix|UseComma|Enclosed",
                    required: true,
                }), r: ['{{ var | safe}}']
            },
            label1: { el: new labelVar(config, { label: bootstrapResampling.t('label1'), style: "mt-3", h: 6 }) },
            ModelType: {
                el: new comboBoxWithChilderen(config, {
                    no: 'ModelSelection',
                    nochild: 'combokid',
                    label: bootstrapResampling.t('ModelSelection'),
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
                    label: bootstrapResampling.t('levelOfInterest'),
                    multiple: false,
                    extraction: "NoPrefix|UseComma",
                    options: [],
                    default: ""
                })
            },
        }
        const content = {
            head: [objects.modelname.el.content, objects.iterator.el.content],
            left: [objects.content_var.el.content],
            right: [objects.dependent.el.content, objects.independent.el.content,],
            bottom: [objects.levelOfInterest.el.content, objects.label1.el.content, objects.ModelType.el.content],
            nav: {
                name: bootstrapResampling.t('navigation'),
                icon: "icon-boot",
                modal: config.id
            }
        }
        super(config, objects, content);
        
        this.help = {
            title: bootstrapResampling.t('help.title'),
            r_help: bootstrapResampling.t('help.r_help'),  //r_help: "help(data,package='utils')",
            body: bootstrapResampling.t('help.body')
        }
;
    }
}

module.exports = {
    render: () => new bootstrapResampling().render()
}
