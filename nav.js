let t = getT('menutoolbar')
const nav = () => ({
    "name": t('modeltuning_top_level_title'),// {ns: 'menutoolbar'}),
    "tab": "model_tuning",
    "buttons": [
        "./bootstrapResampling",
        "./kFoldCrossValidation",
        "./leaveOneOutCrossValidation",
        "./repkFoldCrossValidation"
    ]
})

module.exports = {
    nav: nav(),
    render: () => nav()
}
