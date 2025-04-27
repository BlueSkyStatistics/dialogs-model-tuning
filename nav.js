/**
  * This file is protected by copyright (c) 2023-2025 by BlueSky Statistics, LLC.
  * All rights reserved. The copy, modification, or distribution of this file is not
  * allowed without the prior written permission from BlueSky Statistics, LLC.
 */

const {getT} = global.requireFromRoot("localization");
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
