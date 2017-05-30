"use strict";

exports.setBlockly = function(xml) {
   if(!window["Scheme"]) return ""

    Scheme.workspace.clear()

    var dom = Blockly.Xml.textToDom(xml)
    console.log(dom)
    Blockly.Xml.domToWorkspace(dom, Scheme.workspace)
    
    return xml
};
