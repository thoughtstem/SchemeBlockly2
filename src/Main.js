"use strict";


exports.getBlockly = function(x){
  var xml_dom = Blockly.Xml.workspaceToDom(Scheme.workspace)
  var xml = Blockly.Xml.domToText(xml_dom)
  console.log("GETTING BLOCKLY", xml)
  return xml
}

var last_set_xml = ""
exports.setBlockly = function(xml) {
   if(!window["Scheme"]) return ""

    //if(xml == last_set_xml) return ""
    //xml = last_set_xml

    console.log("SETTING BLOCKLY", xml)


    Scheme.workspace.clear()

    var dom = Blockly.Xml.textToDom(xml)
    console.log(dom)
    Blockly.Xml.domToWorkspace(dom, Scheme.workspace)
    
    return xml
}

exports.setBlocklyToolbox = function(xml) {
  console.log("Setting Toolbox")

    document.getElementById("functions_tab").innerHTML = xml

   if(!window["Scheme"]) init()

   return xml
}

  window["init"] = function(){

    Blockly.Blocks["super_block"] = {
      // x variable getter.
      init: function() {
        this.setMutator(new Blockly.Mutator(['text_create_join_item']));

        this.setOutput(true)

        this.appendDummyInput()
                .appendField(new Blockly.FieldTextInput( this.block_type_ || "default"), "NAME");
      },

    mutationToDom: function() {
      var container = document.createElement('mutation');
      container.setAttribute('items', this.itemCount_);
      container.setAttribute('block_type', this.block_type_);
      container.setAttribute('color', parseInt(this.color_));
      container.setAttribute('value', this.value_ || "undefined");
      return container;
    },
    domToMutation: function(xmlElement) {
      this.itemCount_ = parseInt(xmlElement.getAttribute('items'), 10);
      this.block_type_ = xmlElement.getAttribute('block_type')
      this.color_ = xmlElement.getAttribute('color')
      this.value_ = xmlElement.getAttribute('value')
      this.updateShape_();
    },
    decompose: function(workspace) {
      var containerBlock = workspace.newBlock('text_create_join_container');
      containerBlock.initSvg();
      var connection = containerBlock.getInput('STACK').connection;
      for (var i = 0; i < this.itemCount_; i++) {
        var itemBlock = workspace.newBlock('text_create_join_item');
        itemBlock.initSvg();
        connection.connect(itemBlock.previousConnection);
        connection = itemBlock.nextConnection;
      }
      return containerBlock;
    },
    compose: function(containerBlock) {

      var itemBlock = containerBlock.getInputTargetBlock('STACK');
      // Count number of inputs.
      var connections = [];
      while (itemBlock) {
        connections.push(itemBlock.valueConnection_);
        itemBlock = itemBlock.nextConnection &&
            itemBlock.nextConnection.targetBlock();
      }
      // Disconnect any children that don't belong.
      for (var i = 0; i < this.itemCount_; i++) {
        var connection = this.getInput('OPERAND' + i).connection.targetConnection;
        if (connection && connections.indexOf(connection) == -1) {
          connection.disconnect();
        }
      }
      this.itemCount_ = connections.length;
      this.updateShape_();
      // Reconnect any child blocks.
      for (var i = 0; i < this.itemCount_; i++) {
        Blockly.Mutator.reconnect(connections[i], this, 'OPERAND' + i);
      }
    },
    updateShape_: function() {
      if(this.value_.length > 0)
        this.setFieldValue(this.value_, "NAME")
      else
        this.setFieldValue(this.block_type_, "NAME")

      this.setColour(this.color_)

      if (this.itemCount_ && this.getInput('EMPTY')) {
        this.removeInput('EMPTY');
      } 

      // Add new inputs.
      for (var i = 0; i < this.itemCount_; i++) {
        if (!this.getInput('OPERAND' + i)) {
          var input = this.appendValueInput('OPERAND' + i);
         
          if (i == 0) {
            input.appendField();
          }
        }
      }
      // Remove deleted inputs.
      while (this.getInput('OPERAND' + i)) {
        this.removeInput('OPERAND' + i);
        i++;
      }
    }
   
  }


  var Scheme = {};
  window["Scheme"] = Scheme

  Scheme.workspace = null;

  Scheme.drawVisualization = function() {
    if(document.getElementById("scheme") == document.activeElement ) return
   

    document.getElementById("set_from_blockly").click()
  };

  Scheme.resize = function() {
    var width = Math.max(window.innerWidth - 440, 250);
    document.getElementById('blocklyDiv').style.width = width + 'px';
    Blockly.svgResize(Scheme.workspace);
  };

  Scheme.init = function() {


    Scheme.workspace = Blockly.inject('blocklyDiv',
        {collapse: false,
         disable: false,
         media: '../../media/',
         toolbox: document.getElementById('toolbox')});

    Scheme.workspace.clearUndo();
    Scheme.workspace.addChangeListener(Scheme.drawVisualization);
    Scheme.resize();
  };

  //window.addEventListener('load', Scheme.init);
  window.addEventListener('resize', Scheme.resize);


  Scheme.init()

}
