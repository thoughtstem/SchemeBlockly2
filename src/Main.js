"use strict";

var React = require("react")

var ReactAce = require('react-ace-editor')



var Editor = React.createClass({
  componentDidMount: function(){
    this.ace.editor.on("focus", this.props.onFocus)
    this.ace.editor.setValue(this.props.text)
  },
  shouldComponentUpdate: function(nextProps, nextState){
    return nextProps.should_update
  },
  componentWillUpdate:function(nextProps,nextState){
    if(nextProps.text == this.props.text || nextProps.text == this.last_text || nextProps.text == "" && nextProps.text == undefined || !nextProps.should_update) return

    this.last_text = nextProps.text


    this.ace.editor.setValue(nextProps.text, 0)
  },
  onChange: function(ev){
    var val = this.ace.editor.getValue()

    console.log("EDITOR CHANGE", val)

    if(val == this.last_text) return 

    if(val && val.length != 0)
      this.props.onChange({target: {value: val}})
  },
  render: function(){
    return React.createElement(ReactAce,{mode: "scheme", theme: "eclipse", setBehavioursEnabled: false, setReadOnly: false, onChange: this.onChange, ref: function(instance){ this.ace = instance }.bind(this)})
  }
})

exports.editorComponent_ = Editor

var BlocklyWrapper = React.createClass({
  resize: function(){
    var width = Math.max(window.innerWidth - 440, 250);
    document.getElementById('blocklyDiv').style.width = width + 'px';
    Blockly.svgResize(this.workspace);
  },
  blocklyChanged: function(ev) {

    if(ev.workspaceId){//It's a blockly changed event
      var xml_dom = Blockly.Xml.workspaceToDom(this.workspace)
      var xml = Blockly.Xml.domToText(xml_dom)
    } else {
      var xml = ev.nativeEvent.target.value
    }

    if(xml == this.last_text) return
    if(xml == this.last_sent_text) return

    this.last_sent_text = xml

    console.log("BLOCKLY CHANGE", xml)
    this.props.onChange({target: {value: xml}})
  },
  componentDidMount: function(){
    document.getElementById("toolbox").innerHTML = this.props.toolboxXML
    document.getElementById("blocklyDiv").onclick = this.props.onFocus

    initBlocklySuperBlock()

    this.workspace = Blockly.inject('blocklyDiv',
        {collapse: false,
         disable: false,
         media: '../../media/',
         toolbox: document.getElementById('toolbox')});

    this.workspace.clearUndo();
    this.workspace.addChangeListener(this.blocklyChanged);
    this.resize()
    window.addEventListener('resize', this.resize)

    window.workspace = this.workspace
  },
  shouldComponentUpdate: function(nextProps, nextState){
    return nextProps.should_update
  },
  componentWillUpdate: function(props,state){

    if(props.text == this.props.text){
      return
    }

    this.last_text = props.text
    //console.log("BLOCKLY SET", props.text)

    this.workspace.clear()
    var dom = Blockly.Xml.textToDom(props.text)
    Blockly.Xml.domToWorkspace(dom, this.workspace)  
  },
  render: function(){
    return React.createElement("textarea", {style: {display: "none"}, onFocus: this.props.onFocus, onChange: this.blocklyChanged, value: this.props.text},this.props.text)
  }
})

exports.blocklyComponent_ = BlocklyWrapper


function initBlocklySuperBlock(){
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
}
