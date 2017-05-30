"use strict";

exports.getBlockly = function(x){
  var xml_dom = Blockly.Xml.workspaceToDom(Scheme.workspace)
  var xml = Blockly.Xml.domToText(xml_dom)
  console.log("GETTING BLOCKLY", xml)
  return xml
}

exports.setBlockly = function(xml) {
   if(!window["Scheme"]) return ""

    Scheme.workspace.clear()

    var dom = Blockly.Xml.textToDom(xml)
    console.log(dom)
    Blockly.Xml.domToWorkspace(dom, Scheme.workspace)
    
    return xml
}

window["init"] = function(){
  Blockly.Blocks["raw"] = {
    // x variable getter.
    init: function() {
      this.jsonInit(
        {
          "type": "block_type",
          "message0": "%1",
          "args0": [
            {
              "type": "field_input",
              "name": "NAME",
              "text": "x"
            }
          ],
          "output": null,
          "tooltip": "",
          "helpUrl": ""
        }
      );
    }
  };

  Blockly.JavaScript['raw'] = function(block) {
    var text_name = block.getFieldValue('NAME');
    var code = text_name;
    return [code, Blockly.JavaScript.ORDER_NONE];
  };

function colorFromType(s){
  var types = {"Number": 230, "String": 160, "Image": 290, "Boolean": 210}

  return types[s]
}

function shadowBlockFromType(s){
  var types = {
    "Number": '<shadow type="math_number"></shadow>', 
    "Boolean": '<shadow type="logic_boolean"></shadow>', 
    "String": '<shadow type="text"></shadow>', 
    "Image": '<shadow type="circle"></shadow>'}

  return types[s]
}


function setupFromPredicates(predicates){
  Scheme.predicates = predicates

  for(var i in predicates){
    var predicate = predicates[i]
    makeBlock(predicate)
  }

  Scheme.init()
}


function makeBlock(predicate){

  var input_shadows = []


  if(predicate.inputs){
    for(var i in predicate.inputs){
      var input = predicate.inputs[i]

      var shadow = shadowBlockFromType(input)

      if(shadow)
        input_shadows.push('<value name="OPERAND'+i+'">'+shadow+'</value>')

    }
  }

  var block = '<block type="'+predicate.name+'">'+input_shadows.join("")+'</block>'


  if(predicate.inputs){
    var done = []
    for(var i in predicate.inputs){
      var input = predicate.inputs[i]

      if(input && done.indexOf(input) == -1){
        document.getElementById(input.toLowerCase()+"_inputting_functions_tab").innerHTML += block 
        done.push(input)
      }
    }
  }

  if(predicate.output)
    document.getElementById(predicate.output.toLowerCase()+"_outputting_functions_tab").innerHTML += block
  else
    document.getElementById("other_outputting_functions_tab").innerHTML += block



  Blockly.Blocks[predicate.name] = {
    // x variable getter.
    init: function() {
      this.itemCount_ = predicate.inputs ? predicate.inputs.length : 0
      this.setMutator(new Blockly.Mutator(['text_create_join_item']));

      if(!predicate.operator){
        this.appendDummyInput()
                .appendField(new Blockly.FieldTextInput("default"), "NAME");
      } else {
        this.appendDummyInput()
                .appendField(predicate.operator, "NAME");
      }

      for(var i = 0; i < this.itemCount_; i++){
        this.appendValueInput("OPERAND"+i).setCheck(predicate.inputs[i]);
      }

      this.setOutput(true, predicate.output);

      var color = colorFromType(predicate.output) 

      if(color)
        this.setColour(color);
    },

  mutationToDom: function() {
    var container = document.createElement('mutation');
    container.setAttribute('items', this.itemCount_);
    return container;
  },
  domToMutation: function(xmlElement) {
    this.itemCount_ = parseInt(xmlElement.getAttribute('items'), 10);
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
    if (this.itemCount_ && this.getInput('EMPTY')) {
      this.removeInput('EMPTY');
    } 

    // Add new inputs.
    for (var i = 0; i < this.itemCount_; i++) {
      if (!this.getInput('OPERAND' + i)) {
        var input = this.appendValueInput('OPERAND' + i);
       
        if(predicate.inputs){
          if(predicate.inputs[i]) 
            input.setCheck(predicate.inputs[i])
          else
            input.setCheck(predicate.inputs[predicate.inputs.length - 1]) //When adding arbitrary new inputs, assume the type is the same as the last specified type ["Number", "Number" ....]
        }
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
 
  };

  Blockly.JavaScript[predicate.name] = function(block) {
    if(!predicate.operator)
      var text_name = block.getFieldValue('NAME');
    else
      var text_name = predicate.operator
  

    var elements = new Array(block.itemCount_);
    for (var i = 0; i < block.itemCount_; i++) {
      elements[i] = Blockly.JavaScript.valueToCode(block, 'OPERAND' + i, Blockly.JavaScript.ORDER_NONE)
    }

                
    var code = "("+text_name+" "+elements.join(" ")+")";

    return [code, Blockly.JavaScript.ORDER_NONE];
  };
}

var Scheme = {};
window["Scheme"] = Scheme

Scheme.workspace = null;

Scheme.drawVisualization = function() {
  if(document.getElementById("scheme") == document.activeElement ) return
 
  document.getElementById("set_from_blockly").click()
};

function doEval(){
  var formula = Blockly.JavaScript.workspaceToCode(Scheme.workspace).replace(/'/g, "\"").replace(/;/g, "\n")

  var onError = function(e){ console.error(e); }
  var biwa = new BiwaScheme.Interpreter(onError)
  biwa.evaluate(formula.replace(), function(result) {
    //document.getElementById("output").innerHTML = result
    //console.log(result);
    render(result)
  });
}

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

  // When Blockly changes, update the graph.
  Scheme.workspace.addChangeListener(Scheme.drawVisualization);
  Scheme.resize();
};

//window.addEventListener('load', Scheme.init);
window.addEventListener('resize', Scheme.resize);

function blockNameFromOperator(op){
  var b = Scheme.predicates.filter(function(p){
    return p.operator == op
  })[0]

  if(b) return b.name

  return "raw_function"
}



var htdp_predicates = [
  {name: "raw_function"},

  {name: "define", operator: "define", inputs: [null,null]},
  {name: "if", operator: "if", inputs: ["Boolean",null,null]},

  {name: "string-append", operator: "string-append", inputs: ["String", "String"], output: "String"},

  {name: "plus", operator: "+", inputs: ["Number", "Number"], output: "Number"},
  {name: "minus", operator: "-", inputs: ["Number", "Number"], output: "Number"},
  {name: "times", operator: "*", inputs: ["Number", "Number"], output: "Number"},
  {name: "divide", operator: "/", inputs: ["Number", "Number"], output: "Number"},

  {name: ">", operator: ">", inputs: ["Number", "Number"], output: "Boolean"},
  {name: "<", operator: "<", inputs: ["Number", "Number"], output: "Boolean"},
  {name: "<=", operator: "<=", inputs: ["Number", "Number"], output: "Boolean"},
  {name: ">=", operator: ">=", inputs: ["Number", "Number"], output: "Boolean"},

  {name: "above", operator: "above", inputs: ["Image", "Image"],output: "Image"},
  {name: "beside", operator: "beside", inputs: ["Image", "Image"], output: "Image"},

  {name: "circle", operator: "circle", inputs: ["Number", "String", "String"], output: "Image"},
  {name: "rectangle", operator: "rectangle", inputs: ["Number", "Number", "String", "String"], output: "Image"},

  {name: "not", operator: "not", inputs: ["Boolean"], output: "Boolean"},
  {name: "and", operator: "and", inputs: ["Boolean", "Boolean"], output: "Boolean"},
  {name: "or", operator: "or", inputs: ["Boolean", "Boolean"], output: "Boolean"},
] 

var basic_predicates = [
  {name: "raw_function"},

  {name: "define", operator: "define", inputs: [null,null]},
] 


//Implementing HTDP functions...

BiwaScheme.define_libfunc("circle", 3, 3, function(ar){
  return new fabric.Circle({  radius: ar[0], fill: ar[2] })
});

BiwaScheme.define_libfunc("rectangle", 4, 4, function(ar){
  return new fabric.Rect({  width: ar[0], height: ar[1], fill: ar[3] })
});

BiwaScheme.define_libfunc("above", 2, 2, function(ar){
  var first = fabric.util.object.clone(ar[0])
  var second = fabric.util.object.clone(ar[1])

  second.set({top: first.getHeight()/2 + second.getHeight()/2, left:0})

  var g = new fabric.Group([first, second], {top: 0, left: 0})


  return g
});

BiwaScheme.define_libfunc("beside", 2, 2, function(ar){
  var first = fabric.util.object.clone(ar[0])
  var second = fabric.util.object.clone(ar[1])

  second.set({left: first.getWidth()/2 + second.getWidth()/2, top:0})

  var g = new fabric.Group([first, second], {top: 0, left: 0})


  return g
});


function render(shape){
  shape.set({top: shape.get('top') + 100})
  shape.set({left: shape.get('left') + 100})

  var canvas = new fabric.StaticCanvas('output');

  canvas.add(shape)
}



setupFromPredicates(htdp_predicates)

}
