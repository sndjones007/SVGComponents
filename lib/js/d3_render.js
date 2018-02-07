/*******************************************************************************
 * The MIT License
 *
 * Copyright � 2018-2021 Subhadeep Niogi <sndjones007@gmail.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the �Software�), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED �AS IS�, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 ******************************************************************************/

/**
 * This class executes and creates the svg items using the data matrix
 * @class
 */
var DMRender = (function(){

  // Initialize
  function DMRender() {};

  // Private methods

  /**
   * Add Id to SVG element
   * @param {Object} svgElement The svg element node
   * @param {Number} dmType The data matrix type data
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  function processLineHoriz(svgElement, dmType) {
    // Either the Data matrix type is an array or json with styles
    if(Array.isArray(dmType)) {
      for(var i = 0; i < dmType.length; ++i) {
        var lh = dmType[i];
        D3Wrapper.createLineHoriz(svgElement, lh[0], lh[1], lh[2]);
      }
    }
  }

  // Public methods

  /**
   * Process the svg element
   * @param {String} elementId The id of the element inside which the svg is
   *                           rendered
   * @param {String} dmId The data matrix type id
   * @param {Boolean} isDispText The flag which states if text is to be displayed
   * @returns {Object} The d3 object which corresponds to the svg element
  */
  DMRender.prototype.process = function(elementId, dmId, isDispText) {
    var dmData = gateDMDict[dmId];

    if(typeof dmdata == 'undefined') {
      console.error("The Data Matrix Id '" + dmId + "' does not exixts");
      return;
    }

    var svgElement = UiUtility.createDivCard(elementId, dmData.⍬[0], dmData.⍬[1]);

    // Loop through the json object
    for(var key in dmData) {
      if (dmData.hasOwnProperty(key)) {
        var dmType = dmData[key];

        if(dmType === 'lh') processLineHoriz(svgElement, dmType);
        else if(dmType === 'lv') processLineVert(svgElement, dmType);
      }
    }
  };

})();





function (name, id, isdisptext) {
    var gate_dm_table = gate_data_matrix_dict[id];
    if (typeof gate_dm_table == 'undefined') return;
    var svg_container = create_div_card(name, gate_dm_table[0][0], gate_dm_table[0][1]);
    for (var i = 1; i < gate_dm_table.length; ++i) {
        var id_attr = name + "-" + gate_dm_table[i][1];
        if (gate_dm_table[i][0] === "l") {
            d3_create_line(svg_container, gate_dm_table[i][2], gate_dm_table[i][3], gate_dm_table[i][4], gate_dm_table[i][5], id_attr);
        }
        else if (gate_dm_table[i][0] === "c") {
            d3_create_circle(svg_container, gate_dm_table[i][2], gate_dm_table[i][3], gate_dm_table[i][4], "none", id_attr);
        }
        else if (gate_dm_table[i][0] === "t" && isdisptext) {
            d3_create_text(svg_container, gate_dm_table[i][2], gate_dm_table[i][3], gate_dm_table[i][4], "sans-serif", gate_dm_table[i][5], id_attr);
        }
        else if (gate_dm_table[i][0] === "a") {
            d3_create_path(svg_container, get_arc_path(gate_dm_table[i][2], gate_dm_table[i][3], gate_dm_table[i][4], gate_dm_table[i][5],
                gate_dm_table[i][6], gate_dm_table[i][7], gate_dm_table[i][8], gate_dm_table[i][9], gate_dm_table[i][10]), id_attr);
        }
    }
}
