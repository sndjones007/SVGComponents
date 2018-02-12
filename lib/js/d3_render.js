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
var DMRender = new function () {

    // Initialize
    this.TopicSvgDMDict = {};
    this.SymbolDMDict = {};
    this.isArrwoheadCreated = false;
    this.counter = 0;
    this.katexCounter = 0;

    // Private methods
    var dictSvgTypeToWrapper;

    function initialize() {
        this.isArrwoheadCreated = false;
        this.counter = 0;
        this.katexCounter = 0;

        dictSvgTypeToWrapper = {
          "lh": D3Wrapper.createLineHoriz,
          "lv": D3Wrapper.createLineVert,
          "l": D3Wrapper.createLine,
          "△": D3Wrapper.createTriangle,
          "c": D3Wrapper.createCircle,
          "r": D3Wrapper.createRect,
          "t": D3Wrapper.createText,
          "cc": D3Wrapper.createCubicCurve,
          "ac": D3Wrapper.createArc,
          "pl": D3Wrapper.createPolyline
        }
    }

    /**
     * Process the svg element
     * @param {String} elementId The id of the element inside which the svg is
     *                           rendered
     * @param {String} dmId The data matrix type id
     * @param {Boolean} isDispText The flag which states if text is to be displayed
     * @returns {Object} The d3 object which corresponds to the svg element
     */
    this.processSymbolDm = function (symbolId, dmId, dmData, options) {
        if (dmData == null) {
            console.error("The Symbol Matrix Id '" + dmId + "' does not exists");
            return;
        }

        if (Array.isArray(dmData)) {
            for (var i = 0; i < dmData.length; ++i) {
                processSymbolDm_helper1(dmData[i], symbolId, options);
            }
        } else {
            processSymbolDm_helper1(dmData, symbolId, options);
        }
    };

    function processSymbolDm_helper1(dmData, symbolId, options) {
        if (dmData.hasOwnProperty("s") && options.isSvg) {
            var dmDataSvg = dmData["s"];
            var svgElement = UiUtility.createSvgDivCard(symbolId, dmDataSvg["⍬"][0], dmDataSvg["⍬"][1]);

            // Loop through the json object
            for (var key in dmDataSvg) {
                if (dmDataSvg.hasOwnProperty(key)) {
                    processDmData(svgElement, key, dmDataSvg, symbolId, options);
                }
            }
        }
        else {
            if (dmData["l"][1] === true) {
              this.katexCounter++;
              var elementD3 = UiUtility.getDivCard(symbolId);
              var elementDiv = elementD3.append("div").attr("id", symbolId + "#" + this.katexCounter);
              katex.render(dmData["l"][0], document.getElementById(symbolId + "#" + this.katexCounter));
            }
            else {
                var elementD3 = UiUtility.getDivCard(symbolId);
                elementD3.append("div").text(dmData["l"][0]).style("font-family", "KaTeX_Math")
                    .style("font-size", "16px");
            }
        }
    }

    /**
     * Process the Data matrix object with individual key,values
     * @param {Object} svgElement The svg element node
     * @param {String} dmKey The key element of the data matrix object
     * @param {Object} dmData The data matrix Object
     * @returns {Object} The d3 object
     */
    function processDmData(svgElement, dmKey, dmData, elementId, options) {
      var fnDmWrapper;

      if(dictSvgTypeToWrapper[dmKey] != null) {
        if(dmKey === 't' && options.displayText === false) return;
        fnDmWrapper = dictSvgTypeToWrapper[dmKey];
      } else if (dmKey.match(/pt\d+/) != null) fnDmWrapper = D3Wrapper.createPath;

      if(fnDmWrapper != null)
        processDmValueData(svgElement, dmData[dmKey],
              fnDmWrapper, dmKey, elementId, options);
    }

    /**
     * This method is the intermediate engine to process each element of the
     * svg object
     * @param {Object} svgElement The svg element node
     * @param {Number} dmType The data matrix type data
     * @param {Object} fnDmWrapper The D3Wrapper public method
     * @param {String} dmKey The data matrix object key string
     * @param {String} elementId The id of the element under which svg is
     *                          rendered
     * @param {Object} options The options data
     * @returns {Object} The d3 object which corresponds to the svg element
     */
    function processDmValueData(svgElement, dmType, fnDmWrapper, dmKey,
      elementId, options) {
        // Either the Data matrix type is an array or json with styles
        if (Array.isArray(dmType) && dmType[0].constructor === Array) {
            // The data is either an array of values which is passed to the
            // D3Wrapper method or it contains a json object
            // The json object structure is
            // {"Ar":<array_data_to_draw>, "st":<stroke>, "stw":<stroke_width>, ...}
            for(var i = 0; i < dmType.length; ++i) {
              if(dmType[i].constructor === Array) {
                // If the data item is array object then just pass it as parameter
                // to the D3Wrapper method
                processDmValueData_Helper1(svgElement, dmType[i],
                    fnDmWrapper, dmKey, elementId);
              } else {
                processDmValueData_Helper3(svgElement, dmType[i], fnDmWrapper,
                    dmKey, elementId);
              }
            }
        } else {
          processDmValueData_Helper3(svgElement, dmType, fnDmWrapper, dmKey, elementId);
        }
    }

    function processDmValueData_Helper1(svgElement, dmType, fnDmWrapper, dmKey, elementId) {
        var svgObject = fnDmWrapper(svgElement, dmType);
        if (dmKey !== 't') D3Wrapper.appendAttributeDefault1(svgObject);
        else D3Wrapper.updateFill(svgElement, "black");

        D3Wrapper.appendId(svgObject, getElementId(elementId));

        return svgObject;
    }

    function processDmValueData_Helper3(svgElement, dmType, fnDmWrapper, dmKey, elementId) {
      // The json object structure is
      // {"Ar":<array_data_to_draw>, "st":<stroke>, "stw":<stroke_width>, ...}
      if (dmType["Ar"] != null) {
          if (dmType["Ar"][0].constructor === Array) {
              for (var i = 0; i < dmType["Ar"].length; ++i) {
                  var svgObject = processDmValueData_Helper1(svgElement, dmType["Ar"][i], fnDmWrapper, dmKey, elementId);
                  processDmValueData_Helper2(svgElement, dmType, svgObject);
              }
          } else { // 1D array
              var svgObject = processDmValueData_Helper1(svgElement, dmType["Ar"], fnDmWrapper, dmKey, elementId);
              processDmValueData_Helper2(svgElement, dmType, svgObject);
          }
      } else {
        processDmValueData_Helper1(svgElement, dmType, fnDmWrapper,
          dmKey, elementId);
      }
    }

    function processDmValueData_Helper2(svgElement, dmType, svgObject) {
        if (dmType["st"] != null)
            D3Wrapper.updateStroke(svgObject, dmType["st"]);
        else if (dmType["stw"] != null)
            D3Wrapper.updateStrokeWidth(svgObject, dmType["stw"]);
        else if (dmType["fl"] != null)
            D3Wrapper.updateFill(svgObject, dmType["fl"]);
        else if (dmType["awh"] === true)
            processArrowhead(svgElement, svgObject);
    }

    /**
     * Process the array of data with attributes
     */
    function processArrowhead(svgElement, svgObject) {
        if (!isArrwoheadCreated) {
            D3Wrapper.createArrowHead(svgElement);
            isArrwoheadCreated = true;
        }

        D3Wrapper.appendArrowHead(svgElement, svgObject);
    }

    function getElementId(parentId) {
        counter++;
        return parentId + "#" + counter;
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
    this.process = function (elementId, dmId, isDispText, symbolId, isSvg, symbolDmId) {
        initialize();

        //DMRender.processTopicDm(elementId, dmId, this.TopicSvgDMDict[dmId], isDispText);

        initialize();
        symbolDmId = symbolDmId || dmId;
        DMRender.processSymbolDm(symbolId, symbolDmId, this.SymbolDMDict[symbolDmId], isSvg);
    };

    /**
     * Process the requsted data matrix for Topic images to be rendered
     * @param {String} d3DivObjectOrElementId The id of the element inside which
     *                           the svg is rendered OR the d3 elelemt object
     * @param {String} dmId The data matrix type id
     * @param {Boolean} options The flag which states if text is to be displayed
     * @returns {Object} The d3 object which corresponds to the svg element
     */
    this.processTopicImage = function(d3DivObjectOrElementId, dmId, options) {
        initialize();

        // Set default options
        if(options.displayText == null) options.displayText = false;

        // Get the object from data matrix dictionary using the key id
        var dmData = this.TopicSvgDMDict[dmId];

        // If the dm object is null
        if (dmData == null) {
            console.error("The Data Matrix Id '" + dmId + "' does not exists");
            return;
        }

        var svgElement;

        if(typeof d3DivObjectOrElementId == "string") {
            // Create a default card
            svgElement = UiUtility.createDivCard(d3DivObjectOrElementId, dmData["⍬"][0], dmData["⍬"][1], dmId);
        } else {
            // d3 div object is passed
            svgElement =
              UiUtility.createSvgFromd3(d3DivObjectOrElementId, dmData["⍬"][0], dmData["⍬"][1]);
        }

        // Loop through the data matrix item json object
        for (var key in dmData) {
            if (dmData.hasOwnProperty(key)) {

                processDmData(svgElement, key, dmData,
                    (typeof d3DivObjectOrElementId == "string")?
                        d3DivObjectOrElementId : d3DivObjectOrElementId.attr("id"), options);
            }
        }
    }

    /**
     * Process the requsted data matrix for Topic images to be rendered
     * @param {String} elementId The id of the element inside which the svg is
     *                           rendered
     * @param {String} dmId The data matrix type id
     * @param {Boolean} options The flag which states if text is to be displayed
     * @returns {Object} The d3 object which corresponds to the svg element
     */
    this.processSymbolImage = function(elementId, dmId, options) {
        initialize();

        if(options.isSvg == null) options.isSvg = false;
        if(options.displayText == null) options.displayText = false;

        var dmData = this.SymbolDMDict[dmId];
        if (dmData == null) {
            console.error("The Symbol Matrix Id '" + dmId + "' does not exists");
            return;
        }

        if (Array.isArray(dmData)) {
            for (var i = 0; i < dmData.length; ++i) {
                processSymbolDm_helper1(dmData[i], elementId, options);
            }
        } else {
            processSymbolDm_helper1(dmData, elementId, options);
        }
    }
};
