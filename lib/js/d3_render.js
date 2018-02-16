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
    var isArrwoheadCreated = false, counter = 0, katexCounter = 0;
    var defsArrObj = [];
    var optionsArg = {};

    // Private methods
    var getElementId = function(svgElement) {
      var newId = UiUtility.getElementId(svgElement) + "-" + counter;
      counter++;
      return newId;
    };

    /**
     * Initialize the values required for rendering svg
     */
    function initialize() {
        isArrwoheadCreated = false;
        counter = 0;
        katexCounter = 0;
        defsArrObj = [];
        optionsArg = {};

        dictSvgTypeToWrapper = {
            "lh": D3Wrapper.createLineHoriz,
            "lv": D3Wrapper.createLineVert,
            "l": D3Wrapper.createLine,
            "pg": D3Wrapper.createPolygon,
            "c": D3Wrapper.createCircle,
            "el": D3Wrapper.createEllipse,
            "r": D3Wrapper.createRect,
            "t": D3Wrapper.createText,
            "cc": D3Wrapper.createCubicCurve,
            "ac": D3Wrapper.createArc,
            "pl": D3Wrapper.createPolyline
        }
    }

    /**
     * Process the Data matrix object with individual key,values
     * @param {Object} svgElement The svg element node
     * @param {String} dmKey The key element of the data matrix object
     * @param {Object} dmData The data matrix Object
     * @returns {Object} The d3 object
     */
    function processDmData(
        svgElement,
        dmKey
        ) {

        // Get the object from data matrix dictionary using the key id
        var dmData = this.TopicSvgDMDict[dmId];

        var fnDmWrapper;

        if (dictSvgTypeToWrapper[dmKey] != null) {
            if (dmKey === 't' && optionsArg.displayText === false) return null;
            fnDmWrapper = dictSvgTypeToWrapper[dmKey];
        } else if (dmKey.match(/pt\d+/) != null) fnDmWrapper = D3Wrapper.createPath;

        if (fnDmWrapper != null)
            processDmValueData(svgElement, dmData[dmKey],
                fnDmWrapper, dmKey);
    }

    /**
     * This method is the intermediate engine to process each element of the
     * svg object
     * @param {Object} svgElement The svg element node
     * @param {Number} dmType The data matrix type data
     * @param {Object} fnDmWrapper The D3Wrapper public method
     * @param {String} dmKey The data matrix object key string
     * @param {Object} options The options data
     * @returns {Object} The d3 object which corresponds to the svg element
     */
    function processDmValueData(
        svgElement,
        dmType,
        fnDmWrapper,
        dmKey
    ) {
        // Either the Data matrix type is an array or json with styles
        if (Array.isArray(dmType) && dmType[0].constructor === Array) {
            // The data is either an array of values which is passed to the
            // D3Wrapper method or it contains a json object
            // The json object structure is
            // "pl": [[10, 20, ...], {"Ar":<array_data_to_draw>, "st":<stroke>, "stw":<stroke_width>, ...}, ...]
            for (var i = 0; i < dmType.length; ++i) {
                if (dmType[i].constructor === Array) {
                    // If the data item is array object then just pass it as parameter
                    // to the D3Wrapper method
                    // [10, 20, ...]
                    processDmNormalDataHelper(svgElement, dmType[i],
                        fnDmWrapper, dmKey);
                } else {
                    // {"Ar":<array_data_to_draw>, "st":<stroke>, "stw":<stroke_width>, ...}
                    processDmJsonDataHelper(svgElement, dmType[i], fnDmWrapper,
                        dmKey);
                }
            }
        } else if (Array.isArray(dmType) && dmType[0].constructor !== Array){
            // [10, 20, ...]
            processDmNormalDataHelper(svgElement, dmType,
                fnDmWrapper, dmKey);
        } else {
            // It is a single json object {"Ar":<array_data_to_draw>, "st":<stroke>, "stw":<stroke_width>, ...}
            processDmJsonDataHelper(svgElement, dmType, fnDmWrapper, dmKey);
        }
    }

    /**
     * This helper method parses a list of data for a data matrix type of the
     * form normally, [10, 20, ...]
     * @param {Object} svgElement The svg element node
     * @param {Number} dmType The data matrix type data
     * @param {Object} fnDmWrapper The D3Wrapper public method
     * @param {String} dmKey The data matrix object key string
     * @returns {Object} The d3 object which corresponds to the svg element
     */
    function processDmNormalDataHelper(
        svgElement,
        dmType,
        fnDmWrapper,
        dmKey
    ) {
        // Create the svg item and atatch to the svg element object
        var svgObject = fnDmWrapper(svgElement, dmType);

        // Set default Attributes
        D3Wrapper.setAttribute(svgObject, dmKey);

        // Append Id
        D3Wrapper.appendId(svgObject, getElementId(svgElement));

        return svgObject;
    }

    /**
     * This helper method parses a list of data for a data matrix type of the
     * form normally, {"Ar":<array_data_to_draw>, "st":<stroke>, "stw":<stroke_width>, ...}
     * @param {Object} svgElement The svg element node
     * @param {Number} dmType The data matrix type data
     * @param {Object} fnDmWrapper The D3Wrapper public method
     * @param {String} dmKey The data matrix object key string
     * @returns {Object} The d3 object which corresponds to the svg element
     */
    function processDmJsonDataHelper(
      svgElement,
      dmType,
      fnDmWrapper,
      dmKey
    ) {
        // The json object structure is
        // {"Ar":<array_data_to_draw>, "st":<stroke>, "stw":<stroke_width>, ...}
        if (dmType["Ar"] != null) {
            if (dmType["Ar"][0].constructor === Array) {
                for (var i = 0; i < dmType["Ar"].length; ++i) {
                    var svgObject = processDmNormalDataHelper(svgElement,
                      dmType["Ar"][i], fnDmWrapper, dmKey);
                }
            } else { // 1D array
                var svgObject = processDmNormalDataHelper(svgElement,
                    dmType["Ar"], fnDmWrapper, dmKey);
            }
        } else {
            processDmValueData_Helper1(svgElement, dmType, fnDmWrapper,
                dmKey);
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
     * Process the requsted data matrix for Topic images to be rendered
     * @param {String} d3DivObjectOrElementId The id of the element inside which
     *                           the svg is rendered OR the d3 elelemt object
     * @param {String} dmId The data matrix type id
     * @param {Boolean} options The flag which states if text is to be displayed
     * @returns {Object} The d3 object which corresponds to the svg element
     */
    this.processTopicImage = function (d3DivObjectOrElementId, dmId, options) {
        // Initialize
        initialize();

        // Get the object from data matrix dictionary using the key id
        var dmData = this.TopicSvgDMDict[dmId];

        // If the dm object is null
        if (dmData == null) {
            console.error("The Data Matrix Id '" + dmId + "' does not exists");
            return null;
        }

        // Set default options
        optionsArg = options;
        if (optionsArg.displayText == null) optionsArg.displayText = false;

        var svgElement;

        if (typeof d3DivObjectOrElementId == "string") {
            // Create a default card
            svgElement = UiUtility.createCardType1(d3DivObjectOrElementId,
              dmData["⍬"][0], dmData["⍬"][1], dmId);
        } else if (d3DivObjectOrElementId != null) {
            // d3 div object is passed
            svgElement =
                UiUtility.createSvg(d3DivObjectOrElementId,
                    dmData["⍬"][0], dmData["⍬"][1]);
        } else {
            // The object is null, then simply use an empty rendering object
            svgElement =
                UiUtility.createDivType1(d3DivObjectOrElementId,
                   dmData["⍬"][0], dmData["⍬"][1]);
        }

        // Loop through the data matrix item json object
        for (var key in dmData) {
            if (dmData.hasOwnProperty(key)) {
                processDmData(svgElement, key);
            }
        }

        return svgElement;
    }

    /**
     * Process the requsted data matrix for Topic images to be rendered
     * @param {String} elementId The id of the element inside which the svg is
     *                           rendered
     * @param {String} dmId The data matrix type id
     * @param {Boolean} options The flag which states if text is to be displayed
     * @returns {Object} The d3 object which corresponds to the svg element
     */
    // this.processSymbolImage = function(elementId, dmId, options) {
    //     initialize();
    //
    //     if(options.isSvg == null) options.isSvg = false;
    //     if(options.displayText == null) options.displayText = false;
    //
    //     var dmData = this.SymbolDMDict[dmId];
    //     if (dmData == null) {
    //         console.error("The Symbol Matrix Id '" + dmId + "' does not exists");
    //         return;
    //     }
    //
    //     if (Array.isArray(dmData)) {
    //         for (var i = 0; i < dmData.length; ++i) {
    //             processSymbolDm_helper1(dmData[i], elementId, options);
    //         }
    //     } else {
    //         processSymbolDm_helper1(dmData, elementId, options);
    //     }
    // }
    //
    // /**
    //  * Process the svg element
    //  * @param {String} elementId The id of the element inside which the svg is
    //  *                           rendered
    //  * @param {String} dmId The data matrix type id
    //  * @param {Boolean} isDispText The flag which states if text is to be displayed
    //  * @returns {Object} The d3 object which corresponds to the svg element
    //  */
    // this.processSymbolDm = function (symbolId, dmId, dmData, options) {
    //     if (dmData == null) {
    //         console.error("The Symbol Matrix Id '" + dmId + "' does not exists");
    //         return;
    //     }
    //
    //     if (Array.isArray(dmData)) {
    //         for (var i = 0; i < dmData.length; ++i) {
    //             processSymbolDm_helper1(dmData[i], symbolId, options);
    //         }
    //     } else {
    //         processSymbolDm_helper1(dmData, symbolId, options);
    //     }
    // };
    //
    // function processSymbolDm_helper1(dmData, symbolId, options) {
    //     if (dmData.hasOwnProperty("s") && options.isSvg) {
    //         var dmDataSvg = dmData["s"];
    //         var svgElement = UiUtility.createSvgDivCard(symbolId, dmDataSvg["⍬"][0], dmDataSvg["⍬"][1]);
    //
    //         // Loop through the json object
    //         for (var key in dmDataSvg) {
    //             if (dmDataSvg.hasOwnProperty(key)) {
    //                 processDmData(svgElement, key, dmDataSvg, symbolId, options);
    //             }
    //         }
    //     }
    //     else {
    //         if (dmData["l"][1] === true) {
    //           this.katexCounter++;
    //           var elementD3 = UiUtility.getDivCard(symbolId);
    //           var elementDiv = elementD3.append("div").attr("id", symbolId + "#" + this.katexCounter);
    //           katex.render(dmData["l"][0], document.getElementById(symbolId + "#" + this.katexCounter));
    //         }
    //         else {
    //             var elementD3 = UiUtility.getDivCard(symbolId);
    //             elementD3.append("div").text(dmData["l"][0]).style("font-family", "KaTeX_Math")
    //                 .style("font-size", "16px");
    //         }
    //     }
    // }
};
