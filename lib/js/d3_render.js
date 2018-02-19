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

    /**
     * Get the new element Id
     * @param svgElement
     * @returns {string}
     */
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
          l: D3Wrapper.createLine,
            lh: D3Wrapper.createLineHoriz,
            lv: D3Wrapper.createLineVert,
            pg: D3Wrapper.createPolygon,
            pl: D3Wrapper.createPolyline,
            t: D3Wrapper.createText,
            c: D3Wrapper.createCircle,
            el: D3Wrapper.createEllipse,
            r: D3Wrapper.createRect,
            cc: D3Wrapper.createCubicCurve,
            ac: D3Wrapper.createArc,
            pt: D3Wrapper.createPath,
            mk: D3Wrapper.createMarker
        }
    }

    /**
     * Create a Ui card / holder for svg
     * @param {Object} d3DivObjectOrElementId The DOM element object or the id
     *                      element of the object
     * @param {Dictionary} dmData The dictionary object
     * @param {String} dmId The key for the dictionary object which specifies then
     *                      name
     * @return {Object} The svg element object
     */
    function createUiForSvg(d3DivObjectOrElementId, dmData, dmId) {
      var size = dmData.sz;
      if (typeof d3DivObjectOrElementId == "string") {
          // Create a default card
          return UiUtility.createCardType1(d3DivObjectOrElementId,
            size[0], size[1], dmId);
      } else if (d3DivObjectOrElementId != null) {
          // d3 div object is passed
          return
              UiUtility.createSvg(d3DivObjectOrElementId,
                  size[0], size[1]);
      } else {
          // The object is null, then simply use an empty rendering object
          return
              UiUtility.createDivType1(d3DivObjectOrElementId,
                 size[0], size[1]);
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
        dmKey,
        dmData
    ) {
        var fnDmWrapper;

        if (dictSvgTypeToWrapper[dmKey] != null) {
            if (dmKey === 't' && optionsArg.displayText === false) return null;
            fnDmWrapper = dictSvgTypeToWrapper[dmKey];
        } else if (dmKey.match(/pt\d+/) != null) fnDmWrapper = D3Wrapper.createPath;
        else if(dmKey === "df") processDmDefsDataHelper(svgElement, dmData[dmKey]);

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
            // It is a single json object
            // {"Ar":<array_data_to_draw>, "st":<stroke>, "stw":<stroke_width>, ...}
            processDmJsonDataHelper(svgElement, dmType, fnDmWrapper, dmKey);
        }
    }

    /**
     * Process defs element
     * @param {Object} svgElement The svg element node
     * @param {Number} defsItem The data matrix defs data
     * @returns {Object} The d3 object which corresponds to the svg element
     */
    function processDmDefsDataHelper (
        svgElement,
        defsItem
    ) {
        // Create the defs object
        var defsObj = D3Wrapper.createDefs(svgElement);

        // Process Markers if exists
        if("mk" in defsItem) {
            processDmDefsMarkerDataHelper(defsObj, defsItem["mk"]);
        }
    }

    /**
     *
     * @param {Object} svgElement The svg element node
     * @param {Number} defsItem The data matrix defs data
     * @returns {Object} The d3 object which corresponds to the svg element
     */
    function processDmDefsMarkerDataHelper(
        defsElement,
        markerItem
    ) {
        var markerSvgObj = D3Wrapper.createMarker(defsElement, markerItem.atr);
        if(markerItem.dt) {
            processDataLoopofDmItem(markerSvgObj, markerItem.dt);
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
        // Create the svg item and attach to the svg element object
        var svgObject = fnDmWrapper(svgElement, dmType);

        // Set default Attributes
        D3Wrapper.setAttribute(svgObject, dmKey);

        // Append Id
        D3Wrapper.appendId(svgObject, DmCommon.generateSvgItemID());

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
            processDmDefsDataHelper(svgElement, dmType, fnDmWrapper,
                dmKey);
        }
    }

    /**
     *
     * @param svgElement
     * @param dmData
     */
    function processDataLoopofDmItem(
        svgElement,
        dmData
    ) {
        // Loop through the data matrix item json object
        for (var key in dmData) {
            if (dmData.hasOwnProperty(key)) {
                processDmData(svgElement, key, dmData);
            }
        }
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

        var svgElement = createUiForSvg(d3DivObjectOrElementId, dmData, dmId);

        // Loop through the data matrix item json object
        processDataLoopofDmItem(svgElement, dmData);

        return svgElement;
    }
};
