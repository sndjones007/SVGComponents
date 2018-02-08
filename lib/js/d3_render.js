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
    this.DMDict = {};
    this.isArrwoheadCreated = false;
    this.counter = 0;

    // Private methods

    function initialize() {
        this.isArrwoheadCreated = false;
        this.counter = 0;
    }

    /**
     * Process the Data matrix object with individual key,values
     * @param {Object} svgElement The svg element node
     * @param {String} dmKey The key element of the data matrix object
     * @param {Object} dmData The data matrix Object
     * @returns {Object} The d3 object
     */
    function processDmData(svgElement, dmKey, dmData, elementId) {
        if (dmKey === 'lh') processDmValueData(svgElement, dmData[dmKey], D3Wrapper.createLineHoriz, dmKey, elementId);
        else if (dmKey === 'lv') processDmValueData(svgElement, dmData[dmKey], D3Wrapper.createLineVert, dmKey, elementId);
        else if (dmKey === 'l') processDmValueData(svgElement, dmData[dmKey], D3Wrapper.createLine, dmKey, elementId);
        else if (dmKey === '△') processDmValueData(svgElement, dmData[dmKey], D3Wrapper.createTriangle, dmKey, elementId);
        else if (dmKey === 'c') processDmValueData(svgElement, dmData[dmKey], D3Wrapper.createCircle, dmKey, elementId);
        else if (dmKey === 'r') processDmValueData(svgElement, dmData[dmKey], D3Wrapper.createRect, dmKey, elementId);
        else if (dmKey === 't') processDmValueData(svgElement, dmData[dmKey], D3Wrapper.createText, dmKey, elementId);
        else if (dmKey === 'cc') processDmValueData(svgElement, dmData[dmKey], D3Wrapper.createCubicCurve, dmKey, elementId);
        else if (dmKey === 'ac') processDmValueData(svgElement, dmData[dmKey], D3Wrapper.createArc, dmKey, elementId);
    }

    /**
     * Add Id to SVG element
     * @param {Object} svgElement The svg element node
     * @param {Number} dmType The data matrix type data
     * @returns {Object} The d3 object which corresponds to the svg element
     */
    function processDmValueData(svgElement, dmType, fnDmWrapper, dmKey, elementId) {
        // Either the Data matrix type is an array or json with styles
        if (Array.isArray(dmType)) {
            // Check 2D array
            if (dmType[0].constructor === Array) {
                for (var i = 0; i < dmType.length; ++i) {
                    processDmValueData_Helper1(svgElement, dmType[i], fnDmWrapper, dmKey, elementId);
                }
            } else { // 1D array
                processDmValueData_Helper1(svgElement, dmType, fnDmWrapper, dmKey, elementId);
            }

        } else {
            // The json object structure is
            // {"Ar":<array_data_to_draw>, "st":<stroke>, "stw":<stroke_width>, ...}
            if (dmType["Ar"] != null) {
                if (dmType[0].constructor === Array) {
                    for (var i = 0; i < dmType.length; ++i) {
                        var svgObject = processDmValueData_Helper1(svgElement, dmType[i], fnDmWrapper, dmKey, elementId);
                        processDmValueData_Helper2(svgElement, dmType[i], svgObject);
                    }
                } else { // 1D array
                    var svgObject = processDmValueData_Helper1(svgElement, dmType, fnDmWrapper, dmKey, elementId);
                    processDmValueData_Helper2(svgElement, dmType, svgObject);
                }
            }
        }
    }

    function processDmValueData_Helper1(svgElement, dmType, fnDmWrapper, dmKey, elementId) {
        var svgObject = fnDmWrapper(svgElement, dmType);
        if(dmKey !== 't') D3Wrapper.appendAttributeDefault1(svgObject);
        else D3Wrapper.updateFill(svgElement, "black");

        D3Wrapper.appendId(svgObject, getElementId(elementId));

        return svgObject;
    }

    function processDmValueData_Helper2(svgElement, dmType, svgObject) {
        if (dmType["st"] != null)
            D3Wrapper.updateStroke(svgElement, dmType["st"]);
        else if (dmType["stw"] != null)
            D3Wrapper.updateStrokeWidth(svgElement, dmType["stw"]);
        else if (dmType["fl"] != null)
            D3Wrapper.updateFill(svgElement, dmType["fl"]);
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
    this.process = function (elementId, dmId, isDispText) {
        initialize();
        var dmData = this.DMDict[dmId];

        if (dmData == null) {
            console.error("The Data Matrix Id '" + dmId + "' does not exists");
            return;
        }

        var svgElement = UiUtility.createDivCard(elementId, dmData["⍬"][0], dmData["⍬"][1]);

        // Loop through the json object
        for (var key in dmData) {
            if (dmData.hasOwnProperty(key)) {

                processDmData(svgElement, key, dmData, elementId);
            }
        }
    };

};
