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
 * A class which defines the constants
 * @class
 */
var DmConstants = (function () {

    // Declare public constant variables
    // SVG Element Data matrix key id in short S (SVG) E (Element) ID
    var $_SEID = {
        LINE: "l",
        LINE_HORIZONTAL: "lh",
        LINE_VERTICAL: "lv",
        TRIANGLE: "tr",
        CIRCLE: "c",
        TEXT: "t",
        ELLIPSE: "e",
        PATH: "pt",
        POLYLINE: "pl",
        ARC: "ar",
        BCURVE: "bc", // Beizer Curve
        RECT: "r",
        POLYGON: "po"
    };

    // SVG Attribute Data matrix key id in short S (SVG) A (Attribute) ID
    var $_SAID = {
        SIZE: {name:"s"},
        STROKE: {name: "st", defv: "black", skey: "stroke"},
        STROKEWIDE: {name:"sw", defv: "1", skey: "stroke-width"},
        STROKELNCAP: {name:"slc", defv: "square", skey: "stroke-linecap"},
        FILL: {name:"fl", defv: "none", skey: "fill"}
    };

    // JSON Data matrix attribute
    var $_DMID = {
        ARRAY: "ar"
    };

    // Default values
    var $_FILL_TEXT = "black"
    ;

})();
