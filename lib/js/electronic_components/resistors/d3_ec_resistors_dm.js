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

(function (dmDict) {

    var sizeSimple = [100, 18], size = [102, 40];
    var gapdivider1 = 10;
    var actualheightforrender = sizeSimple[1] - 1;
    var actualheightforrender1 = size[1] - gapdivider1;
    var wireleft = [0, sizeSimple[0]/4, sizeSimple[1]/2],
        wireRight = [3 * sizeSimple[0]/4, sizeSimple[0], sizeSimple[1]/2];
    var wireCircles = 6;
    var wirediff = wireRight[0] - wireleft[1];
    var dividerby2 = wirediff/(wireCircles * 2);
    var gapdivider = 4;

    var wireLeftMid = [0, size[0]/4, size[1]/2],
       wireRightMid = [3 * size[0]/4, size[0], size[1]/2];

    function getIEEEWire() {
      var arr = [wireleft[1], wireleft[2]];

      for(var i = 0; i < wireCircles; ++i) {
        arr.push(wireleft[1] + dividerby2 * (i * 2 + 1));
        arr.push((i % 2) == 0? 1 : actualheightforrender);
      }

      Array.prototype.push.apply(arr, [wireRight[0], wireleft[2]]);
      return arr;
    };

    function getIEEEWire1() {
      var arr = [wireLeftMid[1], wireLeftMid[2]];

      for(var i = 0; i < wireCircles; ++i) {
        arr.push(wireLeftMid[1] + dividerby2 * (i * 2 + 1));
        arr.push((i % 2) == 0? gapdivider1 : actualheightforrender1);
      }

      Array.prototype.push.apply(arr, [wireRightMid[0], wireLeftMid[2]]);
      return arr;
    };

    function getInputText(wire) { return [6, wireleft[2] - 1.5, "A", 10]; }
    function getOutputText(wire) { return [86, wireRight[2] - 1.5, "O", 10]; }
    function getInputText1(wire) { return [6, wireLeftMid[2] - 1.5, "A", 10]; }
    function getOutputText1(wire) { return [86, wireRightMid[2] - 1.5, "O", 10]; }
    var textarr = [getInputText(), getOutputText()];
    var textarr1 = [getInputText1(), getOutputText1()];

    var slantLine = [size[0]/4 - 5, size[1] - 1, 3 * size[0]/4, 1];
    var slantLine1 = [size[0]/4 - 5, size[1] - 1, 3 * size[0]/4 - 3, 3.5];
    var slantArrow = [68, 3.2, 3 * size[0]/4, 1, 71, 8.75];

    function getArrowHead1() {
      var arrowpoint = wireleft[1] + dividerby2 * 7;
      var arrowpointy = actualheightforrender + gapdivider + 7, xdiff = 4;
      return [arrowpoint - xdiff, arrowpointy, arrowpoint,
        actualheightforrender + gapdivider, arrowpoint + xdiff, arrowpointy];
    }

    dmDict["resistor.IEEE"] = {
      "⍬": sizeSimple,
      "lh": [wireleft, wireRight],
      "pl": getIEEEWire(),
      "t": textarr
    };

    dmDict["resistor.IEC"] = {
      "⍬": sizeSimple,
      "lh": [wireleft, wireRight],
      "r": [wireleft[1], 1, wirediff, actualheightforrender - 1],
      "t": textarr
    };

    dmDict["potentiometer.IEEE"] = {
      "⍬": size,
      "lh": [wireleft, wireRight],
        "lv": [wireleft[1] + dividerby2 * 7, actualheightforrender + gapdivider,
         size[1] - 1],
        "pl": [getIEEEWire(), getArrowHead1()],
        "t": textarr
    };

    dmDict["potentiometer.IEC"] = {
      "⍬": size,
      "lh": [wireleft, wireRight],
      "lv": [wireleft[1] + dividerby2 * 7, actualheightforrender + gapdivider,
       size[1] - 1],
      "r": [wireleft[1], 1, wirediff, actualheightforrender - 1],
      "pl": [getArrowHead1()],
      "t": [getInputText(), getOutputText()]
    };

    dmDict["resistor.rheostat.IEEE"] = {
        "⍬": size,
        "lh": [wireLeftMid, wireRightMid],
        "pl": [getIEEEWire1(), slantLine, slantArrow],
        "t": textarr1
    };

    dmDict["resistor.rheostat.IEC"] = {
        "⍬": size,
        "lh": [wireLeftMid, wireRightMid],
        "r": [wireLeftMid[1], gapdivider1, wirediff, actualheightforrender - 1],
        "pl": [slantLine, slantArrow],
        "t": textarr1
    };

    dmDict["trimmer.IEC"] = {
      "⍬": size,
      "lh": [wireLeftMid, wireRightMid],
      "r": [wireLeftMid[1], gapdivider1, wirediff, actualheightforrender - 1],
      "pl": [slantLine1, [70.5, 0.5, 75.5, 6.5]],
      "t": textarr1
    };

    dmDict["thermistor.IEC"] = {
      "⍬": size,
      "lh": [wireLeftMid, wireRightMid, [size[0]/4 - 5, size[0]/4 - 15, size[1] - 1]],
      "r": [wireLeftMid[1], gapdivider1, wirediff, actualheightforrender - 1],
      "pl": [slantLine1],
      "t": textarr1
    };

    var size2 = [100, 75];

    dmDict["photoresitor.IEC"] = {
        "⍬": size2,
        "lh": [[1, 25, 41.5], [73, 100, 41.5]],
        "r": [25, 30, 48, 23],
        "c": [49, 42, 30],
        "l": [[1, 12, 19, 26], [5, 6.5, 23, 20.5]],
        "pl": [[14, 24.75, 19, 26, 16.75, 21.25], [18, 19.25, 23, 20.5, 20.55, 15.5]],
        "t": [[6, 39, "A", 10], [86, 39, "O", 10]]
    };

    dmDict["photoresitor.IEEE"] = {
        "⍬": [102, 75],
        "lh": [[1, 25, 41.5], [73, 100, 41.5]],

        "c": [49, 42, 30],
        "l": [[1, 12, 19, 26], [5, 6.5, 23, 20.5]],
        "pl": [[14, 24.75, 19, 26, 16.75, 21.25], [18, 19.25, 23, 20.5, 20.55, 15.5]],
        "t": [[6, 39, "A", 10], [86, 39, "O", 10]]
    };

    dmDict["resistor.heating.IEC"] = {
        "⍬": size,
        "lh": [[1, 25, 15], [73, 100, 15]],
        "r": [25, 1, 48, 23],
        "lv": [[41, 1, 24], [57, 1, 24]],
        "t": [[6, 12, "A", 10], [86, 12, "O", 10]]
    };

    dmDict["trimming.potentiometer.IEEE"] = {
        "⍬": [102, 52],
        "lh": [[1, 25, 15], [73, 100, 15], [48, 58, 29]],
        "lv": [53, 29, 51],
        "pl": [[25, 15, 29, 4, 37, 26, 45, 4, 53, 26, 61, 4, 69, 26, 73, 15]],
        "t": [[6, 12, "A", 10], [86, 12, "O", 10]]
    };

    dmDict["trimming.potentiometer.IEC"] = {
        "⍬": [102, 52],
        "lh": [[1, 25, 15], [73, 100, 15], [48, 58, 29]],
        "r": [25, 1, 48, 23],
        "lv": [53, 29, 51],
        "t": [[6, 12, "A", 10], [86, 12, "O", 10]]
    };

    dmDict["impedence.IEC"] = {
        "⍬": size,
        "lh": [[1, 25, 15], [73, 100, 15]],
        "r": [25, 1, 48, 23],
        "t": [[6, 12, "A", 10], [44, 20, "Z", 20, "sans-serif"], [86, 12, "O", 10]]
    };

    dmDict["non-flammable.IEC"] = {
        "⍬": [102, 40],
        "lh": [[1, 25, 20], [73, 100, 20]],
        "r": [[21, 6.5, 56, 28], [19, 4.5, 60, 32]],
        "pl": [25, 20.5, 29, 9.5, 37, 31.5, 45, 9.5, 53, 31.5, 61, 9.5, 69, 31.5, 73, 19.5],
        "t": [[6, 12, "A", 10], [86, 12, "O", 10]]
    };

    dmDict["resistor.withfuse.IEC"] = {
        "⍬": [102, 30],
        "lh": [[1, 25, 15], [73, 100, 15]],
        "pl": [25, 15, 28, 7, 34, 23, 40, 7, 46, 23, 49, 15, 53, 15],
        "pt1": ["M", 53, 15, "C", 57, 7, 58, 7, 63, 15, "S", 68, 24, 73, 15],
        "r": [[21, 1, 56, 28]],
        "c":[[53, 15, 0.8], [73, 15, 0.8]],
        "t": [[6, 12, "A", 10], [86, 12, "O", 10]]
    };

    dmDict["attenuator.IEEE"] = {
        "⍬": [102, 40],
        "lh": [[1, 25, 20], [73, 100, 20]],
        "pl": [25, 20, 29, 9, 37, 31, 45, 9, 53, 31, 61, 9, 69, 31, 73, 20],
        "lv": [50, 1, 39],
        "t": [[6, 17, "A", 10], [86, 17, "O", 10]]
    };

    dmDict["memristor.IEC"] = {
        "⍬": [102, 25],
        "lh": [[1, 30, 12.5], [63, 100, 12.5], [30, 41, 6], [41, 52, 20], [52, 63, 6]],
        "lv": [[30, 12.5, 6], [41, 6, 20], [52, 20, 6], [63, 6, 12.5]],
        "r": [[25, 1, 48, 23], {"Ar": [70, 1, 3, 23], "fl": "black"}],
        "t": [[6, 9.5, "A", 10], [86, 9.5, "O", 10]]
    };

})(DMRender.TopicSvgDMDict);
