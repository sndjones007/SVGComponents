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

(function(dmDict){

  dmDict["gate.NOT"] = {
    "⍬":[102, 52],
    "lh":[[1, 25, 25], [85, 100, 25]],
    "△":[25, 2, 25, 51, 75, 25],
    "c":[80, 25, 5],
    "t":["A", 12, 22, 10],
    "t":["O", 92, 22, 10]
  }

})(DMRender.DMDict);

var gateDMDict = {};
gateDMDict["NOT-gate"] = {
  "⍬":[102, 52],
  "lh":[[1, 25, 25], [85, 100, 25]],
  "△":[25, 2, 25, 51, 75, 25],
  "c":[80, 25, 5],
  "t":["A", 12, 22, 10],
  "t":["O", 92, 22, 10]
};
gateDMDict["AND-gate"] = [
    [102, 52],
    ["l", "1", 1, 16.67, 25, 16.67], // input line
    ["l", "2", 1, 33.34, 25, 33.34], // input line
    // Below ids 2,3,4 forms a D
    ["l", "2", 25, 1, 25, 80], // vertical line of the D
    ["l", "3", 25, 1, 50, 1], // horizantal slant line of the triangle towards bottom
    ["l", "4", 25, 50, 50, 50], // horizantal slant line of the triangle towards top
    ["a", "5", 50, 1, 25, 25, 1, 1, 1, 50, 50], // Arc of D
    ["c", "5", 80, 25, 5], // The circle point
    ["l", "6", 85, 25, 100, 25], // The output line
    ["t", "input-A-txt", "A", 12, 22, 10], // The input line text
    ["t", "output-O-txt", "O", 92, 22, 10] // The output line text
];


var IEC_not_line_dm_template = ["l", "70", 75, 20, 90, 25]; // The slant line
var IEC_gate_dm_template = [
    [102, 52],
    ["l", "71", 1, 25, 25, 25], // input line
    // Below ids 2,3,4,5 forms a rectangle
    ["l", "72", 25, 1, 25, 50],
    ["l", "73", 25, 50, 75, 50],
    ["l", "74", 75, 50, 75, 1],
    ["l", "75", 75, 1, 25, 1],
    ["l", "76", 75, 25, 100, 25], // The output line
    ["t", "77", "A", 12, 23, 10], // The input line text
    ["t", "78", "O", 92, 23, 10] // The output line text
];
gateDMDict["NOT-gate-IEC"] = IEC_gate_dm_template.slice(0);
gateDMDict["NOT-gate-IEC"].push(IEC_not_line_dm_template);
gateDMDict["NOT-gate-IEC"].push(["t", "1", "1", 50, 20, 20]);
