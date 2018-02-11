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

  dmDict["extrinsic.general"] = {
      "⍬": [100, 40],
      "l": [23, 39, 78, 5],
      "pl": [70, 5.2, 78, 5, 74.5, 12.75]
  };

  dmDict["extrinsic.general.preset"] = {
      "⍬": [100, 40],
      "l": [[23, 39, 78, 3.5], [75, 0, 80.5, 7.75]]
  };

  dmDict["extrinsic.general.linear"] = {
      "⍬": [100, 40],
      "lh": [14, 31, 39],
      "l": [23, 39, 78, 5],
      "pl": [70, 5.2, 78, 5, 74.5, 12.75]
  };

  dmDict["extrinsic.general.nonlinear"] = {
      "⍬": [100, 40],
      "lh": [14, 23, 39],
      "l": [23, 39, 78, 5],
      "pl": [70, 5.2, 78, 5, 74.5, 12.75]
  };

  dmDict["intrinsic.linear"] = {
      "⍬": [100, 40],
      "l": [23, 39, 78, 5]
  };

  dmDict["intrinsic.nonlinear"] = {
      "⍬": [100, 40],
      "lh": [14, 23, 39],
      "l": [23, 39, 78, 5]
  };

  dmDict["special.adjustability.continuous"] = {
      "⍬": [100, 40],
      "l": [[23, 39, 78, 5], [88, 5, 80, 10]],
      "pl": [70, 5.2, 78, 5, 74.5, 12.75]
  };

  dmDict["special.adjustability.steps"] = {
      "⍬": [100, 40],
      "l": [23, 39, 78, 5],
      "pl": [[70, 5.2, 78, 5, 74.5, 12.75], [84, 12, 84, 5.75, 96, 5.75, 96, 0]]
  };

  dmDict["special.preset.continuous"] = {
      "⍬": [100, 40],
      "l": [[23, 39, 78, 3.5], [75, 0, 80.5, 7.75], [88, 5, 80, 10]]
  };

  dmDict["special.preset.continuous"] = {
      "⍬": [100, 40],
      "l": [[23, 39, 78, 3.5], [75, 0, 80.5, 7.75]],
      "pl": [[84, 12, 84, 5.75, 96, 5.75, 96, 0]]
  };

})(DMRender.TopicSvgDMDict);
