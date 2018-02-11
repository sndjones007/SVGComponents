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

  dmDict["ohms.law"] = [{
      "l": ["{V}{=}{I}\\times{R}", true],
      "s": {
          "⍬": [70, 23],
          "t": [[1, 17, "V = I \u00D7 R", 16]]
      }
  },
  {
      "l": ["{I}{=}\\dfrac{V}{R}", true], // Fractions in KaTeX does not work
      "s": {
          "⍬": [43, 38],
          "lh": [25, 45, 19],
          "t": [[1, 23, "I = ", 16], [29, 12, "V", 16], [28, 37, "R", 16]]
      }
  },
  {
      "l": ["{R}{=}\\dfrac{V}{I}", true], // Fractions in KaTeX does not work
      "s": {
          "⍬": [45, 38],
          "lh": [29, 45, 19],
          "t": [[1, 23, "R = ", 16], [30, 12, "V", 16], [30, 37, "I", 16]]
      }
  }
];

dmDict["resistor.power.consumption"] = [{
    "l": ["{P}{=}{I}\\times{V}", true],
    "s": {
        "⍬": [70, 23],
        "t": [[1, 17, "P = I \u00D7 V", 16]]
    }
  },
    {
        "l": ["{P}{=}{I^2}\\times{V}", true],
        "s": {
            "⍬": [70, 23],
            "t": [[1, 17, "P = I \u00D7 V", 16]]
        }
}];

})(DMRender.SymbolDMDict);
