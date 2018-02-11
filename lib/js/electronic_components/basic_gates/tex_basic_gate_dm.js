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

    dmDict["gate.NOT.IEC"] = dmDict["gate.NOT"] = [
        {
            "l": ["\\overline{A}", true],
            "s": {
                "⍬": [12, 23],
                "lh": [1, 11, 3],
                "t": [1, 17, "A", 16]
            }
        },
        {
            "l": ["~A", false],
            "s": {
                "⍬": [26, 23],
                "t": [[1, 17, "~ A", 16]]
            }
        }
    ];

    dmDict["gate.AND.IEC"] = dmDict["gate.AND"] = {
        "l": ["{A}\\cdot{B}", true],
        "s": {
            "⍬": [37, 23],
            "c": [19, 12, 0.7],
            "t": [[1, 17, "A", 16], [24, 17, "B", 16]]
        }
    };

    dmDict["gate.OR.IEC"] = dmDict["gate.OR"] = {
        "l": ["{A}+{B}", true],
        "s": {
            "⍬": [44, 23],
            "t": [[1, 17, "A + B", 16]]
        }
    };

    dmDict["gate.NAND.IEC"] = dmDict["gate.NAND"] = [{
        "l": ["\\overline{{A}\\cdot{B}}", true],
        "s": {
            "⍬": [37, 23],
            "lh": [1, 37, 3],
            "c": [19, 12, 0.7],
            "t": [[1, 17, "A", 16], [24, 17, "B", 16]]
        }
    },
        {
            "l": ["{A}\\uparrow{B}", true],
            "s": {
                "⍬": [42, 23],
                "t": [[1, 17, "A \u2191 B", 16]]
            }
        }
    ];

    dmDict["gate.NOR.IEC"] = dmDict["gate.NOR"] = [{
        "l": ["\\overline{{A}+{B}}", true],
        "s": {
            "⍬": [44, 23],
            "lh": [1, 44, 3],
            "t": [[1, 17, "A + B", 16]]
        }
    },
        {
            "l": ["{A}\\downarrow{B}", true],
            "s": {
                "⍬": [42, 23],
                "t": [[1, 17, "A \u2193 B", 16]]
            }
        }
    ];

    dmDict["gate.XOR.IEC"] = dmDict["gate.XOR"] = {
        "l": ["{A}\\oplus{B}", true],
        "s": {
            "⍬": [50, 23],
            "t": [[1, 17, "A \u2295 B", 16]]
        }
    };

    dmDict["gate.XNOR.IEC"] = dmDict["gate.XNOR"] = [{
        "l": ["\\overline{{A}\\oplus{B}}", true],
        "s": {
            "⍬": [50, 23],
            "lh": [1, 50, 3],
            "t": [[1, 17, "A \u2295 B", 16]]
        }
    },
        {
            "l": ["{A}\\odot{B}", true],
            "s": {
                "⍬": [50, 23],
                "t": [[1, 17, "A \u2299 B", 16]]
            }
        }
    ];

})(DMRender.SymbolDMDict);
