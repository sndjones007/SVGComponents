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

    var size = [102, 52],
        lh1_in = [1, 25, 16.67],
        lh2_in = [1, 25, 33.34],
        lh3_out = [70, 100, 25],
        lh4_out = [80, 100, 25],
    lh4_d1 = [25, 45, 1.5], lh4_d2 = [25, 45, 51.5],
    txt1_out = [92, 22, "O", 10], txt2_out = [85, 22, "O", 10],
    txt1_in = [12, 13.6, "A", 10], txt2_in = [12, 43, "B", 10],
    lv1 = [25, 1, 80],
    cc1 = [17, 1.4, 29, 8, 29, 45, 17, 51.5], cc2 = [17.3, 1.4, 40, 4, 55, 5, 70, 25.4],
        cc3 = [17.3, 51.5, 40, 48, 55, 47, 70, 25], cc4 = [13, 1.4, 25, 8, 25, 45, 13, 51.5],
    ac1 = [45, 1.5, 25, 25, 0, 0, 1, 45, 51.5],
    c1 = [75.5, 25, 5],
    txt1_arr = [txt1_in, txt2_in, txt2_out], txt2_arr = [txt1_in, txt2_in, txt1_out],
    cc1_arr = [cc1, cc2, cc3, cc4], cc2_arr = [cc1, cc2, cc3],
    lh1_arr = [lh1_in, lh2_in, lh4_out], lh2_arr = [lh1_in, lh2_in, lh3_out];

    dmDict["gate.NOT"] = {
        "⍬": size,
        "lh": [[1, 25, 25], lh4_out],
        "△": [25, 2, 25, 51, 70, 25],
        "c": c1,
        "t": [[12, 22, "A", 10], txt1_out]
    };

    dmDict["gate.AND"] = {
        "⍬": size,
        "lh": [lh1_in, lh2_in, lh3_out, lh4_d1, lh4_d2],
        "lv": lv1,
        "ac": ac1,
        "t": txt1_arr
    };

    dmDict["gate.OR"] = {
        "⍬": size,
        "lh": lh2_arr,
        "cc": cc2_arr,
        "t": txt1_arr
    };

    dmDict["gate.NAND"] = {
        "⍬": size,
        "lh": [lh1_in, lh2_in, lh4_out, lh4_d1, lh4_d2],
        "lv": lv1,
        "ac": ac1,
        "c": c1,
        "t": txt2_arr
    };

    dmDict["gate.NOR"] = {
        "⍬": size,
        "lh": lh1_arr,
        "cc": cc2_arr,
        "c": c1,
        "t": txt2_arr
    };

    dmDict["gate.XOR"] = {
        "⍬": size,
        "lh": lh2_arr,
        "cc": cc1_arr,
        "t": txt1_arr
    };

    dmDict["gate.XNOR"] = {
        "⍬": size,
        "lh": lh1_arr,
        "cc": cc1_arr,
        "c": c1,
        "t": txt1_arr
    };

    // IEC
    var iec_template = {
        "⍬": size,
        "r": [25, 1, 45, 50]
    };

    function without_not(id, text) {
        dmDict[id] = {
            "lh": lh2_arr,
            "t": [txt1_in, txt2_in, txt2_out, [47, 17, text, 16]]
        };
        $.extend(dmDict[id], iec_template);
    }

    function with_not(id, text) {
        without_not(id, text);
        $.extend(dmDict[id], {"l": [70, 20, 85, 25]});
    }

    dmDict["gate.NOT.IEC"] = {
        "lh": [[1, 25, 25], lh3_out],
        "l": [70, 20, 85, 25],
        "t": [[12, 22, "A", 10], txt1_out, [47, 17, "1", 16]]
    };
    $.extend(dmDict["gate.NOT.IEC"], iec_template);

    without_not("gate.AND.IEC", "&");
    without_not("gate.OR.IEC", "\u22651");
    without_not("gate.XOR.IEC", "=1");

    with_not("gate.NAND.IEC", "&");
    with_not("gate.NOR.IEC", "\u22651");
    with_not("gate.XNOR.IEC", "=1");

})(DMRender.DMDict);
