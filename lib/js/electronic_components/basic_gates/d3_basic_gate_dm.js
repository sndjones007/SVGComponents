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

    // Declare variables
    // var size = [100, 50],
    //     lh1_in = [1, 25, 16.67],
    //     lh2_in = [1, 25, 33.34],
    //     lh3_out = [70, 100, 25],
    //     lh4_out = [80, 100, 25],
    // lh4_d1 = [25, 45, 1.5], lh4_d2 = [25, 45, 51.5],
    // txt1_out = [86, 22, "O", 10], txt2_out = [80, 22, "O", 10],
    // txt1_in = [6, 13.6, "A", 10], txt2_in = [6, 43, "B", 10],
    // lv1 = [25, 1, 80],
    // cc1 = [17, 1.4, 29, 8, 29, 45, 17, 51.5], cc2 = [17.3, 1.4, 40, 4, 55, 5, 70, 25.4],
    //     cc3 = [17.3, 51.5, 40, 48, 55, 47, 70, 25], cc4 = [13, 1.4, 25, 8, 25, 45, 13, 51.5],
    // ac1 = [45, 1.5, 25, 25, 0, 0, 1, 45, 51.5],
    // c1 = [75.5, 25, 5],
    // txt1_arr = [txt1_in, txt2_in, txt2_out], txt2_arr = [txt1_in, txt2_in, txt1_out],
    // cc1_arr = [cc1, cc2, cc3, cc4], cc2_arr = [cc1, cc2, cc3],
    // lh1_arr = [lh1_in, lh2_in, lh4_out], lh2_arr = [lh1_in, lh2_in, lh3_out];

    // Private methods
    var size = [100, 50];
    var xstart = size[0]/4;
    var xend = 3 * xstart, xmid = size[0] / 2;
    var ydiff = 2, height = size[1] - ydiff, yend = size[1] - ydiff/2;
    var yone = height/2;
    var yfactor = height/3;
    var notCircleRadius = 5, textLineGap = 3;
    var xtextStart = xstart/4;
    var expectedTextWidth = 7;

    var ptEnd = function(isnotcircle) {
      return [xend + (isnotcircle)? notCircleRadius: -notCircleRadius, size[0], yone];
    };

    var getWires = function(count, isnotcircle) {
      if(count === 1) return [[1, xstart, yone], ptEnd(isnotcircle)];
      else if(count === 2) {
        return [[1, xstart, yfactor], [1, xstart, 2 * yfactor], ptEnd(isnotcircle)];
      }
    };

    var notTriangle = function() {
      return [xstart, 1, xstart, yend, ptEnd(true), yone];
    };

    var notCircle = function() { return [xend, yone, notCircleRadius]; }

    var textForNotStart = function() { return [xtextStart, yone - textLineGap, "A"]; };
    var textForStart = function() {
      return [[xtextStart, yfactor - textLineGap, "A"],[xtextStart, 2 * yfactor - textLineGap, "B"]];
    };
    var textForEnd = function() {
      return [15/16 * size[0] - expectedTextWidth, yone - textLineGap, "O"];
    };
    var textForNot = function() {
      return new Array(textForNotStart(), textForEnd());
    };
    var textForOthers = function() {
      var arr = textForStart();
      arr.push(textForEnd());
      return arr;
    };

    var renderDForAnd = function() {
      var xStartD = xmid - 5;
      return ["M", xstart, 1,
              "L", xstart, yend,
              "L", xStartD, yend,
              "A", xstart, xstart, 0, 0, 0, xStartD, 1,
              "L", xstart, 1];
    };

    var renderOrBlock = function() {
      var orXCurve = xstart - 8, orXControlPt1 = xstart + 4, orXControlPt2 = xmid - 10,
          orXControlPt3 = xmid + 5, orYControlPt1 = 8, orXControlPt4 = xend - notCircleRadius;
      return ["M", orXCurve, 1,
              "C", orXControlPt1, orYControlPt1, orXControlPt1, size[1] - orYControlPt1, orXCurve, yend,
              "C", orXControlPt2, size[1] - 2, orXControlPt3, yend, orXControlPt4, yone,
              "C", orXControlPt2, 4, orXControlPt3, 5, orXControlPt4, yone];
    }

    var renderXorBlock = function() {
      return [xstart - 12, 1, yone, 8, yone, xmid - 5, xstart - 12, yend];
    };

    var dmDictANDGate = function(isnotcircle) {
      dmDict["gate.AND"] = {
          sz: size,
          lh: getWires(2),
          pt1: renderDForAnd(),
          t: textForOthers()
      };

      dmDict["gate.OR"] = {
          sz: size,
          lh: getWires(2),
          pt1: renderOrBlock(),
          t: textForOthers()
      };

      dmDict["gate.XOR"] = {
          sz: size,
          lh: getWires(2),
          pt1: renderOrBlock(),
          cc: renderXorBlock(),
          t: textForOthers()
      };

      dmDict["gate.NAND"] = jQuery.extend(true, {}, dmDict["gate.AND"]);
      dmDict["gate.NOR"] = jQuery.extend(true, {}, dmDict["gate.OR"]);
      dmDict["gate.XNOR"] = jQuery.extend(true, {}, dmDict["gate.XOR"]);
      dmDict["gate.NAND"].c = dmDict["gate.NOR"].c = dmDict["gate.XNOR"].c = notCircle();
      dmDict["gate.NAND"].lh = dmDict["gate.NOR"].lh = dmDict["gate.XNOR"].lh = getWires(2, true);
    }

    dmDict["gate.NOT"] = {
        sz: size,
        lh: getWires(1, true),
        pg: notTriangle(),
        c: notCircle(),
        t: textForNot()
    };

    dmDictANDGate();

    // IEC
    var iec_template = {
        sz: size,
        r: [xstart, 1, xend - xstart - notCircleRadius, height]
    };

    var textarr = textForOthers(),
        textarr1 = textForNot();

    textarr1.push([xmid - 3, 17, "1", 16]);

    function without_not(id, text, isnotcircle) {
      var textIec = [xmid - 3, 17, text, 16];
      var textarr = textForOthers();
      textarr.push(textIec);

        dmDict[id] = {
            lh: getWires(2),
            t: textarr
        };
        $.extend(dmDict[id], iec_template);
    }

    var notLineIec = [xend - notCircleRadius, yone - 5, xend + 10, yone];
    function with_not(id, text) {
        without_not(id, text, true);
        $.extend(dmDict[id], {"l": notLineIec});
    }

    dmDict["gate.NOT.IEC"] = {
        lh: getWires(1),
        l: notLineIec,
        t: textarr1
    };
    $.extend(dmDict["gate.NOT.IEC"], iec_template);

    without_not("gate.AND.IEC", "&");
    without_not("gate.OR.IEC", "\u22651");
    without_not("gate.XOR.IEC", "=1");

    with_not("gate.NAND.IEC", "&");
    with_not("gate.NOR.IEC", "\u22651");
    with_not("gate.XNOR.IEC", "=1");

})(DMRender.TopicSvgDMDict);
