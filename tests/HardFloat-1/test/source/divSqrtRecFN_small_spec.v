
/*============================================================================

This Verilog source file is part of the Berkeley HardFloat IEEE Floating-Point
Arithmetic Package, Release 1, by John R. Hauser.

Copyright 2019 The Regents of the University of California.  All rights
reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice,
    this list of conditions, and the following disclaimer.

 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions, and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 3. Neither the name of the University nor the names of its contributors may
    be used to endorse or promote products derived from this software without
    specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS "AS IS", AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, ARE
DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=============================================================================*/

`include "HardFloat_consts.vi"
`include "HardFloat_specialize.vi"

module
    divSqrtRecF16_small_div (
        input nReset,
        input clock,
        input [(`floatControlWidth - 1):0] control,
        output inReady,
        input inValid,
        input [16:0] a,
        input [16:0] b,
        input [2:0] roundingMode,
        output outValid,
        output [16:0] out,
        output [4:0] exceptionFlags
    );

    wire [21:0] result;
    mkDivSqrtRecF16 divSqrtRecFN(
        .CLK(clock),
        .RST_N(nReset),
        .inReady(inReady),
        .req_inValid(inValid),
        .req_sqrtOp(1'b0),
        .req_a(a),
        .req_b(b),
        .req_roundingMode(roundingMode),
        .outValid_div(outValid),
        .outValid_sqrt(),
        .result_control(control[0]),
        .result(result)
    );
    assign out = result[21:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    divSqrtRecF32_small_div (
        input nReset,
        input clock,
        input [(`floatControlWidth - 1):0] control,
        output inReady,
        input inValid,
        input [32:0] a,
        input [32:0] b,
        input [2:0] roundingMode,
        output outValid,
        output [32:0] out,
        output [4:0] exceptionFlags
    );

    wire [37:0] result;
    mkDivSqrtRecF32 divSqrtRecFN(
        .CLK(clock),
        .RST_N(nReset),
        .inReady(inReady),
        .req_inValid(inValid),
        .req_sqrtOp(1'b0),
        .req_a(a),
        .req_b(b),
        .req_roundingMode(roundingMode),
        .outValid_div(outValid),
        .outValid_sqrt(),
        .result_control(control[0]),
        .result(result)
    );
    assign out = result[37:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    divSqrtRecF64_small_div (
        input nReset,
        input clock,
        input [(`floatControlWidth - 1):0] control,
        output inReady,
        input inValid,
        input [64:0] a,
        input [64:0] b,
        input [2:0] roundingMode,
        output outValid,
        output [64:0] out,
        output [4:0] exceptionFlags
    );

    wire [69:0] result;
    mkDivSqrtRecF64 divSqrtRecFN(
        .CLK(clock),
        .RST_N(nReset),
        .inReady(inReady),
        .req_inValid(inValid),
        .req_sqrtOp(1'b0),
        .req_a(a),
        .req_b(b),
        .req_roundingMode(roundingMode),
        .outValid_div(outValid),
        .outValid_sqrt(),
        .result_control(control[0]),
        .result(result)
    );
    assign out = result[69:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    divSqrtRecF128_small_div (
        input nReset,
        input clock,
        input [(`floatControlWidth - 1):0] control,
        output inReady,
        input inValid,
        input [128:0] a,
        input [128:0] b,
        input [2:0] roundingMode,
        output outValid,
        output [128:0] out,
        output [4:0] exceptionFlags
    );

    wire [133:0] result;
    mkDivSqrtRecF128 divSqrtRecFN(
        .CLK(clock),
        .RST_N(nReset),
        .inReady(inReady),
        .req_inValid(inValid),
        .req_sqrtOp(1'b0),
        .req_a(a),
        .req_b(b),
        .req_roundingMode(roundingMode),
        .outValid_div(outValid),
        .outValid_sqrt(),
        .result_control(control[0]),
        .result(result)
    );
    assign out = result[133:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    divSqrtRecF16_small_sqrt (
        input nReset,
        input clock,
        input [(`floatControlWidth - 1):0] control,
        output inReady,
        input inValid,
        input [16:0] a,
        input [2:0] roundingMode,
        output outValid,
        output [16:0] out,
        output [4:0] exceptionFlags
    );

    wire [21:0] result;
    mkDivSqrtRecF16 divSqrtRecFN(
        .CLK(clock),
        .RST_N(nReset),
        .inReady(inReady),
        .req_inValid(inValid),
        .req_sqrtOp(1'b1),
        .req_a(a),
        .req_b(17'b0),
        .req_roundingMode(roundingMode),
        .outValid_sqrt(outValid),
        .outValid_div(),
        .result_control(control[0]),
        .result(result)
    );
    assign out = result[21:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    divSqrtRecF32_small_sqrt (
        input nReset,
        input clock,
        input [(`floatControlWidth - 1):0] control,
        output inReady,
        input inValid,
        input [32:0] a,
        input [2:0] roundingMode,
        output outValid,
        output [32:0] out,
        output [4:0] exceptionFlags
    );

    wire [37:0] result;
    mkDivSqrtRecF32 divSqrtRecFN(
        .CLK(clock),
        .RST_N(nReset),
        .inReady(inReady),
        .req_inValid(inValid),
        .req_sqrtOp(1'b1),
        .req_a(a),
        .req_b(33'b0),
        .req_roundingMode(roundingMode),
        .outValid_sqrt(outValid),
        .outValid_div(),
        .result_control(control[0]),
        .result(result)
    );
    assign out = result[37:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    divSqrtRecF64_small_sqrt (
        input nReset,
        input clock,
        input [(`floatControlWidth - 1):0] control,
        output inReady,
        input inValid,
        input [64:0] a,
        input [2:0] roundingMode,
        output outValid,
        output [64:0] out,
        output [4:0] exceptionFlags
    );

    wire [69:0] result;
    mkDivSqrtRecF64 divSqrtRecFN(
        .CLK(clock),
        .RST_N(nReset),
        .inReady(inReady),
        .req_inValid(inValid),
        .req_sqrtOp(1'b1),
        .req_a(a),
        .req_b(65'b0),
        .req_roundingMode(roundingMode),
        .outValid_sqrt(outValid),
        .outValid_div(),
        .result_control(control[0]),
        .result(result)
    );
    assign out = result[69:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    divSqrtRecF128_small_sqrt (
        input nReset,
        input clock,
        input [(`floatControlWidth - 1):0] control,
        output inReady,
        input inValid,
        input [128:0] a,
        input [2:0] roundingMode,
        output outValid,
        output [128:0] out,
        output [4:0] exceptionFlags
    );

    wire [133:0] result;
    mkDivSqrtRecF128 divSqrtRecFN(
        .CLK(clock),
        .RST_N(nReset),
        .inReady(inReady),
        .req_inValid(inValid),
        .req_sqrtOp(1'b1),
        .req_a(a),
        .req_b(129'b0),
        .req_roundingMode(roundingMode),
        .outValid_sqrt(outValid),
        .outValid_div(),
        .result_control(control[0]),
        .result(result)
    );
    assign out = result[133:5];
    assign exceptionFlags = result[4:0];

endmodule
