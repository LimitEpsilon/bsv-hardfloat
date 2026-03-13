
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
    addRecF16_add (
        input [(`floatControlWidth - 1):0] control,
        input [16:0] a,
        input [16:0] b,
        input [2:0] roundingMode,
        output [16:0] out,
        output [4:0] exceptionFlags
    );

    wire [21:0] result;
    module_addRecF16 addRecF16(
        .addRecF16_1(1'b0),
        .addRecF16_2(a),
        .addRecF16_3(b),
        .addRecF16_4(roundingMode),
        .addRecF16_5(control[0]),
        .addRecF16(result)
    );
    assign out = result[21:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    addRecF32_add (
        input [(`floatControlWidth - 1):0] control,
        input [32:0] a,
        input [32:0] b,
        input [2:0] roundingMode,
        output [32:0] out,
        output [4:0] exceptionFlags
    );

    wire [37:0] result;
    module_addRecF32 addRecF32(
        .addRecF32_1(1'b0),
        .addRecF32_2(a),
        .addRecF32_3(b),
        .addRecF32_4(roundingMode),
        .addRecF32_5(control[0]),
        .addRecF32(result)
    );
    assign out = result[37:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    addRecF64_add (
        input [(`floatControlWidth - 1):0] control,
        input [64:0] a,
        input [64:0] b,
        input [2:0] roundingMode,
        output [64:0] out,
        output [4:0] exceptionFlags
    );

    wire [69:0] result;
    module_addRecF64 addRecF64(
        .addRecF64_1(1'b0),
        .addRecF64_2(a),
        .addRecF64_3(b),
        .addRecF64_4(roundingMode),
        .addRecF64_5(control[0]),
        .addRecF64(result)
    );
    assign out = result[69:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    addRecF128_add (
        input [(`floatControlWidth - 1):0] control,
        input [128:0] a,
        input [128:0] b,
        input [2:0] roundingMode,
        output [128:0] out,
        output [4:0] exceptionFlags
    );

    wire [133:0] result;
    module_addRecF128 addRecF128(
        .addRecF128_1(1'b0),
        .addRecF128_2(a),
        .addRecF128_3(b),
        .addRecF128_4(roundingMode),
        .addRecF128_5(control[0]),
        .addRecF128(result)
    );
    assign out = result[133:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    addRecF16_sub (
        input [(`floatControlWidth - 1):0] control,
        input [16:0] a,
        input [16:0] b,
        input [2:0] roundingMode,
        output [16:0] out,
        output [4:0] exceptionFlags
    );

    wire [21:0] result;
    module_addRecF16 addRecF16(
        .addRecF16_1(1'b1),
        .addRecF16_2(a),
        .addRecF16_3(b),
        .addRecF16_4(roundingMode),
        .addRecF16_5(control[0]),
        .addRecF16(result)
    );
    assign out = result[21:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    addRecF32_sub (
        input [(`floatControlWidth - 1):0] control,
        input [32:0] a,
        input [32:0] b,
        input [2:0] roundingMode,
        output [32:0] out,
        output [4:0] exceptionFlags
    );

    wire [37:0] result;
    module_addRecF32 addRecF32(
        .addRecF32_1(1'b1),
        .addRecF32_2(a),
        .addRecF32_3(b),
        .addRecF32_4(roundingMode),
        .addRecF32_5(control[0]),
        .addRecF32(result)
    );
    assign out = result[37:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    addRecF64_sub (
        input [(`floatControlWidth - 1):0] control,
        input [64:0] a,
        input [64:0] b,
        input [2:0] roundingMode,
        output [64:0] out,
        output [4:0] exceptionFlags
    );

    wire [69:0] result;
    module_addRecF64 addRecF64(
        .addRecF64_1(1'b1),
        .addRecF64_2(a),
        .addRecF64_3(b),
        .addRecF64_4(roundingMode),
        .addRecF64_5(control[0]),
        .addRecF64(result)
    );
    assign out = result[69:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    addRecF128_sub (
        input [(`floatControlWidth - 1):0] control,
        input [128:0] a,
        input [128:0] b,
        input [2:0] roundingMode,
        output [128:0] out,
        output [4:0] exceptionFlags
    );

    wire [133:0] result;
    module_addRecF128 addRecF128(
        .addRecF128_1(1'b1),
        .addRecF128_2(a),
        .addRecF128_3(b),
        .addRecF128_4(roundingMode),
        .addRecF128_5(control[0]),
        .addRecF128(result)
    );
    assign out = result[133:5];
    assign exceptionFlags = result[4:0];

endmodule
