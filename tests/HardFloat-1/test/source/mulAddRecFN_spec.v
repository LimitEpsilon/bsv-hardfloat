
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
    mulAddRecF16_add (
        input [(`floatControlWidth - 1):0] control,
        input [16:0] a,
        input [16:0] b,
        input [2:0] roundingMode,
        output [16:0] out,
        output [4:0] exceptionFlags
    );

    wire [16:0] recF16_1 = 'h08000;
    wire [21:0] result;
    module_mulAddRecF16 mulAddRecF16(
        .mulAddRecF16_1(2'b0),
        .mulAddRecF16_2(a),
        .mulAddRecF16_3(recF16_1),
        .mulAddRecF16_4(b),
        .mulAddRecF16_5(roundingMode),
        .mulAddRecF16_6(control[0]),
        .mulAddRecF16(result)
    );
    assign out = result[21:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    mulAddRecF32_add (
        input [(`floatControlWidth - 1):0] control,
        input [32:0] a,
        input [32:0] b,
        input [2:0] roundingMode,
        output [32:0] out,
        output [4:0] exceptionFlags
    );

    wire [32:0] recF32_1 = 33'h080000000;
    wire [37:0] result;
    module_mulAddRecF32 mulAddRecF32(
        .mulAddRecF32_1(2'b0),
        .mulAddRecF32_2(a),
        .mulAddRecF32_3(recF32_1),
        .mulAddRecF32_4(b),
        .mulAddRecF32_5(roundingMode),
        .mulAddRecF32_6(control[0]),
        .mulAddRecF32(result)
    );
    assign out = result[37:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    mulAddRecF64_add (
        input [(`floatControlWidth - 1):0] control,
        input [64:0] a,
        input [64:0] b,
        input [2:0] roundingMode,
        output [64:0] out,
        output [4:0] exceptionFlags
    );

    wire [64:0] recF64_1 = 65'h08000000000000000;
    wire [69:0] result;
    module_mulAddRecF64 mulAddRecF64(
        .mulAddRecF64_1(2'b0),
        .mulAddRecF64_2(a),
        .mulAddRecF64_3(recF64_1),
        .mulAddRecF64_4(b),
        .mulAddRecF64_5(roundingMode),
        .mulAddRecF64_6(control[0]),
        .mulAddRecF64(result)
    );
    assign out = result[69:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    mulAddRecF128_add (
        input [(`floatControlWidth - 1):0] control,
        input [128:0] a,
        input [128:0] b,
        input [2:0] roundingMode,
        output [128:0] out,
        output [4:0] exceptionFlags
    );

    wire [128:0] recF128_1 = 129'h080000000000000000000000000000000;
    wire [133:0] result;
    module_mulAddRecF128 mulAddRecF128(
        .mulAddRecF128_1(2'b0),
        .mulAddRecF128_2(a),
        .mulAddRecF128_3(recF128_1),
        .mulAddRecF128_4(b),
        .mulAddRecF128_5(roundingMode),
        .mulAddRecF128_6(control[0]),
        .mulAddRecF128(result)
    );
    assign out = result[133:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    mulAddRecF16_mul (
        input [(`floatControlWidth - 1):0] control,
        input [16:0] a,
        input [16:0] b,
        input [2:0] roundingMode,
        output [16:0] out,
        output [4:0] exceptionFlags
    );

    wire [16:0] zeroAddend = {a[16] ^ b[16], 16'b0};
    wire [21:0] result;
    module_mulAddRecF16 mulAddRecF16(
        .mulAddRecF16_1(2'b0),
        .mulAddRecF16_2(a),
        .mulAddRecF16_3(b),
        .mulAddRecF16_4(zeroAddend),
        .mulAddRecF16_5(roundingMode),
        .mulAddRecF16_6(control[0]),
        .mulAddRecF16(result)
    );
    assign out = result[21:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    mulAddRecF32_mul (
        input [(`floatControlWidth - 1):0] control,
        input [32:0] a,
        input [32:0] b,
        input [2:0] roundingMode,
        output [32:0] out,
        output [4:0] exceptionFlags
    );

    wire [32:0] zeroAddend = {a[32] ^ b[32], 32'b0};
    wire [37:0] result;
    module_mulAddRecF32 mulAddRecF32(
        .mulAddRecF32_1(2'b0),
        .mulAddRecF32_2(a),
        .mulAddRecF32_3(b),
        .mulAddRecF32_4(zeroAddend),
        .mulAddRecF32_5(roundingMode),
        .mulAddRecF32_6(control[0]),
        .mulAddRecF32(result)
    );
    assign out = result[37:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    mulAddRecF64_mul (
        input [(`floatControlWidth - 1):0] control,
        input [64:0] a,
        input [64:0] b,
        input [2:0] roundingMode,
        output [64:0] out,
        output [4:0] exceptionFlags
    );

    wire [64:0] zeroAddend = {a[64] ^ b[64], 64'b0};
    wire [69:0] result;
    module_mulAddRecF64 mulAddRecF64(
        .mulAddRecF64_1(2'b0),
        .mulAddRecF64_2(a),
        .mulAddRecF64_3(b),
        .mulAddRecF64_4(zeroAddend),
        .mulAddRecF64_5(roundingMode),
        .mulAddRecF64_6(control[0]),
        .mulAddRecF64(result)
    );
    assign out = result[69:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    mulAddRecF128_mul (
        input [(`floatControlWidth - 1):0] control,
        input [128:0] a,
        input [128:0] b,
        input [2:0] roundingMode,
        output [128:0] out,
        output [4:0] exceptionFlags
    );

    wire [128:0] zeroAddend = {a[128] ^ b[128], 128'b0};
    wire [133:0] result;
    module_mulAddRecF128 mulAddRecF128(
        .mulAddRecF128_1(2'b0),
        .mulAddRecF128_2(a),
        .mulAddRecF128_3(b),
        .mulAddRecF128_4(zeroAddend),
        .mulAddRecF128_5(roundingMode),
        .mulAddRecF128_6(control[0]),
        .mulAddRecF128(result)
    );
    assign out = result[133:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    mulAddRecF16 (
        input [(`floatControlWidth - 1):0] control,
        input [1:0] op,
        input [16:0] a,
        input [16:0] b,
        input [16:0] c,
        input [2:0] roundingMode,
        output [16:0] out,
        output [4:0] exceptionFlags
    );

    wire [21:0] result;
    module_mulAddRecF16 mulAddRecF16(
        .mulAddRecF16_1(op),
        .mulAddRecF16_2(a),
        .mulAddRecF16_3(b),
        .mulAddRecF16_4(c),
        .mulAddRecF16_5(roundingMode),
        .mulAddRecF16_6(control[0]),
        .mulAddRecF16(result)
    );
    assign out = result[21:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    mulAddRecF32 (
        input [(`floatControlWidth - 1):0] control,
        input [1:0] op,
        input [32:0] a,
        input [32:0] b,
        input [32:0] c,
        input [2:0] roundingMode,
        output [32:0] out,
        output [4:0] exceptionFlags
    );

    wire [37:0] result;
    module_mulAddRecF32 mulAddRecF32(
        .mulAddRecF32_1(op),
        .mulAddRecF32_2(a),
        .mulAddRecF32_3(b),
        .mulAddRecF32_4(c),
        .mulAddRecF32_5(roundingMode),
        .mulAddRecF32_6(control[0]),
        .mulAddRecF32(result)
    );
    assign out = result[37:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    mulAddRecF64 (
        input [(`floatControlWidth - 1):0] control,
        input [1:0] op,
        input [64:0] a,
        input [64:0] b,
        input [64:0] c,
        input [2:0] roundingMode,
        output [64:0] out,
        output [4:0] exceptionFlags
    );

    wire [69:0] result;
    module_mulAddRecF64 mulAddRecF64(
        .mulAddRecF64_1(op),
        .mulAddRecF64_2(a),
        .mulAddRecF64_3(b),
        .mulAddRecF64_4(c),
        .mulAddRecF64_5(roundingMode),
        .mulAddRecF64_6(control[0]),
        .mulAddRecF64(result)
    );
    assign out = result[69:5];
    assign exceptionFlags = result[4:0];

endmodule

module
    mulAddRecF128 (
        input [(`floatControlWidth - 1):0] control,
        input [1:0] op,
        input [128:0] a,
        input [128:0] b,
        input [128:0] c,
        input [2:0] roundingMode,
        output [128:0] out,
        output [4:0] exceptionFlags
    );

    wire [133:0] result;
    module_mulAddRecF128 mulAddRecF128(
        .mulAddRecF128_1(op),
        .mulAddRecF128_2(a),
        .mulAddRecF128_3(b),
        .mulAddRecF128_4(c),
        .mulAddRecF128_5(roundingMode),
        .mulAddRecF128_6(control[0]),
        .mulAddRecF128(result)
    );
    assign out = result[133:5];
    assign exceptionFlags = result[4:0];

endmodule
