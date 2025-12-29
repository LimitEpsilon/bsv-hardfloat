
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
    ui32ToRecF16 (
        input [(`floatControlWidth - 1):0] control,
        input [31:0] in,
        input [2:0] roundingMode,
        output [16:0] out,
        output [4:0] exceptionFlags
    );
  module_ui32ToRecF16 inst(in, roundingMode, control, {out, exceptionFlags});
endmodule

module
    ui32ToRecF32 (
        input [(`floatControlWidth - 1):0] control,
        input [31:0] in,
        input [2:0] roundingMode,
        output [32:0] out,
        output [4:0] exceptionFlags
    );
  module_ui32ToRecF32 inst(in, roundingMode, control, {out, exceptionFlags});
endmodule

module
    ui32ToRecF64 (
        input [(`floatControlWidth - 1):0] control,
        input [31:0] in,
        input [2:0] roundingMode,
        output [64:0] out,
        output [4:0] exceptionFlags
    );
  module_ui32ToRecF64 inst(in, roundingMode, control, {out, exceptionFlags});
endmodule

module
    ui32ToRecF128 (
        input [(`floatControlWidth - 1):0] control,
        input [31:0] in,
        input [2:0] roundingMode,
        output [128:0] out,
        output [4:0] exceptionFlags
    );
  module_ui32ToRecF128 inst(in, roundingMode, control, {out, exceptionFlags});
endmodule

module
    ui64ToRecF16 (
        input [(`floatControlWidth - 1):0] control,
        input [63:0] in,
        input [2:0] roundingMode,
        output [16:0] out,
        output [4:0] exceptionFlags
    );
  module_ui64ToRecF16 inst(in, roundingMode, control, {out, exceptionFlags});
endmodule

module
    ui64ToRecF32 (
        input [(`floatControlWidth - 1):0] control,
        input [63:0] in,
        input [2:0] roundingMode,
        output [32:0] out,
        output [4:0] exceptionFlags
    );
  module_ui64ToRecF32 inst(in, roundingMode, control, {out, exceptionFlags});
endmodule

module
    ui64ToRecF64 (
        input [(`floatControlWidth - 1):0] control,
        input [63:0] in,
        input [2:0] roundingMode,
        output [64:0] out,
        output [4:0] exceptionFlags
    );
  module_ui64ToRecF64 inst(in, roundingMode, control, {out, exceptionFlags});
endmodule

module
    ui64ToRecF128 (
        input [(`floatControlWidth - 1):0] control,
        input [63:0] in,
        input [2:0] roundingMode,
        output [128:0] out,
        output [4:0] exceptionFlags
    );
  module_ui64ToRecF128 inst(in, roundingMode, control, {out, exceptionFlags});
endmodule

module
    i32ToRecF16 (
        input [(`floatControlWidth - 1):0] control,
        input [31:0] in,
        input [2:0] roundingMode,
        output [16:0] out,
        output [4:0] exceptionFlags
    );
  module_i32ToRecF16 inst(in, roundingMode, control, {out, exceptionFlags});
endmodule

module
    i32ToRecF32 (
        input [(`floatControlWidth - 1):0] control,
        input [31:0] in,
        input [2:0] roundingMode,
        output [32:0] out,
        output [4:0] exceptionFlags
    );
  module_i32ToRecF32 inst(in, roundingMode, control, {out, exceptionFlags});
endmodule

module
    i32ToRecF64 (
        input [(`floatControlWidth - 1):0] control,
        input [31:0] in,
        input [2:0] roundingMode,
        output [64:0] out,
        output [4:0] exceptionFlags
    );
  module_i32ToRecF64 inst(in, roundingMode, control, {out, exceptionFlags});
endmodule

module
    i32ToRecF128 (
        input [(`floatControlWidth - 1):0] control,
        input [31:0] in,
        input [2:0] roundingMode,
        output [128:0] out,
        output [4:0] exceptionFlags
    );
  module_i32ToRecF128 inst(in, roundingMode, control, {out, exceptionFlags});
endmodule

module
    i64ToRecF16 (
        input [(`floatControlWidth - 1):0] control,
        input [63:0] in,
        input [2:0] roundingMode,
        output [16:0] out,
        output [4:0] exceptionFlags
    );
  module_i64ToRecF16 inst(in, roundingMode, control, {out, exceptionFlags});
endmodule

module
    i64ToRecF32 (
        input [(`floatControlWidth - 1):0] control,
        input [63:0] in,
        input [2:0] roundingMode,
        output [32:0] out,
        output [4:0] exceptionFlags
    );
  module_i64ToRecF32 inst(in, roundingMode, control, {out, exceptionFlags});
endmodule

module
    i64ToRecF64 (
        input [(`floatControlWidth - 1):0] control,
        input [63:0] in,
        input [2:0] roundingMode,
        output [64:0] out,
        output [4:0] exceptionFlags
    );
  module_i64ToRecF64 inst(in, roundingMode, control, {out, exceptionFlags});
endmodule

module
    i64ToRecF128 (
        input [(`floatControlWidth - 1):0] control,
        input [63:0] in,
        input [2:0] roundingMode,
        output [128:0] out,
        output [4:0] exceptionFlags
    );
  module_i64ToRecF128 inst(in, roundingMode, control, {out, exceptionFlags});
endmodule

