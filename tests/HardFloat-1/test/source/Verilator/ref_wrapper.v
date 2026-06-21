`include "HardFloat_consts.vi"
`include "HardFloat_specialize.vi"

module divSqrtRecF16_small_div_ref (
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
    wire sqrtOpOut;
    divSqrtRecFN_small#(5, 11, 0)
        divSqrtRecFN(
            nReset, clock, control, inReady, inValid, 1'b0,
            a, b, roundingMode, outValid, sqrtOpOut, out, exceptionFlags
        );
endmodule
