import HardFloat::*;

(* noinline *)
function RecFNFromFN#(5, 11) f16ToRecF16;
  return recFNFromFN;
endfunction

(* noinline *)
function RecFNFromFN#(8, 24) f32ToRecF32;
  return recFNFromFN;
endfunction

(* noinline *)
function RecFNFromFN#(11, 53) f64ToRecF64;
  return recFNFromFN;
endfunction

(* noinline *)
function RecFNFromFN#(15, 113) f128ToRecF128;
  return recFNFromFN;
endfunction

(* noinline *)
function FNFromRecFN#(5, 11) recF16ToF16;
  return fNFromRecFN;
endfunction

(* noinline *)
function FNFromRecFN#(8, 24) recF32ToF32;
  return fNFromRecFN;
endfunction

(* noinline *)
function FNFromRecFN#(11, 53) recF64ToF64;
  return fNFromRecFN;
endfunction

(* noinline *)
function FNFromRecFN#(15, 113) recF128ToF128;
  return fNFromRecFN;
endfunction

(* noinline *)
function INToRecFN#(32, 5, 11) ui32ToRecF16;
  return mkINToRecFN(False);
endfunction

(* noinline *)
function INToRecFN#(32, 8, 24) ui32ToRecF32;
  return mkINToRecFN(False);
endfunction

(* noinline *)
function INToRecFN#(32, 11, 53) ui32ToRecF64;
  return mkINToRecFN(False);
endfunction

(* noinline *)
function INToRecFN#(32, 15, 113) ui32ToRecF128;
  return mkINToRecFN(False);
endfunction

(* noinline *)
function INToRecFN#(64, 5, 11) ui64ToRecF16;
  return mkINToRecFN(False);
endfunction

(* noinline *)
function INToRecFN#(64, 8, 24) ui64ToRecF32;
  return mkINToRecFN(False);
endfunction

(* noinline *)
function INToRecFN#(64, 11, 53) ui64ToRecF64;
  return mkINToRecFN(False);
endfunction

(* noinline *)
function INToRecFN#(64, 15, 113) ui64ToRecF128;
  return mkINToRecFN(False);
endfunction

(* noinline *)
function INToRecFN#(32, 5, 11) i32ToRecF16;
  return mkINToRecFN(True);
endfunction

(* noinline *)
function INToRecFN#(32, 8, 24) i32ToRecF32;
  return mkINToRecFN(True);
endfunction

(* noinline *)
function INToRecFN#(32, 11, 53) i32ToRecF64;
  return mkINToRecFN(True);
endfunction

(* noinline *)
function INToRecFN#(32, 15, 113) i32ToRecF128;
  return mkINToRecFN(True);
endfunction

(* noinline *)
function INToRecFN#(64, 5, 11) i64ToRecF16;
  return mkINToRecFN(True);
endfunction

(* noinline *)
function INToRecFN#(64, 8, 24) i64ToRecF32;
  return mkINToRecFN(True);
endfunction

(* noinline *)
function INToRecFN#(64, 11, 53) i64ToRecF64;
  return mkINToRecFN(True);
endfunction

(* noinline *)
function INToRecFN#(64, 15, 113) i64ToRecF128;
  return mkINToRecFN(True);
endfunction

(* noinline *)
function RecFNToIN#(5, 11, 32) recF16ToUi32;
  return mkRecFNToIN(False);
endfunction

(* noinline *)
function RecFNToIN#(5, 11, 64) recF16ToUi64;
  return mkRecFNToIN(False);
endfunction

(* noinline *)
function RecFNToIN#(5, 11, 32) recF16ToI32;
  return mkRecFNToIN(True);
endfunction

(* noinline *)
function RecFNToIN#(5, 11, 64) recF16ToI64;
  return mkRecFNToIN(True);
endfunction

(* noinline *)
function RecFNToIN#(8, 24, 32) recF32ToUi32;
  return mkRecFNToIN(False);
endfunction

(* noinline *)
function RecFNToIN#(8, 24, 64) recF32ToUi64;
  return mkRecFNToIN(False);
endfunction

(* noinline *)
function RecFNToIN#(8, 24, 32) recF32ToI32;
  return mkRecFNToIN(True);
endfunction

(* noinline *)
function RecFNToIN#(8, 24, 64) recF32ToI64;
  return mkRecFNToIN(True);
endfunction

(* noinline *)
function RecFNToIN#(11, 53, 32) recF64ToUi32;
  return mkRecFNToIN(False);
endfunction

(* noinline *)
function RecFNToIN#(11, 53, 64) recF64ToUi64;
  return mkRecFNToIN(False);
endfunction

(* noinline *)
function RecFNToIN#(11, 53, 32) recF64ToI32;
  return mkRecFNToIN(True);
endfunction

(* noinline *)
function RecFNToIN#(11, 53, 64) recF64ToI64;
  return mkRecFNToIN(True);
endfunction

(* noinline *)
function RecFNToIN#(15, 113, 32) recF128ToUi32;
  return mkRecFNToIN(False);
endfunction

(* noinline *)
function RecFNToIN#(15, 113, 64) recF128ToUi64;
  return mkRecFNToIN(False);
endfunction

(* noinline *)
function RecFNToIN#(15, 113, 32) recF128ToI32;
  return mkRecFNToIN(True);
endfunction

(* noinline *)
function RecFNToIN#(15, 113, 64) recF128ToI64;
  return mkRecFNToIN(True);
endfunction

(* noinline *)
function RecFNToRecFN#(8, 24, 5, 11) recF32ToRecF16;
  return mkRecFNToRecFN;
endfunction

(* noinline *)
function RecFNToRecFN#(11, 53, 5, 11) recF64ToRecF16;
  return mkRecFNToRecFN;
endfunction

(* noinline *)
function RecFNToRecFN#(15, 113, 5, 11) recF128ToRecF16;
  return mkRecFNToRecFN;
endfunction

(* noinline *)
function RecFNToRecFN#(5, 11, 8, 24) recF16ToRecF32;
  return mkRecFNToRecFN;
endfunction

(* noinline *)
function RecFNToRecFN#(11, 53, 8, 24) recF64ToRecF32;
  return mkRecFNToRecFN;
endfunction

(* noinline *)
function RecFNToRecFN#(15, 113, 8, 24) recF128ToRecF32;
  return mkRecFNToRecFN;
endfunction

(* noinline *)
function RecFNToRecFN#(5, 11, 11, 53) recF16ToRecF64;
  return mkRecFNToRecFN;
endfunction

(* noinline *)
function RecFNToRecFN#(8, 24, 11, 53) recF32ToRecF64;
  return mkRecFNToRecFN;
endfunction

(* noinline *)
function RecFNToRecFN#(15, 113, 11, 53) recF128ToRecF64;
  return mkRecFNToRecFN;
endfunction

(* noinline *)
function RecFNToRecFN#(5, 11, 15, 113) recF16ToRecF128;
  return mkRecFNToRecFN;
endfunction

(* noinline *)
function RecFNToRecFN#(8, 24, 15, 113) recF32ToRecF128;
  return mkRecFNToRecFN;
endfunction

(* noinline *)
function RecFNToRecFN#(11, 53, 15, 113) recF64ToRecF128;
  return mkRecFNToRecFN;
endfunction

(* noinline *)
function CompareRecFN#(5, 11) compareRecF16;
  return mkCompareRecFN;
endfunction

(* noinline *)
function CompareRecFN#(8, 24) compareRecF32;
  return mkCompareRecFN;
endfunction

(* noinline *)
function CompareRecFN#(11, 53) compareRecF64;
  return mkCompareRecFN;
endfunction

(* noinline *)
function CompareRecFN#(15, 113) compareRecF128;
  return mkCompareRecFN;
endfunction

