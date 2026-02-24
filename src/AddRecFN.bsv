import Vector::*;
import FloatingPoint::*;
import HardFloat::*;

typedef function Tuple2#(Bool, RawFloat#(expWidth, TAdd#(2, sigWidth))) _
  ( Bool subOp,
    RawFloat#(expWidth, sigWidth) a,
    RawFloat#(expWidth, sigWidth) b,
    Bit#(3) roundingMode
  ) AddRawFN#(
  numeric type expWidth, numeric type sigWidth
);

function AddRawFN#(expWidth, sigWidth) mkAddRawFN
  provisos (
    Add#(1, fractWidth, sigWidth)
  );

  function Tuple2#(Bool, RawFloat#(expWidth, TAdd#(2, sigWidth))) f(
    Bool subOp,
    RawFloat#(expWidth, sigWidth) a,
    RawFloat#(expWidth, sigWidth) b,
    Bit#(3) roundingMode
  );
    Integer alignDistWidth = valueOf(TLog#(sigWidth));
    Integer sW = valueOf(sigWidth);

    Bool effSignB = unpack(pack(b.sign) ^ pack(subOp));
    Bool eqSigns = a.sign == effSignB;
    Bool notEqSigns_signZero = roundingMode == round_min;
    Int#(TAdd#(2, expWidth)) sDiffExps = a.sExp - b.sExp;
    Bit#(TAdd#(2, expWidth)) bDiffExps = pack(sDiffExps);
    Int#(TLog#(sigWidth)) modNatAlignDist = 
        unpack(pack((sDiffExps < 0) ? b.sExp - a.sExp : sDiffExps)[alignDistWidth - 1 : 0]);
    Bool isMaxAlign =
      (sDiffExps >> alignDistWidth) != 0 &&
        ((sDiffExps >> alignDistWidth) != -1 || bDiffExps[alignDistWidth - 1 : 0] == 0);
    Int#(TLog#(sigWidth)) alignDist = isMaxAlign ? ((1 << alignDistWidth) - 1) : modNatAlignDist;
    Bool closeSubMags = !eqSigns && !isMaxAlign && (modNatAlignDist <= 1);
    
    UInt#(TAdd#(1, sigWidth)) close_alignedSigA = 
      ((0 <= sDiffExps && (bDiffExps[0] == 1)) ? a.sig << 2 : 0) |
      ((0 <= sDiffExps && (bDiffExps[0] == 0)) ? a.sig << 1 : 0) |
      (sDiffExps < 0                           ? a.sig      : 0);
    UInt#(TAdd#(1, sigWidth)) close_sSigSum = unpack(pack(close_alignedSigA - (b.sig << 1)));
    Int#(TAdd#(sigWidth, 2)) close_sigSum = unpack(pack(close_sSigSum < 0 ? -unpack(pack(close_sSigSum)) : unpack(pack(close_sSigSum)))[sigWidth + 1 : 0]);
    Int#(TAdd#(sigWidth, 2)) close_adjustedSigSum = close_sigSum << (fromInteger(sW) & 1);
    Bit#(TDiv#(TAdd#(sigWidth, 2), 2)) close_reduced2SigSum = orReduceBy2(pack(close_adjustedSigSum));
    let close_normDistReduced2 = pack(countLeadingZeros(close_reduced2SigSum));
    Bit#(alignDistWidth) close_nearNormDist = (close_normDistReduced2 << 1)[alignDistWidth - 1 : 0];
    UInt#(TAdd#(sigWidth, 3)) close_sigOut = unpack(pack((close_sigSum << close_nearNormDist) << 1)[sW + 2 : 0]);
    Bool close_totalCancellation = !(orR(pack(close_sigOut)[sW + 2 : sW + 1]));
    Bool close_notTotalCancellation_signOut = unpack(pack(a.sign) ^ pack(close_sSigSum < 0));

    Bool far_signOut = sDiffExps < 0 ? effSignB : a.sign;
    UInt#(sigWidth) far_sigLarger  = truncate(sDiffExps < 0 ? b.sig : a.sig);
    UInt#(sigWidth) far_sigSmaller = truncate(sDiffExps < 0 ? a.sig : b.sig);
    UInt#(sigWidth) far_mainAlignedSigSmaller = (far_sigSmaller << 5) >> alignDist;
    Bit#(TDiv#(sigWidth, 4)) far_reduced4SigSmaller = unpack(orReduceBy4(pack(far_sigSmaller << 2)));
    let far_roundExtraMask = lowMask(pack(alignDist)[alignDistWidth - 1 : 2], (sW + 5)/4, 0);
    Bit#(TAdd#(sigWidth, 1)) far_alignedSigSmaller =
        { pack(far_mainAlignedSigSmaller) >> 3,
            pack(orR(pack(far_mainAlignedSigSmaller)[2 : 0]) || 
                 orR(far_reduced4SigSmaller & far_roundExtraMask)) };
    Bool far_subMags = !eqSigns;
    Bit#(TAdd#(sigWidth, 2)) far_negAlignedSigSmaller = far_subMags ? 
        { 1, ~far_alignedSigSmaller } : zeroExtend(far_alignedSigSmaller);
    UInt#(TAdd#(sigWidth, 3)) far_sigSum = (far_sigLarger << 3) + unpack(far_negAlignedSigSmaller) + unpack(pack(far_subMags));
    UInt#(TAdd#(sigWidth, 3)) far_sigOut = far_subMags ? 
        unpack(pack(far_sigSum)) : unpack(pack((far_sigSum>>1) | unpack(zeroExtend(pack(far_sigSum)[0]))));
    
    Bool notSigNaN_invalidExc = a.isInf && b.isInf && !eqSigns;
    Bool notNaN_isInfOut = a.isInf || b.isInf;
    Bool addZeros = a.isZero && b.isZero;
    Bool notNaN_specialCase = notNaN_isInfOut || addZeros;
    Bool notNaN_isZeroOut = addZeros || (!notNaN_isInfOut && closeSubMags && close_totalCancellation);
    Bool notNaN_signOut =
        (eqSigns                      && a.sign          ) ||
        (a.isInf                   && a.sign          ) ||
        (b.isInf                   && effSignB           ) ||
        (notNaN_isZeroOut && !eqSigns && notEqSigns_signZero) ||
        (!notNaN_specialCase && closeSubMags && !close_totalCancellation && close_notTotalCancellation_signOut) ||
        (!notNaN_specialCase && !closeSubMags && far_signOut);
    Int#(TAdd#(2, expWidth)) common_sExpOut =
        ((closeSubMags || (sDiffExps < 0)) ? b.sExp : a.sExp)
            - unpack(zeroExtend(closeSubMags ? close_nearNormDist : pack(far_subMags)));
    UInt#(TAdd#(3, sigWidth)) common_sigOut = closeSubMags ? close_sigOut : far_sigOut;

    Bool invalidExc = isSigNaNRawFloat(a) || isSigNaNRawFloat(b) || notSigNaN_invalidExc;
    RawFloat#(expWidth, sigWidth) rawOut;
    rawOut.isInf = notNaN_isInfOut;
    rawOut.isZero = notNaN_isZeroOut;
    rawOut.sExp = common_sExpOut;
    rawOut.isNaN = a.isNaN || b.isNaN;
    rawOut.sign = notNaN_signOut;
    rawOut.sig = unpack(pack(common_sigOut)[sW : 0]);
    return tuple2(invalidExc, rawOut);
  endfunction

  return f;
endfunction

typedef function Tuple2#(Bit#(TAdd#(1, TAdd#(expWidth, sigWidth))), Exception) _
  ( Bool subOp,
    Bit#(TAdd#(1, TAdd#(expWidth, sigWidth))) a,
    Bit#(TAdd#(1, TAdd#(expWidth, sigWidth))) b,
    Bit#(3) roundingMode,
    Bool detectTininess
  ) AddRecFN#(
  numeric type expWidth, numeric type sigWidth
);

function AddRecFN#(expWidth, sigWidth) mkAddRecFN
  provisos (
    Add#(2, _1, expWidth),
    Add#(1, fractWidth, sigWidth),
    Add#(1, _2, fractWidth)
  );

  function Tuple2#(Bit#(TAdd#(1, TAdd#(expWidth, sigWidth))), Exception) f(
    Bool subOp,
    Bit#(TAdd#(1, TAdd#(expWidth, sigWidth))) a,
    Bit#(TAdd#(1, TAdd#(expWidth, sigWidth))) b,
    Bit#(3) roundingMode,
    Bool detectTininess
  );
    RawFloat#(expWidth, sigWidth) a_raw = rawFloatFromRecFN(a);
    RawFloat#(expWidth, sigWidth) b_raw = rawFloatFromRecFN(b);
    let addRes = mkAddRawFN(
      subOp,
      a_raw,
      b_raw,
      roundingMode
    );

    RoundRawFNToRecFN#(expWidth, sigWidth) roundRawFNToRecFN = mkRoundRawFNToRecFN(0);
    let roundRes = roundRawFNToRecFN(
      tpl_1(addRes),
      False,
      tpl_2(addRes),
      roundingMode,
      pack(detectTininess)
    );
    
    return roundRes;
  endfunction

  return f;
endfunction