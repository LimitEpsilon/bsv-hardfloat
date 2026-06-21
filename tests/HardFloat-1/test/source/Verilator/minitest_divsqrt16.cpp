#include <stdio.h>
#include <verilated.h>
#include "VdivSqrtRecF16_small_div.h"
#include "testCommon.h"

int main(int argc, char *argv[]) {
    VdivSqrtRecF16_small_div *m = new VdivSqrtRecF16_small_div;
    m->control = 0;
    m->inValid = 0;
    m->roundingMode = 0;
    m->clock = 1;
    m->nReset = 0;
    m->eval();
    m->nReset = 1;
    m->eval();

    // request 1: 1.0 / 3.0
    long recA1 = recF16FromF16(0x3C00, true);
    long recB1 = recF16FromF16(0x4200, true);
    // request 2: 1.0 / 7.0
    long recA2 = recF16FromF16(0x3C00, true);
    long recB2 = recF16FromF16(0x4700, true);

    int numSent = 0;
    int numRecv = 0;
    int cyc = 0;
    while (numRecv < 2 && cyc < 60) {
        printf("cyc=%d inReady=%d outValid=%d\n", cyc, (int)m->inReady, (int)m->outValid);
        if (m->inReady && numSent < 2) {
            m->inValid = 1;
            m->a = (numSent == 0) ? recA1 : recA2;
            m->b = (numSent == 0) ? recB1 : recB2;
            printf("  -> sending req #%d at cycle=%d\n", numSent, cyc);
            numSent++;
        } else {
            m->inValid = 0;
        }
        if (m->outValid) {
            printf("  <- outValid #%d at cycle=%d out=%lx exc=%x\n",
                numRecv, cyc, (long)m->out, (int)m->exceptionFlags);
            numRecv++;
        }
        m->clock = 0;
        m->eval();
        m->clock = 1;
        m->eval();
        cyc++;
    }
    return 0;
}
