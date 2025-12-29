## How to run tests

- First run `git submodule update --init`
- Then edit `tests/berkeley-softfloat-3/build/<target architecture>/Makefile` to have `SPECIALIZE_TYPE ?= RISCV`
- Then run `make` in `tests/berkeley-softfloat-3/build/<target-architecture>`
- Then run `make` in `tests/berkeley-testfloat-3/build/<target-architecture>`
- Then run `make` in the project root, to copy the generated Verilog files to `tests/HardFloat-1/source`
- Assuming `Verilator` is installed, run `make test-level1` in `tests/HardFloat-1/test/build/Verilator-GCC`

