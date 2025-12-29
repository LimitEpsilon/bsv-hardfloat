BSC_FLAGS=-p ./src:+ -verilog -aggressive-conditions -keep-fires -show-schedule

compile:
	mkdir -p buildDir
	bsc -u -bdir buildDir -info-dir buildDir -simdir buildDir -vdir buildDir $(BSC_FLAGS) TestBench.bsv
	cp buildDir/*.v ./tests/HardFloat-1/source

all: compile

clean:
	rm -rf buildDir
	rm ./tests/HardFloat-1/source/module_*

.PHONY: clean all compile
.DEFAULT_GOAL := all
