all: soc

soc: src/soc.adb
	gprbuild -p -Psoc.gpr

.PHONY: clean

clean:
	gprclean -Psoc.gpr
