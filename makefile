all: soc

SRCS	=	src/soc.adb \
		src/oberon.ad[sb] \
		src/oberon-files.ad[sb] \
		src/oberon-lexer.ad[sb] \
		src/oberon-scanner.ad[sb]

soc: $(SRCS)
	gprbuild -p -Psoc.gpr

.PHONY: clean

clean:
	gprclean -Psoc.gpr

