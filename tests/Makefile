TARGETS=$(dir $(wildcard */Makefile))
all:${TARGETS}

.PHONY:${TARGETS}
${TARGETS}:
	${MAKE} -C $@
