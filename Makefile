all: compile test

compile:
	gprbuild -j0 -p
test:
	${MAKE} -C tests
clean:

tag:
