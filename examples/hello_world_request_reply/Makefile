
PREFIX?=${NDDSHOME}
TARGET?=${DESTDIR}${PREFIX}/resource/template/rti_workspace/examples/connext_dds/ada/hello_world_request_reply

src/gen/%.h:src/%.idl
	rm -rf src/gen; mkdir -p src/gen
	rtiddsgen -language Ada -d src/gen src/Primes.idl

all:generate compile

generate:src/gen/Primes.h

compile:
	gprbuild -p -j0 -P hello_world_request_reply.gpr

install:
	mkdir -p ${TARGET}/src
	cp Makefile  READ_ME.txt  USER_QOS_PROFILES.xml  hello_world_request_reply.gpr  ${TARGET}
	cp Makefile  src/*.idl src/*.ad?  ${TARGET}/src

	sed "s/^.--@REPLACE//" Makefile >${TARGET}/Makefile

uninstall:
	rm -rf ${TARGET}
	
clean:
	git clean . -xdf #--@REPLACE	rm -rf .obj bin src/gen
