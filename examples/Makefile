all:
	rm -rf src/gen; mkdir -p src/gen 
	rtiddsgen -language Ada -d src/gen src/Primes.idl
	gprbuild -p -j0 -P hello_world_request_reply.gpr

