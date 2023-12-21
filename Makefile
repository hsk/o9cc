.PHONY: test

test:
	dune clean
	dune test
	cd test; make
clean:
	dune clean
	rm -rf tmp.s test/a.exe _build

