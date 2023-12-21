.PHONY: test

test:
	cd test; make
clean:
	dune clean
	rm -rf tmp.s test/a.exe _build

