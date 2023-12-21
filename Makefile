.PHONY: test

test:
	dune clean
	dune test
	cd test; bash test.sh
clean:
	dune clean
	rm -rf tmp* test/a.exe test/tmp* _build
dot:
	dot -T png test/tmp.dot -o tmp.png
