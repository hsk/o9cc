CC = gcc
CFLAGS =
ifeq ($(shell uname -m),arm64)
	CFLAGS += -arch x86_64
endif

test:
	cd ..; echo 0 | dune exec -- o9cc 
	$(CC) $(CFLAGS) ../tmp.s -o a.exe
	./a.exe ; test $$? = 0
	cd ..; echo 1 | dune exec -- o9cc
	$(CC) $(CFLAGS) ../tmp.s -o a.exe
	./a.exe ; test $$? = 1

	cd ..; echo "1+2+3" | dune exec -- o9cc 
	$(CC) $(CFLAGS) ../tmp.s -o a.exe
	./a.exe ; test $$? = 6

	cd ..; echo "10 - 1 - 2" | dune exec -- o9cc
	$(CC) $(CFLAGS) ../tmp.s -o a.exe
	./a.exe ; test $$? = 7


