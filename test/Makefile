CC = gcc
CFLAGS =
ifeq ($(shell uname -m),arm64)
	CFLAGS += -arch x86_64
endif

test:
	cd ..; dune exec -- o9cc 0
	$(CC) $(CFLAGS) ../tmp.s -o a.exe
	./a.exe ; test $$? = 0
	cd ..; dune exec -- o9cc 1
	$(CC) $(CFLAGS) ../tmp.s -o a.exe
	./a.exe ; test $$? = 1
	cd ..; dune exec -- o9cc -1
	$(CC) $(CFLAGS) ../tmp.s -o a.exe
	./a.exe ; test $$? = 255
