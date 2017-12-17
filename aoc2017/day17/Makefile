
PHONY: all

spinlock.c:
	cython -3 --embed spinlock.py -o spinlock.c

spinlock: spinlock.c
	gcc -Ofast -march=native spinlock.c $(shell pkg-config --libs --cflags python3) -o spinlock

all: spinlock
	echo '348' | /usr/bin/time ./spinlock
