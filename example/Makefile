# See LICENSE file for copyright and license details.

all: __filename__

__filename__: __filename__.o
	@gcc -o __filename__ __filename__.o

__filename__.o: __filename__.c
	@gcc -c __filename__.c

clean:
	@rm -f __filename__ __filename__.o

.PHONY: all clean
