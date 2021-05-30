CC = clang

CFLAGS += -std=c99
CFLAGS += -D_POSIX_C_SOURCE=200809L
CFLAGS += -Wall -Wextra -Werror -Wsign-conversion
CFLAGS += -Wno-unused-parameter -Wno-unused-function
CFLAGS += -fcolor-diagnostics
CFLAGS += -fPIC

LDFLAGS += -shared

LDLIBS += -lsqlite3

NIF_LIB = priv/sqlite_nif.so
NIF_SRC = $(wildcard c_src/*.c)
NIF_OBJ = $(subst .c,.o,$(NIF_SRC))

PLATFORM = $(shell uname -s)

ifeq ($(PLATFORM), Linux)
	CFLAGS += -I/usr/lib/erlang/usr/include

	LDFLAGS += -L/usr/lib/erlang/usr/lib
endif

ifeq ($(PLATFORM), FreeBSD)
	CFLAGS += -I/usr/local/include
	CFLAGS += -I/usr/local/lib/erlang/usr/include

	LDFLAGS += -I/usr/local/lib
	LDFLAGS += -L/usr/local/lib/erlang/usr/lib
endif

all: dialyzer test

dialyzer:
	QUIET=1 rebar3 dialyzer

build: nif
	QUIET=1 rebar3 compile

test: nif
	QUIET=1 rebar3 eunit

nif: $(NIF_LIB)

$(NIF_LIB): $(NIF_OBJ)
	$(CC) -o $@ $(LDFLAGS) $^ $(LDLIBS)

%.o: %.c
	$(CC) -o $@ $(CFLAGS) -c $<

cover:
	QUIET=1 rebar3 eunit --cover
	QUIET=1 rebar3 cover

clean:
	$(RM) -r _build $(NIF_LIB) $(wildcard c_src/*.o)

.PHONY: all dialyzer build test nif cover clean
