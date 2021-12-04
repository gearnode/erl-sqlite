CC ?= clang

NIF_CFLAGS  = -std=c99 -D_POSIX_C_SOURCE=200809L
NIF_CFLAGS += -Wall -Wextra -Werror -Wsign-conversion
NIF_CFLAGS += -Wno-unused-parameter -Wno-unused-function
ifeq ($(CC),clang)
NIF_CFLAGS += -fcolor-diagnostics
endif
NIF_CFLAGS += -fPIC
NIF_CFLAGS += -Ic_src/sqlite3

SQLITE3_CFLAGS += -std=c99 -D_POSIX_C_SOURCE=200809L
SQLITE3_CFLAGS += -fPIC
SQLITE3_CFLAGS += -O2
SQLITE3_CFLAGS += $(addprefix -D,$(SQLITE3_OPTIONS))

SQLITE3_OPTIONS = SQLITE_ENABLE_API_ARMOR \
		  SQLITE_ENABLE_FTS5 \
		  SQLITE_ENABLE_MATH_FUNCTIONS \
		  SQLITE_ENABLE_DBSTAT_VTAB \
		  SQLITE_ENABLE_JSON1 \
		  SQLITE_ENABLE_RTREE \
		  SQLITE_DQS=0 \
		  SQLITE_LIKE_DOESNT_MATCH_BLOBS \
		  HAVE_FDATASYNC \
		  HAVE_MALLOC_USABLE_SIZE \
		  HAVE_USLEEP \
		  HAVE_UTIME

NIF_LDFLAGS += -shared

NIF_SRC = $(wildcard c_src/*.c)
SQLITE3_SRC = $(wildcard c_src/sqlite3/*.c)

NIF_OBJ  = $(subst .c,.o,$(NIF_SRC))
NIF_OBJ += $(subst .c,.o,$(SQLITE3_SRC))

NIF_LIB = priv/sqlite_nif.so

PLATFORM = $(shell uname -s)

ifeq ($(PLATFORM), Linux)
	NIF_CFLAGS += -I/usr/lib/erlang/usr/include
	NIF_LDFLAGS += -L/usr/lib/erlang/usr/lib
endif

ifeq ($(PLATFORM), FreeBSD)
	NIF_CFLAGS += -I/usr/local/lib/erlang/usr/include
	NIF_LDFLAGS += -L/usr/local/lib/erlang/usr/lib
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
	$(CC) -o $@ $(NIF_LDFLAGS) $^

c_src/%.o: c_src/%.c
	$(CC) -o $@ $(NIF_CFLAGS) -c $<

c_src/sqlite3/%.o: c_src/sqlite3/%.c
	$(CC) -o $@ $(SQLITE3_CFLAGS) -c $<

cover:
	QUIET=1 rebar3 eunit --cover
	QUIET=1 rebar3 cover

clean:
	$(RM) -r _build
	$(RM) $(NIF_LIB)
	$(RM) $(wildcard c_src/*.o)
	$(RM) $(wildcard c_src/sqlite3/*.o)

.PHONY: all dialyzer build test nif cover clean
