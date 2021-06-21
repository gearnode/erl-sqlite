% erl-sqlite

# Introduction
This repository contains an interface for the [SQLite](https://www.sqlite.org)
database engine.

# Interface
Functions usually return an `ok` or `{ok, Value}` term on success and a
`{error, Reason}` tuple on failure, where `Reason` is of type
`sqlite:error_reason()`.

## Databases
The `sqlite:open/2` and `sqlite:open/3` functions are used to open a database.
On success, a process is created and is linked to the current process.

The `sqlite:close/1` function is used to close a database handle. Note that
the process which open the database is responsible to make sure it closes it,
in order to avoid leaking a file descriptor.

## Queries
The `sqlite:query/3` and `sqlite:query/4` functions are used to execute
queries. On success, the result is a `{ok, {Rows, Tail}}` where `Rows` is the
list of rows returned by the query and `Tail` is what is left of the initial
query once the first statement has been processed.

## Example
```erlang
try
  {ok, _} = sqlite:open(test_db, <<":memory:">>, #{}),
  {ok, [[1, 2, 3]], _} = sqlite:query(test_db, "SELECT 1, 2, ?", [3]),
after
  sqlite:close(test_db)
end.
```
