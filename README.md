# hledger-graphs

[![hledger-graphs Build Status](https://travis-ci.org/prikhi/hledger-graphs.svg?branch=master)](https://travis-ci.org/prikhi/hledger-graphs)


Generate Graphs from your HLedger Journal.

![Pie and Bar Graphs Showing Income & Expenses](https://raw.githubusercontent.com/prikhi/hledger-graphs/master/sample.png "hledger-graphs Screenshot")


## Build

You can build the project with stack:

```
stack build
```

For development, you can enable fast builds with file-watching,
documentation-building, & test-running:

```
stack test --haddock --fast --file-watch --pedantic
```

To build & open the documentation, run

```
stack haddock --open hledger-graphs
```


## LICENSE

BSD-3
