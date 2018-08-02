# conduit-cereal

Differences from [cereal-conduit](http://hackage.haskell.org/package/cereal-conduit-0.8.0):

- This package provides a `sinkGet` and `conduitGet` which do not hold on to consumed input. Resuming deserialization after one of these conduits fails will yields unexpected results, but dropping consumed input is important for `Get`'s that scan long streams. `scarySinkGet` and `scaryConduitGet` behave like the other package's `sinkGet` and `conduitGet`.

- This package returns errors explicitly, as opposed to `throwM`ing them.
