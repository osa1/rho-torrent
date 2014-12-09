`rho` is a BitTorrent client written in Haskell. Main features are:

- Fully-functional, supporting most widely-used extensions. (including encryption protocols)
- User-friendly, with GTK GUI.
- Stable, fast, and memory-efficient. It should be comparable with widely-used BitTorrent clients like tranmission, qtorrent and µTorrent.

## Current status

It's not in a usable state right now. As of [e7623a6](https://github.com/osa1/rho-torrent/commit/e7623a65b27d0de2da6c20fd599e21c85fe04009) it can download pieces from peers and generate files from completed pieces. Tracker communications are complete. Peer messages are missing some extensions. The driver are very inefficient and poorly written.

## Protocol extensions

Extensions that are currently in code base (at least partially):

| Extension | Description                                | Status |
|-----------|--------------------------------------------|--------|
| BEP 3     | The BitTorrent Protocol Specification      | Done   |
| BEP 6     | Fast extension                             | WIP    |
| BEP 9     | Extension for Peers to Send Metadata Files | WIP    |
| BEP 15    | UDP Tracker Protocol                       | Done   |
| BEP 23    | Tracker Returns Compact Peer List          | Done   |

Most other extensions listed in [BEP 0](http://bittorrent.org/beps/bep_0000.html) should be supported. In addition, widely-used encryption protocols should also be supported.

## GUI

Not started yet. GTK3 should be used.

## Contributing

There are lots of `TODO` and `FIXME`s in the code. Most internal algortihms and data structures are written in a very inefficient way. Some of them are missing unit/property tests. Those parts should be 1) benchmarked 2) thoroughly tested(with property tests when possible) 3) replaced with efficient implementations.

TODO: Add more.
