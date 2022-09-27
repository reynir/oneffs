# OneFFS - One File Filesystem
One-file filesystem is a filesystem for storing a single unnamed file.
It is suitable for storing data that may be lost on updates.
For example a cache of pre-computed cryptographic checksums that are slow to compute, but that we would rather recompute than have stale checksums.

CRC32 checksums are used to ensure integrity of both the header and the data.

With [mirage-block-partition](https://github.com/reynir/mirage-block-partition/) you can store multiple files with static allocation.
