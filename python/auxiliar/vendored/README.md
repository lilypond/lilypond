This directory contains an archive of the source of
[Pygments](https://pygments.org).  It is used for syntax highlighting
in the documentation.  It has been included in the source tree to
avoid an extra dependency until versions of Pygments starting from
2.11.0, released December 30, 2021, are available in wide enough a
range of GNU/Linux distributions.  This utilizes Python's ability to
import from ZIP archives.  DO NOT MAKE CHANGES HERE; instead, submit
them upstream at https://github.com/pygments/pygments/ and update the
archive when they are accepted.

The archive can be updated from a [release on
PyPI](https://pypi.org/project/Pygments/) or from [latest master via
GitHub](https://github.com/pygments/pygments/archive/master.zip).  In
the latter method, be sure to unpack and re-pack the archive.  Python
prior to version 3.8 did not support importing from ZIP archives
containing a comment.

The current version is 2.11.0.
