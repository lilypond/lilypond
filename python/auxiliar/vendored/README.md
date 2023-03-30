This directory contains an archive of the source of
[Pygments](https://pygments.org).  It is used for syntax highlighting
in the documentation.  It has been included in the source tree to
avoid an extra dependency until versions of Pygments starting from
2.11.0, released December 30, 2021, are available in wide enough a
range of GNU/Linux distributions.  This utilizes Python's ability to
import from ZIP archives.  DO NOT MAKE CHANGES HERE; instead, submit
them upstream at https://github.com/pygments/pygments/ and update the
archive when they are accepted.

In order to keep the size of the archive minimal, it is being trimmed.
To update it, run:

```
python3 scripts/auxiliar/update_pygments.py <rev>
```

where `<rev>` is any reference in the Pygments Git repository (such as
a commit, branch or tag).  The latest update was made with `<rev>` =
`2.14.0`.
