---
name: lilypond-doc-navigation
description: >
  Read a named passage from Lilypond's Texinfo manual sources without groping
  or reading unnecessary context.
---

Translated documents retain the English node names.

[node-extent.py](scripts/node-extent.py) locates a passage (section,
subsection, etc.) in a file (or files) given the name of its anchor node.
Usage: `$SKILL/scripts/node-extent.py <node_name> <file_name>...`
Output: `<file_name>:<start_line>-<end_line>`
