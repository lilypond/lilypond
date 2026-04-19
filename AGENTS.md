[.gitlab-ci.yml](.gitlab-ci.yml) contains build and test commands.

[ROADMAP](ROADMAP) describes the project directory structure.

Do not read translations (`Documentation/{ca,de,es,fr,it,ja,tr,zh}/`) except by
explicit request!  Assume that they are outdated and incomplete!

| File extensions | Description |
|-----------------|-------------|
| `*.cc` `*.hh` `*.icc` `*.tcc` | C++ |
| `*.ly` `*.ily` | LilyPond notation |
| `*.scm` | Scheme (GNU Guile) |
| `*.tely` `*.itely` | Texinfo with inline or included LilyPond |
| `*.texi` `*.itexi` | Texinfo |

When uncertain about musical vocabulary, trust the definitions in the [Music
Glossary](Documentation/en/music-glossary.tely).

# Viewing Texinfo sections

When referred to a named section of a Texinfo file, view that section only:

1. Find the start of the cited section and the start of the next (if any).
   ```sh
   grep -n '^@section ' <file> | grep -A1 <section_name>
   ```
2. View the file between those two lines (or to EOF, in the case of the final
   section).

# Architecture

- When reviewing or debugging an engraver, view the `Engraver tutorial` section
  in `Documentation/en/contributor/programming-work.itexi`.
