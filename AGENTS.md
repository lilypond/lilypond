[.gitlab-ci.yml](.gitlab-ci.yml) contains build and test commands.

[ROADMAP](ROADMAP) describes the project directory structure.

The primary manuals are in English (`Documentation/en`).  Do not read
translations (`Documentation/{ca,de,es,fr,it,ja,tr,zh}/`) except by explicit
request; assume that they are outdated and incomplete!

| File extensions | Description |
| --- | --- |
| `*.cc` `*.hh` `*.icc` `*.tcc` | C++ |
| `*.ly` `*.ily` | LilyPond notation |
| `*.scm` | Scheme (GNU Guile) |
| `*.tely` `*.itely` | Texinfo with inline or included LilyPond |
| `*.texi` `*.itexi` | Texinfo |

When searching the manuals broadly, include all of `*.{itely,itexi,tely,texi}`.

When uncertain about musical vocabulary, trust the definitions in the [Music
Glossary](Documentation/en/music-glossary.tely).

# Architecture

- When reviewing or debugging an engraver, read "Engraver tutorial" in
  `Documentation/en/contributor/programming-work.itexi`.
