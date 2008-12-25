
\header {
  texidoc = "test identifiers."
  }
\version "2.12.0"

num = #3
mus = { c'4 }
paperId = \paper { line-width = 5.0 \cm }
layoutId = \layout { ragged-right = ##t }
perfId = \midi {}
markupId = \markup { hoi }
stringId = "title"

% headers not allowed  as Id's

scoreId = \score {
  \repeat unfold \num \mus
  \layout { \layoutId }
  \midi { \perfId }
  }

bookId = \book {
  \score { \scoreId }
  \paper { \paperId }
  }


\book { \bookId }


