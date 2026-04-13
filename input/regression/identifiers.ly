
\header {
  texidoc = "test identifiers."
  }

\version "2.25.35"

num = #3
mus = { c'4 }
paperId = \paper { line-width = 5.0 \cm }
layoutId = \layout { ragged-right = ##t }
perfId = \midi {}
markupId = \markup { hoi \italic polloi }
stringId = "title"
scoreHeaderId = \header { piece = \markupId }
bookHeaderId = \header { composer = "Composer" title = \stringId }

scoreId = \score {
  \scoreHeaderId
  \*\num \mus
  \layout { \layoutId }
  \midi { \perfId }
  }

bookId = \book {
  \bookHeaderId
  \score { \scoreId }
  \paper { \paperId }
  }


\book { \bookId }
