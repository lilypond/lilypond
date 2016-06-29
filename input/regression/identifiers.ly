
\header {
  texidoc = "test identifiers."
  }
\version "2.19.45"

num = #3
mus = { c'4 }
paperId = \paper { line-width = 5.0 \cm }
layoutId = \layout { ragged-right = ##t }
scoreHeaderId = \header { piece = "Piece" }
bookHeaderId = \header { composer = "Composer" title = "Title" }
perfId = \midi {}
markupId = \markup { hoi }
stringId = "title"

scoreId = \score {
  \scoreHeaderId
  \repeat unfold \num \mus
  \layout { \layoutId }
  \midi { \perfId }
  }

bookId = \book {
  \bookHeaderId
  \score { \scoreId }
  \paper { \paperId }
  }


\book { \bookId }


