\version "2.25.35"

\header {

  texidoc = "The first two pages are numbered in Roman format.
On page@tie{}1 you should see `Roman one', on page@tie{}2 `Roman two'
and the corresponding Roman style page number.
From page@tie{}3 to the end the page numbers are formatted with Arabic numbers.
On page@tie{}3 you see `Arabic one' and on page@tie{}4 `Arabic two'
together with the corresponding Arabic style page number.
On the last page, you should see a kind of a table of contents with four rows
showing the page numbers in their respective formats ``I II 1 2''.
Each of the four rows should have a link to the corresponding page,
so clicking on `I' should jump to page@tie{}1 and so on."

}

\book {

  \paper {
    print-first-page-number = ##t
  }

  \bookpart {
    \paper {
      page-number-type = #'roman-upper
      bookpart-level-page-numbering = ##t
    }

    \label #'one
    \markup "roman one"

    \pageBreak

    \label #'two
    \markup "roman two"
  }

  \bookpart {
    \label #'three
    \markup "arabic one"
  }

  \bookpart {
    \label #'four
    \markup "arabic two"
  }

  \bookpart {
    \markup \left-column {
      \with-link #'one \page-ref #'one "X" "?"
      \with-link #'two \page-ref #'two "X" "?"
      \with-link #'three \page-ref #'three "X" "?"
      \with-link #'four \page-ref #'four "X" "?"
    }
  }
}
