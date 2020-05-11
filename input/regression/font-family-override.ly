\header {

  texidoc = "The default font families for text can be
  overridden with @code{make-pango-font-tree}"

}

\version "2.19.21"

\paper {
  % change for other default global staff size.
  myStaffSize = #20


  %{

  run

      lilypond -dshow-available-fonts

  to show all fonts available in the process log.

  %}


  #(define fonts
    (make-pango-font-tree "Times New Roman"
                          "Nimbus Sans,Nimbus Sans L"
                          "Luxi Mono"

;;     "Helvetica"
;;     "Courier"

     (/ myStaffSize 20)))
}

\score {
  \relative {

    c''^\markup { roman: foo \bold bla \italic bar \italic \bold baz }
    c'_\markup {
      \override #'(font-family . sans)
      {
        sans: foo \bold bla \italic bar \italic \bold baz
      }
    }
    c'^\markup {
      \override #'(font-family . typewriter)
      {
        mono: foo \bold bla \italic bar \italic \bold baz
      }
    }
  }

  \layout {
    \context {
      \Score
      \override PaperColumn.keep-inside-line = ##f
    }
  }

}
