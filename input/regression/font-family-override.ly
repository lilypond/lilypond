\header {

  texidoc = "The default font families for text can be
  overridden with @code{make-pango-font-tree}"

}

\version "2.12.0"

\paper  {
  % change for other default global staff size. 
  myStaffSize = #20


  %{

  run

      lilypond -dshow-available-fonts blabla

  to show all fonts available in the process log.  
  
  %}

  
  #(define fonts
    (make-pango-font-tree "Times New Roman"
                          "Nimbus Sans"
                          "Luxi Mono"

;;     "Helvetica"
;;     "Courier"

     (/ myStaffSize 20)))
}

\relative {

  c'^\markup { roman: foo \bold bla \italic bar \italic \bold baz }
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
