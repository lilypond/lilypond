\version "2.23.13"

\header {
  texidoc = "@code{markup->string} takes a markup and tries
to convert it into an approximate string representation."
}

#(use-modules (lily display-lily))

#(define-markup-command (custom layout props arg) (string?)
   #:properties ((some-prop))
   #:as-string (format #f "~a ~a" arg some-prop)
   (interpret-markup layout props
    (make-line-markup (list arg some-prop))))

lipsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed nec ultrices neque. Mauris ut consequat felis, scelerisque ullamcorper mauris. Nunc a tellus ut augue viverra mollis vel finibus elit. Phasellus finibus faucibus finibus. Maecenas id consectetur libero. Duis viverra orci nibh, in ultricies turpis sagittis eget. Curabitur urna nibh, finibus sed faucibus id, sagittis in augue. Etiam in risus tortor."

head = \header {
  title = \markup \bold Title
  circular = \markup \fromproperty #'header:circular
  lipsum = \lipsum
}

test =
#(define-void-function (header mkup expected) ((module?) scheme? string?)
   (let ((str (apply
               markup->string
               mkup
               (if header
                   (list #:props (headers-property-alist-chain
                                  (list header)))
                   '()))))
     (if (not (equal? expected str))
         ;; FIXME: can't show the original with markup->lily-string
         ;; because it doesn't handle markup lists.
         (ly:warning "Expected output of markup->string was: '~a'.  \
Actual output is: '~a'.  If this change is intended, please update \
input/regression/markup-to-string.ly."
                     expected
                     str))))

\test "A"
      "A"

\test \markup \simple "A"
      "A"

\test \markup { }
      ""

\test \markup \bold A
      "A"

\test \markup { A B }
      "A B"

\test \markup \combine A B
      "A B"

\test \markup \bold { A B }
      "A B"

\test \markup { \bold { A B } C }
      "A B C"

\test \markuplist { A B }
      "A B"

\test \markuplist { }
      ""

\test \markup \with-color "red" A
      "A"

\test \markup \with-color "red" { A B }
      "A B"

\test \markup \with-color "red" \bold { A B }
      "A B"

\test \markup \coda
      #(ly:wide-char->utf-8 #x1d10c)

\test \markup \varcoda
      #(ly:wide-char->utf-8 #x1d10c)

\test \markup \segno
      #(ly:wide-char->utf-8 #x1d10b)

\test \markup \fermata
      #(ly:wide-char->utf-8 #x1d110)

\test \markup \override #`(direction . ,DOWN) \fermata
      #(ly:wide-char->utf-8 #x1d111)

\test \markup \concat { A B }
      "AB"

\test \markup \put-adjacent #X #RIGHT A B
      "AB"

\test \markup \fill-with-pattern #1.0 #RIGHT - A B
      "A B"

\test \head
       \markup \fromproperty #'header:title
       "Title"

\test \head \markup \bold \fromproperty #'header:title
      "Title"

\test \markup \fromproperty #'header:title
      ""

%% Intentionally no warning.  \fromproperty on an
%% undefined property can be useful with \first-visible.
\test \head \markup \bold \fromproperty #'header:undefined
      ""

#(ly:expect-warning (G_ "Recursive definition of property ~a detected!") 'header:circular)
\test \head \markup \bold \fromproperty #'header:circular
      ""

\test \markup \override #'((some-prop . "<Property value>"))
              \custom "<Argument>"
      "<Argument> <Property value>"

\test \markup \with-url "https://lilypond.org" LilyPond
      "LilyPond [https://lilypond.org]"

\test \head
      \markup \wordwrap-field #'header:lipsum
      \lipsum

\test \head
      \markup \justify-field #'header:lipsum
      \lipsum

\test \markup { \with-outline Outline Markup }
      "Markup"

\test \markup \with-dimensions-from DimensionsSource Markup
      "Markup"

\test \markup { AAA \transparent BBB CCC }
      "AAA     CCC"

\test \markup \footnote Created 2022
      "Created [2022]"

\test \markup \auto-footnote Created 2022
      "Created [2022]"

\test \markup \markletter #20
      "U"

\test \markup \markalphabet #20
      "T"

\test \markup \circle A
      "(A)"

\test \markup \oval A
      "(A)"

\test \markup \box A
      "[A]"

\test \markup \rounded-box A
      "[A]"

\test \markup \left-brace #20
      "{"

\test \markup \right-brace #20
      "}"

\test \markup \parenthesize \fraction \italic \char ##x03c0 6
      #(format #f "(~a/6)" (ly:wide-char->utf-8 #x03c0))

\test \markup \page-ref #'dummy dummy dummy
      ""

\test \markup \pattern #3 #X #2 "?"
      "? ? ?"

\test \markup \fret-diagram-verbose
        #'((mute 6) (mute 5) (open 4)
           (place-fret 3 2) (place-fret 2 3) (place-fret 1 2))
      ""

\test \markup \fret-diagram-terse "x;x;o;2;3;2;"
      ""

\test \markup \fret-diagram #"s:0.75;6-x;5-x;4-o;3-2;2-3;1-2;"
      ""

\test \markup \postscript "0 5 lineto stroke"
      ""

\test \markup \tied-lyric "a~b~c"
      #"a\u203fb\u203fc"
