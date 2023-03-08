\version "2.25.3"

\header {
  texidoc = "Test QR codes with different versions (i.e., matrix sizes)."
}

#(ly:set-option 'warning-as-error)

\markup \qr-code #10 "short" % version 1
\markup \qr-code #10 "This is a bit longer" % version 2
\markup \qr-code #10 "This is quite a bit longer as it contains
 some useless text" % version 4
\markup \qr-code #10 #"Lorem ipsum dolor sit amet, consectetur adipiscing elit. \
Donec ultricies elementum mi, in lacinia magna. Sed convallis bibendum dolor. \
Quisque id enim."
\markup \qr-code #10 #"Lorem ipsum dolor sit amet, consectetur adipiscing elit. \
Aliquam ullamcorper, quam at dignissim dictum, ex metus sollicitudin purus, ac \
congue nulla ex eu enim. Curabitur in sodales metus. Nam diam tortor, cursus \
non efficitur ut, tincidunt in quam. Sed vel dictum sem, eu commodo lacus. \
Sed luctus suscipit ligula id imperdiet." % version 12

% This is too large for a QR code.  It should cause a gentle warning.
#(ly:expect-warning
  (G_ "too many bytes (~a) to fit in a QR code with \
correction level ~a (max ~a)")
  3000 'low 2953)
\markup \qr-code #10 #(make-string 3000 #\a)
