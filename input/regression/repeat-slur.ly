\version "2.17.14"

\header {
  texidoc = "Slurs are automatically broken at repeats.
This behavior can be changed by setting @code{\\slurOverRepeat}
to @code{#t}. To manually break a slur at a bar line, use
@code{\\breakSlur}. To manually start a free slur at a bar, use
@code{\\free} with an opening parenthesis. To manually stop a
free slur at a bar, use @code{\\free} with a closing parenthesis.
"
}

\new Staff {
  a'4 ( b' c' d'
  \repeat volta 3
    { a' ) b' ( c' d' }
    \alternative { { a' ) b' c' ( d' }
                   { a' b' ) b'2 ( }
                   { a'4 b' ) b'2 ( } }

  a'1 ) \bar "|."
}

\new Staff {
  a'4 ( b' c' d'
  \repeat volta 2
    { a' ) b' ( c' d' }
    \alternative { { a' ) b' c' d' ( }
                   { a' b' ) b'2 ( } }

  a'1 ) \bar "|."
}

\new Staff {
  a'4 ( b' c' d'
  \repeat volta 2
    { a' ) b' ( c' d' }
    \alternative { { a' ) b' c' ( d' }
                   { a' ) b' b'2 ( } }

  a'1 ) \bar "|."
}

\new Staff {
  a'4 ( b' c' d'
  \repeat volta 2
    { a' ) b' ( c' d' }
    \alternative { { a' ) b' c' d' ( }
                   { a' ) b' b'2 ( } }

  a'1 ) \bar "|."
}

\new Staff {
  \set slurOverRepeat = ##t
  a'4 ( b' c' d'
  \repeat volta 2
    { a' ) b' ( c' d' }
    \alternative { { a' ) b' c' ( d' }
                   { a' b' ) b'2 ( } }

  a'1 ) \bar "|."
}

\new Staff {
  \repeat volta 2
    { \free ( a'4 ) b' ( c' d' }
    \alternative { { a' ) b' c' ( d' }
                   { a' b' ) b'2 ( } }

  a'1 ) \bar "|."
}

\new Staff {
  a'4 ( b' c' d'
  \repeat volta 2
    { a' ) b' ( c' d' }
    \alternative { { a' ) b' c' ( d' \free ) }
                   { a' b' b'2 ( } }

  a'1 ) \bar "|."
}

\new Staff {
  a'4 ( b' c' d' \breakSlur | a' b' c' d' ) |
}

%% phrasing slurs

\new Staff {
  a'4 \( b' c' d'
  \repeat volta 2
    { a' \) b' \( c' d' }
    \alternative { { a' \) b' c' \( d' }
                   { a' b' \) b'2 \( } }

  a'1 \) \bar "|."
}

\new Staff {
  a'4 \( b' c' d'
  \repeat volta 2
    { a' \) b' \( c' d' }
    \alternative { { a' \) b' c' d' \( }
                   { a' b' \) b'2 \( } }

  a'1 \) \bar "|."
}

\new Staff {
  a'4 \( b' c' d'
  \repeat volta 2
    { a' \) b' \( c' d' }
    \alternative { { a' \) b' c' \( d' }
                   { a' \) b' b'2 \( } }

  a'1 \) \bar "|."
}

\new Staff {
  a'4 \( b' c' d'
  \repeat volta 2
    { a' \) b' \( c' d' }
    \alternative { { a' \) b' c' d' \( }
                   { a' \) b' b'2 \( } }

  a'1 \) \bar "|."
}

\new Staff {
  \set slurOverRepeat = ##t
  a'4 \( b' c' d'
  \repeat volta 2
    { a' \) b' \( c' d' }
    \alternative { { a' \) b' c' \( d' }
                   { a' b' \) b'2 \( } }

  a'1 \) \bar "|."
}

\new Staff {
  \repeat volta 2
    { \free \( a'4 \) b' \( c' d' }
    \alternative { { a' \) b' c' \( d' }
                   { a' b' \) b'2 \( } }

  a'1 \) \bar "|."
}

\new Staff {
  a'4 \( b' c' d'
  \repeat volta 2
    { a' \) b' \( c' d' }
    \alternative { { a' \) b' c' \( d' \free \) }
                   { a' b' b'2 \( } }

  a'1 \) \bar "|."
}

\new Staff {
  a'4 \( b' c' d' \breakPhrasingSlur | a' b' c' d' \) |
}
