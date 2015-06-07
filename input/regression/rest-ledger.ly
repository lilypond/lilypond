\version "2.19.22"
\header {
texidoc = "Breve, whole and half rests moving outside the staff should get
ledger lines."
}


\paper { ragged-right = ##t }

rPos =
#(define-music-function (y) (number?)
  #{ \override Rest.staff-position = #y #})

{
  \set Score.timing = ##f
  \rPos #2
  r\breve \rPos #4
  r \rPos #5
  r \rPos #-4
  r \rPos #-6
  r \rPos #-7
  r

  \rPos #2
  r1 \rPos #4
  r \rPos #5
  r \rPos #-6
  r \rPos #-7
  r \rPos #-8
  r

  \rPos #4
  r2 \rPos #6
  r \rPos #7
  r \rPos #-4
  r \rPos #-6
  r \rPos #-7
  r

}

