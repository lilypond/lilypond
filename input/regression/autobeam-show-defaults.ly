\version "2.16.0"

\header {

  texidoc = "
Default autobeam settings have been set for a number of time signatures.
Each score shows the desired beaming
"

}

{
  \time 2/2
  a8^\markup "Beams should end at 4/8, 6/8, and 8/8"
  \repeat unfold 6 a8 a16 a32 a64 a128 a
}
{
  \time 2/4
  a8^\markup "Beams should end at 2/8 and 4/8"
  \repeat unfold 6 a8
  a16 a32 a64 a128 a
}
{
  \time 2/8
  a8^\markup "Beams should end at 1/8 and 2/8"
  \repeat unfold 2 a8
  a16 a32 a64 a128 a
}
{
  \time 2/16
  a16^\markup "Beams should end at 1/16 and 2/16"
  a a a32 a64 a128 a
}
{
  \time 3/2
  a8^\markup "Beams should end at 4/8, 8/8, 10/8 and 12/8"
  \repeat unfold 10 a8 a16 a32 a64 a128 a }
{
  \time 3/4
  a8^\markup "1/8 beams should end at 3/4; smaller beams should end at 1/4, 2/4, and 3/4"
  \repeat unfold 5 a8
  \repeat unfold 11 a16 a32 a64 a128 a
}
{
  \time 3/8
  a8^\markup "Beams should end at 3/8"
  a a
  \repeat unfold 5 a16 a32 a64 a128 a
}
{
  \time 3/16
  a16^\markup "Beams should end at 1/16, 2/16, and 3/16"
  \repeat unfold 4 a16
  a32 a64 a128 a
}
{
  \time 4/2
  a8^\markup "Beams should end at 4/8, 8/8, 12/8, 14/8, and 16/8"
  \repeat unfold 14 a8
  a16 a32 a64 a128 a
}
{
  \time 4/4
  a8^\markup "Beams should end at 4/8, 6/8, and 8/8"
  \repeat unfold 6 a8
  a16 a32 a64 a128 a
}
{
  \time 4/16
  a8^\markup "Beams should end at 1/16, 2/16, 3/16, and 4/16"
  a \repeat unfold 7 a16
  a32 a64 a128 a
}
{
  \time 4/8
  a8^\markup "Beams should end at 2/8 and 4/8"
  \repeat unfold 6 a8
  a16 a32 a64 a128 a
}
{
  \time 6/4
  a8^\markup "Beams should end at 6/8, 8/8, 10/8, and 12/8"
  \repeat unfold 10 a8
  a16 a32 a64 a128 a
}
{
  \time 6/8
  a8^\markup "Beams should end at 3/8 and 6/8"
  \repeat unfold 10 a8
  a16 a32 a64 a128 a
}
{
  \time 9/4
  a8^\markup "Beams should end at 6/8, 12/8, 14/8, 16/8, and 18/8"
  \repeat unfold 16
  a8 a16 a32 a64 a128 a
}
{
  \time 9/8
  a8^\markup "Beams should end at 3/8, 6/8, and 9/8"
  \repeat unfold 7 a8
  a16 a32 a64 a128 a
}
{
  \time 9/16
  a8^\markup "Beams should end at 3/16, 6/16, and 9/16"
  \repeat unfold 3 a8
  \repeat unfold 9 a16
  a32 a64 a128 a
}
{
  \time 12/4
  a8^\markup "Beams should end at 6/8, 12/8, 18/8, 20/8, 22/8, and 24/8"
  \repeat unfold 22 a8
  a16 a32 a64 a128 a
}
{
  \time 12/8
  a8^\markup "Beams should end at 3/8, 6/8, 9/8, and 12/8"
  \repeat unfold 22 a8
  a16 a32 a64 a128 a
}
{
  \time 12/16
  a8^\markup
    \column {
      "1/8 beams should end at 6/16 and 12/16"
      "Shorter beams should end at 3/16, 6/16, 9/16, and 12/16"
    }
  \repeat unfold 5 a8
  \repeat unfold 11 a16
  a32 a64 a128 a
}
{ \time 5/8
  a8^\markup "Beams should end at 3/8 and 5/8"
  \repeat unfold 8 a8
  a16 a32 a64 a128 a
}
{
  \time 8/8
  a8^\markup "Beams should end at 3/8, 6/8, and 8/8"
  \repeat unfold 14 a8 a
  16 a32 a64 a128 a
}
