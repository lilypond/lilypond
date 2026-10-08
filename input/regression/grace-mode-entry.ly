\version "2.27.4"

\header {
  texidoc = "Grace mode settings don't take over @code{\\stemDown} from
right before."
}

\new Staff
{
  \mark \markup \typewriter { "\\stemDown \\grace {...}" }
  \stemDown \grace { f'4 f'' } c'2 2
}

\new Staff
{
  \mark \markup \typewriter { "\\grace {\\stemDown ...}" }
  \grace { \stemDown f'4 f'' } c'2 2
}
