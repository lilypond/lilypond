

\relative {

    \override Slur #'after-line-breaking-callback = #New_slur::after_line_breaking
   \override Slur #'print-function = #New_slur::print     
    \override Slur #'height = ##f
    <c g> ( f <g b>)
     f
    <c g>^( f <g b>)
    f
    <c g>_( f <g b>)
    <g b>_( g  <b d>)
    <g b>_( \stemDown g \stemBoth  <b d>)
    
}
