\paper {
    raggedright = ##t
}

\relative {

    \override Slur #'after-line-breaking-callback = #New_slur::after_line_breaking
    \override Slur #'print-function = #New_slur::print     
    \override Slur #'height = ##f

    f'=''16( e)    d( c)
    c'=''2(~c8 d16 c b8 a)
    f='4
    <c g>4 ( f <g b>)
     f
    <c g>^( f <g b>)
    f
    <c g>_( f <g b>)
    <g b>_( g  <b d>)
    <g b>_( \stemDown g \stemBoth  <b d>)
    c,^( c'' c) 
    c,,^( c'')
    c,,^( c')
    
}
