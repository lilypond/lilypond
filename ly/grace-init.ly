\version "1.5.68"

startGraceMusic = {
    \property Voice.Stem \override  #'direction = #1
    \property Voice.Stem \override #'length = #5.5
    \property Voice.Stem \override #'lengths = 
        #(map (lambda (x) (* 0.8 x)) '(3.5 3.5 3.5 4.5 5.0))
    \property Voice.Stem \override #'beamed-lengths =
        #(map (lambda (x) (* 0.8 x)) '(0.0 2.5 2.0 1.5))
    \property Voice.Stem \override #'beamed-minimum-lengths =
        #(map (lambda (x) (* 0.8 x)) '(0.0 1.5 1.25 1.0))
    \property Voice.Stem \override #'no-stem-extend = ##t
    \property Voice.Stem \override #'flag-style  = #"grace"
    \property Voice.Beam \override #'thickness = #0.384

    %% Instead of calling Beam::space_function, we should invoke
    %% the previously active beam function...
    \property Voice.Beam \override #'space-function =
      #(lambda (beam mult) (* 0.8 (Beam::space_function beam mult)))

    \property Voice.Beam \override #'position-callbacks =
      #`(,Beam::least_squares
	 ,Beam::check_concave
	 ,Beam::slope_damping)
    
    % Can't use Staff.fontSize, since time sigs, keys sigs, etc. will
    % be smaller as well.

    \property Voice.fontSize = #-2
    \property Staff.Accidental \override #'font-relative-size = #-2
    \property Voice.Slur \override #'direction = #-1
}

stopGraceMusic = {
    \property Voice.Slur \revert #'direction
    \property Staff.Accidental \revert #'font-relative-size
    \property Voice.Beam \revert #'thickness

    \property Voice.Stem \revert #'flag-style
    \property Voice.Stem \revert #'no-stem-extend
    \property Voice.Stem \revert #'beamed-lengths
    \property Voice.Stem \revert #'beamed-minimum-lengths
    \property Voice.Stem \revert #'lengths    
    \property Voice.Stem \revert #'length
    \property Voice.Stem \revert #'direction    
    \property Voice.Beam \revert #'space-function
    
    \property Voice.Beam \revert #'position-callbacks
    
    % Can't use Staff.fontSize, since time sigs, keys sigs, etc. will
    % be smaller as well.

    \property Voice.fontSize \unset
}
