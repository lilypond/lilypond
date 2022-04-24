\version "2.17.30"
% the example header file caused the head to be chopped off
%\include "example-header.ily"


\header {
  dedication = "Carin Levine"
  instrument = "bass flute"
  title = "ČÁRY"
  year = "2004--2006"

  subtitle = \markup { \italic Sorcery (extract) }
  % Measures 6 - 10;

  composer = "Trevor Bača"
  copyright = "Copyright 2006 Trevor Bača, licensed under the FDL 1.1 or higher"

}

% Verbatim from composer's score.
%
% Upper staff gives fingerings;
% Lower staff gives special types of breath.
%
% Copyright 2006 Trevor Baca, but this exerpt is licensed under
% the FDL 1.1 or higher.

\include "english.ly"
\include "cary-layout.ily"

\score {
  <<

	\new StaffGroup <<

		\new Staff \with {
			\override Stem.direction = #down
			\override Beam.positions = #'(-7 . -7)
			\override TupletBracket.direction = #down
			\override TupletBracket.staff-padding = #5
			\override TupletBracket.padding = #2.25
		} <<

			\new Voice {

            \override Score.MetronomeMark.extra-offset = #'(0 . 6)
            \override Score.MetronomeMark.font-size = #3
            \tempo 8=42
				\time 5/16	 s4 s16			\noBreak		 % measure 6
				\time 4/8	 s2 			\noBreak		 % measure 7
				\time 4/8	 s2 			\noBreak		 % measure 8

			}

			\new Voice \with {
				\remove Forbid_line_break_engraver
			} {

				% measure 6
				\fraction \tuplet 3/5 {
					\tuplet 5/4 {
						a'64 [ % 36
						cs''64 % 37
						f''64 % 38
						cs''64 % 39
						ef'''64 ] % 40
					}
					\tuplet 5/4 {
						g''64 [ % 41
						c''64 % 42
						e''64 % 43
						g''64 % 44
						ef'''64 ] % 45
					}
					bf''16 % 46
				}

				% measure 7
				r8 % 47
				\tuplet 5/4 {
					d''32 [ % 48
					af''32 % 49
					e'32 % 50
					b'32 % 51
					b'32 ] % 52
				}
				ef'''4 % 53

				% measure 8
				b''8 [ % 54
				g''8 ] % 55
				d''4 % 56

			}
		>>

		\new Staff \with {
			\hide Clef
			\override StaffSymbol.line-positions = #'(-4 -2   2 4)
			\override Stem.direction = #down
			\override TupletBracket.staff-padding = #5
			\override TupletBracket.padding = #2.25
		} <<

			\new Voice \with {
				\remove Forbid_line_break_engraver
				\override Stem.direction = #up
				\override Flag.stroke-style = "grace"
				\override Stem.font-size = #-3
				\hide Rest
				\override NoteHead.no-ledgers = ##t
				\hide Dots
				\hide TupletBracket
				\hide TupletNumber
			} {

				% measure 6
				\fraction \tuplet 3/5 {
					\square f''16 * 1/8 % 40
					\square f''16 * 1/8 % 41
					\square f''16 * 1/8 % 42
					\square f''16 * 1/8 % 43
					\square f''16 * 1/8 % 44
					\square f''16 * 1/8 % 45
					\square f''16 * 1/8 % 46
					\square f''16 * 1/8 % 47
					\tuplet 3/2 {
						\square f''16 % 48
						\square f''16 * 1/2 % 49
					}
					\tuplet 3/2 {
						\square f''16 * 1/4 % 50
						\square f''16 * 1/4 % 51
						\square f''16 * 1/4 % 52
						\square f''16 * 1/4 % 53
						\square f''16 * 1/4 % 54
						\square f''16 * 1/4 % 55
					}
				}

				% measure 7
				s8 % 56
				\square g''16 * 2/1 % 57
				\square g''16 % 58
				\square g''16 % 59
				\tuplet 9/8 {
					s16 % 60
					f''16 * 1/4 % 61
					f''16 * 1/4 % 62
					f''16 * 1/4 % 63
					f''16 * 1/4 % 64
					f''16 * 1/4 % 65
				}

				% measure 8
				\tuplet 5/4 {
					\tuplet 7/4 {
						s4 % 66
						\triangle a''16 % 67
						\triangle a''16 % 68
						\triangle a''16 % 69
					}
					s8 % 70
					\triangle a''16 * 1/2 % 71
					\triangle a''16 * 1/2 % 72
					\triangle a''16 * 1/2 % 73
					\triangle a''16 * 1/2 % 74
					\triangle a''16 * 1/2 % 75
					\triangle g''16 * 1/2 % 76
					\triangle g''16 * 1/2 % 77
					\triangle g''16 * 1/2 % 78
				}

			}

			\new Voice \with {
				\remove Forbid_line_break_engraver
				\hide Stem
				\hide Rest
				\override Rest.staff-position = #-0.5
				\hide Dots
				\hide Beam
				\hide TupletBracket
				\hide TupletNumber
			} {

				% measure 6
				\fraction \tuplet 3/5 {
					\blackdiamond f'128 [ % 40
					\blackdiamond f'128 % 41
					\blackdiamond f'128 % 42
					\blackdiamond f'128 % 43
					\blackdiamond f'128 % 44
					\blackdiamond f'128 % 45
					\blackdiamond f'128 % 46
					\blackdiamond f'128 ] % 47
					\tuplet 3/2 {
						\blackdiamond f'16 % 48
						\blackdiamond f'32 % 49
					}
					\tuplet 3/2 {
						\blackdiamond f'64 % 50
						\blackdiamond f'64 % 51
						\blackdiamond f'64 % 52
						\blackdiamond f'64 % 53
						\blackdiamond f'64 % 54
						\blackdiamond f'64 % 55
					}
				}

				% measure 7
				s8 % 56
				\harmonic a'8 \glissando % 57
				\harmonic a'16 \glissando % 58
				\blackdiamond a'16 % 59
				\tuplet 9/8 {
					s16 % 60
					\harmonic g'64 \glissando % 61
					\harmonic g'64 \glissando % 62
					\harmonic g'64 \glissando % 63
					\harmonic g'64 \glissando % 64
					\blackdiamond g'64 % 65
				}

				% measure 8
				\tuplet 5/4 {
					\tuplet 7/4 {
						s4 % 66
						\blackdiamond f'16 % 67
						\blackdiamond f'16 % 68
						\blackdiamond f'16 % 69
					}
					s8 % 70
					\blackdiamond f'32 % 71
					\blackdiamond f'32 % 72
					\blackdiamond f'32 % 73
					\blackdiamond f'32 % 74
					\blackdiamond f'32 % 75
					\harmonic e'32 \glissando % 76
					\harmonic e'32 \glissando % 77
					\blackdiamond e'32 % 78
				}

			}

			\new Voice \with {
				\remove Forbid_line_break_engraver
				\override Stem.direction = #down
				\override Stem.font-size = #-3
				\override Flag.stroke-style = "grace"
				\hide Rest
				\override NoteHead.no-ledgers = ##t
				\hide Dots
				\hide TupletBracket
				\hide TupletNumber
			} {

				% measure 6
				\fraction \tuplet 3/5 {
					s128 % 40
					s128 % 41
					s128 % 42
					s128 % 43
					s128 % 44
					s128 % 45
					s128 % 46
					s128 % 47
					\tuplet 3/2 {
						\semicircle a16 % 48
						\semicircle a16 * 1/2 % 49
					}
					\tuplet 3/2 {
						\semicircle a16 * 1/4 % 50
						\semicircle a16 * 1/4 % 51
						\semicircle a16 * 1/4 % 52
						\semicircle a16 * 1/4 % 53
						\semicircle a16 * 1/4 % 54
						\semicircle a16 * 1/4 % 55
					}
				}

				% measure 7
				s8 % 56
				s8 % 57
				s16 % 58
				\triangle a16 % 59
				\tuplet 9/8 {
					s16 % 60
					s64 % 61
					s64 % 62
					s64 % 63
					s64 % 64
					\triangle a16 * 1/4 % 65
				}

				% measure 8
				\tuplet 5/4 {
					\tuplet 7/4 {
						s4 % 66
						\semicircle a16 % 67
						\semicircle a16 % 68
						\semicircle a16 % 69
					}
					s8 % 70
					\semicircle a16 * 1/2 % 71
					\semicircle a16 * 1/2 % 72
					\semicircle a16 * 1/2 % 73
					\semicircle a16 * 1/2 % 74
					\semicircle a16 * 1/2 % 75
					s32 % 76
					s32 % 77
					\semicircle a16 * 1/2 % 78
				}

			}

			\new Voice \with {
				\remove Forbid_line_break_engraver
				\override Stem.direction = #down
				\hide NoteHead
				\override NoteHead.no-ledgers = ##t
				\override Rest.staff-position = #-18
				\override Stem.length = #10
				\override Beam.positions = #'(-13 . -13)
				\override DynamicLineSpanner.staff-padding = #18
				\override TextSpanner.bound-details.left.text = #(markup (#:italic "covered"))
				\override TextSpanner.dash-period = #1
				\override TextSpanner.dash-fraction = #0.2
				\override TextSpanner.bound-details.left.padding = #0.5
				\override TextSpanner.bound-details.right.padding = #0.5
				\override TextSpanner.staff-padding = #4
			} {

				% measure 6
				\fraction \tuplet 3/5 {
					\beam #0 #5 g,128 \sffp \< [ % 40
					\beam #5 #5 g,128 % 41
					\beam #5 #5 g,128 % 42
					\beam #5 #5 g,128 % 43
					\beam #5 #5 g,128 % 44
					\beam #5 #5 g,128 % 45
					\beam #5 #5 g,128 % 46
					\beam #5 #1 g,128 % 47
					\tuplet 3/2 {
						\beam #1 #2 g,16 % 48
						\beam #3 #1 g,32 \fX % 49
					}
					\tuplet 3/2 {
						\beam #1 #4 g,64 \sffp \< % 50
						\beam #4 #4 g,64 % 51
						\beam #4 #4 g,64 % 52
						\beam #4 #4 g,64 % 53
						\beam #4 #4 g,64 % 54
						\beam #4 #0 g,64 \fX ] % 55
					}
				}

				% measure 7
				r8 % 56
				g,8 \sf \< [ % 57
				g,16 % 58
				\beam #2 #0 g,16 \ffX ] % 59
				\tuplet 9/8 {
					r16 % 60
					\beam #4 #4 g,64 \sfp \< [ % 61
					\beam #4 #4 g,64 % 62
					\beam #4 #4 g,64 % 63
					\beam #4 #4 g,64 % 64
					\beam #4 #0 g,64 \fX ] % 65
				}

				% measure 8
				\tuplet 5/4 {
					\tuplet 7/4 {
						r4 % 66
						\beam #2 #2 g,16 \fX [ % 67
						\beam #2 #2 g,16 \f % 68
						\beam #2 #0 g,16 \fX ] % 69
					}
					r8 % 70
					\beam #3 #3 g,32 \fX [ % 71
					\beam #3 #3 g,32 \fX % 72
					\beam #3 #3 g,32 \fX % 73
					\beam #3 #3 g,32 \fX % 74
					\beam #3 #3 g,32 \fX % 75
					\beam #3 #3 g,32 \sf \< % 76
					\beam #3 #3 g,32 % 77
					\beam #3 #0 g,32 \ffX ] % 78
				}

			}
		>>
	>>
  >>
}
