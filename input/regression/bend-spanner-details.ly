\version "2.23.0"

\header {
  texidoc = "A @code{BendSpanner} may be customized by tweaking the
subproperties of @code{'details}.

@itemize
@item
@code{'bend-arrowhead-height}
@item
@code{'bend-arrowhead-width}
@item
@code{'arrow-stencil}
best to override it with a procedure (as an argument to the
@code{after-line-breaking} property) setting this subproperty.
@item
@code{'curvature-factor}
@item
@code{'bend-amount-strings}
@item
@code{'dashed-line-settings}
@item
@code{'horizontal-left-padding}
@item
@code{'vertical-padding}
@item
@code{'y-distance-from-tabstaff-to-arrow-tip}
@item
@code{'target-visibility}
@end itemize

Line-breaking behavior may be customized with:

@itemize
@item
@code{'curve-x-padding-line-end}
@item
@code{'curve-y-padding-line-end}
@item
@code{'head-text-break-visibility}
@end itemize"
}

%% switch to arrow heads from feta
%% REMARK the result is not overwhelming, it does not scale properly with
%% changed fontSize, a TODO.
%% Maybe a better result happens if the arrow heads are slightly rotated,
%% relying on the curve's control-points
#(define (feta-arrow-head dir)
  (lambda (grob)
    (let* ((layout (ly:grob-layout grob))
           (props (ly:grob-alist-chain grob))
           (font (ly:paper-get-font layout (cons '((font-encoding . fetaMusic)
                                                   (font-name . #f))
                                                 props)))
           (glyph-string
             (format #f "arrowheads.open.1~a1" (if (> dir -1) "" "M"))))
      (ly:font-get-glyph font glyph-string))))

#(define set-feta-arrow-stencil!
  (lambda (grob)
    (ly:grob-set-nested-property! grob '(details arrow-stencil)
      (lambda (a b c d e) ;; thickness end-curve-coords height width dir
        (let* ((new-stil ((feta-arrow-head e) grob))
               (new-stil-y-ext (ly:stencil-extent new-stil Y))
               (new-stil-y-length (interval-length new-stil-y-ext))
               (font-size (ly:grob-property grob 'font-size))
               (size-factor (magstep font-size))
               (thick (ly:grob-property grob 'thickness))
               (staff-symbol-line-thickness
                 (ly:staff-symbol-line-thickness grob))

               (bend-line-thickness
                 (* staff-symbol-line-thickness
                    size-factor
                    (ly:grob-property grob 'thickness))))
        (ly:grob-set-nested-property! grob '(details bend-arrowhead-height)
            (if (> e -1)
                (- (cdr new-stil-y-ext) bend-line-thickness)
                (+ (car new-stil-y-ext) bend-line-thickness)))

        (ly:stencil-translate
          new-stil
          (cons
            (car b)
            (+ (cdr b)
               (* e new-stil-y-length)))))))))
mus = { c'2\3\^ d'\3 }

music = {
  %% defaults
  \mus

  %% arrow heads
  \temporary \override BendSpanner.details.bend-arrowhead-height = 1.8
  \temporary \override BendSpanner.details.bend-arrowhead-width = 0.6
  \mus
  \revert BendSpanner.details.bend-arrowhead-height
  \revert BendSpanner.details.bend-arrowhead-width

  \temporary
    \override BendSpanner.after-line-breaking = #set-feta-arrow-stencil!
  \mus
  \revert BendSpanner.after-line-breaking

  %% curvature-factor
  \temporary \override BendSpanner.details.curvature-factor = 0.7
  \mus
  \revert BendSpanner.details.curvature-factor


  %% bend-amount-strings
  \temporary \override BendSpanner.details.bend-amount-strings.full = "full"
  \mus
  \revert BendSpanner.details.bend-amount-strings.full

  %% dashed-line-settings
  \temporary \override BendSpanner.thickness = 2
  \temporary \override BendSpanner.details.dashed-line-settings = #'(0 0.4 0)
  c'2\3\preBendHold \^ \once \override NoteColumn.bend-me = ##f d' d'1
  \revert BendSpanner.details.dashed-line-settings
  \revert BendSpanner.thickness

  %% horizontal-left-padding and vertical-padding
  \temporary \override BendSpanner.details.horizontal-left-padding = 1
  \temporary \override BendSpanner.details.vertical-padding = 0.4
  \mus
  \revert BendSpanner.details.horizontal-left-padding
  \revert BendSpanner.details.vertical-padding

  %% y-distance-from-tabstaff-to-arrow-tip
  \temporary
    \override BendSpanner.details.y-distance-from-tabstaff-to-arrow-tip = 5
  \mus
  \revert BendSpanner.details.y-distance-from-tabstaff-to-arrow-tip

  %% target-visibility
  \temporary \override BendSpanner.details.target-visibility = ##t
  \mus
  \revert BendSpanner.details.target-visibility

  %% successive-level
  \temporary \override BendSpanner.details.successive-level = 2
  \mus
  \revert BendSpanner.details.successive-level
}

\score {
  \header {
    piece = "subproperties of 'details"
  }
  \new StaffGroup
  <<
    \new Staff \music
    \new TabStaff \music
  >>
  \layout {
    \context {
      \Voice
      \omit StringNumber
    }
    \context {
      \TabVoice
      \consists "Bend_spanner_engraver"
    }
  }
}

mlb = { <d\5 e'\2>1 \^ \break <e\5 fis'\2>  }
mus-line-breaks = {

  <d\5 e'\2>1
  %% defaults
  \mlb

  %% curve-x-padding-line-end and curve-y-padding-line-end
  \temporary \override BendSpanner.details.curve-x-padding-line-end = 3
  \temporary \override BendSpanner.details.curve-y-padding-line-end = 3
  \mlb
  \revert BendSpanner.details.curve-x-padding-line-end
  \revert BendSpanner.details.curve-y-padding-line-end

  %% head-text-break-visibility
  %% Remark: this override is _needed_ if style is 'pre-bend, 'pre-bend-hold or
  %% 'hold
  %% Otherwise the text and the arrow head would be printed at next line
  %% Thus it's part of bendHold, preBendHold and preBend
  \temporary \override BendSpanner.style = #'pre-bend-hold
  \temporary
    \override BendSpanner.details.head-text-break-visibility = ##(#t #t #f)
  \mlb
  \revert BendSpanner.style
  \revert BendSpanner.details.head-text-break-visibility
}

\score {
  \header {
    piece = "subproperties of 'details for line-breaking behavior"
  }
  \new StaffGroup
  <<
    \new Staff { \clef "G_8" \mus-line-breaks }
    \new TabStaff \mus-line-breaks
  >>
  \layout {
    line-width = 70
    \context {
      \Voice
      \omit StringNumber
    }
  }
}
