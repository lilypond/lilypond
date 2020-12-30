\header {

  texidoc = "A multi measure rest reminder is a reminder printed at
  the top of the page, to remember how many measures you were counting.

  This is a demo of user-defined engravers, and defining grobs using
  @code{ly:make-grob-properties}."
}

\version "2.21.0"

% On seeing the end of a multi measure rest, create a MultiMeasureRestReminder item.
multiMeasureReminderEngraver =
#(make-engraver
  (end-acknowledgers
    ((multi-measure-rest-interface engraver grob source-engraver)
     (let*
      ((reminder (ly:engraver-make-grob engraver 'MultiMeasureRestReminder grob)))
      (set!
       ;; copy over the 'measure-count property.
       (ly:grob-property reminder 'measure-count)
       (ly:grob-property grob 'measure-count))))))


% Set the type of MultiMeasureRestReminder so we can assign to it.
#(set-object-property!  'MultiMeasureRestReminder 'translation-type? ly:grob-properties?)
% Confirm MultiMeasureRestReminder is a grob syntactically.
#(set-object-property! 'MultiMeasureRestReminder 'is-grob? #t)

\layout {
  \context {
    \Voice
    \consists \multiMeasureReminderEngraver

    % Define the grobs. The layout is crude, and should be refined for
    % "production" scores.
    MultiMeasureRestReminder = #(ly:make-grob-properties `(
      (break-align-symbols . (left-edge staff-bar))
      (break-visibility . ,begin-of-line-visible)
      (direction . ,UP)
      (extra-spacing-width . (+inf.0 . -inf.0))
      (font-family . roman)
      (font-series . bold)
      (font-size . -2)

      ;; only print the reminder at the top of the page (ie rank-on-page == 0)
      (text . ,(lambda (grob)
                (let*
                 ((r (ly:grob-property (ly:grob-system grob) 'rank-on-page)))
                 (if (< 0 r)
                  (begin
                   (ly:grob-suicide! grob)
                   "")
                  (ly:format "(~a)"  (ly:grob-property grob 'measure-count))))))
      (non-musical . #t)
      (horizon-padding . 0.05)
      (outside-staff-priority . 100)
      (padding . 1.0)
      (self-alignment-X . ,RIGHT)
      (side-axis . ,Y)
      (stencil . ,ly:text-interface::print)
      (X-offset . 4)
      (Y-offset . ,side-position-interface::y-aligned-side)
      (Y-extent . (0 . 0))
      (X-extent . (0 . 0))
      (meta .
       ((class . Item)
        ;; This is clumsy, but we have to repeat the name
        (name . MultiMeasureRestReminder)
        (interfaces . (break-alignable-interface
                       font-interface
                       outside-staff-interface
                       self-alignment-interface
                       side-position-interface
                       text-interface))))))

  }
}

\book {
  \score {
    {
      \set Score.skipBars = ##t
      \time 3/4
      R2.*4 \break
      R2.*5 \pageBreak
      R2.*6 \break
      R2.*7 \pageBreak
      R2.*8 \break
    }

  }
  \paper {
    #(set-paper-size "a7")
  }
}
