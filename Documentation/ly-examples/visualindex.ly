% visualindex.ly
%
% Originally written by Joram Berger <joramberger.de>.
%
% Completely rewritten and updated by Werner Lemberg <wl@gnu.org>.
%
% This file is provided under the licenses
% CC BY-SA 4.0 (https://creativecommons.org/licenses/by-sa/4.0/) and the
% GFDL 1.3 or later (https://www.gnu.org/licenses/fdl-1.3.html).

% open issues:
%
% - Links are not clickable in SVG output


\version "2.25.8"

#(set-default-paper-size "a4")
% #(set-default-paper-size '(cons (* 160 mm) (* (+ 673.2 4) pt)))


\pointAndClickOff
#(set-global-staff-size 17.82)


\header {
  title = \markup \larger \larger "Visual LilyPond Grob Index"
}


\paper {
  % annotate-spacing = ##t

  page-count = 2

  left-margin = 15\mm
  right-margin = 15\mm
  top-margin = 15\mm
  bottom-margin = 15\mm

  oddHeaderMarkup = ##f
  evenHeaderMarkup = ##f
  oddFooterMarkup = ##f
  evenFooterMarkup = ##f

  footnote-separator-markup = \markup { \override #'(span-factor . 1/6)
                                        \draw-hline }

  ragged-bottom = ##t
  ragged-last-bottom = ##t

  system-system-spacing.padding = 4

  score-markup-spacing.basic-distance = 0
  score-markup-spacing.minimum-distance = 0
  score-markup-spacing.padding = 0
  score-markup-spacing.stretchability = 0

  markup-system-spacing.basic-distance = 0
  markup-system-spacing.minimum-distance = 0
  markup-system-spacing.padding = 0
  markup-system-spacing.stretchability = 0

  markup-markup-spacing.basic-distance = 0
  markup-markup-spacing.minimum-distance = 0
  markup-markup-spacing.padding = 0
  markup-markup-spacing.stretchability = 0

  system-system-spacing.basic-distance = 0
  system-system-spacing.minimum-distance = 0
  system-system-spacing.padding = 4
  system-system-spacing.stretchability = 0

  top-markup-spacing.basic-distance = 0
  top-markup-spacing.minimum-distance = 0
  top-markup-spacing.padding = 0
  top-markup-spacing.stretchability = 0

  top-system-spacing.basic-distance = 0
  top-system-spacing.minimum-distance = 0
  top-system-spacing.padding = 0
  top-system-spacing.stretchability = 0

  last-bottom-spacing.basic-distance = 0
  last-bottom-spacing.minimum-distance = 0
  last-bottom-spacing.padding = 0
  last-bottom-spacing.stretchability = 0

  property-defaults.fonts.serif = "Linux Libertine O"
  property-defaults.baseline-skip = 2.3
}


balloon =
#(define-music-function (grob-name x-off y-off
                                   text-align-x text-align-y
                                   x-attach y-attach
                                   text)
   (symbol? number? number?
            boolean-or-number? boolean-or-number?
            boolean-or-number? boolean-or-number?
            markup?)
   "Attach TEXT to GROB-NAME at offsets X-OFF and Y-OFF.  If
TEXT-ALIGN-X or TEXT-ALIGN-Y are numbers, use them as values for the
'text-alignment-X' and 'text-alignment-Y' properties.  If X-ATTACH or
Y-ATTACH are numbers, use them as values for the 'X-attachment' and
'Y-attachment' properties.  If one of the four last-mentioned
arguments is '#f', don't set the corresponding property."
   (let ((proplist (list 'symbol grob-name
                         'X-offset x-off
                         'Y-offset y-off
                         'text text))
         (tweaklist '()))
     (when (number? text-align-x)
       (set! tweaklist (cons (cons 'text-alignment-X text-align-x)
                             tweaklist)))
     (when (number? text-align-y)
       (set! tweaklist (cons (cons 'text-alignment-Y text-align-y)
                             tweaklist)))
     (when (number? x-attach)
       (set! tweaklist (cons (cons 'X-attachment x-attach)
                             tweaklist)))
     (when (number? y-attach)
       (set! tweaklist (cons (cons 'Y-attachment y-attach)
                             tweaklist)))

     (when (not (null? tweaklist))
       (set! proplist (cons* 'tweaks tweaklist proplist)))

     (make-event-chord (list (apply make-music
                                    'AnnotateOutputEvent proplist)))))

% Object link functions based on code from Jean Abou Samra.

#(use-modules (ice-9 match))

#(define notation-format
   (match-let (((major minor _) (ly:version)))
     (format #f
             "https://lilypond.org/doc/v~a.~a/Documentation/notation/~~a"
             major minor)))
#(define internals-format
   (match-let (((major minor _) (ly:version)))
     (format #f
             "https://lilypond.org/doc/v~a.~a/Documentation/internals/~~a"
             major minor)))

#(define ((add-link doc) grob original)
   (if (ly:stencil? original)
       (let ((url (format #f notation-format doc)))
         (grob-interpret-markup
          grob
          (make-with-url-markup url (make-stencil-markup original))))
       original))

addLink =
#(define-music-function (path doc)
   (symbol-list? string?)
   "Add a clickable link for a grob of type PATH to the documentation
page DOC in the Notation Reference.  All grobs in the score are
affected."
   (propertyOverride
    (append path '(stencil))
    (grob-transformer 'stencil (add-link doc))))

#(define-markup-command (doclink layout props text)
   (string?)
   "Return a blue markup linked to the Internals reference TEXT
(lower-cased).  This is intended for grobs."
   (interpret-markup layout props
     #{
       \markup \with-url
         #(format #f internals-format (string-downcase text))
         \with-color #(universal-color "blue") #text
     #}))

#(define-markup-command (contextlink layout props text)
   (string?)
   "Return a markup in a larger, bold, red purple, rounded box linked
to the Internals Reference TEXT (lower-cased).  This is intended for
contexts.  If TEXT contains '|', use the text before this character as
the link and the text after it as the shown link text."
   (let* ((sep (string-index text #\|))
          (link (if sep
                    (substring text 0 sep)
                    text))
          (linktext (if sep
                        (substring text (1+ sep))
                        text)))
     (interpret-markup layout props
       #{
         \markup \with-url
           #(format #f internals-format (string-downcase link))
           \bold \larger \with-color #(universal-color "redpurple")
           \rounded-box #linktext
       #})))

#(define-markup-command (engraverlink layout props link text)
   (string? string?)
   "Return a blue-green markup to link TEXT to the Internals
Reference LINK (lower-cased).  This is intended for engravers.  If
TEXT equals '*', print symbol '*', otherwise show it as the link text."
   (interpret-markup layout props
     #{
       \markup \with-url
         #(format
           #f
           (string-append internals-format "~a")
           (ly:regex-replace (ly:make-regex "_")
                             (string-downcase link)
                             "_005f")
           "_005fengraver")
         \with-color #(universal-color "bluegreen")
           #(if (string=? "*" text)
                #{ \markup \fontsize #4 \translate-scaled #'(0 . -0.8) "*" #}
                #{ \markup \italic #text #})
     #}))

#(define-markup-command (docengraverlink layout props doc engraver)
   (string? string?)
   "This is a combination of '\\doclink DOC' and '\\engraverlink
ENGRAVER \"*\"', without space inbetween."
   (interpret-markup layout props
     #{
       \markup \concat {
         \with-url
           #(format #f internals-format (string-downcase doc))
           \with-color #(universal-color "blue") #doc
         \with-url
           #(format
             #f
             (string-append internals-format "~a")
             (ly:regex-replace (ly:make-regex "_")
                               (string-downcase engraver)
                               "_005f")
             "_005fengraver")
           \with-color #(universal-color "bluegreen")
             \fontsize #4 \translate-scaled #'(0 . -0.8) "*" }
       #}))


%%% Top Stuff %%%

\markup \vspace #1

\markup {
  \override #'(baseline-skip . 2.7)
  \fontsize #-2 \with-color #grey
  \fill-line {
    \column {
      \bold "Legend:"
      \line {
        "  "
        \with-url #(format #f internals-format "all-layout-objects")
          \with-color #(universal-color "blue") "GraphicalObject (Grob)" }
      \vspace #-0.05
      \line {
        "  "
        \with-url #(format #f internals-format "engravers-and-performers")
          \italic \with-color #(universal-color "bluegreen")
            \concat {
              "Engraver"
              \fontsize #4 \translate-scaled #'(0 . -0.8) "*" } }
      \vspace #0.1
      \line {
        "  "
        \smaller \contextlink "Contexts|Context" } }
    \null
    \column {
      \line { \bold "Example:"
              \concat { "‘BarNumber"
                        \fontsize #4 \translate-scaled #'(0 . -0.8) "*"
                        "’" }
              "means:" }
      "   • the graphical object is called: BarNumber"
      "   • suggested search terms: bar number"
      "   • the engraver name (if not specified otherwise): Bar_number_engraver"
      "Note that not all engravers are shown due to space constraints." }
    \null
    \null
    \null } }


%%% Score %%%

\markup \vspace #2

\markup {
  \line {
    % Without `\left-align` the box around the link is not taken
    % into account for the horizontal extent.
    \left-align \contextlink "Score"
    \override #'(baseline-skip . 2.3)
    \column {
      \line {
        "["
        \doclink "BreakAlignGroup"
        \doclink "BreakAlignment"
        \doclink "ControlPoint"
        \doclink "ControlPolygon"
        \doclink "GraceSpacing" }
      \line {
        \transparent "["
        \doclink "NonMusicalPaperColumn"
        \doclink "PaperColumn"
        \doclink "SpacingSpanner"
        \doclink "VerticalAlignment"
        "]" } } } }

\markup \vspace #1

\score {
  \new StaffGroup <<
    % `collapse-height` must be lower than the actual number of staff
    % lines.
    \override StaffGroup.SystemStartBracket.collapse-height = 1
    \override Score.SystemStartBar.collapse-height = 1

    \new Staff \with {
      \offset X-offset 1 InstrumentName
      instrumentName = \markup \column {
        \line { \doclink "SystemStartBar" "|" }
        \line { \doclink "SystemStartBrace" "{" }
        \line { \doclink "SystemStartBracket" "→" }
        \line { \doclink "SystemStartSquare" "[" }
        \line { \engraverlink "System_start_delimiter"
                              "System_start_delimiter" } }
    } \relative c'' {
      \override Score.SpacingSpanner.common-shortest-duration =
         \musicLength 32

      \override Score.BarNumber.break-visibility = ##(#t #t #t)

      \addLink Score.BarLine "bar-lines"
      \addLink Score.BarNumber "bar-numbers"
      \addLink Score.CenteredBarNumber "bar-numbers"
      \addLink Score.CodaMark "manual-repeat-marks"
      \addLink Score.Footnote "creating-footnotes"
      \addLink Score.JumpScript "al_002dfine-repeats"
      \addLink Score.MetronomeMark "metronome-marks"
      \addLink Score.Parentheses "parentheses"
      \addLink Score.RehearsalMark "rehearsal-marks"
      \addLink Score.SectionLabel "segno-repeat-structure"
      \addLink Score.SegnoMark "segno-repeat-structure"
      \addLink Score.StaffHighlight "staff-highlights"
      \addLink Score.SystemStartBar "grouping-staves"
      \addLink Score.TextMark "text-marks"
      \addLink Score.SystemStartBracket "grouping-staves"
      \addLink Score.VoltaBracket "alternative-endings"

      \addLink NoteHead "writing-pitches"

      \omit Staff.Clef
      \omit Staff.TimeSignature

      \balloon LeftEdge -1 6.5 ##f ##f ##f ##f \markup \line {
        \engraverlink "Break_align" "Break_align"
        \doclink "LeftEdge" }
      \balloon MetronomeMark -1.5 5 0.9 ##f -0.9 1.1 \markup
        \docengraverlink "MetronomeMark" "Metronome_mark"
      \tweak X-offset 1 \tempo Lento
      a1

      \balloon BarNumber -1 1 -0.2 ##f -0.5 ##f \markup
        \docengraverlink "BarNumber" "Bar_number"
      a1

      \balloon RehearsalMark -2 -6 0.7 ##f ##f ##f \markup \right-column {
        \doclink "RehearsalMark"
        \engraverlink "Mark" "Mark" }
      \once\override Score.RehearsalMark.outside-staff-priority = ##f
      \mark \default

      \balloon CenteredBarNumber -1 3 0.1 ##f -0.5 ##f \markup \right-column {
        \doclink "CenteredBarNumberLineSpanner"
        \doclink "CenteredBarNumber" }
      \set Score.centerBarNumbers = ##t
      \once\override Score.CenteredBarNumberLineSpanner.padding = 3
      a1
      \set Score.centerBarNumbers = ##f

      \revert Score.BarNumber.break-visibility

      \balloon Parentheses -1 -2 0.2 ##f -0.9 ##f \markup \column {
        \doclink "Parentheses"
        \engraverlink "Parenthesis" "Parenthesis" }
      \parenthesize a1

      \balloon TextMark -2 1 0.1 ##f -1 0 \markup
        \docengraverlink "TextMark" "Text_mark"
      \once\override Score.TextMark.outside-staff-priority = ##f
      <>\textMark "unis."

      \repeat volta 2 {
        \balloon Footnote -1 -1 0.2 ##f 17 -12 \markup
          \docengraverlink "Footnote" "Footnote"
        \override Score.Footnote.color = #(universal-color "redpurple")
        \once \override TextScript.Y-offset = -6
        % XXX The horizontal position of the footnote counter is hard-coded;
        %     it gets attached to the footnote text at the left side,
        %     right-aligned, and in the left margin (this is part of issue
        %     #2561).  Since we set the left margin to zero, this makes the
        %     counter outside of the paper.  We thus fake a counter using a
        %     markup `\footnote` command; this command doesn't insert such a
        %     hard-coded counter.
        \footnote \markup { \footnote \tiny "1" "" }
                  #'(1 . -2)
                  \markup \concat { \raise #0.7 \tiny "1" "Footnote" }
        a1

        \balloon VoltaBracket -1 1 -0.5 ##f -1 ##f \markup \column {
          \line {
            \doclink "VoltaBracket"
            \engraverlink "Volta" "Volta" }
          \doclink "VoltaBracketSpanner" }
        \alternative {
          { a1 }
          { \once\override Score.VoltaBracket.shorten-pair = #'(1 . 2)
            a1 } } }

      \repeat segno 2 {
        \balloon SegnoMark 1 3 -0.2 ##f ##f ##f \markup
          \doclink "SegnoMark"
        a1

        \balloon CodaMark -1 1 -0.5 ##f -0.5 ##f \markup
          \doclink "CodaMark"
        \once\override Score.CodaMark.outside-staff-priority = ##f
        \alternative {
          \volta 1 { a1 }
          \volta 2 \volta #'() {
            \section
            \once\override Score.SectionLabel.outside-staff-priority = ##f
            \balloon SectionLabel -3 4 -0.5 ##f -0.5 ##f \markup
              \doclink "SectionLabel"
            \sectionLabel "Coda" } } }

      \balloon JumpScript -2 -0.5 ##f 0 -0.8 0 \markup {
        \engraverlink "Jump" "Jump"
        \doclink "JumpScript" }
      <>

      a1

      \bar "|."
    }
  >>

  \layout {
    indent = 3.5\cm

    \context {
      \Score
      \consists Balloon_engraver
      \override BalloonText.annotation-balloon = ##f
      \balloonLengthOff
      % XXX Temporary work-around for issue #6240.
      \override BalloonText.after-line-breaking =
        #(lambda (grob)
           (ly:side-position-interface::move-to-extremal-staff grob)
           (ly:axis-group-interface::add-element
             (ly:grob-object (ly:grob-parent grob Y)
                             'axis-group-parent-Y)
             grob))

      dalSegnoTextFormatter = #format-dal-segno-text-brief
    }
  }
}


%%% Staff %%%

\markup {
  \vspace #1.9
  \line {
    \left-align \contextlink "Staff"
    \override #'(baseline-skip . 2.3)
    \column {
      \line {
        "["
        \doclink "DotColumn"
        \doclink "LedgerLineSpanner"
        \doclink "NoteCollision"
        \doclink "RestCollision" }
      \line {
        \transparent "["
        \doclink "ScriptRow"
        \doclink "SpanBarStub"
        \doclink "VerticalAxisGroup"
        "]" } } } }

\markup \vspace #1

\score {
  \new Staff \with {
    \override InstrumentName.Y-offset = -10
    instrumentName = \markup
      \docengraverlink "InstrumentName" "Instrument_name"
  } \new Voice \relative c'' {
    \override Score.SpacingSpanner.common-shortest-duration =
       \musicLength 128

    \addLink Staff.BarLine "bar-lines"
    \addLink Staff.CaesuraScript "caesuras"
    \addLink Staff.Clef "clef"
    \addLink Staff.ClefModifier "clef"
    \addLink Staff.CueClef "formatting-cue-notes"
    \addLink Staff.CueEndClef "formatting-cue-notes"
    \addLink Staff.KeyCancellation "key-signature"
    \addLink Staff.KeySignature "key-signature"
    \addLink Staff.OttavaBracket "ottava-brackets"
    \addLink Staff.PianoPedalBracket "piano-pedals"
    \addLink Staff.SostenutoPedal "piano-pedals"
    \addLink Staff.SustainPedal "piano-pedals"
    \addLink Staff.StaffEllipsis "skipping-corrected-music"
    \addLink Staff.TimeSignature "time-signature"
    \addLink Staff.UnaCordaPedal "piano-pedals"

    \addLink Accidental "writing-pitches"
    \addLink AccidentalCautionary "writing-pitches"
    \addLink AccidentalSuggestion
      "annotational-accidentals-_0028musica-ficta_0029"
    \addLink Dots "durations"
    \addLink NoteHead "writing-pitches"
    \addLink Rest "rests"
    \addLink Stem "stems"

    \omit Score.BarNumber

    \balloon Clef -2 -1 ##f ##f -0.9 -0.9 \markup
      \docengraverlink "Clef" "Clef"
    \balloon ClefModifier 1 -1.5 -0.5 ##f ##f ##f \markup
      \doclink "ClefModifier"
    \clef "treble_8"

    \balloon KeySignature -1 1 ##f ##f ##f ##f \markup \line {
      \engraverlink "Key" "Key"
      \doclink "KeySignature" }
    \time 4/4

    \balloon TimeSignature -1 6 0.5 ##f -0.5 ##f \markup
      \docengraverlink "TimeSignature" "Time_signature"
    \key g \major
    a,2

    \set Score.skipTypesetting = ##t
    a2
    \set Score.skipTypesetting = ##f
    <>
    \balloon StaffEllipsis -1 3 0.7 ##f 0 0.3 \markup \center-column {
      \doclink "StaffEllipsis"
      \engraverlink "Skip_typesetting" "Skip_typesetting" }

    \balloon BarLine -2 -1 0.7 ##f ##f ##f \markup \line {
      \engraverlink "Bar" "Bar"
      \doclink "BarLine" }

    \balloon KeyCancellation 1 2 -0.5 ##f ##f ##f \markup
      \doclink "KeyCancellation"
    \key c \major

    \balloon Accidental -0.5 -1 0 ##f -1 ##f \markup \right-column {
      \docengraverlink "Accidental" "Accidental"
      \doclink "AccidentalPlacement" }
    es2

    \balloon AccidentalCautionary 0.5 0.5 -0.5 ##f ##f 0.5 \markup
      \doclink "AccidentalCautionary"
    es'?2

    \balloon SostenutoPedal 0 -1 0.4 ##f ##f ##f \markup \right-column {
      \doclink "SostenutoPedal"
      \doclink "SostenutoPedalLineSpanner" }
    a,1\tweak X-offset #-2 \sostenutoOn
    \after 2. \sostenutoOff a1

    \balloon UnaCordaPedal 0 -1 ##f ##f ##f ##f \markup \center-column {
      \doclink "UnaCordaPedal"
      \doclink "UnaCordaPedalLineSpanner" }
    \balloon AccidentalSuggestion -1 1 ##f ##f ##f ##f \markup
      \doclink "AccidentalSuggestion"
    \set suggestAccidentals = ##t
    ais1\tweak X-offset 0.5 \unaCorda
    \set suggestAccidentals = ##f

    \balloon OttavaBracket 0 1 ##f ##f ##f ##f \markup \column {
      \engraverlink "Ottava_spanner" "Ottava_spanner"
      \doclink "OttavaBracket" }
    \ottava 1
    a'1
    \ottava 0

    \break

    \balloon SustainPedal 2 -0.5 ##f 0.3 ##f 0 \markup \column {
      \doclink "SustainPedal"
      \doclink "SustainPedalLineSpanner" }
    a,1\tweak X-offset 0 \sustainOn

    \after 2.\sustainOff a1

    \balloon CaesuraScript -1 1.5 ##f 0 ##f 0 \markup \right-column {
      \engraverlink "Caesura" "Caesura"
      \doclink "CaesuraScript" }
    \once \set Staff.caesuraType = #'((underlying-bar-line . "||"))
    \once \set Staff.caesuraTypeTransform = ##f
    <>\caesura^\fermata

    a1

    \balloon CueClef 1 1.5 ##f ##f ##f ##f \markup
      \docengraverlink "CueClef" "Cue_clef"
    \cueClef "bass"

    \balloon StaffHighlight -1 -2 ##f ##f 0.3 ##f \markup
      \docengraverlink "StaffHighlight" "Staff_Highlight"
    \staffHighlight #(x11-color "grey80")
    \new CueVoice { c,1 }
    \stopStaffHighlight

    \balloon CueEndClef 1 1.5 ##f ##f ##f 0.8 \markup
      \doclink "CueEndClef"
    \cueClefUnset

    \balloon PianoPedalBracket 2 -0.5 ##f 0.3 ##f ##f \markup \column {
      \doclink "PianoPedalBracket"
      \engraverlink "Piano_pedal" "Piano_pedal" }
    \set Staff.pedalSustainStyle = #'bracket
    f2 \sustainOn
    f2 \sustainOff

    \tweak X-offset 1.5 \tweak Y-offset 0.5 \mark \markup {
      \normalsize
      \column {
        \doclink "StaffSymbol"
        \doclink "StaffSpacing" } }

    \bar "|."
  }

  \layout {
    indent = 2.6\cm
    ragged-last = ##t

    \context {
      \Score
      \consists Balloon_engraver
      \override BalloonText.annotation-balloon = ##f
      \balloonLengthOff
      % XXX Temporary work-around for issue #6240.
      \override BalloonText.after-line-breaking =
        #(lambda (grob)
           (ly:side-position-interface::move-to-extremal-staff grob)
           (ly:axis-group-interface::add-element
             (ly:grob-object (ly:grob-parent grob Y)
                             'axis-group-parent-Y)
             grob))
    }
  }
}


%%% Voice %%%

% The `InstrumentSwitch` grob is obsolete.

\markup {
  \vspace #2
  \line {
    \left-align \contextlink "Voice"
    \override #'(baseline-skip . 2.3)
    \column {
      \line {
        "["
        \doclink "CombineTextScript"
        \doclink "FingeringColumn"
        \doclink "LaissezVibrerTieColumn"
        \doclink "NoteColumn"
        \doclink "NoteSpacing" }
      \line {
        \transparent "["
        \doclink "RepeatTieColumn"
        \doclink "StemStub"
        \doclink "ScriptColumn"
        \doclink "TieColumn"
        "]" } } } }

\markup \vspace #1

\score {
  \new Voice \relative c' {
    \addLink Staff.BarLine "bar-lines"

    \addLink Arpeggio "arpeggio"
    \addLink Beam "beams"
    \addLink BendAfter "falls-and-doits"
    \addLink BreathingSign "breath-marks"
    \addLink ClusterSpanner "clusters"
    \addLink Dots "durations"
    \addLink DoublePercentRepeat "percent-repeats"
    \addLink DoublePercentRepeatCounter "percent-repeats"
    \addLink DoubleRepeatSlash "percent-repeats"
    \addLink DynamicText "dynamics"
    \addLink DynamicTextSpanner "dynamics"
    \addLink FingerGlideSpanner "gliding-fingers"
    \addLink Fingering "fingering-instructions"
    \addLink Flag "durations"
    \addLink Glissando "glissando"
    \addLink Hairpin "dynamics"
    \addLink LaissezVibrerTie "ties"
    \addLink LigatureBracket "ligatures"
    \addLink MultiMeasureRest "full-measure-rests"
    \addLink MultiMeasureRestNumber "full-measure-rests"
    \addLink MultiMeasureRestScript "full-measure-rests"
    \addLink MultiMeasureRestText "full-measure-rests"
    \addLink NoteHead "writing-pitches"
    \addLink PercentRepeat "percent-repeats"
    \addLink PercentRepeatCounter "percent-repeats"
    \addLink PhrasingSlur "phrasing-slurs"
    \addLink RepeatSlash "percent-repeats"
    \addLink RepeatTie "ties"
    \addLink Rest "rests"
    \addLink Script "expressive-marks-attached-to-notes"
    \addLink Slur "slurs"
    \addLink StemTremolo "tremolo-repeats"
    \addLink StringNumber "string-number-indications"
    \addLink TextSpanner "text-spanners"
    \addLink Tie "ties"
    \addLink TrillPitchAccidental "trills"
    \addLink TrillPitchHead "trills"
    \addLink TrillPitchParentheses "trills"
    \addLink TrillSpanner "trills"
    \addLink TupletBracket "tuplets"
    \addLink TupletNumber "tuplets"

    \omit Staff.Clef
    \omit Staff.TimeSignature
    \omit Score.BarNumber

    \set countPercentRepeats = ##t

    \after 1 {
      \newSpacingSection
      \override Score.SpacingSpanner.spacing-increment = 1 }
    \repeat percent 2 {
      \balloon NoteHead 0.5 -2 -0.5 ##f 0 ##f \markup
        \docengraverlink "NoteHead" "Note_head"
      \balloon Stem -0.5 2 0 ##f ##f ##f \markup
        \docengraverlink "Stem" "Stem"
      d2

      \balloon Rest -0.5 -1.5 -0.3 ##f ##f ##f \markup
        \docengraverlink "Rest" "Rest"
      r4

      \after 8 {
        \balloon RepeatSlash -1 -2 0 ##f -0.5 ##f \markup \column {
          \doclink "RepeatSlash"
          \engraverlink "Slash_repeat" "Slash_repeat" } }
      \repeat percent 2 {
        d32[ e f g] }

      \balloon PercentRepeat -1.5 2.5 ##f ##f ##f ##f \markup
        \docengraverlink "PercentRepeat" "Percent_repeat"
      \balloon PercentRepeatCounter -1 2 0.7 ##f ##f ##f \markup
        \doclink "PercentRepeatCounter" }

    \newSpacingSection
    \revert Score.SpacingSpanner.spacing-increment

    \override DoublePercentRepeat.extra-spacing-width = #'(-6 . 6)

    % The correct time offset to access the `DoublePercentRepeat` grob
    % is the beginning of the second bar.
    \after 1*3 {
      \balloon DoublePercentRepeat 1 -4.5 0.2 ##f 0.8 ##f \markup
        \docengraverlink "DoublePercentRepeat" "Double_percent_repeat"
      \balloon DoublePercentRepeatCounter -0.5 1 0.05 ##f ##f ##f \markup
        \doclink "DoublePercentRepeatCounter"
      \newSpacingSection
    }

    \repeat percent 2 {
      \balloon Flag -0.5 -2.5 0.5 ##f 0.5 ##f \markup
        \doclink "Flag"
      g8\noBeam

      \balloon StemTremolo 1 -3.5 -0.6 ##f ##f ##f \markup
        \doclink "StemTremolo"
      g4:16

      \balloon Beam -0.5 1.5 1 ##f 0 ##f \markup \right-column {
        \engraverlink "Auto_beam" "Auto_beam"
        \docengraverlink "Beam" "Beam" }
      g16[ g]

      \after 8 {
        \balloon DoubleRepeatSlash -2.5 -4.5 0.7 ##f -0.5 ##f \markup
          \doclink "DoubleRepeatSlash" }
      \repeat percent 2 {
        \balloon Dots -0.5 2 0 ##f -0.5 ##f \markup
          \docengraverlink "Dots" "Dots"
        e'16. e32 }

      \balloon Script -1 1 -0.2 ##f -1.1 0 \markup
        \docengraverlink "Script" "Script"
      e4->

      \balloon Fingering -2 3.5 ##f -0.5 ##f 0 \markup
        \docengraverlink "Fingering" "Fingering"
      \balloon StringNumber 1 1 ##f 0.5 0.5 1 \markup \column {
        \doclink "StringNumber"
        \engraverlink "New_fingering" "New_fingering" }
      \balloon StrokeFinger -1 -1.5 0.7 ##f ##f ##f \markup
        \doclink "StrokeFinger"
      g,4\2-1\rightHandFinger 1

      \balloon Arpeggio -0.5 -4.5 0.5 ##f ##f ##f \markup
        \docengraverlink "Arpeggio" "Arpeggio"
      <g b e g>4\arpeggio

      \balloon Glissando -1 -2.5 0 ##f 0 -0.2 \markup
        \docengraverlink "Glissando" "Glissando"
      e'4\tweak springs-and-rods #ly:spanner::set-spacing-rods
        \tweak minimum-length 4 \glissando
      e,4

      \balloon BreathingSign 0.5 -4.5 ##f ##f ##f ##f \markup
        \docengraverlink "BreathingSign" "Breathing_sign"
      \breathe }

    \break

    \balloon TextSpanner -0.5 -1 0.1 ##f 0 ##f \markup
      \docengraverlink "TextSpanner" "Text_spanner"
    \once\override TextSpanner.bound-details.left.text = "rit."
    \once\override TextSpanner.outside-staff-priority = ##f
    \once\override TextSpanner.padding = 0.4
    \textSpannerDown
    f4\startTextSpan f f f\stopTextSpan

    \override Staff.MultiMeasureRest.space-increment = 0
    \compressMMRests {
      \balloon MultiMeasureRest -1.5 2 ##f ##f ##f ##f \markup
        \docengraverlink "MultiMeasureRest" "Multi_measure_rest"
      \balloon MultiMeasureRestNumber -1 1 0.6 ##f ##f ##f \markup
        \doclink "MultiMeasureRestNumber"
      R1*4 }

    \balloon MultiMeasureRestScript 0.5 1 -0.5 ##f ##f ##f \markup
      \doclink "MultiMeasureRestScript"
    \balloon MultiMeasureRestText -1 -3 ##f ##f ##f ##f \markup
      \doclink "MultiMeasureRestText"
    \once\override MultiMeasureRestText.outside-staff-priority = ##f
    R1\fermata_\markup \italic "lunga"

    \bar "||"

    \balloon DynamicTextSpanner -1 -1.5 0.4 ##f 0 -0.1 \markup
      \doclink "DynamicTextSpanner"
    e'4\cresc e

    \balloon TextScript 1 1.5 ##f 0.3 ##f ##f \markup \column {
      \doclink "TextScript"
      \engraverlink "Text" "Text" }
    \once\override TextScript.outside-staff-priority = ##f
    e4^\markup {
      \with-url #(format #f notation-format "writing-text")
      \italic "dolce" }

    \balloon DynamicText -0.5 -3 0.9 ##f ##f ##f \markup \line {
      \engraverlink "Dynamic" "Dynamic"
      \doclink "DynamicText" }
    e4\ff

    \balloon Hairpin -1 -1.5 -0.6 ##f -0.9 ##f \markup \column {
      \doclink "Hairpin"
      \doclink "DynamicLineSpanner" }
    e4\> e\!

    \balloon FingerGlideSpanner -0.5 2.5 -0.2 ##f 0.2 0.2 \markup \column {
      \doclink "FingerGlideSpanner"
      \engraverlink "Finger_glide" "Finger_glide" }
    \set fingeringOrientations = #'(right)
    <e\glide-1>2
    \set fingeringOrientations = #'(left)
    <f,-1>2

    \balloon TrillSpanner 1 1 0 -0.7 0 0.5 \markup
      \docengraverlink "TrillSpanner" "Trill_spanner"
    \balloon TrillPitchParentheses -0.5 -4 -0.2 ##f -0.8 ##f \markup \column {
      \line {
        \doclink "TrillPitchGroup"
        \engraverlink "Pitched_trill" "Pitched_trill" }
      \doclink "TrillPitchAccidental"
      \doclink "TrillPitchHead"
      \doclink "TrillPitchParentheses" }
    \once\override TrillSpanner.outside-staff-priority = ##f
    \pitchedTrill es'2 \startTrillSpan fes

    \bar "||"

    \break

    <>\stopTrillSpan

    \balloon PhrasingSlur 0.5 1 -0.5 ##f -0.9 0.3 \markup
      \docengraverlink "PhrasingSlur" "Phrasing_slur"
    c4\(

    \balloon Slur 0 -1 ##f ##f -0.8 ##f \markup
      \docengraverlink "Slur" "Slur"
    g4( a4)

    \balloon Tie 0 -1 ##f ##f -0.8 ##f \markup
      \docengraverlink "Tie" "Tie"
    g4 ~ g

    \balloon RepeatTie -1 -3 ##f ##f ##f ##f \markup
      \docengraverlink "RepeatTie" "Repeat_tie"
    e\repeatTie

    \balloon LaissezVibrerTie 0 -1 ##f ##f ##f ##f \markup \column {
      \doclink "LaissezVibrerTie"
      \engraverlink "Laissez_vibrer" "Laissez_vibrer" }
    e\laissezVibrer

    \balloon BendAfter -1 3 ##f ##f 0 ##f \markup \line {
      \engraverlink "Bend" "Bend"
      \doclink "BendAfter" }
    e'\)\bendAfter #-6

    \bar "||"

    \balloon TupletNumber -0.5 -1 0.2 ##f ##f ##f \markup
      \doclink "TupletNumber"
    \balloon TupletBracket 1 -0.5 ##f 0.5 ##f ##f \markup \column {
      \doclink "TupletBracket"
      \engraverlink "Tuplet" "Tuplet" }
    \tuplet 3/2 { e4 d c }

    \balloon LigatureBracket -1 1 0.5 ##f ##f ##f \markup
      \docengraverlink "LigatureBracket" "Ligature_bracket"
    \once\override LigatureBracket.padding = 1
    \[ c4 b \]

    \balloon ClusterSpanner 0 -1.5 -0.2 ##f 0 ##f \markup \right-column {
      \docengraverlink "ClusterSpanner" "Cluster_spanner"
      \doclink "ClusterSpannerBeacon" }
    \makeClusters { <c e>4  <b f'> <b g'> <c g> }

    \bar "|."
  }

  \layout {
    indent = 0

    \context {
      \Score
      \consists Balloon_engraver
      \override BalloonText.annotation-balloon = ##f
      \balloonLengthOff
      % XXX Temporary work-around for issue #6240.
      \override BalloonText.after-line-breaking =
        #(lambda (grob)
           (ly:side-position-interface::move-to-extremal-staff grob)
           (ly:axis-group-interface::add-element
             (ly:grob-object (ly:grob-parent grob Y)
                             'axis-group-parent-Y)
             grob))
    }
  }
}


\pageBreak


%%% ChordNames, FiguredBass, Lyrics %%%

\markup \with-dimensions #'(0 . 0) #'(2.5 . 0) {
  \general-align #Y #UP
  \column {
    \left-align \contextlink "ChordNames"
    \vspace #1
    \line {
      \left-align \contextlink "FiguredBass"
      \engraverlink "Figured_bass" "*" }
    \vspace #2.1
    \left-align \contextlink "Lyrics" }
}

\score {
  <<
    \new ChordNames \chordmode {
      \addLink ChordName "displaying-chords"

      \balloon ChordName 1 1 ##f ##f ##f ##f \markup
        \docengraverlink "ChordName" "Chord_name"
      f1 bes1 c1:7
    }

    \new Staff \with {
      \override InstrumentName.X-offset = -32
      \override InstrumentName.Y-offset = -9
      instrumentName = \markup \column {
        \doclink "BassFigureAlignment"
        \doclink "BassFigureAlignmentPositioning"
        \vspace #1.7
        \doclink "LyricSpace" }
    } \new Voice = "voice" \relative c' {
      \override Score.SpacingSpanner.common-shortest-duration =
         \musicLength 64

      \addLink NoteHead "writing-pitches"
      \addLink Slur "slurs"

      \omit Staff.BarLine
      \omit Staff.Clef
      \omit Staff.TimeSignature

      f4 f f( e') f, f a f
      e'2 e
    }

    \new FiguredBass \with {
      \override VerticalAxisGroup.nonstaff-nonstaff-spacing.padding = 2
    } \figuremode {
      \addLink BassFigure "figured-bass"
      \addLink BassFigureContinuation "figured-bass"
      \addLink BassFigureBracket "figured-bass"

      \bassFigureExtendersOn

      \balloon BassFigure #1 #-1.5 ##f ##f ##f ##f \markup
        \doclink "BassFigure"
      <5>2

      \balloon BassFigureBracket #1 #-1.5 ##f ##f ##f ##f \markup
        \doclink "BassFigureBracket"
      <[6]>2

      \balloon BassFigureLine #2 #-1.5 ##f 0 ##f ##f \markup \column {
        \doclink "BassFigureLine"
        \doclink "BassFigureContinuation" }
      <5>2 <5>
    }

    \new Lyrics \with {
      \override LyricText.Y-extent = #'(-0.5 . 3.5)

      \addLink LyricText "aligning-lyrics-to-a-melody"
      \addLink StanzaNumber "adding-stanza-numbers"
      \addLink VowelTransition "gradual-changes-of-vowel"
    } \lyricsto "voice" {
      \balloon StanzaNumber #1 #-2 #-0.8 ##f 0 ##f \markup
        \docengraverlink "StanzaNumber" "Stanza_number"
      \set stanza = "1."
      Lo -- rem

      \balloon LyricHyphen #-1 #-1.5 #-0.3 ##f ##f ##f \markup \column {
        \doclink "LyricHyphen"
        \engraverlink "Hyphen" "Hyphen" }
      ip -- sum

      \balloon LyricText #-0.5 #-1 #-0.1 ##f ##f ##f \markup \column {
        \doclink "LyricText"
        \engraverlink "Lyric" "Lyric" }
      do --

      \balloon LyricExtender #-0.5 #-1.5 #-0.4 ##f 0 ##f \markup \column {
        \doclink "LyricExtender"
        \engraverlink "Extender" "Extender" }
      lor __

      \balloon VowelTransition #1 #-1.5 ##f ##f #0.4 ##f \markup
        \doclink "VowelTransition"
      sit \vowelTransition

      a -- met.
    }
  >>

  \layout {
    indent = 5\cm

    \context {
      \Score
      \consists Balloon_engraver
      \override BalloonText.annotation-balloon = ##f
      \balloonLengthOff
      % XXX Temporary work-around for issue #6240.
      \override BalloonText.after-line-breaking =
        #(lambda (grob)
           (let* ((host (ly:grob-object grob 'sticky-host))
                  (group (ly:grob-object host 'axis-group-parent-Y)))
             (ly:axis-group-interface::add-element
              (if (grob::has-interface group 'align-interface)
                  (ly:grob-parent group Y)
                  group)
              grob)))
    }
  }
}


%%% TabVoice %%%

TabVoiceMarkup =
\markup \line {
  \hspace #2
  \score {
    \new TabStaff \relative {
      \omit TabStaff.BarLine

      \addLink Score.SystemStartBar "grouping-staves"

      \addLink TabStaff.Clef "clef"

      \addLink TabNoteHead "default-tablatures"
      \addLink BendSpanner "default-tablatures"

      \balloon TabNoteHead -0.5 -2 -0.5 ##f ##f ##f \markup
        \docengraverlink "TabNoteHead" "Tab_note_head"
      a,1.

      \balloon BendSpanner -1.5 2 0.7 ##f 0 -0.2 \markup
        \docengraverlink "BendSpanner" "Bend_spanner"
      f''8\^ g\^ f
    }

    \layout {
      \context {
        \Score
        \consists Balloon_engraver
        \override BalloonText.annotation-balloon = ##f
        \balloonLengthOff
      }
      \context {
        \TabStaff
        minimumFret = 5
      }
      \context {
        \TabVoice
        \consists Bend_spanner_engraver
      }
    }
  }
}


% We manually insert a custos to avoid a line break.  Code from Jean
% Abou Samra.
custos =
#(define-music-function (pos) (integer?)
   #{
     \new Bottom \with {
       \consists
       #(lambda (context)
          (make-engraver
           ((process-music engraver)
            (let ((custos (ly:engraver-make-grob engraver 'Custos '())))
              (ly:grob-set-property! custos 'staff-position pos)))))
     }
     { }
   #})


VaticanaMarkup =
\markup \line {
  \hspace #2
  \score {
    \new VaticanaScore {
      \new VaticanaStaff \with {
        \override StaffSymbol.break-align-symbols = #'(break-alignment)

        \consists Balloon_engraver
        \override BalloonText.annotation-balloon = ##f
        % Compensate for thinner lines in this engraver.
        \override BalloonText.thickness = #(/ 1 0.5)
        \balloonLengthOff

        % XXX This is temporary hack; the `Divisio` stencil is later
        %     reset by the engraver according to `breathMarkType.
        %     Code from Jean Abou Samra.
        \override Divisio.before-line-breaking =
        #(lambda (grob)
           (let ((orig (ly:grob-property grob 'stencil)))
             (set! (ly:grob-property grob 'stencil)
                   ((add-link "divisiones")
                    grob orig))))
        % XXX This is another temporary hack; there is no specific
        %     stencil for `VaticanaLigature`.
        \override NoteHead.before-line-breaking =
        #(lambda (grob)
           (let ((orig (ly:grob-property grob 'stencil)))
             (set! (ly:grob-property grob 'stencil)
                   ((add-link "gregorian-square-neume-ligatures")
                    grob orig))))
      } \relative c' {
        \addLink VaticanaStaff.Clef "clef"
        \addLink VaticanaStaff.Custos "custodes"
        \addLink VaticanaStaff.Divisio "divisiones"

        \addLink Episema "gregorian-articulation-signs"
        \addLink NoteHead "writing-pitches"
        % XXX This doesn't work currently, see below.
        \addLink VaticanaLigature "gregorian-square-neume-ligatures"

        c b a c
        \balloon Divisio -1 1.5 0.1 ##f ##f ##f \markup
          \docengraverlink "Divisio" "Divisio"
        \section

        \balloon VaticanaLigature -1 -4.5 -0.2 ##f 8 -5 \markup
          \docengraverlink "VaticanaLigature" "Vaticana_ligature"
        % XXX This is another temporary hack; there is no specific
        %     stencil for `VaticanaLigature`.
        \override NoteHead.before-line-breaking =
        #(lambda (grob)
           (let ((orig (ly:grob-property grob 'stencil)))
             (set! (ly:grob-property grob 'stencil)
                   ((add-link "gregorian-square-neume-ligatures")
                    grob orig))))
        \[ c \flexa b \pes \deminutum d \]

        \balloon Episema 0.5 1.5 -0.5 ##f 0 ##f \markup
          \docengraverlink "Episema" "Episema"
        \[ a\episemInitium \pes b \flexa a\episemFinis \]

        \balloon Custos -1 -1.5 -0.1 ##f 0 -1.1 \markup
          \docengraverlink "Custos" "Custos"
        \custos -1
      }
    }
  }
}

MensuralMarkup =
\markup \line {
  \hspace #2
  \score {
    \new MensuralStaff \with {
      \override StaffSymbol.break-align-symbols = #'(break-alignment)

      \consists Balloon_engraver
      \override BalloonText.annotation-balloon = ##f
      % Compensate for thinner lines in this engraver.
      \override BalloonText.thickness = #(/ 1 0.6)
      \balloonLengthOff

      \omit TimeSignature
    } \relative c' {
      \addLink MensuralStaff.Clef "clef"
      \addLink MensuralStaff.Custos "custodes"

      % XXX This doesn't work currently, see below.
      \addLink MensuralLigature "white-mensural-ligatures"
      \addLink NoteHead "writing-pitches"
      \addLink Stem "stems"

      \clef "neomensural-c3"
      f2 e1 d\breve

      \balloon MensuralLigature -1 -1.5 -0.2 ##f -2 -18 \markup
        \docengraverlink "MensuralLigature" "Mensural_ligature"
      % XXX This is another temporary hack; there is no specific
      %     stencil for `MensuralLigature`.
      \override NoteHead.before-line-breaking =
      #(lambda (grob)
         (let ((orig (ly:grob-property grob 'stencil)))
           (set! (ly:grob-property grob 'stencil)
                 ((add-link "white-mensural-ligatures")
                  grob orig))))
      \[ a1 g a \]

      \balloon Custos -0.5 3 0.5 ##f 0 0 \markup
        \docengraverlink "Custos" "Custos"
      \custos -1
    }

    \layout {
      \context {
        \Score
        \override SpacingSpanner.packed-spacing = ##t
      }
    }
  }
}


PetrucciMarkup =
\markup \line {
  \hspace #2
  \score {
    \new PetrucciStaff \with {
      \override StaffSymbol.break-align-symbols = #'(break-alignment)

      \consists Balloon_engraver
      \override BalloonText.annotation-balloon = ##f
      % Compensate for thicker lines in this engraver.
      \override BalloonText.thickness = #(/ 1 1.3)
      \balloonLengthOff

      \omit TimeSignature
    } \relative c {
      \addLink PetrucciStaff.Clef "clef"
      \addLink PetrucciStaff.Custos "custodes"
      \addLink PetrucciStaff.SignumRepetitionis "simple-repeats"

      \addLink NoteHead "writing-pitches"
      \addLink Stem "stems"

      \clef "petrucci-f"

      \repeat volta 2 { a'2 g4 f e2 d\breve }

      \balloon SignumRepetitionis -0.5 2 0.3 ##f 0 ##f \markup
        \docengraverlink "SignumRepetitionis" "Signum_repetitionis"
      <>

      \balloon Custos -0.5 -2 0 ##f 0 -1.1 \markup
        \docengraverlink "Custos" "Custos"
      \custos -1
    }

    \layout {
      \context {
        \Score
        \override SpacingSpanner.packed-spacing = ##t
      }
    }
  }
}


KievanMarkup =
\markup \line {
  \hspace #2
  \score {
    \new KievanStaff \with {
      \consists Balloon_engraver
      \override BalloonText.annotation-balloon = ##f
      \balloonLengthOff
    } \relative c' {
      \addLink Score.BarLine "kievan-bar-lines"

      \addLink KievanStaff.Clef "kievan-clefs"

      \addLink Accidental "kievan-accidentals"
      % XXX This doesn't work currently.
      \addLink KievanLigature "kievan-melismata"
      \addLink NoteHead "kievan-notes"
      \addLink Stem "stems"

      \cadenzaOn

      \balloon KievanLigature -1 -2.5 -0.4 ##f 10 -10 \markup
        \docengraverlink "KievanLigature" "Kievan_ligature"
      \[ cis2 d4 e d \] e1 \fine
    }

    \layout {
      indent = 0
      line-width = 3\cm
      ragged-last = ##f
    }
  }
}


GregorianTranscriptionMarkup =
\markup \line {
  \hspace #2
  \score {
    <<
      \new GregorianTranscriptionStaff {
        \addLink Score.BarLine "bar-lines"

        \addLink Staff.Clef "clef"

        \addLink NoteHead "writing-pitches"

        \repeat volta 2 { e'2 f' }
      }

      \new GregorianTranscriptionLyrics \with {
        \consists Balloon_engraver
        \override BalloonText.annotation-balloon = ##f
        \balloonLengthOff
      } \lyricmode {
        \addLink LyricText "aligning-lyrics-to-a-melody"
        \addLink LyricRepeatCount "XXX not documented yet"

        \repeat volta 2 { Do2 -- lor. }

        \balloon LyricRepeatCount #-0.5 #-1.5 #0 ##f ##f ##f \markup
          \docengraverlink "LyricRepeatCount" "Lyric_repeat_count"
      }
    >>
  }
}


\include "predefined-guitar-fretboards.ly"

FretBoardMarkup =
\markup \line {
  \hspace #2
  \score {
    \new FretBoards \with {
      \consists Balloon_engraver
      \override BalloonText.annotation-balloon = ##f
      % Compensate for thicker lines in this engraver.
      \override BalloonText.thickness = #(/ 1 1.3)
      \balloonLengthOff
    } \chordmode {
      \addLink FretBoard "fret-diagram-markups"

      \balloon FretBoard #-1 #-4 #0.2 ##f #-1 #0 \markup \column {
        \doclink "FretBoard"
        \engraverlink "Fretboard" "Fretboard" }
      f
    }
  }
}

% XXX Using `layout-set-staff-size` is currently broken, see
%     issue #6569.  We thus use `\scale` instead.
ChordGridMarkup =
\markup \line {
  \hspace #2
  \scale #'(0.5 . 0.5) \score {
    \new ChordGrid \with {
      \consists Balloon_engraver
      \override BalloonText.annotation-balloon = ##f

      % Compensate for thicker lines in this engraver.
      % XXX Disabled right now because of `\scale`.
      % \override BalloonText.thickness = #(/ 1 2)

      % Compensate smaller staff size.
      \override BalloonText.font-size = 6 % XXX 5

      \balloonLengthOff
    } \chordmode {
      \addLink ChordSquare "chord-grids"
      \addLink GridChordName "chord-grids"

      \balloon ChordSquare #-1 #-2.5 #-0.6 ##f #-0.5 ##f \markup
        \docengraverlink "ChordSquare" "Chord_square"
      \balloon GridChordName #-1 #5 #-0.6 ##f #0 ##f \markup
        \docengraverlink "GridChordName" "Grid_chord_name"
      g2:7+ bes4:m7 es:7
    }

    \layout {
      % XXX Broken, see above.
      % #(layout-set-staff-size 10)

      line-width = 5\cm % XXX 2.5\cm
      ragged-last = ##f
    }
  }
}


NoteNamesMarkup =
\markup \line {
  \hspace #2
  \score {
    <<
      \new Staff {
        \omit Staff.TimeSignature
        \omit Staff.BarLine
        \set Staff.extraNatural = ##f

        \addLink Staff.Clef "clef"

        \addLink Accidental "writing-pitches"
        \addLink NoteHead "writing-pitches"
        \addLink Stem "stems"

        feses'2 fes'
        fis'2 fisis'
      }

      \new NoteNames \with {
        \consists Balloon_engraver
        \override BalloonText.annotation-balloon = ##f
        \balloonLengthOff

        \override NoteName.self-alignment-X = 0
      } {
        \set printAccidentalNames = #'lily

        \addLink NoteName "note-names"

        feses'2 fes'

        \balloon NoteName -1 -1.5 0.1 ##f 0 ##f \markup
          \docengraverlink "NoteName" "Note_name"
        fis2 fisis'
      }
    >>
  }
}


\markup \vspace #2

\markuplist {
  \override #'(padding . 12)
  \override #'(baseline-skip . 1)

  \table #'(-1 -1 -1) {
    % row 1a
    \override #'(baseline-skip . 0)
    \general-align #Y #DOWN
    \column {
      \contextlink "TabVoice"
      \contextlink "TabStaff" }

        \override #'(baseline-skip . 0)
        \general-align #Y #DOWN
        \contextlink "FretBoards"

            \override #'(baseline-skip . 0)
            \general-align #Y #DOWN
            \column {
              \contextlink "ChordGrid"
              \contextlink "ChordGridScore" }

    % XXX `\table` only supports `baseline-skip`, which is meaningless
    %     if cells in different rows have different vertical alignment.
    \null \null \null

    % row 1b
    \general-align #Y #CENTER
    \TabVoiceMarkup
        \general-align #Y #CENTER
        \FretBoardMarkup
            \general-align #Y #CENTER
            \ChordGridMarkup

    \vspace #2 \null \null

    % row 2a
    \override #'(baseline-skip . 0)
    \general-align #Y #DOWN
    \column {
      \contextlink "VaticanaVoice"
      \contextlink "VaticanaStaff"
      \contextlink "VaticanaScore"
      \contextlink "VaticanaLyrics" }

        \override #'(baseline-skip . 0)
        \general-align #Y #DOWN
        \column {
          \contextlink "GregorianTranscriptionVoice"
          \contextlink "GregorianTranscriptionStaff"
          \contextlink "GregorianTranscriptionLyrics" }

            \override #'(baseline-skip . 0)
            \general-align #Y #DOWN
            \column {
              \contextlink "PetrucciVoice"
              \contextlink "PetrucciStaff" }

    \null \null \null

    % row 2b
    \general-align #Y #CENTER
    \VaticanaMarkup
        \general-align #Y #CENTER
        \GregorianTranscriptionMarkup
        \general-align #Y #CENTER
            \PetrucciMarkup

    \vspace #2 \null \null

    % row 3a
    \override #'(baseline-skip . 0)
    \general-align #Y #DOWN
    \column {
      \contextlink "KievanVoice"
      \contextlink "KievanStaff" }

        \override #'(baseline-skip . 0)
        \general-align #Y #DOWN
        \column {
          \contextlink "MensuralVoice"
          \contextlink "MensuralStaff" }

            \override #'(baseline-skip . 0)
            \general-align #Y #DOWN
            \contextlink "NoteNames"

    \null \null \null

    % row 3b
    \general-align #Y #CENTER
    \KievanMarkup
        \general-align #Y #CENTER
        \MensuralMarkup
            \general-align #Y #CENTER
            \NoteNamesMarkup
  }
}


VariousMarkup =
\markup \line {
  \hspace #2
  \score {
    <<
      \new Staff = "one" \with {
        \override VerticalAxisGroup.staff-staff-spacing.padding = 5

        \consists Ambitus_engraver
      } \relative c' {
        \addLink Score.GridLine "grid-lines"
        \addLink Score.SpanBar "mensurstriche-layout"

        \addLink Staff.AmbitusAccidental "ambitus"
        \addLink Staff.AmbitusLine "ambitus"
        \addLink Staff.AmbitusNoteHead "ambitus"
        \addLink Staff.Clef "clef"
        \addLink Staff.MeasureCounter "measure-counts"
        \addLink Staff.MeasureSpanner "analysis-brackets"

        \addLink DurationLine "graphical-notation"
        \addLink HorizontalBracket "analysis-brackets"
        \addLink HorizontalBracketText "analysis-brackets"
        \addLink NoteHead "writing-pitches"
        \addLink VoiceFollower "staff_002dchange-lines"

        \omit Staff.BarLine
        \omit Staff.TimeSignature
        \omit Score.GridLine

        \time 5/1

        \balloon Ambitus -3 1.5 ##f 0.8 -0.2 0.7 \markup \column {
          \doclink "AmbitusNoteHead"
          \doclink "AmbitusLine"
          \doclink "AmbitusAccidental"
          \docengraverlink "Ambitus" "Ambitus" }

        \balloon HorizontalBracket -1 2 ##f ##f ##f ##f \markup
          \docengraverlink "HorizontalBracket" "Horizontal_bracket"
        \balloon HorizontalBracketText -1 3.5 ##f ##f ##f ##f \markup
          \doclink "HorizontalBracketText"
        fis1-\tweak HorizontalBracketText.text "a" \startGroup e'\stopGroup

        \balloon NoteHead -1 -2 ##f ##f ##f ##f \markup \right-column {
          \with-url #(format #f notation-format "balloon-help")
            "annotation"
            \line { \doclink "BalloonTextItem" \hspace #2 }
            \line { \engraverlink "Balloon" "Balloon" \hspace #2 } }
        \balloon VoiceFollower -1 -1.5 0.8 1 -0.7 0.5 \markup \column {
          \line { \doclink "VoiceFollower" \fontsize #-3 "(Voice)" }
          \engraverlink "Note_head_line" "Note_head_line" }
        \balloon DurationLine 1 -3 -0.2 ##f 0 ##f \markup
          \docengraverlink "DurationLine" "Duration_line"
        \showStaffSwitch
        \change Staff = "two"
        a,1\- s1

        \hideStaffSwitch
        \change Staff = "one"
        a1

        \balloon SpanBar -1 1 ##f 0 ##f -165 \markup {
          \override #'(baseline-skip . 1)
          \column {
            \docengraverlink "SpanBar" "Span_bar"
            \fontsize #-3 "(StaffGroup)" } }
        <>

        \time 2/1

        \balloon MeasureCounter -1 1 ##f ##f ##f ##f \markup
          \docengraverlink "MeasureCounter" "Measure_counter"
        \startMeasureCount
        d1 e

        \undo \omit Score.GridLine
        \undo \omit Staff.BarLine

        \balloon MeasureSpanner 0 1 ##f ##f ##f 1.2 \markup
          \docengraverlink "MeasureSpanner" "Measure_spanner"
        \startMeasureSpanner
        d1 e

        \omit Score.GridLine
        d1 e
        \stopMeasureSpanner
      }

      \new Staff = "two" \with {
        \consists Measure_grouping_engraver
        \override MeasureGrouping.staff-padding = 0
        \override MeasureGrouping.padding = 1
      } \relative c' {
        \addLink Staff.Clef "clef"
        \addLink Staff.MeasureGrouping "setting-automatic-beam-behavior"

        \omit Staff.BarLine
        \omit Staff.TimeSignature

        s1*5
        s2

        \undo \omit Staff.BarLine
        s1 s
        \stopMeasureCount

        \balloon GridLine -1 -3 0.9 ##f ##f ##f \markup \right-column {
          \doclink "GridLine"
          \engraverlink "Grid_line_span" "Grid_line_span"
          \docengraverlink "GridPoint" "Grid_point" }
        s2 s s

        \balloon MeasureGrouping -1 -6 -0.2 ##f 0 ##f \markup
          \docengraverlink "MeasureGrouping" "Measure_grouping"
        \set Timing.baseMoment = \musicLength 4
        \set Timing.beatStructure = 3,3,2
        s1 s
      }
    >>

    \layout {
      indent = 3\cm

      \context {
        \Score

        \consists Balloon_engraver
        \override BalloonText.annotation-balloon = ##f
        \balloonLengthOff
        \consists Grid_line_span_engraver
        \consists Span_bar_engraver
      }

      \context {
        \Staff

        \consists Measure_counter_engraver
        \override MeasureCounter.outside-staff-priority = ##f
        \consists Measure_spanner_engraver
      }

      \context {
        \Voice

        \consists Duration_line_engraver
        \consists Grid_point_engraver
        gridInterval = \musicLength 4
        \consists Horizontal_bracket_engraver
        \override HorizontalBracket.direction = #UP
      }
    }
  }
}

\markup \vspace #2

\markup \left-align {
  \contextlink "Contexts|Various"
  "[" \doclink "MelodyItem"
      \doclink "System"
      \doclink "SpanBarStub" \fontsize #-3 "(StaffGroup)" "]" }

\markup \vspace #1

\markup \VariousMarkup


\markup \vspace #4
\markup \fill-line {
          ""
          \line { This document is part of LilyPond version
                  \concat { #(lilypond-version) . } }
          "" }

% EOF
