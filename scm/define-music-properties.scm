;;;; This file is part of LilyPond, the GNU music typesetter.
;;;;
;;;; Copyright (C) 1998--2022  Han-Wen Nienhuys <hanwen@xs4all.nl>
;;;;                 Jan Nieuwenhuizen <janneke@gnu.org>
;;;;
;;;; LilyPond is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; LilyPond is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

(define (music-property-description symbol type? description)
  (if (not (equal? #f (object-property symbol 'music-doc)))
      (ly:error (G_ "symbol ~S redefined") symbol))
  (set-object-property! symbol 'music-type? type?)
  (set-object-property! symbol 'music-doc description)
  symbol)

(define-public all-music-properties
  (map
   (lambda (x) (apply music-property-description x))
   `(
     (absolute-octave ,integer?
                      "The absolute octave for an octave check note.")
     (alteration ,number? "Alteration for figured bass.")
     (alternative-dir ,ly:dir? "Indicates that an
@code{alternative-@/event} is the first (-1), middle (0), or last (1)
of group of alternate endings.")
     (alternative-number ,index? "The index of the current
@code{\\alternative} element, starting from one.")
     (articulation-type ,symbol? "Key for script definitions alist.")
     (articulations ,ly:music-list?
                    "Articulation events specifically for this note.")
     (associated-context ,string? "Name of the context associated with
this @code{\\lyricsto} section.")
     (associated-context-type ,symbol? "Type of the context associated with
this @code{\\lyricsto} section.")
     (augmented ,boolean? "This figure is for an augmented figured
bass (with @code{+} sign).")
     (augmented-slash ,boolean? "This figure is for an augmented
figured bass (back-slashed number).")
     (automatically-numbered ,boolean? "Should a footnote be automatically
numbered?")
     (autosplit-end ,boolean? "Duration of event was truncated by automatic
splitting in @code{Completion_heads_engraver}.")

     (bar-type ,string? "The type of bar line to create, e.g., @code{\"|\"}")
     (bass ,boolean? "Set if this note is a bass note in a chord.")
     (beat-structure ,list? "A beatStructure to be used in autobeaming.")
     (bracket-start ,boolean? "Start a bracket here.

TODO: Use SpanEvents?")
     (bracket-stop ,boolean? "Stop a bracket here.")
     (alteration-bracket ,boolean? "Put brackets around bass figure alteration.")
     (break-penalty ,number? "Penalty for line break hint.")
     (break-permission ,symbol?
                       "Whether to allow, forbid or force a line break.")

     (cautionary ,boolean? "If set, this alteration needs a
cautionary accidental.")
     (change-tag ,symbol? "Tag identifying the musical scope of a
context change.  The change applies to the nearest enclosing music
with this tag.")
     (change-to-id ,string? "Name of the context to change to.")
     (change-to-type ,symbol? "Type of the context to change to.")
     (color ,color? "The color of a highlight.")
     (class ,symbol? "The class name of an event class.")
     (context ,ly:context? "The context to which an event is sent.")
     (context-id ,string? "Name of context.")
     (context-type ,symbol?  "Type of context.")
     (create-new ,boolean? "Create a fresh context.")

     (delta-step ,number? "How much should a fall change pitch?")
     (denominator ,integer? "Denominator in a time signature.")
     (digit ,index? "Digit for fingering.")
     (diminished ,boolean? "This bass figure should be slashed.")
     (direction ,ly:dir? "Print this up or down?")
     (drum-type ,symbol? "Which percussion instrument to play this note on.")
     (duration ,ly:duration? "Duration of this note or lyric.")

     (element ,ly:music? "The single child of a Music_wrapper music object,
or the body of a repeat.")
     (elements ,ly:music-list? "A list of elements for sequential of
simultaneous music, or the alternatives of repeated music.")
     (elements-callback ,procedure? "Return a list of children, for use by
a sequential iterator.  Takes a single music parameter.")
     (error-found ,boolean?
                  "If true, a parsing error was found in this expression.")

     (figure ,integer? "A bass figure.")
     (fine-folded ,boolean? "True in a @code{fine-event} that is
issued from within a folded repeat (segno or volta).")
     (footnote-text ,markup? "Text to appear in a footnote.")
     (force-accidental ,boolean? "If set, a cautionary accidental should
always be printed on this note.")

     (grob-property ,symbol? "The symbol of the grob property to set.")
     (grob-property-path ,list? "A list of symbols, locating a nested grob
property, e.g., @code{(beamed-lengths details)}.")
     (grob-value ,scheme? "The value of the grob property to set.")


     (horizontal-direction ,ly:dir? "This is @code{RIGHT} for
@code{\\textMark}, and @code{LEFT} for @code{\\textEndMark}.")

     (id ,symbol? "The ID of an event.")
     (input-tag ,scheme? "Arbitrary marker to relate input and output.")
     (inversion ,boolean? "If set, this chord note is inverted.")
     (iterator-ctor ,procedure? "Function to construct a
@code{music-event-iterator} object for this music.")

     (label ,index? "Sequence number of a mark.  1 is first.")
     (last-pitch ,ly:pitch? "The last pitch after relativization.")
     (length ,ly:moment? "The endpoint of this music.  This property
is unhappily named in that it does not account for any initial grace
notes: the full length of the music is @code{length} minus the start
time.  A value of @code{INF-MOMENT} indicates indefinite length.")
     (length-callback ,procedure? "How to compute the duration of this music.
This property can only be defined as initializer in
@file{scm/@/define-music-types.scm}.")
     (line-break-permission ,symbol? "When the music is at top-level,
whether to allow, forbid or force a line break.")

     (metronome-count ,number-or-pair? "How many beats in a minute?")
     (midi-extra-velocity ,integer? "How much louder or softer should
this note be in MIDI output? The default is 0.")
     (midi-length ,procedure? "Function to determine how long to play
a note in MIDI. It should take a moment (the written length of the
note) and a context, and return a moment (the length to play the
note).")
     (moment ,ly:moment? "The moment at which an event happens.")
     (music-cause ,ly:music? "The music object that is the cause of
an event.")

     (name ,symbol? "Name of this music object.")
     (no-continuation ,boolean? "If set, disallow continuation lines.")
     (numerator ,integer? "Numerator of a time signature.")

     (octavation ,integer? "This pitch was octavated by how many octaves?
For chord inversions, this is negative.")
     (once ,boolean? "Apply this operation only during one time step?")
     (ops ,scheme? "The operations to apply during the creation of a
context.")
     (origin ,ly:input-location? "Where was this piece of music defined?")
     (ottava-number ,integer? "The octavation for @code{\\ottava}.")

     (page-break-permission ,symbol? "When the music is at top-level,
whether to allow, forbid or force a page break.")
     (page-label ,symbol? "The label of a page marker.")
     (page-marker ,boolean? "If true, and the music expression is found at
top-level, a page marker object is instanciated instead of a score.")
     (page-turn-permission ,symbol? "When the music is at top-level,
whether to allow, forbid or force a page turn.")
     (part-combine-status ,symbol? "Change to what kind of state?
Options are @code{solo1}, @code{solo2} and @code{unisono}.")
     (pitch ,ly:pitch? "The pitch of this note.")
     (pitch-alist ,list? "A list of pitches jointly forming the scale
of a key signature.")
     (pop-first ,boolean? "Do a revert before we try to do an override
on some grob property.")
     (procedure ,procedure? "The function to run with @code{\\applycontext}.
It must take a single argument, being the context.")
     (property-operations ,list? "Do these operations for instantiating
the context.")
     (property-path ,symbol? "The path of a property.")

     (quoted-context-id ,string? "The ID of the context to direct quotes to,
e.g., @code{cue}.")
     (quoted-context-type ,symbol? "The name of the context to
direct quotes to, e.g., @code{Voice}.")
     (quoted-events ,vector? "A vector of with @code{moment} and
@code{event-list} entries.")
     (quoted-music-clef ,string? "The clef of the voice to quote.")
     (quoted-music-name ,string? "The name of the voice to quote.")
     (quoted-transposition ,ly:pitch? "The pitch used for the quote,
overriding @code{\\transposition}.")
     (quoted-voice-direction ,ly:dir? "Should the quoted voice be up-stem
or down-stem?")

     (repeat-body-start-moment ,ly:moment? "In a @emph{D.S.} event,
the moment of the segno.")
     (repeat-count ,index? "The number of times to perform a @code{\\repeat}.")
     (return-count ,index? "The number of times to perform a @emph{D.S.}")

     (search-direction ,ly:dir? "Limits the scope of @code{\\context} searches.")
     (slash-count ,integer? "The number of slashes in a single-beat repeat.
If zero, signals a beat containing varying durations.")
     (span-direction ,ly:dir? "Does this start or stop a spanner?")
     (span-type ,symbol? "What kind of dynamic spanner should be created?
Options are @code{'text} and @code{'hairpin}.")
     (span-text ,markup? "The displayed text for dynamic text
spanners (e.g., cresc.).")
     (spanner-id ,key? "Identifier to distinguish concurrent spanners.")
     (start-callback ,procedure? "Function to compute the negative length
of starting grace notes.  This property can only be defined as initializer
in @file{scm/@/define-music-types.scm}.")
     (string-number ,integer? "The number of the string in
a @code{StringNumberEvent}.")
     (symbol ,symbol? "Grob name to perform an override or revert on.")

     (tags ,list? "List of symbols that for denoting extra details, e.g.,
@code{\\tag #'part @dots{}} could tag a piece of music as only being active
in a part.")
     (tempo-unit ,ly:duration? "The unit for the metronome count.")
     (text ,markup? "Markup expression to be printed.")
     (to-relative-callback ,procedure? "How to transform a piece of music
to relative pitches.")
     (tonic ,ly:pitch? "Base of the scale.")
     (tremolo-type ,integer? "Speed of tremolo, e.g., 16 for @code{c4:16}.")
     (trill-pitch ,ly:pitch? "Pitch of other note of the trill.")
     (tweaks ,list? "An alist of properties to override in the backend
for the grob made of this event.")
     (type ,symbol? "The type of this music object.
Determines iteration in some cases.")
     (types ,list? "The types of this music object; determines by what
engraver this music expression is processed.")

     (untransposable ,boolean? "If set, this music is not transposed.")

     (value ,scheme? "Assignment value for a translation property.")
     (void ,boolean? "If this property is @code{#t}, then the
music expression is to be discarded by the toplevel music handler.")
     (volta-depth ,index? "The depth in the repeat structure.")
     (volta-numbers ,number-list? "Volte to which this music applies.")

     (what ,symbol? "What to change for auto-change.

FIXME: Naming.")

     (X-offset ,number?
               "Offset of resulting grob; only used for balloon texts.")

     (Y-offset ,number?
               "Offset of resulting grob; only used for balloon texts.")
     )))
