%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2011--2012 Han-Wen Nienhuys <hanwen@xs4all.nl>
%%%%                    Jan Nieuwenhuizen <janneke@gnu.org>
%%%%
%%%% LilyPond is free software: you can redistribute it and/or modify
%%%% it under the terms of the GNU General Public License as published by
%%%% the Free Software Foundation, either version 3 of the License, or
%%%% (at your option) any later version.
%%%%
%%%% LilyPond is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License for more details.
%%%%
%%%% You should have received a copy of the GNU General Public License
%%%% along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.

\version "2.15.31"

RemoveEmptyStaves = \with {
  \remove "Axis_group_engraver"
  % If RemoveEmptyStaves is called twice, two
  % Hara_kiri_engravers would be added, which leads to a
  % warning.
  % This code makes sure that no previous Hara_kiri_engraver
  % is left before adding a new one.
  \remove "Hara_kiri_engraver"
  \consists "Hara_kiri_engraver"
  \override VerticalAxisGroup #'remove-empty = ##t
  \description "Remove staves which are considered to be empty according
to the list of interfaces set by @code{keepAliveInterfaces}."
}

"layout-from" =
#(define-void-function (parser location bottom music)
   ((symbol? 'Voice) ly:music?)
   (_i "To be used in output definitions.  Take the layout instruction
events from @var{music} and do the equivalent of context modifications
duplicating their effect.

This is handy for making layout definitions by using property
definitions like @code{\\accidentalStyle} or definitions like
@code{\\tabFullNotation} that may work in multiple or unknown
contexts.

Layout instructions specified without explicit context get mapped to
the context symbol @var{bottom}, with a default of @code{'Voice}.

For example, you can tell @code{\\layout-from} to apply a contained
@example
\\override #'font-size = #2
@end example
to @samp{TabVoice} or @samp{Lyrics} context instead of the default
@samp{Voice} context, if the context where you would normally use
@var{music} is not one that would have @samp{Voice} as its
@samp{Bottom}.")
   (let loop ((m music) (mods #f))
     ;; The parser turns all sets, overrides etc into something
     ;; wrapped in ContextSpeccedMusic.  If we ever get a set,
     ;; override etc that is not wrapped in ContextSpeccedMusic, the
     ;; user has created it in Scheme himself without providing the
     ;; required wrapping.  In that case, using #f in the place of a
     ;; context modification results in a reasonably recognizable
     ;; error.
     (if (music-is-of-type? m 'layout-instruction-event)
	 (ly:add-context-mod
	  mods
	  (case (ly:music-property m 'name)
	    ((PropertySet)
	     (list 'assign
		   (ly:music-property m 'symbol)
		   (ly:music-property m 'value)))
	    ((PropertyUnset)
	     (list 'unset
		   (ly:music-property m 'symbol)))
	    ((OverrideProperty)
	     (list 'push
		   (ly:music-property m 'symbol)
		   (ly:music-property m 'grob-property-path)
		   (ly:music-property m 'grob-value)))
	    ((RevertProperty)
	     (list 'pop
		   (ly:music-property m 'symbol)
		   (ly:music-property m 'grob-property-path)))))
	 (case (ly:music-property m 'name)
	   ((SequentialMusic SimultaneousMusic)
	    (fold loop mods (ly:music-property m 'elements)))
	   ((ContextSpeccedMusic)
	    ;; It is actually rather embarrassing that we have no
	    ;; reliable way to check for the type of a context-def.
	    ;; Nor have a Scheme way to add to a context-def.
	    (let ((sym (ly:music-property m 'context-type)))
	      (if (eq? sym 'Bottom)
		  (set! sym bottom))
	      (if (module-bound? (current-module) sym)
		  (module-set!
		   (current-module)
		   sym
		   #{ \context {
				 $(module-ref (current-module) sym)
				 $(loop (ly:music-property m 'element)
					(ly:make-context-mod))
		      }
		   #})
		  (ly:warning music (_f "Cannot find context-def \\~a"
					sym)))))))
     mods))
