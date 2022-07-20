%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2011--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

\version "2.23.12"

EnableGregorianDivisiones = \with
{
  \description "Configure division commands such as @code{\\section}
to create @code{Divisio} grobs rather than @code{BarLine} grobs.  This
does not affect measure bar lines or the properties of the grobs
themselves."

  caesuraTypeTransform = #caesura-to-divisio

  %% The caesuraTypeTransform procedure will create finalis signs for
  %% \section, \fine, and \repeat volta, so we don't need visible bar
  %% lines.
  %%
  %% This is most likely to be used when forbidBreakBetweenBarLines is
  %% false, but to support the other case, we set "" to allow line
  %% breaks.
  doubleRepeatBarType = #'()
  endRepeatBarType = #'()
  fineBarType = ""
  sectionBarType = ""
  startRepeatBarType = #'()
  underlyingRepeatBarType = ""
  %% It does not seem likely that anyone will use in-staff segno, but ...
  doubleRepeatSegnoBarType = "S-||"
  endRepeatSegnoBarType = "S-||"
  fineSegnoBarType = "S-||"
  fineStartRepeatSegnoBarType = "S-||"
  segnoBarType = "S-||"
  startRepeatSegnoBarType = "S-||"
}

RemoveEmptyStaves = \with {
  \override VerticalAxisGroup.remove-empty = ##t
  \description "Remove staves which are considered to be empty according
to the list of interfaces set by @code{keepAliveInterfaces}."
}

RemoveAllEmptyStaves = \with {
  \override VerticalAxisGroup.remove-empty = ##t
  \override VerticalAxisGroup.remove-first = ##t
  \description "Remove staves which are considered to be empty according
to the list of interfaces set by @code{keepAliveInterfaces}, including those
in the first system."
}

inherit-acceptability =
#(define-void-function (to from)
   (symbol? symbol?)
   (_i "When used in an output definition, will modify all context
definitions such that context @var{to} is accepted as a child by all
contexts that also accept @var{from}.")
   (let* ((module (current-module))
	  (cmod (ly:make-context-mod)))
     (ly:add-context-mod cmod (list 'accepts to))
     (if (output-module? module)
	 (module-map
	  (lambda (_sym var)
	    (if (variable-bound? var)
		(let ((cdef (variable-ref var)))
		  (if (ly:context-def? cdef)
		      (let ((accepts (ly:context-def-lookup cdef 'accepts)))
			(if (and (memq from accepts)
				 (not (memq to accepts)))
			    (variable-set! var
					   (ly:context-def-modify cdef cmod))))))))
	  module)
	 (ly:parser-error (G_ "Not in an output definition")))))

enablePolymeter =
#(define-void-function () ()
  (_i "For use within an output definition.  Enables polymetry, moving
timing management from @code{Score} to @code{Staff}-like contexts.
This is done by removing the @code{Timing_@/translator} from
@code{Score}, and adding it to all contexts having the @code{Staff}
alias.")
  (let ((module (current-module))
        (cmod-remove (ly:make-context-mod))
        (cmod-consists (ly:make-context-mod)))
   (if (not (output-module? module))
       (ly:parser-error (G_ "Not in an output definition")))
   (ly:add-context-mod cmod-remove (list 'remove 'Timing_translator))
   (ly:add-context-mod cmod-consists (list 'consists 'Timing_translator))
   ;; FIXME: any chance to use ly:output-find-context-def here?  The
   ;; problem is that we don't have access to the context def, just
   ;; its scope (module).
   (module-map
    (lambda (_sym var)
     (if (variable-bound? var)
         (let ((cdef (variable-ref var)))
           (if (ly:context-def? cdef)
               (let* ((context-name (ly:context-def-lookup cdef 'context-name))
                      (aliases (ly:context-def-lookup cdef 'aliases))
                      (all-names (cons context-name aliases)))
                 (cond
                  ((memq 'Score all-names)
                   (variable-set! var (ly:context-def-modify cdef cmod-remove)))
                  ((memq 'Staff all-names)
                   (variable-set! var (ly:context-def-modify cdef cmod-consists)))))))))
    module)))
