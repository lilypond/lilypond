%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2011--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

\version "2.17.6"

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
	 (ly:parser-error (_ "Not in an output definition")))))
