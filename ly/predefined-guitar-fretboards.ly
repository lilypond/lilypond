%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2008--2022 Carl D. Sorensen <c_sorensen@byu.edu>
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

% chord definitions require default pitchnames
\languageSaveAndChange #default-language

\include "predefined-guitar-ninth-fretboards.ly"

%%%  Add basic chordshapes

\version "2.16.0"

\addChordShape #'f #guitar-tuning "1-1-(;3-3;3-4;2-2;1-1;1-1-);"
\addChordShape #'f:m #guitar-tuning "1-1-(;3-3;3-4;1-1;1-1;1-1-);"
\addChordShape #'f:7 #guitar-tuning "1-1-(;3-3;1-1;2-2;1-1;1-1-);"
\addChordShape #'f:m7 #guitar-tuning "1-1-(;3-3;1-1;1-1;1-1;1-1-);"
\addChordShape #'bes #guitar-tuning "x;1-1-(;3-2;3-3;3-4;1-1-);"
\addChordShape #'bes:m #guitar-tuning "x;1-1-(;3-3;3-4;2-2;1-1-);"
\addChordShape #'bes:dim #guitar-tuning "x;1-1;2-2;3-4;2-3;x;"
\addChordShape #'bes:m7 #guitar-tuning "x;1-1-(;3-3;1-1;2-2;1-1-);"
\addChordShape #'bes:7 #guitar-tuning "x;1-1-(;3-2;1-1;3-3;1-1-);"
\addChordShape #'bes:maj7 #guitar-tuning "x;1-1;3-3;2-2;3-4;x;"
\addChordShape #'c:dim7 #guitar-tuning "x;x;1-1;2-3;1-2;2-4;"
\addChordShape #'c:aug #guitar-tuning "x;x;2-2;1-1-(;1-1-);4-4;"
\addChordShape #'cis #guitar-tuning "x;x;3-3;1-1-(;2-2;1-1-);"
\addChordShape #'d:dim #guitar-tuning "x;x;3-3;1-1-(;3-4;1-1-);"
\addChordShape #'ees:dim7 #guitar-tuning "x;x;1-1;2-3;1-2;2-4;"

%%%  Add predefined chords

% definitions of predefined diagrams below here

%%%%  c chords
%
\storePredefinedDiagram #default-fret-table \chordmode {c}
                        #guitar-tuning
                        "x;3-3;2-2;o;1-1;o;"
\storePredefinedDiagram #default-fret-table \chordmode {c:m}
                        #guitar-tuning
                        #(offset-fret 2 (chord-shape 'bes:m guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {c:aug}
                        #guitar-tuning
                        #(chord-shape 'c:aug guitar-tuning)
\storePredefinedDiagram #default-fret-table \chordmode {c:dim}
                        #guitar-tuning
                        #(offset-fret 2 (chord-shape 'bes:dim guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {c:dim7}
                        #guitar-tuning
                        #(chord-shape 'c:dim7 guitar-tuning)
\storePredefinedDiagram #default-fret-table \chordmode {c:7}
                        #guitar-tuning
                        "o;3-3;2-2;3-4;1-1;o;"
\storePredefinedDiagram #default-fret-table \chordmode {c:maj7}
                        #guitar-tuning
                        "x;3-3;2-2;o;o;o;"
\storePredefinedDiagram #default-fret-table \chordmode {c:m7}
                        #guitar-tuning
                        #(offset-fret 2 (chord-shape 'bes:m7 guitar-tuning))

%%%%  cis chords
%
\storePredefinedDiagram #default-fret-table \chordmode {cis}
                        #guitar-tuning
                        #(chord-shape 'cis guitar-tuning)
\storePredefinedDiagram #default-fret-table \chordmode {cis:m}
                        #guitar-tuning
                        "x;x;2-2;1-1;2-3;o;"
\storePredefinedDiagram #default-fret-table \chordmode {cis:aug}
                        #guitar-tuning
                        "x;4-4;3-3;2-1;2-2;x;"
\storePredefinedDiagram #default-fret-table \chordmode {cis:dim}
                        #guitar-tuning
                        "x;x;2-3;o;2-4;o;"
\storePredefinedDiagram #default-fret-table \chordmode {cis:dim7}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'c:dim7 guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {cis:7}
                        #guitar-tuning
                        "x;x;3-2;4-3;2-1;4-4;"
\storePredefinedDiagram #default-fret-table \chordmode {cis:maj7}
                        #guitar-tuning
                        "x;4-4;3-3;1-1-(;1-1;1-1-);"
\storePredefinedDiagram #default-fret-table \chordmode {cis:m7}
                        #guitar-tuning
                        "x;4-4;2-2;1-1;o;o;"

%%%%  des chords
%
\storePredefinedDiagram #default-fret-table \chordmode {des}
                        #guitar-tuning
                        "x;x;3-3;1-1-(;2-2;1-1-);"
\storePredefinedDiagram #default-fret-table \chordmode {des:m}
                        #guitar-tuning
                        "x;x;2-2;1-1;2-3;o;"
\storePredefinedDiagram #default-fret-table \chordmode {des:aug}
                        #guitar-tuning
                        "x;4-4;3-3;2-1;2-2;x;"
\storePredefinedDiagram #default-fret-table \chordmode {des:dim}
                        #guitar-tuning
                        "x;x;2-3;o;2-4;o;"
\storePredefinedDiagram #default-fret-table \chordmode {des:dim7}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'c:dim7 guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {des:7}
                        #guitar-tuning
                        "x;x;3-2;4-3;2-1;4-4;"
\storePredefinedDiagram #default-fret-table \chordmode {des:maj7}
                        #guitar-tuning
                        "x;4-4;3-3;1-1-(;1-1;1-1-);"
\storePredefinedDiagram #default-fret-table \chordmode {des:m7}
                        #guitar-tuning
                        "x;4-4;2-2;1-1;o;o;"

%%%%  d chords
%
\storePredefinedDiagram #default-fret-table \chordmode {d}
                        #guitar-tuning
                        "x;x;o;2-1;3-3;2-2;"
\storePredefinedDiagram #default-fret-table \chordmode {d:m}
                        #guitar-tuning
                        "x;x;o;2-2;3-3;1-1;"
\storePredefinedDiagram #default-fret-table \chordmode {d:aug}
                        #guitar-tuning
                        "x;x;o;3-2;3-3;2-1;"
\storePredefinedDiagram #default-fret-table \chordmode {d:dim}
                        #guitar-tuning
                        "x;x;o;1-1-(;3-3;1-1-);"
\storePredefinedDiagram #default-fret-table \chordmode {d:dim7}
                        #guitar-tuning
                        "x;x;o;1-1;o;1-2;"
\storePredefinedDiagram #default-fret-table \chordmode {d:7}
                        #guitar-tuning
                        "x;x;o;2-2;1-1;2-3;"
\storePredefinedDiagram #default-fret-table \chordmode {d:maj7}
                        #guitar-tuning
                        "x;x;o;2-1;2-2;2-3;"
\storePredefinedDiagram #default-fret-table \chordmode {d:m7}
                        #guitar-tuning
                        "x;x;o;2-2;1-1-(;1-1-);"

%%%%  dis chords
%
\storePredefinedDiagram #default-fret-table \chordmode {dis}
                        #guitar-tuning
                        #(offset-fret 2 (chord-shape 'cis guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {dis:m}
                        #guitar-tuning
                        "x;x;4-3;3-2;4-4;1-1;"
\storePredefinedDiagram #default-fret-table \chordmode {dis:aug}
                        #guitar-tuning
                        "3-3;2-2;1-1;o;o;3-4;"
\storePredefinedDiagram #default-fret-table \chordmode {dis:dim}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'd:dim guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {dis:dim7}
                        #guitar-tuning
                        #(chord-shape 'ees:dim7 guitar-tuning)
\storePredefinedDiagram #default-fret-table \chordmode {dis:7}
                        #guitar-tuning
                        "x;x;1-1;3-3;2-2;3-4;"
\storePredefinedDiagram #default-fret-table \chordmode {dis:maj7}
                        #guitar-tuning
                        "x;x;1-1;3-2;3-3;3-4;"
\storePredefinedDiagram #default-fret-table \chordmode {dis:m7}
                        #guitar-tuning
                        "x;x;1-1;3-2;2-3;2-4;"

%%%%  ees chords
%
\storePredefinedDiagram #default-fret-table \chordmode {ees}
                        #guitar-tuning
                        #(offset-fret 2 (chord-shape 'cis guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {ees:m}
                        #guitar-tuning
                        "x;x;4-3;3-2;4-4;1-1;"
\storePredefinedDiagram #default-fret-table \chordmode {ees:aug}
                        #guitar-tuning
                        "3-3;2-2;1-1;o;o;3-4;"
\storePredefinedDiagram #default-fret-table \chordmode {ees:dim}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'd:dim guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {ees:dim7}
                        #guitar-tuning
                        #(chord-shape 'ees:dim7 guitar-tuning)
\storePredefinedDiagram #default-fret-table \chordmode {ees:7}
                        #guitar-tuning
                        "x;x;1-1;3-3;2-2;3-4;"
\storePredefinedDiagram #default-fret-table \chordmode {ees:maj7}
                        #guitar-tuning
                        "x;x;1-1;3-2;3-3;3-4;"
\storePredefinedDiagram #default-fret-table \chordmode {ees:m7}
                        #guitar-tuning
                        "x;x;1-1;3-2;2-3;2-4;"

%%%%  e chords
%
\storePredefinedDiagram #default-fret-table \chordmode {e}
                        #guitar-tuning
                        "o;2-2;2-3;1-1;o;o;"
\storePredefinedDiagram #default-fret-table \chordmode {e:m}
                        #guitar-tuning
                        "o;2-2;2-3;o;o;o;"
\storePredefinedDiagram #default-fret-table \chordmode {e:aug}
                        #guitar-tuning
                        "o;3-3;2-2;1-1;x;x;"
\storePredefinedDiagram #default-fret-table \chordmode {e:dim}
                        #guitar-tuning
                        #(offset-fret 2 (chord-shape 'd:dim guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {e:dim7}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'ees:dim7 guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {e:7}
                        #guitar-tuning
                        "o;2-2;o;1-1;o;o;"
\storePredefinedDiagram #default-fret-table \chordmode {e:maj7}
                        #guitar-tuning
                        "o;2-3;1-1;1-2;o;x;"
\storePredefinedDiagram #default-fret-table \chordmode {e:m7}
                        #guitar-tuning
                        "o;2-2;o;o;o;o;"

%%%%  f chords
%
\storePredefinedDiagram #default-fret-table \chordmode {f}
                        #guitar-tuning
                        #(chord-shape 'f guitar-tuning)
\storePredefinedDiagram #default-fret-table \chordmode {f:m}
                        #guitar-tuning
                        #(chord-shape 'f:m guitar-tuning)
\storePredefinedDiagram #default-fret-table \chordmode {f:aug}
                        #guitar-tuning
                        "x;x;1-1;4-3;4-4;3-2;"
\storePredefinedDiagram #default-fret-table \chordmode {f:dim}
                        #guitar-tuning
                        #(offset-fret 3 (chord-shape 'd:dim guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {f:dim7}
                        #guitar-tuning
                        "x;x;o;1-1;o;1-2;"
\storePredefinedDiagram #default-fret-table \chordmode {f:7}
                        #guitar-tuning
                        #(chord-shape 'f:7 guitar-tuning)
\storePredefinedDiagram #default-fret-table \chordmode {f:maj7}
                        #guitar-tuning
                        "x;x;3-3;2-2;1-1;o;"
\storePredefinedDiagram #default-fret-table \chordmode {f:m7}
                        #guitar-tuning
                        #(chord-shape 'f:m7 guitar-tuning)

%%%%  fis chords
%
\storePredefinedDiagram #default-fret-table \chordmode {fis}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'f guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {fis:m}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'f:m guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {fis:aug}
                        #guitar-tuning
                        "2-2;1-1;o;3-4-(;3-4-);2-3;"
\storePredefinedDiagram #default-fret-table \chordmode {fis:dim}
                        #guitar-tuning
                        #(offset-fret 4 (chord-shape 'd:dim guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {fis:dim7}
                        #guitar-tuning
                        #(chord-shape 'ees:dim7 guitar-tuning)
\storePredefinedDiagram #default-fret-table \chordmode {fis:7}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'f:7 guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {fis:maj7}
                        #guitar-tuning
                        "x;x;4-4;3-3;2-2;1-1;"
\storePredefinedDiagram #default-fret-table \chordmode {fis:m7}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'f:m7 guitar-tuning))

%%%%  ges chords
%
\storePredefinedDiagram #default-fret-table \chordmode {ges}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'f guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {ges:m}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'f:m guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {ges:aug}
                        #guitar-tuning
                        "2-2;1-1;o;3-4-(;3-4-);2-3;"
\storePredefinedDiagram #default-fret-table \chordmode {ges:dim}
                        #guitar-tuning
                        #(offset-fret 4 (chord-shape 'd:dim guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {ges:dim7}
                        #guitar-tuning
                        #(chord-shape 'ees:dim7 guitar-tuning)
\storePredefinedDiagram #default-fret-table \chordmode {ges:7}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'f:7 guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {ges:maj7}
                        #guitar-tuning
                        "x;x;4-4;3-3;2-2;1-1;"
\storePredefinedDiagram #default-fret-table \chordmode {ges:m7}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'f:m7 guitar-tuning))

%%%%  g chords
%
\storePredefinedDiagram #default-fret-table \chordmode {g}
                        #guitar-tuning
                        "3-2;2-1;o;o;o;3-3;"
\storePredefinedDiagram #default-fret-table \chordmode {g:m}
                        #guitar-tuning
                        #(offset-fret 2 (chord-shape 'f:m guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {g:aug}
                        #guitar-tuning
                        "x;x;5-1;8-3;8-4;7-2;"
\storePredefinedDiagram #default-fret-table \chordmode {g:dim}
                        #guitar-tuning
                        #(offset-fret 5 (chord-shape 'd:dim guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {g:dim7}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'ees:dim7 guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {g:7}
                        #guitar-tuning
                        "3-3;2-2;o;o;o;1-1;"
\storePredefinedDiagram #default-fret-table \chordmode {g:maj7}
                        #guitar-tuning
                        "x;x;5-4;4-3;3-2;2-1;"
\storePredefinedDiagram #default-fret-table \chordmode {g:m7}
                        #guitar-tuning
                        #(offset-fret 2 (chord-shape 'f:m7 guitar-tuning))

%%%%  gis chords
%
\storePredefinedDiagram #default-fret-table \chordmode {gis}
                        #guitar-tuning
                        #(offset-fret 3 (chord-shape 'f guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {gis:m}
                        #guitar-tuning
                        #(offset-fret 3 (chord-shape 'f:m guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {gis:aug}
                        #guitar-tuning
                        "o;3-4;2-3;1-1;1-2;o;"
\storePredefinedDiagram #default-fret-table \chordmode {gis:dim}
                        #guitar-tuning
                        #(offset-fret 6 (chord-shape 'd:dim guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {gis:dim7}
                        #guitar-tuning
                        "x;x;o;1-1;o;1-2;"
\storePredefinedDiagram #default-fret-table \chordmode {gis:7}
                        #guitar-tuning
                        #(offset-fret 3 (chord-shape 'f:7 guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {gis:maj7}
                        #guitar-tuning
                        "x;x;1-1-(;1-1;1-1-);3-3;"
\storePredefinedDiagram #default-fret-table \chordmode {gis:m7}
                        #guitar-tuning
                        #(offset-fret 3 (chord-shape 'f:m7 guitar-tuning))

%%%%  aes chords
%
\storePredefinedDiagram #default-fret-table \chordmode {aes}
                        #guitar-tuning
                        #(offset-fret 3 (chord-shape 'f guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {aes:m}
                        #guitar-tuning
                        #(offset-fret 3 (chord-shape 'f:m guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {aes:aug}
                        #guitar-tuning
                        "o;3-4;2-3;1-1;1-2;o;"
\storePredefinedDiagram #default-fret-table \chordmode {aes:dim}
                        #guitar-tuning
                        #(offset-fret 6 (chord-shape 'd:dim guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {aes:dim7}
                        #guitar-tuning
                        "x;x;o;1-1;o;1-2;"
\storePredefinedDiagram #default-fret-table \chordmode {aes:7}
                        #guitar-tuning
                        #(offset-fret 3 (chord-shape 'f:7 guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {aes:maj7}
                        #guitar-tuning
                        "x;x;1-1-(;1-1;1-1-);3-3;"
\storePredefinedDiagram #default-fret-table \chordmode {aes:m7}
                        #guitar-tuning
                        #(offset-fret 3 (chord-shape 'f:m7 guitar-tuning))

%%%%  a chords
%
\storePredefinedDiagram #default-fret-table \chordmode {a}
                        #guitar-tuning
                        "x;o;2-1;2-2;2-3;o;"
\storePredefinedDiagram #default-fret-table \chordmode {a:m}
                        #guitar-tuning
                        "x;o;2-2;2-3;1-1;o;"
\storePredefinedDiagram #default-fret-table \chordmode {a:aug}
                        #guitar-tuning
                        "x;o;3-4;2-2;2-3;1-1;"
\storePredefinedDiagram #default-fret-table \chordmode {a:dim}
                        #guitar-tuning
                        "x;o;1-1;2-2;1-3;x;"
\storePredefinedDiagram #default-fret-table \chordmode {a:dim7}
                        #guitar-tuning
                        #(chord-shape 'ees:dim7 guitar-tuning)
\storePredefinedDiagram #default-fret-table \chordmode {a:7}
                        #guitar-tuning
                        "x;o;2-1;o;2-3;o;"
\storePredefinedDiagram #default-fret-table \chordmode {a:maj7}
                        #guitar-tuning
                        "x;o;2-2;1-1;2-3;o;"
\storePredefinedDiagram #default-fret-table \chordmode {a:m7}
                        #guitar-tuning
                        "x;o;2-2;o;1-1;o;"

%%%%  ais chords
%
\storePredefinedDiagram #default-fret-table \chordmode {ais}
                        #guitar-tuning
                        #(chord-shape 'bes guitar-tuning)
\storePredefinedDiagram #default-fret-table \chordmode {ais:m}
                        #guitar-tuning
                        #(chord-shape 'bes:m guitar-tuning)
\storePredefinedDiagram #default-fret-table \chordmode {ais:aug}
                        #guitar-tuning
                        "2-2;1-1;o;3-4-(;3-4-);2-3;"
\storePredefinedDiagram #default-fret-table \chordmode {ais:dim}
                        #guitar-tuning
                        #(chord-shape 'bes:dim guitar-tuning)
\storePredefinedDiagram #default-fret-table \chordmode {ais:dim7}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'ees:dim7 guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {ais:7}
                        #guitar-tuning
                        #(chord-shape 'bes:7 guitar-tuning)
\storePredefinedDiagram #default-fret-table \chordmode {ais:maj7}
                        #guitar-tuning
                        "x;1-1;3-3;2-2;3-4;x;"
\storePredefinedDiagram #default-fret-table \chordmode {ais:m7}
                        #guitar-tuning
                        #(chord-shape 'bes:m7 guitar-tuning)

%%%%  bes chords
%
\storePredefinedDiagram #default-fret-table \chordmode {bes}
                        #guitar-tuning
                        #(chord-shape 'bes guitar-tuning)
\storePredefinedDiagram #default-fret-table \chordmode {bes:m}
                        #guitar-tuning
                        #(chord-shape 'bes:m guitar-tuning)
\storePredefinedDiagram #default-fret-table \chordmode {bes:aug}
                        #guitar-tuning
                        "2-2;1-1;o;3-4-(;3-4-);2-3;"
\storePredefinedDiagram #default-fret-table \chordmode {bes:dim}
                        #guitar-tuning
                        #(chord-shape 'bes:dim guitar-tuning)
\storePredefinedDiagram #default-fret-table \chordmode {bes:dim7}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'ees:dim7 guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {bes:7}
                        #guitar-tuning
                        #(chord-shape 'bes:7 guitar-tuning)
\storePredefinedDiagram #default-fret-table \chordmode {bes:maj7}
                        #guitar-tuning
                        "x;1-1;3-3;2-2;3-4;x;"
\storePredefinedDiagram #default-fret-table \chordmode {bes:m7}
                        #guitar-tuning
                        #(chord-shape 'bes:m7 guitar-tuning)

%%%%  b chords
%
\storePredefinedDiagram #default-fret-table \chordmode {b}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'bes guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {b:m}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'bes:m guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {b:aug}
                        #guitar-tuning
                        "x;3-2;2-1;o;o;x;"
\storePredefinedDiagram #default-fret-table \chordmode {b:dim}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'bes:dim guitar-tuning))
\storePredefinedDiagram #default-fret-table \chordmode {b:dim7}
                        #guitar-tuning
                        "x;x;o;1-1;o;1-2;"
\storePredefinedDiagram #default-fret-table \chordmode {b:7}
                        #guitar-tuning
                        "x;2-2;1-1;2-3;o;2-4;"
\storePredefinedDiagram #default-fret-table \chordmode {b:maj7}
                        #guitar-tuning
                        "x;2-1;4-3;3-2;4-4;x;"
\storePredefinedDiagram #default-fret-table \chordmode {b:m7}
                        #guitar-tuning
                        #(offset-fret 1 (chord-shape 'bes:m7 guitar-tuning))

\languageRestore
