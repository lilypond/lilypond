%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2009--2022 Matt Corks <mvcorks@alumni.uwaterloo.ca>
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

\version "2.18.0"

%%%% sources:
%%%%   ukulele hall of fame museum website (http://www.ukulele.org/),
%%%%   kiwi ukulele website (http://www.kiwiukulele.co.nz/)

% chord definitions require default pitchnames
\languageSaveAndChange #default-language

%%% a chords

\storePredefinedDiagram #default-fret-table \chordmode {a}
                        #ukulele-tuning
                        "2-2;1-1;o;o;"

\storePredefinedDiagram #default-fret-table \chordmode {a:m}
                        #ukulele-tuning
                        "2-2;o;o;o;"

\storePredefinedDiagram #default-fret-table \chordmode {a:7}
                        #ukulele-tuning
                        "o;1-1;o;o;"

\storePredefinedDiagram #default-fret-table \chordmode {a:m7}
                        #ukulele-tuning
                        "o;o;o;o;"

\storePredefinedDiagram #default-fret-table \chordmode {a:dim7}
                        #ukulele-tuning
                        "2-1;3-3;2-2;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {a:maj7}
                        #ukulele-tuning
                        "1-1;1-2;o;o;"

\storePredefinedDiagram #default-fret-table \chordmode {a:6}
                        #ukulele-tuning
                        "2-1;4-3;2-2;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {a:sus2}
                        #ukulele-tuning
                        "2-1;4-3;o;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {a:sus4}
                        #ukulele-tuning
                        "2-1;2-2;o;o;"

\storePredefinedDiagram #default-fret-table \chordmode {a:aug}
                        #ukulele-tuning
                        "2-3;1-1;1-2;O;"

\storePredefinedDiagram #default-fret-table \chordmode {a:9}
                        #ukulele-tuning
                        "o;1-1;o;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {a:dim}
                        #ukulele-tuning
                        "2-1;3-2;5-4;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {a:m6}
                        #ukulele-tuning
                        "2-1-(;4-3;2-1-);3-2;"

\storePredefinedDiagram #default-fret-table \chordmode {a:m6-}
                        #ukulele-tuning
                        "2-2;4-4;1-1;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {a:m7.5-}
                        #ukulele-tuning
                        "2-1;3-2-(;3-2;3-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {a:m7+}
                        #ukulele-tuning
                        "1-1;o;o;o;"

\storePredefinedDiagram #default-fret-table \chordmode {a:7sus4}
                        #ukulele-tuning
                        "o;2-2;o;o;"

\storePredefinedDiagram #default-fret-table \chordmode {a:7.9-}
                        #ukulele-tuning
                        "o;1-1;o;1-2;"

%%% ais chords

\storePredefinedDiagram #default-fret-table \chordmode {ais}
                        #ukulele-tuning
                        "3-3;2-2;1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ais:m}
                        #ukulele-tuning
                        "3-3;1-1-(;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ais:7}
                        #ukulele-tuning
                        "1-1-(;2-2;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ais:m7}
                        #ukulele-tuning
                        "1-1-(;1-1;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ais:dim7}
                        #ukulele-tuning
                        "o;1-1;o;1-2;"

\storePredefinedDiagram #default-fret-table \chordmode {ais:maj7}
                        #ukulele-tuning
                        "2-2-(;2-2-);1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ais:6}
                        #ukulele-tuning
                        "o;2-2;1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ais:sus2}
                        #ukulele-tuning
                        "3-3;o;1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ais:sus4}
                        #ukulele-tuning
                        "3-2;3-3;1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ais:aug}
                        #ukulele-tuning
                        "3-4;2-3;2-2;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {ais:9}
                        #ukulele-tuning
                        "1-1;2-3;1-2;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ais:dim}
                        #ukulele-tuning
                        "3-3;1-1;o;1-2;"

\storePredefinedDiagram #default-fret-table \chordmode {ais:m6}
                        #ukulele-tuning
                        "o;1-1-(;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ais:m6-}
                        #ukulele-tuning
                        "3-2;5-4;2-1;4-3;"

\storePredefinedDiagram #default-fret-table \chordmode {ais:m7.5-}
                        #ukulele-tuning
                        "1-1;1-2;o;1-3;"

\storePredefinedDiagram #default-fret-table \chordmode {ais:m7+}
                        #ukulele-tuning
                        "2-2;1-1-(;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ais:7sus4}
                        #ukulele-tuning
                        "1-1-(;3-3;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ais:7.9-}
                        #ukulele-tuning
                        "1-1;2-3;1-2;2-4;"

%%% bes chords

\storePredefinedDiagram #default-fret-table \chordmode {bes}
                        #ukulele-tuning
                        "3-3;2-2;1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {bes:m}
                        #ukulele-tuning
                        "3-3;1-1-(;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {bes:7}
                        #ukulele-tuning
                        "1-1-(;2-2;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {bes:m7}
                        #ukulele-tuning
                        "1-1-(;1-1;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {bes:dim7}
                        #ukulele-tuning
                        "o;1-1;o;1-2;"

\storePredefinedDiagram #default-fret-table \chordmode {bes:maj7}
                        #ukulele-tuning
                        "2-2-(;2-2-);1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {bes:6}
                        #ukulele-tuning
                        "o;2-2;1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {bes:sus2}
                        #ukulele-tuning
                        "3-3;o;1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {bes:sus4}
                        #ukulele-tuning
                        "3-2;3-3;1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {bes:aug}
                        #ukulele-tuning
                        "3-4;2-3;2-2;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {bes:9}
                        #ukulele-tuning
                        "1-1;2-3;1-2;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {bes:dim}
                        #ukulele-tuning
                        "3-3;1-1;o;1-2;"

\storePredefinedDiagram #default-fret-table \chordmode {bes:m6}
                        #ukulele-tuning
                        "o;1-1-(;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {bes:m6-}
                        #ukulele-tuning
                        "3-2;5-4;2-1;4-3;"

\storePredefinedDiagram #default-fret-table \chordmode {bes:m7.5-}
                        #ukulele-tuning
                        "1-1;1-2;o;1-3;"

\storePredefinedDiagram #default-fret-table \chordmode {bes:m7+}
                        #ukulele-tuning
                        "2-2;1-1-(;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {bes:7sus4}
                        #ukulele-tuning
                        "1-1-(;3-3;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {bes:7.9-}
                        #ukulele-tuning
                        "1-1;2-3;1-2;2-4;"

%%% b chords

\storePredefinedDiagram #default-fret-table \chordmode {b}
                        #ukulele-tuning
                        "4-3;3-2;2-1-(;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {b:m}
                        #ukulele-tuning
                        "4-3;2-1-(;2-1;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {b:7}
                        #ukulele-tuning
                        "2-1-(;3-2;2-1;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {b:m7}
                        #ukulele-tuning
                        "2-1-(;2-1;2-1;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {b:dim7}
                        #ukulele-tuning
                        "1-1;2-3;1-2;2-4;"

\storePredefinedDiagram #default-fret-table \chordmode {b:maj7}
                        #ukulele-tuning
                        "3-2-(;3-2-);2-1-(;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {b:6}
                        #ukulele-tuning
                        "1-1;3-4;2-2;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {b:sus2}
                        #ukulele-tuning
                        "5-4;1-1;2-3;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {b:sus4}
                        #ukulele-tuning
                        "4-2;4-3;2-1-(;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {b:aug}
                        #ukulele-tuning
                        "o;3-2;3-3;2-1;"

\storePredefinedDiagram #default-fret-table \chordmode {b:9}
                        #ukulele-tuning
                        "2-1;3-3;2-2;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {b:dim}
                        #ukulele-tuning
                        "4-4;2-1;1-2;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {b:m6}
                        #ukulele-tuning
                        "1-1;2-2-(;2-2;2-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {b:m6-}
                        #ukulele-tuning
                        "o;2-1;2-2;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {b:m7.5-}
                        #ukulele-tuning
                        "2-2;2-3;1-1;2-4;"

\storePredefinedDiagram #default-fret-table \chordmode {b:m7+}
                        #ukulele-tuning
                        "3-2;2-1-(;2-1;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {b:7sus4}
                        #ukulele-tuning
                        "2-1-(;4-3;2-1;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {b:7.9-}
                        #ukulele-tuning
                        "2-1;3-3;2-2;3-4;"

%%% c chords

\storePredefinedDiagram #default-fret-table \chordmode {c}
                        #ukulele-tuning
                        "o;o;o;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {c:m}
                        #ukulele-tuning
                        "o;3-1;3-2;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {c:7}
                        #ukulele-tuning
                        "o;o;o;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {c:m7}
                        #ukulele-tuning
                        "3-1-(;3-1;3-1;3-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {c:dim7}
                        #ukulele-tuning
                        "2-1;3-3;2-2;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {c:maj7}
                        #ukulele-tuning
                        "o;o;o;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {c:6}
                        #ukulele-tuning
                        "o;o;o;o;"

\storePredefinedDiagram #default-fret-table \chordmode {c:sus2}
                        #ukulele-tuning
                        "o;2-1;3-3-(;3-3-);"

\storePredefinedDiagram #default-fret-table \chordmode {c:sus4}
                        #ukulele-tuning
                        "o;o;1-1;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {c:aug}
                        #ukulele-tuning
                        "1-1;o;o;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {c:9}
                        #ukulele-tuning
                        "o;2-2;o;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {c:dim}
                        #ukulele-tuning
                        "5-4;3-1;2-2;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {c:m6}
                        #ukulele-tuning
                        "2-1;3-2-(;3-2;3-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {c:m6-}
                        #ukulele-tuning
                        "1-1;3-2;3-3;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {c:m7.5-}
                        #ukulele-tuning
                        "3-2;3-3;2-1;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {c:m7+}
                        #ukulele-tuning
                        "4-2;3-1-(;3-1;3-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {c:7sus4}
                        #ukulele-tuning
                        "o;o;1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {c:7.9-}
                        #ukulele-tuning
                        "o;1-1;o;1-2;"

%%% cis chords

\storePredefinedDiagram #default-fret-table \chordmode {cis}
                        #ukulele-tuning
                        "1-1-(;1-1;1-1-);4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:m}
                        #ukulele-tuning
                        "1-1;1-2;o;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:7}
                        #ukulele-tuning
                        "1-1-(;1-1;1-1-);2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:m7}
                        #ukulele-tuning
                        "1-1;1-2;o;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:dim7}
                        #ukulele-tuning
                        "o;1-1;o;1-2;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:maj7}
                        #ukulele-tuning
                        "1-1-(;1-1;1-1-);3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:6}
                        #ukulele-tuning
                        "1-1-(;1-1;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {cis:sus2}
                        #ukulele-tuning
                        "1-1;3-3;4-4-(;4-4-);"

\storePredefinedDiagram #default-fret-table \chordmode {cis:sus4}
                        #ukulele-tuning
                        "1-1-(;1-1-);2-2;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:aug}
                        #ukulele-tuning
                        "2-3;1-1;1-2;o;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:9}
                        #ukulele-tuning
                        "1-1-(;3-3;1-1-);2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:dim}
                        #ukulele-tuning
                        "o;1-1;o;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:m6}
                        #ukulele-tuning
                        "1-1;1-2;o;1-3;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:m6-}
                        #ukulele-tuning
                        "1-1;1-2;o;o;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:m7.5-}
                        #ukulele-tuning
                        "o;1-1;o;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:m7+}
                        #ukulele-tuning
                        "1-1;1-2;o;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:7sus4}
                        #ukulele-tuning
                        "1-1-(;1-1-);2-2-(;2-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {cis:7.9-}
                        #ukulele-tuning
                        "1-1;2-3;1-2;2-4;"

%%% des chords

\storePredefinedDiagram #default-fret-table \chordmode {des}
                        #ukulele-tuning
                        "1-1-(;1-1;1-1-);4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {des:m}
                        #ukulele-tuning
                        "1-1;1-2;o;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {des:7}
                        #ukulele-tuning
                        "1-1-(;1-1;1-1-);2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {des:m7}
                        #ukulele-tuning
                        "1-1;1-2;o;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {des:dim7}
                        #ukulele-tuning
                        "o;1-1;o;1-2;"

\storePredefinedDiagram #default-fret-table \chordmode {des:maj7}
                        #ukulele-tuning
                        "1-1-(;1-1;1-1-);3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {des:6}
                        #ukulele-tuning
                        "1-1-(;1-1;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {des:sus2}
                        #ukulele-tuning
                        "1-1;3-3;4-4-(;4-4-);"

\storePredefinedDiagram #default-fret-table \chordmode {des:sus4}
                        #ukulele-tuning
                        "1-1-(;1-1-);2-2;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {des:aug}
                        #ukulele-tuning
                        "2-3;1-1;1-2;o;"

\storePredefinedDiagram #default-fret-table \chordmode {des:9}
                        #ukulele-tuning
                        "1-1-(;3-3;1-1-);2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {des:dim}
                        #ukulele-tuning
                        "o;1-1;o;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {des:m6}
                        #ukulele-tuning
                        "1-1;1-2;o;1-3;"

\storePredefinedDiagram #default-fret-table \chordmode {des:m6-}
                        #ukulele-tuning
                        "1-1;1-2;o;o;"

\storePredefinedDiagram #default-fret-table \chordmode {des:m7.5-}
                        #ukulele-tuning
                        "o;1-1;o;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {des:m7+}
                        #ukulele-tuning
                        "1-1;1-2;o;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {des:7sus4}
                        #ukulele-tuning
                        "1-1-(;1-1-);2-2-(;2-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {des:7.9-}
                        #ukulele-tuning
                        "1-1;2-3;1-2;2-4;"

%%% d chords

\storePredefinedDiagram #default-fret-table \chordmode {d}
                        #ukulele-tuning
                        "2-1;2-2;2-3;o;"

\storePredefinedDiagram #default-fret-table \chordmode {d:m}
                        #ukulele-tuning
                        "2-2;2-3;1-1;o;"

\storePredefinedDiagram #default-fret-table \chordmode {d:7}
                        #ukulele-tuning
                        "2-1-(;2-1;2-1-);3-2;"

\storePredefinedDiagram #default-fret-table \chordmode {d:m7}
                        #ukulele-tuning
                        "2-2;2-3;1-1;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {d:dim7}
                        #ukulele-tuning
                        "1-1;2-3;1-2;2-4;"

\storePredefinedDiagram #default-fret-table \chordmode {d:maj7}
                        #ukulele-tuning
                        "2-1-(;2-1;2-1-);4-3;"

\storePredefinedDiagram #default-fret-table \chordmode {d:6}
                        #ukulele-tuning
                        "2-1-(;2-1;2-1;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {d:sus2}
                        #ukulele-tuning
                        "2-1;2-2;o;o;"

\storePredefinedDiagram #default-fret-table \chordmode {d:sus4}
                        #ukulele-tuning
                        "o;2-1;3-2;o;"

\storePredefinedDiagram #default-fret-table \chordmode {d:aug}
                        #ukulele-tuning
                        "3-4;2-3;2-2;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {d:9}
                        #ukulele-tuning
                        "2-1-(;4-3;2-1-);3-2;"

\storePredefinedDiagram #default-fret-table \chordmode {d:dim}
                        #ukulele-tuning
                        "1-1-(;2-2;1-1-);5-4;"

\storePredefinedDiagram #default-fret-table \chordmode {d:m6}
                        #ukulele-tuning
                        "2-2;2-3;1-1;2-4;"

\storePredefinedDiagram #default-fret-table \chordmode {d:m6-}
                        #ukulele-tuning
                        "2-2-(;2-2-);1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {d:m7.5-}
                        #ukulele-tuning
                        "1-1;2-3;1-2;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {d:m7+}
                        #ukulele-tuning
                        "2-2;2-3;1-1;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {d:7sus4}
                        #ukulele-tuning
                        "2-1-(;2-1-);3-2-(;3-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {d:7.9-}
                        #ukulele-tuning
                        "2-1;3-3;2-2;3-4;"

%%%% dis chords

\storePredefinedDiagram #default-fret-table \chordmode {dis}
                        #ukulele-tuning
                        "o;3-2;3-3;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:m}
                        #ukulele-tuning
                        "3-3;3-4;2-2;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:7}
                        #ukulele-tuning
                        "3-1-(;3-1;3-1-);4-2;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:m7}
                        #ukulele-tuning
                        "3-2;3-3;2-1;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:dim7}
                        #ukulele-tuning
                        "2-1;3-3;2-2;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:maj7}
                        #ukulele-tuning
                        "3-1-(;3-1;3-1-);5-2;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:6}
                        #ukulele-tuning
                        "3-1-(;3-1;3-1;3-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {dis:sus2}
                        #ukulele-tuning
                        "3-2;3-3;1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {dis:sus4}
                        #ukulele-tuning
                        "1-1-(;3-3;4-4;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {dis:aug}
                        #ukulele-tuning
                        "o;3-2;3-3;2-1;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:9}
                        #ukulele-tuning
                        "o;1-1-(;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {dis:dim}
                        #ukulele-tuning
                        "2-1;3-3;2-3;o;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:m6}
                        #ukulele-tuning
                        "3-2;3-3;2-1;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:m6-}
                        #ukulele-tuning
                        "3-2-(;3-2-);2-1-(;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {dis:m7.5-}
                        #ukulele-tuning
                        "2-1;3-3;2-2;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:m7+}
                        #ukulele-tuning
                        "3-2;3-3;2-1;5-4;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:7sus4}
                        #ukulele-tuning
                        "3-1-(;3-1-);4-2-(;4-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {dis:7.9-}
                        #ukulele-tuning
                        "o;1-1;o;1-2;"

%%%% ees chords

\storePredefinedDiagram #default-fret-table \chordmode {ees}
                        #ukulele-tuning
                        "o;3-2;3-3;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:m}
                        #ukulele-tuning
                        "3-3;3-4;2-2;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:7}
                        #ukulele-tuning
                        "3-1-(;3-1;3-1-);4-2;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:m7}
                        #ukulele-tuning
                        "3-2;3-3;2-1;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:dim7}
                        #ukulele-tuning
                        "2-1;3-3;2-2;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:maj7}
                        #ukulele-tuning
                        "3-1-(;3-1;3-1-);5-2;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:6}
                        #ukulele-tuning
                        "3-1-(;3-1;3-1;3-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ees:sus2}
                        #ukulele-tuning
                        "3-2;3-3;1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ees:sus4}
                        #ukulele-tuning
                        "1-1-(;3-3;4-4;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ees:aug}
                        #ukulele-tuning
                        "o;3-2;3-3;2-1;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:9}
                        #ukulele-tuning
                        "o;1-1-(;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ees:dim}
                        #ukulele-tuning
                        "2-1;3-3;2-3;o;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:m6}
                        #ukulele-tuning
                        "3-2;3-3;2-1;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:m6-}
                        #ukulele-tuning
                        "3-2-(;3-2-);2-1-(;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ees:m7.5-}
                        #ukulele-tuning
                        "2-1;3-3;2-2;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:m7+}
                        #ukulele-tuning
                        "3-2;3-3;2-1;5-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:7sus4}
                        #ukulele-tuning
                        "3-1-(;3-1-);4-2-(;4-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {ees:7.9-}
                        #ukulele-tuning
                        "o;1-1;o;1-2;"

%%%% e chords

\storePredefinedDiagram #default-fret-table \chordmode {e}
                        #ukulele-tuning
                        "1-1;4-4;o;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {e:m}
                        #ukulele-tuning
                        "o;4-3;o;2-1;"

\storePredefinedDiagram #default-fret-table \chordmode {e:7}
                        #ukulele-tuning
                        "1-1;2-2;o;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {e:m7}
                        #ukulele-tuning
                        "o;2-1;o;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {e:dim7}
                        #ukulele-tuning
                        "o;1-1;o;1-2;"

\storePredefinedDiagram #default-fret-table \chordmode {e:maj7}
                        #ukulele-tuning
                        "1-1;3-3;o;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {e:6}
                        #ukulele-tuning
                        "1-1;1-2;o;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {e:sus2}
                        #ukulele-tuning
                        "4-2;4-3;2-1-(;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {e:sus4}
                        #ukulele-tuning
                        "2-2;4-4;o;2-1;"

\storePredefinedDiagram #default-fret-table \chordmode {e:aug}
                        #ukulele-tuning
                        "1-1;o;o;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {e:9}
                        #ukulele-tuning
                        "1-1;2-2;2-3;2-4;"

\storePredefinedDiagram #default-fret-table \chordmode {e:dim}
                        #ukulele-tuning
                        "o;4-4;o;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {e:m6}
                        #ukulele-tuning
                        "o;1-1;o;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {e:m6-}
                        #ukulele-tuning
                        "o;o;o;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {e:m7.5-}
                        #ukulele-tuning
                        "o;2-2;o;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {e:m7+}
                        #ukulele-tuning
                        "o;3-2;o;2-1;"

\storePredefinedDiagram #default-fret-table \chordmode {e:7sus4}
                        #ukulele-tuning
                        "2-1;2-2;o;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {e:7.9-}
                        #ukulele-tuning
                        "1-1;2-3;1-2;2-4;"

%%%% f chords

\storePredefinedDiagram #default-fret-table \chordmode {f}
                        #ukulele-tuning
                        "2-2;o;1-1;o;"

\storePredefinedDiagram #default-fret-table \chordmode {f:m}
                        #ukulele-tuning
                        "1-1;o;1-2;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {f:7}
                        #ukulele-tuning
                        "2-2;3-3;1-1;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {f:m7}
                        #ukulele-tuning
                        "1-1;3-3;1-2;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {f:dim7}
                        #ukulele-tuning
                        "1-1;2-3;1-2;2-4;"

\storePredefinedDiagram #default-fret-table \chordmode {f:maj7}
                        #ukulele-tuning
                        "2-2;4-4;1-1;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {f:6}
                        #ukulele-tuning
                        "2-2;2-3;1-1;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {f:sus2}
                        #ukulele-tuning
                        "o;o;1-1;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {f:sus4}
                        #ukulele-tuning
                        "3-3;o;1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {f:aug}
                        #ukulele-tuning
                        "2-3;1-1;1-2;o;"

\storePredefinedDiagram #default-fret-table \chordmode {f:9}
                        #ukulele-tuning
                        "2-1;3-2;3-3;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {f:dim}
                        #ukulele-tuning
                        "1-1-(;5-4;1-1-);2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {f:m6}
                        #ukulele-tuning
                        "1-1;2-3;1-2;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {f:m6-}
                        #ukulele-tuning
                        "1-1-(;1-1;1-1-);3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {f:m7.5-}
                        #ukulele-tuning
                        "1-1-(;3-3;1-1-);2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {f:m7+}
                        #ukulele-tuning
                        "1-1-(;4-4;1-1-);3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {f:7sus4}
                        #ukulele-tuning
                        "3-2;3-3;1-1;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {f:7.9-}
                        #ukulele-tuning
                        "2-1;3-3;2-2;3-4;"

%%%% fis chords

\storePredefinedDiagram #default-fret-table \chordmode {fis}
                        #ukulele-tuning
                        "3-3;1-1-(;2-2;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {fis:m}
                        #ukulele-tuning
                        "2-2;1-1;2-3;o;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:7}
                        #ukulele-tuning
                        "3-2;4-3;2-1;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:m7}
                        #ukulele-tuning
                        "2-1;4-3;2-2;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:dim7}
                        #ukulele-tuning
                        "2-1;3-3;2-2;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:maj7}
                        #ukulele-tuning
                        "3-2;5-4;2-1;4-3;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:6}
                        #ukulele-tuning
                        "3-2;3-3;2-1;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:sus2}
                        #ukulele-tuning
                        "1-1-(;1-1-);2-2;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:sus4}
                        #ukulele-tuning
                        "4-4;1-1;2-2;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:aug}
                        #ukulele-tuning
                        "3-4;2-3;2-2;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:9}
                        #ukulele-tuning
                        "1-1;1-2;o;1-3;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:dim}
                        #ukulele-tuning
                        "2-1;o;2-2;o;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:m6}
                        #ukulele-tuning
                        "2-1;3-3;2-2;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:m6-}
                        #ukulele-tuning
                        "2-1-(;2-1;2-1-);4-3;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:m7.5-}
                        #ukulele-tuning
                        "2-1-(;4-3;2-1-);3-2;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:m7+}
                        #ukulele-tuning
                        "2-1-(;5-4;2-1-);4-3;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:7sus4}
                        #ukulele-tuning
                        "4-2;4-3;2-1;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:7.9-}
                        #ukulele-tuning
                        "o;1-1;o;1-2;"

%%%% ges chords

\storePredefinedDiagram #default-fret-table \chordmode {ges}
                        #ukulele-tuning
                        "3-3;1-1-(;2-2;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ges:m}
                        #ukulele-tuning
                        "2-2;1-1;2-3;o;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:7}
                        #ukulele-tuning
                        "3-2;4-3;2-1;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:m7}
                        #ukulele-tuning
                        "2-1;4-3;2-2;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:dim7}
                        #ukulele-tuning
                        "2-1;3-3;2-2;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:maj7}
                        #ukulele-tuning
                        "3-2;5-4;2-1;4-3;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:6}
                        #ukulele-tuning
                        "3-2;3-3;2-1;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:sus2}
                        #ukulele-tuning
                        "1-1-(;1-1-);2-2;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:sus4}
                        #ukulele-tuning
                        "4-4;1-1;2-2;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:aug}
                        #ukulele-tuning
                        "3-4;2-3;2-2;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:9}
                        #ukulele-tuning
                        "1-1;1-2;o;1-3;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:dim}
                        #ukulele-tuning
                        "2-1;o;2-2;o;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:m6}
                        #ukulele-tuning
                        "2-1;3-3;2-2;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:m6-}
                        #ukulele-tuning
                        "2-1-(;2-1;2-1-);4-3;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:m7.5-}
                        #ukulele-tuning
                        "2-1-(;4-3;2-1-);3-2;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:m7+}
                        #ukulele-tuning
                        "2-1-(;5-4;2-1-);4-3;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:7sus4}
                        #ukulele-tuning
                        "4-2;4-3;2-1;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:7.9-}
                        #ukulele-tuning
                        "o;1-1;o;1-2;"

%%%% g chords

\storePredefinedDiagram #default-fret-table \chordmode {g}
                        #ukulele-tuning
                        "o;2-1;3-3;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {g:m}
                        #ukulele-tuning
                        "o;2-2;3-3;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {g:7}
                        #ukulele-tuning
                        "o;2-2;1-1;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {g:m7}
                        #ukulele-tuning
                        "o;2-2;1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {g:dim7}
                        #ukulele-tuning
                        "o;1-1;o;1-2;"

\storePredefinedDiagram #default-fret-table \chordmode {g:maj7}
                        #ukulele-tuning
                        "o;2-1;2-2;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {g:6}
                        #ukulele-tuning
                        "o;2-1;o;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {g:sus2}
                        #ukulele-tuning
                        "o;2-1;3-2;o;"

\storePredefinedDiagram #default-fret-table \chordmode {g:sus4}
                        #ukulele-tuning
                        "o;2-1;3-2;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {g:aug}
                        #ukulele-tuning
                        "o;3-2;3-3;2-1;"

\storePredefinedDiagram #default-fret-table \chordmode {g:9}
                        #ukulele-tuning
                        "2-2;2-3;1-1;2-4;"

\storePredefinedDiagram #default-fret-table \chordmode {g:dim}
                        #ukulele-tuning
                        "o;1-1;3-4;1-2;"

\storePredefinedDiagram #default-fret-table \chordmode {g:m6}
                        #ukulele-tuning
                        "o;2-2;o;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {g:m6-}
                        #ukulele-tuning
                        "3-1-(;3-1;3-1-);5-2;"

\storePredefinedDiagram #default-fret-table \chordmode {g:m7.5-}
                        #ukulele-tuning
                        "o;1-1-(;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {g:m7+}
                        #ukulele-tuning
                        "o;2-2;2-3;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {g:7sus4}
                        #ukulele-tuning
                        "o;2-2;1-1;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {g:7.9-}
                        #ukulele-tuning
                        "1-1;2-3;1-2;2-4;"

%%%% gis chords

\storePredefinedDiagram #default-fret-table \chordmode {gis}
                        #ukulele-tuning
                        "5-4;3-1;4-3;3-2;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:m}
                        #ukulele-tuning
                        "1-1;3-3;4-4;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:7}
                        #ukulele-tuning
                        "1-1;3-3;2-2;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:m7}
                        #ukulele-tuning
                        "1-1;3-4;2-2;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:dim7}
                        #ukulele-tuning
                        "1-1;2-3;1-2;2-4;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:maj7}
                        #ukulele-tuning
                        "1-1;3-2;3-3;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:6}
                        #ukulele-tuning
                        "1-1;3-3;1-2;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:sus2}
                        #ukulele-tuning
                        "1-1-(;3-3;4-4;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {gis:sus4}
                        #ukulele-tuning
                        "1-1;3-3;4-4-(;4-4-);"

\storePredefinedDiagram #default-fret-table \chordmode {gis:aug}
                        #ukulele-tuning
                        "1-1;o;o;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:9}
                        #ukulele-tuning
                        "3-2;3-3;2-1;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:dim}
                        #ukulele-tuning
                        "1-1;2-2;4-4;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:m6}
                        #ukulele-tuning
                        "1-1-(;3-3;1-1-);2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:m6-}
                        #ukulele-tuning
                        "1-1;3-3;o;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:m7.5-}
                        #ukulele-tuning
                        "1-1;2-2-(;2-2;2-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {gis:m7+}
                        #ukulele-tuning
                        "1-1;3-3;3-4;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:7sus4}
                        #ukulele-tuning
                        "1-1;3-3;2-2;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:7.9-}
                        #ukulele-tuning
                        "2-1;3-3;2-2;3-4;"

%%%% aes chords

\storePredefinedDiagram #default-fret-table \chordmode {aes}
                        #ukulele-tuning
                        "5-4;3-1;4-3;3-2;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:m}
                        #ukulele-tuning
                        "1-1;3-3;4-4;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:7}
                        #ukulele-tuning
                        "1-1;3-3;2-2;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:m7}
                        #ukulele-tuning
                        "1-1;3-4;2-2;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:dim7}
                        #ukulele-tuning
                        "1-1;2-3;1-2;2-4;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:maj7}
                        #ukulele-tuning
                        "1-1;3-2;3-3;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:6}
                        #ukulele-tuning
                        "1-1;3-3;1-2;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:sus2}
                        #ukulele-tuning
                        "1-1-(;3-3;4-4;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {aes:sus4}
                        #ukulele-tuning
                        "1-1;3-3;4-4-(;4-4-);"

\storePredefinedDiagram #default-fret-table \chordmode {aes:aug}
                        #ukulele-tuning
                        "1-1;o;o;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:9}
                        #ukulele-tuning
                        "3-2;3-3;2-1;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:dim}
                        #ukulele-tuning
                        "1-1;2-2;4-4;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:m6}
                        #ukulele-tuning
                        "1-1-(;3-3;1-1-);2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:m6-}
                        #ukulele-tuning
                        "1-1;3-3;o;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:m7.5-}
                        #ukulele-tuning
                        "1-1;2-2-(;2-2;2-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {aes:m7+}
                        #ukulele-tuning
                        "1-1;3-3;3-4;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:7sus4}
                        #ukulele-tuning
                        "1-1;3-3;2-2;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:7.9-}
                        #ukulele-tuning
                        "2-1;3-3;2-2;3-4;"

\languageRestore
