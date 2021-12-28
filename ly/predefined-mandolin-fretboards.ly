%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2011--2022 Marc Hohl <marc@hohlart.de>
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

\version "2.16.0"

%%%% sources:
%%%%   mandolincafe website (http://www.mandolincafe.com/),
%%%%   sheetmusicdigital website (http://www.sheetmusicdigital.com/)

% chord definitions require default pitchnames
\languageSaveAndChange #default-language

%%% a chords

\storePredefinedDiagram #default-fret-table \chordmode {a}
                        #mandolin-tuning
                        "2-1-(;2-1-);4-3;o;"

\storePredefinedDiagram #default-fret-table \chordmode {a:m}
                        #mandolin-tuning
                        "2-1-(;2-1-);3-2;o;"

\storePredefinedDiagram #default-fret-table \chordmode {a:7}
                        #mandolin-tuning
                        "2-1-(;2-1-);4-3;3-2;"

\storePredefinedDiagram #default-fret-table \chordmode {a:m7}
                        #mandolin-tuning
                        "2-1-(;2-1-);3-2-(;3-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {a:m7.5-}
                        #mandolin-tuning
                        "2-2;1-1;3-3;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {a:dim7}
                        #mandolin-tuning
                        "2-2;1-1;3-4;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {a:maj7}
                        #mandolin-tuning
                        "2-1-(;2-1-);4-3-(;4-3-);"

\storePredefinedDiagram #default-fret-table \chordmode {a:6}
                        #mandolin-tuning
                        "2-1-(;2-1;4-3;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {a:sus2}
                        #mandolin-tuning
                        "2-1-(;2-1;2-1-);o;"

\storePredefinedDiagram #default-fret-table \chordmode {a:sus4}
                        #mandolin-tuning
                        "2-1;o;o;o;"

\storePredefinedDiagram #default-fret-table \chordmode {a:aug}
                        #mandolin-tuning
                        "2-2;3-3;4-4;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {a:9}
                        #mandolin-tuning
                        "2-1;5-3;4-2;7-4;"

%%% ais chords

\storePredefinedDiagram #default-fret-table \chordmode {ais}
                        #mandolin-tuning
                        "3-3;o;1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ais:m}
                        #mandolin-tuning
                        "3-1-(;3-1-);4-2;6-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ais:7}
                        #mandolin-tuning
                        "3-1-(;3-1-);5-3;4-2;"

\storePredefinedDiagram #default-fret-table \chordmode {ais:m7}
                        #mandolin-tuning
                        "3-1-(;3-1-);4-2-(;4-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {ais:m7.5-}
                        #mandolin-tuning
                        "3-2;2-1;4-3;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ais:dim7}
                        #mandolin-tuning
                        "3-2;2-1;4-4;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {ais:maj7}
                        #mandolin-tuning
                        "3-3;o;o;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {ais:6}
                        #mandolin-tuning
                        "o;o;1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ais:sus2}
                        #mandolin-tuning
                        "3-1-(;3-1;3-1-);6-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ais:sus4}
                        #mandolin-tuning
                        "3-3;1-1-(;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ais:aug}
                        #mandolin-tuning
                        "3-3;o;1-1;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {ais:9}
                        #mandolin-tuning
                        "3-1;o;3-2;4-3;"

%%% bes chords

\storePredefinedDiagram #default-fret-table \chordmode {bes}
                        #mandolin-tuning
                        "3-3;o;1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {bes:m}
                        #mandolin-tuning
                        "3-1-(;3-1-);4-2;6-4;"

\storePredefinedDiagram #default-fret-table \chordmode {bes:7}
                        #mandolin-tuning
                        "3-1-(;3-1-);5-3;4-2;"

\storePredefinedDiagram #default-fret-table \chordmode {bes:m7}
                        #mandolin-tuning
                        "3-1-(;3-1-);4-2-(;4-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {bes:m7.5-}
                        #mandolin-tuning
                        "3-2;2-1;4-3;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {bes:dim7}
                        #mandolin-tuning
                        "3-2;2-1;4-4;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {bes:maj7}
                        #mandolin-tuning
                        "3-3;o;o;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {bes:6}
                        #mandolin-tuning
                        "o;o;1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {bes:sus2}
                        #mandolin-tuning
                        "3-1-(;3-1;3-1-);6-4;"

\storePredefinedDiagram #default-fret-table \chordmode {bes:sus4}
                        #mandolin-tuning
                        "3-3;1-1-(;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {bes:aug}
                        #mandolin-tuning
                        "3-3;o;1-1;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {bes:9}
                        #mandolin-tuning
                        "3-1;o;3-2;4-3;"

%%% b chords

\storePredefinedDiagram #default-fret-table \chordmode {b}
                        #mandolin-tuning
                        "4-1-(;4-1-);6-3;7-4;"

\storePredefinedDiagram #default-fret-table \chordmode {b:m}
                        #mandolin-tuning
                        "4-3;o;2-1-(;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {b:7}
                        #mandolin-tuning
                        "4-1-(;4-1-);6-3;5-2;"

\storePredefinedDiagram #default-fret-table \chordmode {b:m7}
                        #mandolin-tuning
                        "4-3;o;o;2-1;"

\storePredefinedDiagram #default-fret-table \chordmode {b:m7.5-}
                        #mandolin-tuning
                        "2-2;o;2-3;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {b:dim7}
                        #mandolin-tuning
                        "1-1;o;2-3;1-2;"

\storePredefinedDiagram #default-fret-table \chordmode {b:maj7}
                        #mandolin-tuning
                        "4-4;1-1-(;1-1-);2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {b:6}
                        #mandolin-tuning
                        "1-1-(;1-1-);2-2-(;2-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {b:sus2}
                        #mandolin-tuning
                        "4-1-(;4-1;4-1-);7-4;"

\storePredefinedDiagram #default-fret-table \chordmode {b:sus4}
                        #mandolin-tuning
                        "4-3;2-1-(;2-1;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {b:aug}
                        #mandolin-tuning
                        "4-4;1-1;2-2;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {b:9}
                        #mandolin-tuning
                        "4-2;1-1;4-3;5-4;"

%%% c chords

\storePredefinedDiagram #default-fret-table \chordmode {c}
                        #mandolin-tuning
                        "5-4;2-1;3-2;o;"

\storePredefinedDiagram #default-fret-table \chordmode {c:m}
                        #mandolin-tuning
                        "5-1-(;5-1-);6-2;8-4;"

\storePredefinedDiagram #default-fret-table \chordmode {c:7}
                        #mandolin-tuning
                        "5-4;2-2;1-1;o;"

\storePredefinedDiagram #default-fret-table \chordmode {c:m7}
                        #mandolin-tuning
                        "5-1-(;5-1-);6-2-(;6-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {c:m7.5-}
                        #mandolin-tuning
                        "3-3;1-1;3-4;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {c:dim7}
                        #mandolin-tuning
                        "2-2;1-1;3-4;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {c:maj7}
                        #mandolin-tuning
                        "5-4;2-1-(;2-1-);3-2;"

\storePredefinedDiagram #default-fret-table \chordmode {c:6}
                        #mandolin-tuning
                        "2-1-(;2-1-);3-2-(;3-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {c:sus2}
                        #mandolin-tuning
                        "5-3;o;3-1-(;3-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {c:sus4}
                        #mandolin-tuning
                        "5-3;3-1-(;3-1;3-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {c:aug}
                        #mandolin-tuning
                        "5-4;2-1;3-2;4-3;"

\storePredefinedDiagram #default-fret-table \chordmode {c:9}
                        #mandolin-tuning
                        "5-1;o;7-3;6-2;"

%%% cis chords

\storePredefinedDiagram #default-fret-table \chordmode {cis}
                        #mandolin-tuning
                        "6-4;3-2;4-3;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:m}
                        #mandolin-tuning
                        "6-2;6-3;4-1;o;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:7}
                        #mandolin-tuning
                        "6-4;3-2;2-1;4-3;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:m7}
                        #mandolin-tuning
                        "6-1-(;6-1-);7-2-(;7-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {cis:m7.5-}
                        #mandolin-tuning
                        "4-3;2-1;4-4;3-2;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:dim7}
                        #mandolin-tuning
                        "3-2;2-1;1-1;o;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:maj7}
                        #mandolin-tuning
                        "6-4;3-1-(;3-1-);4-2;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:6}
                        #mandolin-tuning
                        "3-1-(;3-1-);4-2-(;4-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {cis:sus2}
                        #mandolin-tuning
                        "1-1-(;1-1-);4-3;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:sus4}
                        #mandolin-tuning
                        "6-3;4-1-(;4-1;4-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {cis:aug}
                        #mandolin-tuning
                        "6-4;3-1;o5-3;"

\storePredefinedDiagram #default-fret-table \chordmode {cis:9}
                        #mandolin-tuning
                        "6-2;3-1;6-3;7-4;"

%%% des chords

\storePredefinedDiagram #default-fret-table \chordmode {des}
                        #mandolin-tuning
                        "6-4;3-2;4-3;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {des:m}
                        #mandolin-tuning
                        "6-2;6-3;4-1;o;"

\storePredefinedDiagram #default-fret-table \chordmode {des:7}
                        #mandolin-tuning
                        "6-4;3-2;2-1;4-3;"

\storePredefinedDiagram #default-fret-table \chordmode {des:m7}
                        #mandolin-tuning
                        "6-1-(;6-1-);7-2-(;7-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {des:m7.5-}
                        #mandolin-tuning
                        "4-3;2-1;4-4;3-2;"

\storePredefinedDiagram #default-fret-table \chordmode {des:dim7}
                        #mandolin-tuning
                        "3-2;2-1;1-1;o;"

\storePredefinedDiagram #default-fret-table \chordmode {des:maj7}
                        #mandolin-tuning
                        "6-4;3-1-(;3-1-);4-2;"

\storePredefinedDiagram #default-fret-table \chordmode {des:6}
                        #mandolin-tuning
                        "3-1-(;3-1-);4-2-(;4-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {des:sus2}
                        #mandolin-tuning
                        "1-1-(;1-1-);4-3;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {des:sus4}
                        #mandolin-tuning
                        "6-3;4-1-(;4-1;4-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {des:aug}
                        #mandolin-tuning
                        "6-4;3-1;o5-3;"

\storePredefinedDiagram #default-fret-table \chordmode {des:9}
                        #mandolin-tuning
                        "6-2;3-1;6-3;7-4;"

%%% d chords

\storePredefinedDiagram #default-fret-table \chordmode {d}
                        #mandolin-tuning
                        "2-1;o;o;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {d:m}
                        #mandolin-tuning
                        "2-2;o;o;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {d:7}
                        #mandolin-tuning
                        "2-1;o;3-3;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {d:m7}
                        #mandolin-tuning
                        "2-2;o;3-3;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {d:m7.5-}
                        #mandolin-tuning
                        "1-1;o;3-3;1-2;"

\storePredefinedDiagram #default-fret-table \chordmode {d:dim7}
                        #mandolin-tuning
                        "1-1;o;2-3;1-2;"

\storePredefinedDiagram #default-fret-table \chordmode {d:maj7}
                        #mandolin-tuning
                        "2-1;o;4-4;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {d:6}
                        #mandolin-tuning
                        "2-1;o;2-2;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {d:sus2}
                        #mandolin-tuning
                        "2-1;o;o;o;"

\storePredefinedDiagram #default-fret-table \chordmode {d:sus4}
                        #mandolin-tuning
                        "2-1;o;o;3-2;"

\storePredefinedDiagram #default-fret-table \chordmode {d:aug}
                        #mandolin-tuning
                        "3-3;o;1-1;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {d:9}
                        #mandolin-tuning
                        "7-4;4-2;3-1;o;"

%%%% dis chords

\storePredefinedDiagram #default-fret-table \chordmode {dis}
                        #mandolin-tuning
                        "3-3;1-1-(;1-1-);3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:m}
                        #mandolin-tuning
                        "3-3;1-1-(;1-1-);2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:7}
                        #mandolin-tuning
                        "3-2;1-1;4-4;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:m7}
                        #mandolin-tuning
                        "3-3;1-1;4-4;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:m7.5-}
                        #mandolin-tuning
                        "2-2;1-1;4-4;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:dim7}
                        #mandolin-tuning
                        "2-2;1-1;3-4;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:maj7}
                        #mandolin-tuning
                        "3-2;1-1;5-4;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:6}
                        #mandolin-tuning
                        "3-2;1-1;3-3;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:sus2}
                        #mandolin-tuning
                        "3-3;1-1-(;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {dis:sus4}
                        #mandolin-tuning
                        "3-3;1-1-(;1-1-);4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:aug}
                        #mandolin-tuning
                        "o;1-1;2-2;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {dis:9}
                        #mandolin-tuning
                        "8-2;5-1;8-3;9-4;"

%%%% ees chords

\storePredefinedDiagram #default-fret-table \chordmode {ees}
                        #mandolin-tuning
                        "3-3;1-1-(;1-1-);3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:m}
                        #mandolin-tuning
                        "3-3;1-1-(;1-1-);2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:7}
                        #mandolin-tuning
                        "3-2;1-1;4-4;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:m7}
                        #mandolin-tuning
                        "3-3;1-1;4-4;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:m7.5-}
                        #mandolin-tuning
                        "2-2;1-1;4-4;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:dim7}
                        #mandolin-tuning
                        "2-2;1-1;3-4;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:maj7}
                        #mandolin-tuning
                        "3-2;1-1;5-4;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:6}
                        #mandolin-tuning
                        "3-2;1-1;3-3;3-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:sus2}
                        #mandolin-tuning
                        "3-3;1-1-(;1-1;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ees:sus4}
                        #mandolin-tuning
                        "3-3;1-1-(;1-1-);4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:aug}
                        #mandolin-tuning
                        "o;1-1;2-2;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {ees:9}
                        #mandolin-tuning
                        "8-2;5-1;8-3;9-4;"

%%%% e chords

\storePredefinedDiagram #default-fret-table \chordmode {e}
                        #mandolin-tuning
                        "1-1;2-2;2-3;o;"

\storePredefinedDiagram #default-fret-table \chordmode {e:m}
                        #mandolin-tuning
                        "o;2-2;2-3;o;"

\storePredefinedDiagram #default-fret-table \chordmode {e:7}
                        #mandolin-tuning
                        "1-1;o;2-2;o;"

\storePredefinedDiagram #default-fret-table \chordmode {e:m7}
                        #mandolin-tuning
                        "o;o;2-2;o;"

\storePredefinedDiagram #default-fret-table \chordmode {e:m7.5-}
                        #mandolin-tuning
                        "o;o;1-1;o;"

\storePredefinedDiagram #default-fret-table \chordmode {e:dim7}
                        #mandolin-tuning
                        "3-2;2-1;4-4;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {e:maj7}
                        #mandolin-tuning
                        "1-1-(;1-1-);2-2;o;"

\storePredefinedDiagram #default-fret-table \chordmode {e:6}
                        #mandolin-tuning
                        "4-1;6-3;4-2;o;"

\storePredefinedDiagram #default-fret-table \chordmode {e:sus2}
                        #mandolin-tuning
                        "4-3;2-1-(;2-1;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {e:sus4}
                        #mandolin-tuning
                        "4-3;2-1;o;o;"

\storePredefinedDiagram #default-fret-table \chordmode {e:aug}
                        #mandolin-tuning
                        "1-1;2-2;3-3;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {e:9}
                        #mandolin-tuning
                        "9-2;6-1;9-3;10-4;"

%%%% f chords

\storePredefinedDiagram #default-fret-table \chordmode {f}
                        #mandolin-tuning
                        "2-2;3-3;o;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {f:m}
                        #mandolin-tuning
                        "1-1-(;3-3;3-4;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {f:7}
                        #mandolin-tuning
                        "2-2;1-1-(;3-3;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {f:m7}
                        #mandolin-tuning
                        "1-1-(;1-1;3-3;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {f:m7.5-}
                        #mandolin-tuning
                        "1-1-(;1-1;2-2;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {f:dim7}
                        #mandolin-tuning
                        "1-1;o;2-3;1-2;"

\storePredefinedDiagram #default-fret-table \chordmode {f:maj7}
                        #mandolin-tuning
                        "2-2;2-3;3-4;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {f:6}
                        #mandolin-tuning
                        "2-2;o;3-3;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {f:sus2}
                        #mandolin-tuning
                        "o;3-3;3-4;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {f:sus4}
                        #mandolin-tuning
                        "5-4;3-2;1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {f:aug}
                        #mandolin-tuning
                        "2-1;3-2;4-3;5-4;"

\storePredefinedDiagram #default-fret-table \chordmode {f:9}
                        #mandolin-tuning
                        "10-2;7-1;10-3;11-4;"

%%%% fis chords

\storePredefinedDiagram #default-fret-table \chordmode {fis}
                        #mandolin-tuning
                        "3-2;4-3;4-4;2-1;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:m}
                        #mandolin-tuning
                        "2-1-(;4-3;4-4;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {fis:7}
                        #mandolin-tuning
                        "3-2;2-1-(;4-3;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {fis:m7}
                        #mandolin-tuning
                        "2-1-(;2-1;4-3;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {fis:m7.5-}
                        #mandolin-tuning
                        "2-1-(;2-1;3-2;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {fis:dim7}
                        #mandolin-tuning
                        "2-2;1-1;3-4;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:maj7}
                        #mandolin-tuning
                        "3-2;3-3;4-4;2-1;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:6}
                        #mandolin-tuning
                        "3-3;1-1;4-4;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:sus2}
                        #mandolin-tuning
                        "6-3;4-1-(;4-1;4-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {fis:sus4}
                        #mandolin-tuning
                        "6-4;4-2;2-1-(;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {fis:aug}
                        #mandolin-tuning
                        "3-1;4-2;5-3;6-4;"

\storePredefinedDiagram #default-fret-table \chordmode {fis:9}
                        #mandolin-tuning
                        "11-2;8-1;11-3;o;"

%%%% ges chords

\storePredefinedDiagram #default-fret-table \chordmode {ges}
                        #mandolin-tuning
                        "3-2;4-3;4-4;2-1;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:m}
                        #mandolin-tuning
                        "2-1-(;4-3;4-4;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ges:7}
                        #mandolin-tuning
                        "3-2;2-1-(;4-3;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ges:m7}
                        #mandolin-tuning
                        "2-1-(;2-1;4-3;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ges:m7.5-}
                        #mandolin-tuning
                        "2-1-(;2-1;3-2;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ges:dim7}
                        #mandolin-tuning
                        "2-2;1-1;3-4;2-3;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:maj7}
                        #mandolin-tuning
                        "3-2;3-3;4-4;2-1;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:6}
                        #mandolin-tuning
                        "3-3;1-1;4-4;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:sus2}
                        #mandolin-tuning
                        "6-3;4-1-(;4-1;4-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ges:sus4}
                        #mandolin-tuning
                        "6-4;4-2;2-1-(;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {ges:aug}
                        #mandolin-tuning
                        "3-1;4-2;5-3;6-4;"

\storePredefinedDiagram #default-fret-table \chordmode {ges:9}
                        #mandolin-tuning
                        "11-2;8-1;11-3;o;"

%%%% g chords

\storePredefinedDiagram #default-fret-table \chordmode {g}
                        #mandolin-tuning
                        "o;o;2-1;3-2;"

\storePredefinedDiagram #default-fret-table \chordmode {g:m}
                        #mandolin-tuning
                        "o;o;1-1;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {g:7}
                        #mandolin-tuning
                        "o;o;2-2;1-1;"

\storePredefinedDiagram #default-fret-table \chordmode {g:m7}
                        #mandolin-tuning
                        "o;o;1-1-(;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {g:m7.5-}
                        #mandolin-tuning
                        "3-1-(;3-1;4-2;3-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {g:dim7}
                        #mandolin-tuning
                        "3-2;2-1;4-4;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {g:maj7}
                        #mandolin-tuning
                        "o;o;2-1-(;2-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {g:6}
                        #mandolin-tuning
                        "o;o;2-2;o;"

\storePredefinedDiagram #default-fret-table \chordmode {g:sus2}
                        #mandolin-tuning
                        "o;o;o;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {g:sus4}
                        #mandolin-tuning
                        "o;o;3-1-(;3-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {g:aug}
                        #mandolin-tuning
                        "o;1-1;2-2;3-3;"

\storePredefinedDiagram #default-fret-table \chordmode {g:9}
                        #mandolin-tuning
                        "o;3-1;o;7-4;"

%%%% gis chords

\storePredefinedDiagram #default-fret-table \chordmode {gis}
                        #mandolin-tuning
                        "1-1-(;1-1-);3-3;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:m}
                        #mandolin-tuning
                        "1-1-(;1-1-);2-2;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:7}
                        #mandolin-tuning
                        "1-1-(;1-1-);3-3;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:m7}
                        #mandolin-tuning
                        "1-1-(;1-1-);2-2-(;2-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {gis:m7.5-}
                        #mandolin-tuning
                        "1-1;o;2-2-(;2-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {gis:dim7}
                        #mandolin-tuning
                        "1-1;o;2-3;1-2;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:maj7}
                        #mandolin-tuning
                        "1-1-(;1-1-);3-3-(;3-3-);"

\storePredefinedDiagram #default-fret-table \chordmode {gis:6}
                        #mandolin-tuning
                        "1-1-(;1-1;3-3;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {gis:sus2}
                        #mandolin-tuning
                        "1-1-(;1-1;1-1-);4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:sus4}
                        #mandolin-tuning
                        "1-1-(;1-1-);4-3;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:aug}
                        #mandolin-tuning
                        "1-1;2-2;3-3;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {gis:9}
                        #mandolin-tuning
                        "1-1;4-3;3-2;6-4;"
%%%% aes chords

\storePredefinedDiagram #default-fret-table \chordmode {aes}
                        #mandolin-tuning
                        "1-1-(;1-1-);3-3;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:m}
                        #mandolin-tuning
                        "1-1-(;1-1-);2-2;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:7}
                        #mandolin-tuning
                        "1-1-(;1-1-);3-3;2-2;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:m7}
                        #mandolin-tuning
                        "1-1-(;1-1-);2-2-(;2-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {aes:m7.5-}
                        #mandolin-tuning
                        "1-1;o;2-2-(;2-2-);"

\storePredefinedDiagram #default-fret-table \chordmode {aes:dim7}
                        #mandolin-tuning
                        "1-1;o;2-3;1-2;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:maj7}
                        #mandolin-tuning
                        "1-1-(;1-1-);3-3-(;3-3-);"

\storePredefinedDiagram #default-fret-table \chordmode {aes:6}
                        #mandolin-tuning
                        "1-1-(;1-1;3-3;1-1-);"

\storePredefinedDiagram #default-fret-table \chordmode {aes:sus2}
                        #mandolin-tuning
                        "1-1-(;1-1;1-1-);4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:sus4}
                        #mandolin-tuning
                        "1-1-(;1-1-);4-3;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:aug}
                        #mandolin-tuning
                        "1-1;2-2;3-3;4-4;"

\storePredefinedDiagram #default-fret-table \chordmode {aes:9}
                        #mandolin-tuning
                        "1-1;4-3;3-2;6-4;"

\languageRestore
