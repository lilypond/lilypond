%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2009--2010 Matt Corks <mvcorks@alumni.uwaterloo.ca>
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

\version "2.13.9"

%%%% sources:
%%%%   ukulele hall of fame museum website (http://www.ukulele.org/),
%%%%   kiwi ukulele website (http://www.kiwiukulele.co.nz/)


%%% a chords

\storePredefinedDiagram \chordmode {a}
                        #ukulele-tuning
                        #"2-2;1-1;o;o;"

\storePredefinedDiagram \chordmode {a:m}
                        #ukulele-tuning
                        #"2-1;o;o;o;"

\storePredefinedDiagram \chordmode {a:7}
                        #ukulele-tuning
                        #"o;1-1;o;o;"

\storePredefinedDiagram \chordmode {a:m7}
                        #ukulele-tuning
                        #"o;o;o;o;"

\storePredefinedDiagram \chordmode {a:dim}
                        #ukulele-tuning
                        #"2-1;3-3;2-2;3-4;"

\storePredefinedDiagram \chordmode {a:maj7}
                        #ukulele-tuning
                        #"1-1;1-2;o;o;"

\storePredefinedDiagram \chordmode {a:6}
                        #ukulele-tuning
                        #"2-1;4-3;2-2;4-4;"

\storePredefinedDiagram \chordmode {a:sus2}
                        #ukulele-tuning
                        #"2-2;4-3;5-4;2-1;"

\storePredefinedDiagram \chordmode {a:sus4}
                        #ukulele-tuning
                        #"2-1;2-2;o;o;"

\storePredefinedDiagram \chordmode {a:aug}
                        #ukulele-tuning
                        #"2-2;1-1-(;1-1-);4-4;"

\storePredefinedDiagram \chordmode {a:9}
                        #ukulele-tuning
                        #"o;1-1;o;2-2;"

%%% ais chords

\storePredefinedDiagram \chordmode {ais}
                        #ukulele-tuning
                        #"3-3;2-2;1-1-(;1-1-);"

\storePredefinedDiagram \chordmode {ais:m}
                        #ukulele-tuning
                        #"3-3;1-1-(;1-1;1-1-);"

\storePredefinedDiagram \chordmode {ais:7}
                        #ukulele-tuning
                        #"1-1-(;2-2;1-1;1-1-);"

\storePredefinedDiagram \chordmode {ais:m7}
                        #ukulele-tuning
                        #"1-1-(;1-1;1-1;1-1-);"

\storePredefinedDiagram \chordmode {ais:dim}
                        #ukulele-tuning
                        #"o;1-1;o;1-2;"

\storePredefinedDiagram \chordmode {ais:maj7}
                        #ukulele-tuning
                        #"2-2-(;2-2-);1-1-(;1-1-);"

\storePredefinedDiagram \chordmode {ais:6}
                        #ukulele-tuning
                        #"o;2-2;1-1-(;1-1-);"

\storePredefinedDiagram \chordmode {ais:sus2}
                        #ukulele-tuning
                        #"3-3;o;1-1-(;1-1-);"

\storePredefinedDiagram \chordmode {ais:sus4}
                        #ukulele-tuning
                        #"3-3-(;3-3-);1-1-(;1-1-);"

\storePredefinedDiagram \chordmode {ais:aug}
                        #ukulele-tuning
                        #"3-2;1-1-(;1-1-);5-4;"

\storePredefinedDiagram \chordmode {ais:9}
                        #ukulele-tuning
                        #"1-1-(;2-2;1-1-);3-3;"

%%% bes chords

\storePredefinedDiagram \chordmode {bes}
                        #ukulele-tuning
                        #"3-3;2-2;1-1-(;1-1-);"

\storePredefinedDiagram \chordmode {bes:m}
                        #ukulele-tuning
                        #"3-3;1-1-(;1-1;1-1-);"

\storePredefinedDiagram \chordmode {bes:7}
                        #ukulele-tuning
                        #"1-1-(;2-2;1-1;1-1-);"

\storePredefinedDiagram \chordmode {bes:m7}
                        #ukulele-tuning
                        #"1-1-(;1-1;1-1;1-1-);"

\storePredefinedDiagram \chordmode {bes:dim}
                        #ukulele-tuning
                        #"o;1-1;o;1-2;"

\storePredefinedDiagram \chordmode {bes:maj7}
                        #ukulele-tuning
                        #"2-2-(;2-2-);1-1-(;1-1-);"

\storePredefinedDiagram \chordmode {bes:6}
                        #ukulele-tuning
                        #"o;2-2;1-1-(;1-1-);"

\storePredefinedDiagram \chordmode {bes:sus2}
                        #ukulele-tuning
                        #"3-3;o;1-1-(;1-1-);"

\storePredefinedDiagram \chordmode {bes:sus4}
                        #ukulele-tuning
                        #"3-3-(;3-3-);1-1-(;1-1-);"

\storePredefinedDiagram \chordmode {bes:aug}
                        #ukulele-tuning
                        #"3-2;1-1-(;1-1-);5-4;"

\storePredefinedDiagram \chordmode {bes:9}
                        #ukulele-tuning
                        #"1-1-(;2-2;1-1-);3-3;"

%%% b chords

\storePredefinedDiagram \chordmode {b}
                        #ukulele-tuning
                        #"4-3;3-2;2-1-(;2-1-);"

\storePredefinedDiagram \chordmode {b:m}
                        #ukulele-tuning
                        #"4-3;2-1-(;2-1;2-1-);"

\storePredefinedDiagram \chordmode {b:7}
                        #ukulele-tuning
                        #"2-1-(;3-2;2-1;2-1-);"

\storePredefinedDiagram \chordmode {b:m7}
                        #ukulele-tuning
                        #"2-1-(;2-1;2-1;2-1-);"

\storePredefinedDiagram \chordmode {b:dim}
                        #ukulele-tuning
                        #"1-1;2-3;1-2;2-4;"

\storePredefinedDiagram \chordmode {b:maj7}
                        #ukulele-tuning
                        #"3-2-(;3-2-);2-1-(;2-1-);"

\storePredefinedDiagram \chordmode {b:6}
                        #ukulele-tuning
                        #"1-1;3-4;2-2;2-3;"

\storePredefinedDiagram \chordmode {b:sus2}
                        #ukulele-tuning
                        #"5-4;1-1;2-3;2-2;"

\storePredefinedDiagram \chordmode {b:sus4}
                        #ukulele-tuning
                        #"4-2-(;4-2-);2-1-(;2-1-);"

\storePredefinedDiagram \chordmode {b:aug}
                        #ukulele-tuning
                        #"o;3-2-(;3-2-);2-1;"

\storePredefinedDiagram \chordmode {b:9}
                        #ukulele-tuning
                        #"2-2-(;3-3;2-2-);4-4;"

%%% c chords

\storePredefinedDiagram \chordmode {c}
                        #ukulele-tuning
                        #"o;o;o;3-3;"

\storePredefinedDiagram \chordmode {c:m}
                        #ukulele-tuning
                        #"o;3-1;3-2;3-3;"

\storePredefinedDiagram \chordmode {c:7}
                        #ukulele-tuning
                        #"o;o;o;1-1;"

\storePredefinedDiagram \chordmode {c:m7}
                        #ukulele-tuning
                        #"3-1-(;3-1;3-1;3-1-);"

\storePredefinedDiagram \chordmode {c:dim}
                        #ukulele-tuning
                        #"2-1;3-3;2-2;3-4;"

\storePredefinedDiagram \chordmode {c:maj7}
                        #ukulele-tuning
                        #"o;o;o;2-1;"

\storePredefinedDiagram \chordmode {c:6}
                        #ukulele-tuning
                        #"o;o;o;o;"

\storePredefinedDiagram \chordmode {c:sus2}
                        #ukulele-tuning
                        #"o;2-1;3-2-(;3-2-);"

\storePredefinedDiagram \chordmode {c:sus4}
                        #ukulele-tuning
                        #"o;o;1-1;3-3;"

\storePredefinedDiagram \chordmode {c:aug}
                        #ukulele-tuning
                        #"1-1;o;o;3-4;"

\storePredefinedDiagram \chordmode {c:9}
                        #ukulele-tuning
                        #"o;2-2;o;1-1;"

%%% cis chords

\storePredefinedDiagram \chordmode {cis}
                        #ukulele-tuning
                        #"1-1-(;1-1;1-1-);4-4;"

\storePredefinedDiagram \chordmode {cis:m}
                        #ukulele-tuning
                        #"1-1;4-2;4-3-(;4-3-);"

\storePredefinedDiagram \chordmode {cis:7}
                        #ukulele-tuning
                        #"1-1-(;1-1;1-1-);2-2;"

\storePredefinedDiagram \chordmode {cis:m7}
                        #ukulele-tuning
                        #"2-2-(;2-2-);1-1;3-3;"

\storePredefinedDiagram \chordmode {cis:dim}
                        #ukulele-tuning
                        #"o;1-1;o;1-2;"

\storePredefinedDiagram \chordmode {cis:maj7}
                        #ukulele-tuning
                        #"1-1-(;1-1;1-1-);3-3;"

\storePredefinedDiagram \chordmode {cis:6}
                        #ukulele-tuning
                        #"1-1-(;1-1;1-1;1-1-);"

\storePredefinedDiagram \chordmode {cis:sus2}
                        #ukulele-tuning
                        #"1-1;3-2;4-3-(;4-3-);"

\storePredefinedDiagram \chordmode {cis:sus4}
                        #ukulele-tuning
                        #"1-1-(;1-1-);2-2;4-4;"

\storePredefinedDiagram \chordmode {cis:aug}
                        #ukulele-tuning
                        #"2-2;1-1-(;1-1-);4-4;"

\storePredefinedDiagram \chordmode {cis:9}
                        #ukulele-tuning
                        #"1-1-(;3-3;1-1-);2-2;"

%%% des chords

\storePredefinedDiagram \chordmode {des}
                        #ukulele-tuning
                        #"1-1-(;1-1;1-1-);4-4;"

\storePredefinedDiagram \chordmode {des:m}
                        #ukulele-tuning
                        #"1-1;4-2;4-3-(;4-3-);"

\storePredefinedDiagram \chordmode {des:7}
                        #ukulele-tuning
                        #"1-1-(;1-1;1-1-);2-2;"

\storePredefinedDiagram \chordmode {des:m7}
                        #ukulele-tuning
                        #"2-2-(;2-2-);1-1;3-3;"

\storePredefinedDiagram \chordmode {des:dim}
                        #ukulele-tuning
                        #"o;1-1;o;1-2;"

\storePredefinedDiagram \chordmode {des:maj7}
                        #ukulele-tuning
                        #"1-1-(;1-1;1-1-);3-3;"

\storePredefinedDiagram \chordmode {des:6}
                        #ukulele-tuning
                        #"1-1-(;1-1;1-1;1-1-);"

\storePredefinedDiagram \chordmode {des:sus2}
                        #ukulele-tuning
                        #"1-1;3-2;4-3-(;4-3-);"

\storePredefinedDiagram \chordmode {des:sus4}
                        #ukulele-tuning
                        #"1-1-(;1-1-);2-2;4-4;"

\storePredefinedDiagram \chordmode {des:aug}
                        #ukulele-tuning
                        #"2-2;1-1-(;1-1-);4-4;"

\storePredefinedDiagram \chordmode {des:9}
                        #ukulele-tuning
                        #"1-1-(;3-3;1-1-);2-2;"

%%% d chords

\storePredefinedDiagram \chordmode {d}
                        #ukulele-tuning
                        #"2-1;2-2;2-3;o;"

\storePredefinedDiagram \chordmode {d:m}
                        #ukulele-tuning
                        #"2-2-(;2-2-);1-1;o;"

\storePredefinedDiagram \chordmode {d:7}
                        #ukulele-tuning
                        #"2-1-(;2-1;2-1-);3-2;"

\storePredefinedDiagram \chordmode {d:m7}
                        #ukulele-tuning
                        #"2-2-(;2-2-);1-1;3-3;"

\storePredefinedDiagram \chordmode {d:dim}
                        #ukulele-tuning
                        #"1-1;2-3;1-2;2-4;"

\storePredefinedDiagram \chordmode {d:maj7}
                        #ukulele-tuning
                        #"2-1-(;2-1;2-1-);4-3;"

\storePredefinedDiagram \chordmode {d:6}
                        #ukulele-tuning
                        #"2-1-(;2-1;2-1;2-1-);"

\storePredefinedDiagram \chordmode {d:sus2}
                        #ukulele-tuning
                        #"2-1;2-2;o;o;"

\storePredefinedDiagram \chordmode {d:sus4}
                        #ukulele-tuning
                        #"o;2-1;3-2;o;"

\storePredefinedDiagram \chordmode {d:aug}
                        #ukulele-tuning
                        #"3-2;2-1-(;2-1-);5-4;"

\storePredefinedDiagram \chordmode {d:9}
                        #ukulele-tuning
                        #"2-1-(;4-3;2-1-);3-2;"

%%%% dis chords

\storePredefinedDiagram \chordmode {dis}
                        #ukulele-tuning
                        #"o;3-2-(;3-2-);1-1;"

\storePredefinedDiagram \chordmode {dis:m}
                        #ukulele-tuning
                        #"3-3-(;3-3-);2-2;1-1;"

\storePredefinedDiagram \chordmode {dis:7}
                        #ukulele-tuning
                        #"3-1-(;3-1;3-1-);4-2;"

\storePredefinedDiagram \chordmode {dis:m7}
                        #ukulele-tuning
                        #"3-2-(;3-2-);2-1;4-4;"

\storePredefinedDiagram \chordmode {dis:dim}
                        #ukulele-tuning
                        #"2-1;3-3;2-1;3-4;"

\storePredefinedDiagram \chordmode {dis:maj7}
                        #ukulele-tuning
                        #"3-1-(;3-2;3-1-);5-2;"

\storePredefinedDiagram \chordmode {dis:6}
                        #ukulele-tuning
                        #"3-1-(;3-1;3-1;3-1-);"

\storePredefinedDiagram \chordmode {dis:sus2}
                        #ukulele-tuning
                        #"3-2-(;3-2-);1-1-(;1-1-);"

\storePredefinedDiagram \chordmode {dis:sus4}
                        #ukulele-tuning
                        #"1-2;3-3;4-4;1-1;"

\storePredefinedDiagram \chordmode {dis:aug}
                        #ukulele-tuning
                        #"o;3-2-(;3-2-);2-1;"

\storePredefinedDiagram \chordmode {dis:9}
                        #ukulele-tuning
                        #"o;1-1-(;1-1;1-1-);"

%%%% ees chords

\storePredefinedDiagram \chordmode {ees}
                        #ukulele-tuning
                        #"o;3-2-(;3-2-);1-1;"

\storePredefinedDiagram \chordmode {ees:m}
                        #ukulele-tuning
                        #"3-3-(;3-3-);2-2;1-1;"

\storePredefinedDiagram \chordmode {ees:7}
                        #ukulele-tuning
                        #"3-1-(;3-1;3-1-);4-2;"

\storePredefinedDiagram \chordmode {ees:m7}
                        #ukulele-tuning
                        #"3-2-(;3-2-);2-1;4-4;"

\storePredefinedDiagram \chordmode {ees:dim}
                        #ukulele-tuning
                        #"2-1;3-3;2-1;3-4;"

\storePredefinedDiagram \chordmode {ees:maj7}
                        #ukulele-tuning
                        #"3-1-(;3-2;3-1-);5-2;"

\storePredefinedDiagram \chordmode {ees:6}
                        #ukulele-tuning
                        #"3-1-(;3-1;3-1;3-1-);"

\storePredefinedDiagram \chordmode {ees:sus2}
                        #ukulele-tuning
                        #"3-2-(;3-2-);1-1-(;1-1-);"

\storePredefinedDiagram \chordmode {ees:sus4}
                        #ukulele-tuning
                        #"1-2;3-3;4-4;1-1;"

\storePredefinedDiagram \chordmode {ees:aug}
                        #ukulele-tuning
                        #"o;3-2-(;3-2-);2-1;"

\storePredefinedDiagram \chordmode {ees:9}
                        #ukulele-tuning
                        #"o;1-1-(;1-1;1-1-);"

%%%% e chords

\storePredefinedDiagram \chordmode {e}
                        #ukulele-tuning
                        #"4-2;4-3;4-4;2-1;"

\storePredefinedDiagram \chordmode {e:m}
                        #ukulele-tuning
                        #"4-3;4-3;3-2;2-1;"

\storePredefinedDiagram \chordmode {e:7}
                        #ukulele-tuning
                        #"1-1;2-2;o;2-3;"

\storePredefinedDiagram \chordmode {e:m7}
                        #ukulele-tuning
                        #"o;2-1;o;2-2;"

\storePredefinedDiagram \chordmode {e:dim}
                        #ukulele-tuning
                        #"o;1-1;o;1-2;"

\storePredefinedDiagram \chordmode {e:maj7}
                        #ukulele-tuning
                        #"1-1;3-3;o;2-2;"

\storePredefinedDiagram \chordmode {e:6}
                        #ukulele-tuning
                        #"4-1-(;4-1;4-1;4-1-);"

\storePredefinedDiagram \chordmode {e:sus2}
                        #ukulele-tuning
                        #"4-3-(;4-3-);2-1-(;2-1-);"

\storePredefinedDiagram \chordmode {e:sus4}
                        #ukulele-tuning
                        #"2-2;4-4;o;2-1;"

\storePredefinedDiagram \chordmode {e:aug}
                        #ukulele-tuning
                        #"1-1;o;o;3-4;"

\storePredefinedDiagram \chordmode {e:9}
                        #ukulele-tuning
                        #"1-1;2-2-(;2-2;2-2-);"

%%%% f chords

\storePredefinedDiagram \chordmode {f}
                        #ukulele-tuning
                        #"2-2;o;1-1;o;"

\storePredefinedDiagram \chordmode {f:m}
                        #ukulele-tuning
                        #"1-1;o;1-2;3-4;"

\storePredefinedDiagram \chordmode {f:7}
                        #ukulele-tuning
                        #"2-2;3-3;1-1;3-4;"

\storePredefinedDiagram \chordmode {f:m7}
                        #ukulele-tuning
                        #"1-1;3-3;1-2;3-4;"

\storePredefinedDiagram \chordmode {f:dim}
                        #ukulele-tuning
                        #"1-1;2-3;1-2;2-4;"

\storePredefinedDiagram \chordmode {f:maj7}
                        #ukulele-tuning
                        #"2-2;4-4;1-1;3-3;"

\storePredefinedDiagram \chordmode {f:6}
                        #ukulele-tuning
                        #"2-2-(;2-2-);1-1;3-4;"

\storePredefinedDiagram \chordmode {f:sus2}
                        #ukulele-tuning
                        #"o;o;1-1;3-3;"

\storePredefinedDiagram \chordmode {f:sus4}
                        #ukulele-tuning
                        #"3-3;o;1-1-(;1-1-);"

\storePredefinedDiagram \chordmode {f:aug}
                        #ukulele-tuning
                        #"2-2;1-1-(;1-1-);4-4;"

\storePredefinedDiagram \chordmode {f:9}
                        #ukulele-tuning
                        #"2-1;3-2-(;3-2;3-2-);"

%%%% fis chords

\storePredefinedDiagram \chordmode {fis}
                        #ukulele-tuning
                        #"3-3;1-1;2-2;1-1;"

\storePredefinedDiagram \chordmode {fis:m}
                        #ukulele-tuning
                        #"2-2;1-1;2-3;o;"

\storePredefinedDiagram \chordmode {fis:7}
                        #ukulele-tuning
                        #"3-3;4-4;2-2;1-1;"

\storePredefinedDiagram \chordmode {fis:m7}
                        #ukulele-tuning
                        #"2-1;4-3;2-2;4-4;"

\storePredefinedDiagram \chordmode {fis:dim}
                        #ukulele-tuning
                        #"2-1;3-3;2-2;3-4;"

\storePredefinedDiagram \chordmode {fis:maj7}
                        #ukulele-tuning
                        #"3-2;5-4;2-1;4-3;"

\storePredefinedDiagram \chordmode {fis:6}
                        #ukulele-tuning
                        #"3-2-(;3-2-);2-1;4-4;"

\storePredefinedDiagram \chordmode {fis:sus2}
                        #ukulele-tuning
                        #"1-1-(;1-1-);2-2;4-4;"

\storePredefinedDiagram \chordmode {fis:sus4}
                        #ukulele-tuning
                        #"4-4;1-1;2-2;2-3;"

\storePredefinedDiagram \chordmode {fis:aug}
                        #ukulele-tuning
                        #"3-2;2-1-(;2-1-);5-4;"

\storePredefinedDiagram \chordmode {fis:9}
                        #ukulele-tuning
                        #"3-1;4-2-(;4-2;4-2-);"

%%%% ges chords

\storePredefinedDiagram \chordmode {ges}
                        #ukulele-tuning
                        #"3-3;1-1;2-2;1-1;"

\storePredefinedDiagram \chordmode {ges:m}
                        #ukulele-tuning
                        #"2-2;1-1;2-3;o;"

\storePredefinedDiagram \chordmode {ges:7}
                        #ukulele-tuning
                        #"3-3;4-4;2-2;1-1;"

\storePredefinedDiagram \chordmode {ges:m7}
                        #ukulele-tuning
                        #"2-1;4-3;2-2;4-4;"

\storePredefinedDiagram \chordmode {ges:dim}
                        #ukulele-tuning
                        #"2-1;3-3;2-2;3-4;"

\storePredefinedDiagram \chordmode {ges:maj7}
                        #ukulele-tuning
                        #"3-2;5-4;2-1;4-3;"

\storePredefinedDiagram \chordmode {ges:6}
                        #ukulele-tuning
                        #"3-2-(;3-2-);2-1;4-4;"

\storePredefinedDiagram \chordmode {ges:sus2}
                        #ukulele-tuning
                        #"1-1-(;1-1-);2-2;4-4;"

\storePredefinedDiagram \chordmode {ges:sus4}
                        #ukulele-tuning
                        #"4-4;1-1;2-2;2-3;"

\storePredefinedDiagram \chordmode {ges:aug}
                        #ukulele-tuning
                        #"3-2;2-1-(;2-1-);5-4;"

\storePredefinedDiagram \chordmode {ges:9}
                        #ukulele-tuning
                        #"3-1;4-2-(;4-2;4-2-);"

%%%% g chords

\storePredefinedDiagram \chordmode {g}
                        #ukulele-tuning
                        #"o;2-1;3-3;2-2;"

\storePredefinedDiagram \chordmode {g:m}
                        #ukulele-tuning
                        #"o;2-2;3-3;1-1;"

\storePredefinedDiagram \chordmode {g:7}
                        #ukulele-tuning
                        #"o;2-2;1-1;2-3;"

\storePredefinedDiagram \chordmode {g:m7}
                        #ukulele-tuning
                        #"o;2-2;1-1-(;1-1-);"

\storePredefinedDiagram \chordmode {g:dim}
                        #ukulele-tuning
                        #"o;1-1;o;1-2;"

\storePredefinedDiagram \chordmode {g:maj7}
                        #ukulele-tuning
                        #"o;2-1;2-2;2-3;"

\storePredefinedDiagram \chordmode {g:6}
                        #ukulele-tuning
                        #"o;2-1;o;2-2;"

\storePredefinedDiagram \chordmode {g:sus2}
                        #ukulele-tuning
                        #"o;2-1;3-2;o;"

\storePredefinedDiagram \chordmode {g:sus4}
                        #ukulele-tuning
                        #"o;2-1;3-2;3-3;"

\storePredefinedDiagram \chordmode {g:aug}
                        #ukulele-tuning
                        #"o;3-2-(;3-2-);2-1;"

\storePredefinedDiagram \chordmode {g:9}
                        #ukulele-tuning
                        #"2-2;2-3;1-1;2-4;"

%%%% gis chords

\storePredefinedDiagram \chordmode {gis}
                        #ukulele-tuning
                        #"5-3;3-1-(;4-2;3-1-);"

\storePredefinedDiagram \chordmode {gis:m}
                        #ukulele-tuning
                        #"1-1;3-3;4-4;2-2;"

\storePredefinedDiagram \chordmode {gis:7}
                        #ukulele-tuning
                        #"1-1;3-3;2-2;3-4;"

\storePredefinedDiagram \chordmode {gis:m7}
                        #ukulele-tuning
                        #"1-1;3-4;2-2;2-3;"

\storePredefinedDiagram \chordmode {gis:dim}
                        #ukulele-tuning
                        #"1-1;2-3;1-2;2-4;"

\storePredefinedDiagram \chordmode {gis:maj7}
                        #ukulele-tuning
                        #"1-1;3-2;3-3-(;3-3-);"

\storePredefinedDiagram \chordmode {gis:6}
                        #ukulele-tuning
                        #"1-1;3-3;1-2;3-4;"

\storePredefinedDiagram \chordmode {gis:sus2}
                        #ukulele-tuning
                        #"1-2;3-3;4-4;1-1;"

\storePredefinedDiagram \chordmode {gis:sus4}
                        #ukulele-tuning
                        #"1-1;2-3;4-3-(;4-3-);"

\storePredefinedDiagram \chordmode {gis:aug}
                        #ukulele-tuning
                        #"1-1;o;o;3-4;"

\storePredefinedDiagram \chordmode {gis:9}
                        #ukulele-tuning
                        #"1-1;o;2-3;1-2;"

%%%% aes chords

\storePredefinedDiagram \chordmode {aes}
                        #ukulele-tuning
                        #"5-3;3-1-(;4-2;3-1-);"

\storePredefinedDiagram \chordmode {aes:m}
                        #ukulele-tuning
                        #"1-1;3-3;4-4;2-2;"

\storePredefinedDiagram \chordmode {aes:7}
                        #ukulele-tuning
                        #"1-1;3-3;2-2;3-4;"

\storePredefinedDiagram \chordmode {aes:m7}
                        #ukulele-tuning
                        #"1-1;3-4;2-2;2-3;"

\storePredefinedDiagram \chordmode {aes:dim}
                        #ukulele-tuning
                        #"1-1;2-3;1-2;2-4;"

\storePredefinedDiagram \chordmode {aes:maj7}
                        #ukulele-tuning
                        #"1-1;3-2;3-3-(;3-3-);"

\storePredefinedDiagram \chordmode {aes:6}
                        #ukulele-tuning
                        #"1-1;3-3;1-2;3-4;"

\storePredefinedDiagram \chordmode {aes:sus2}
                        #ukulele-tuning
                        #"1-2;3-3;4-4;1-1;"

\storePredefinedDiagram \chordmode {aes:sus4}
                        #ukulele-tuning
                        #"1-1;2-3;4-3-(;4-3-);"

\storePredefinedDiagram \chordmode {aes:aug}
                        #ukulele-tuning
                        #"1-1;o;o;3-4;"

\storePredefinedDiagram \chordmode {aes:9}
                        #ukulele-tuning
                        #"1-1;o;2-3;1-2;"
