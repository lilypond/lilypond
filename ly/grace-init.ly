%%%% Grace notes predefined commands.
%%%% This file is part of LilyPond, the GNU music typesetter.
%%%%
%%%% Copyright (C) 2001--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>
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

startGraceSlur = #(make-music 'SlurEvent 'span-direction START 'spanner-id 'grace)
stopGraceSlur = #(make-music 'SlurEvent 'span-direction STOP 'spanner-id 'grace)


startGraceMusic =  {
}

stopGraceMusic =  {
}

startAppoggiaturaMusic =
{
    <>\startGraceSlur
}

stopAppoggiaturaMusic =  {
    <>\stopGraceSlur
}

startAcciaccaturaMusic =  {
    <>\startGraceSlur
    \temporary \override Flag.stroke-style = "grace"
}

stopAcciaccaturaMusic =  {
    \revert Flag.stroke-style
    <>\stopGraceSlur
}

startSlashedGraceMusic =  {
  \temporary \override Flag.stroke-style = "grace"
}

stopSlashedGraceMusic =  {
  \revert Flag.stroke-style
}
