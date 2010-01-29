/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1996--2010 Han-Wen Nienhuys

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef KEYWORD_HH
#define KEYWORD_HH

#include "std-vector.hh"

/* for the keyword table */
struct Keyword_ent
{
  char const *name_;
  int tokcode_;
};

/*
  junkme, use  hash table.
*/
struct Keyword_table
{
  vector<Keyword_ent> table_;

  Keyword_table (Keyword_ent *);
  vsize lookup (char const *s) const;
};

#endif // KEYWORD_HH

