/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef INCLUDABLE_LEXER_HH
#define INCLUDABLE_LEXER_HH

#ifndef LEXER_CC
#include <FlexLexer.h>
#endif

#include "std-string.hh"
#include "std-vector.hh"
#include "lily-proto.hh"

// GIGA urg!
typedef struct yy_buffer_state *YY_BUFFER_STATE;

/**
   an yyFlexLexer child with provisions for inclusion.
*/
class Includable_lexer : public yyFlexLexer
{
  vector<YY_BUFFER_STATE> state_stack_;

protected:
  bool close_input ();
  vector<Source_file*> include_stack_;
  vector<int> char_count_stack_;

public:

  Includable_lexer ();
  ~Includable_lexer ();
  string main_input_name_;

  /// store dependencies for Makefile stuff.
  vector<string> file_name_strings_;

  Source_file *get_source_file () const;
  virtual void new_input (string s, Sources *);
  
  void new_input (string name, string data, Sources *);
  
  char const *here_str0 () const;
};

#endif // INCLUDABLE_LEXER_HH
