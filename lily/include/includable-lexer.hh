/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#include "lily-proto.hh"

#include <vector>

/**
   an yyFlexLexer child with provisions for inclusion.
*/
class Includable_lexer : public yyFlexLexer
{
protected:
  void close_input ();
  std::vector<Source_file *> include_stack_;
  std::vector<size_t> char_count_stack_;

public:
  Includable_lexer () = default;
  ~Includable_lexer ();
  std::string main_input_name_;

  // A list of all files opened so far
  std::vector<std::string> file_name_strings_;

  Source_file *get_source_file () const;
  void new_input (const std::string &s, Sources *);

  void new_input (const std::string &name, std::string data, Sources *);

protected:
  void new_input (const std::string &name, Source_file *);

public:
  char const *here_str0 () const;

  void skip_chars (size_t count);
};

#endif // INCLUDABLE_LEXER_HH
