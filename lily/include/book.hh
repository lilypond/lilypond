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

#ifndef BOOK_HH
#define BOOK_HH

#include "lily-proto.hh"
#include "std-string.hh"
#include "virtual-methods.hh"
#include "smobs.hh"

class Book : public Smob<Book>
{
public:
  SCM mark_smob () const;
  static const char *const type_p_name_;
  virtual ~Book ();
  SCM header_;
  Output_def *paper_;

  SCM scores_;    // SCM list; reverse order (most recently added first)
  SCM bookparts_; // SCM list; reverse order (most recently added first)
  SCM input_location_;

  Book (Book const &);
  Input *origin () const;
  VIRTUAL_CLASS_NAME (Book);
  virtual Book *clone () const { return new Book (*this); }
  Book ();
  void add_score (SCM);
  void add_bookpart (SCM);
  Paper_book *process (Output_def *def_paper, Output_def *def_layout);
  Paper_book *process (Output_def *default_paper, Output_def *default_layout,
                       Paper_book *parent_part);
  void set_keys ();

protected:
  void set_parent (Book *parent);
  void add_scores_to_bookpart ();
  bool error_found () const;
  void process_score (SCM score, Paper_book *output_paper_book,
                      Output_def *layout);
  void process_bookparts (Paper_book *output_paper_book, Output_def *paper,
                          Output_def *layout);
};

#endif /* BOOK_HH */
