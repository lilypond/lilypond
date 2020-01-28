/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2020  Jan Nieuwenhuizen <janneke@gnu.org>

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
#ifndef PAPER_BOOK_HH
#define PAPER_BOOK_HH

#include "lily-proto.hh"
#include "smobs.hh"
#include "std-vector.hh"

/** Paper_book collects headers, systems (Paper_system) and texts, and
    exports them to the output backend, either as systems or as
    completely formatted pages.  */

class Paper_book : public Smob<Paper_book>
{
public:
  SCM mark_smob () const;
  static const char *const type_p_name_;
  virtual ~Paper_book ();

private:
  SCM systems_;
  SCM pages_;
  SCM performances_;

  void add_score_title (SCM);
  SCM get_score_title (SCM);

public:
  SCM header_;
  SCM header_0_;
  SCM scores_;
  SCM bookparts_;
  Paper_book *parent_;
  Output_def *paper_;

  Paper_book ();

  Output_def *top_paper ();

  void add_score (SCM);
  void add_bookpart (SCM);
  void add_performance (SCM);

  SCM performances () const;
  SCM systems ();
  SCM pages ();
  SCM get_system_specs ();

  Stencil book_title ();
  Stencil score_title (SCM);

  void classic_output (SCM output_channel);
  void output (SCM output_channel);

protected:
  void classic_output_aux (SCM output, long *first_performance_number);
  long output_aux (SCM output_channel, bool is_last, long *first_page_number,
                   long *first_performance_number);
};

#endif /* PAPER_BOOK_HH */
