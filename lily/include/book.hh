/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2026 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

class Book_or_bookpart : public Smob<Book_or_bookpart>
{
public:
  SCM mark_smob () const;
  int print_smob (SCM port, scm_print_state *) const;
  virtual void derived_mark () const { };
  static const char *const type_p_name_;
  virtual ~Book_or_bookpart ();
  SCM header_;
  SCM scores_;    // SCM list; reverse order (most recently added first)

  Book_or_bookpart ();
  Book_or_bookpart (Book_or_bookpart const &);
  Input *origin () const;
  virtual const char * class_name () const = 0;
  virtual Book_or_bookpart *clone () const = 0;
  bool error_found () const;
  void add_score (SCM);
  SCM scope () const { return scm_module_public_interface (scope_module_); }
  SCM scope_module () const { return scope_module_; }
  Output_def *paper () const;
  Output_def *layout () const;
  // We don't need programmatic access for \midi because there are no implicit
  // \midi blocks we would need to reference.
protected:
  SCM input_location_;
  void process_score (SCM score, Paper_book *output_paper_book,
                      Output_def *layout);
private:
  SCM scope_module_;
};

struct Preinit_book {
  SCM bookparts_ = SCM_EOL; // SCM list; reverse order (most recently added
                            // first)
};

class Book : public Preinit_book, public Book_or_bookpart
{
public:
  virtual ~Book ();
  void set_paper (SCM);
  Book () = default;
  Book (Book const &);
  OVERRIDE_CLASS_NAME (Book);
  Book *clone () const override { return new Book (*this); };
  bool error_found () const;
  void add_scores_to_bookpart ();
  void add_bookpart (SCM);
  Paper_book *process (Output_def *def_paper, Output_def *def_layout);
protected:
  void process_bookparts (Paper_book *output_paper_book, Output_def *paper,
                          Output_def *layout);
private:
    void derived_mark () const override;
};

class Bookpart: public Book_or_bookpart
{
public:
  virtual ~Bookpart ();
  Bookpart () = default;
  Bookpart (const Bookpart &) = default;
  OVERRIDE_CLASS_NAME (Bookpart);
  Bookpart *clone () const override { return new Bookpart (*this); };
  void set_parent (Book *parent);

  Paper_book *process (Output_def *default_paper, Output_def *default_layout,
                       Paper_book *parent_part);
};

#endif /* BOOK_HH */
