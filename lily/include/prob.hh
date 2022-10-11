/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022  Jan Nieuwenhuizen <janneke@gnu.org>

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

#ifndef PROPERTY_OBJECT_HH
#define PROPERTY_OBJECT_HH

#include "pitch.hh"
#include "stencil.hh"
#include "virtual-methods.hh"

/*
  A formatted "system" (A block of titling also is a Property_object)

  To save memory, we don't keep around the System grobs, but put the
  formatted content of the grob is put into a
  Property_object. Page-breaking handles Property_object objects.
*/

class Prob : public Smob<Prob>
{
public:
  int print_smob (SCM, scm_print_state *) const;
  SCM mark_smob () const;
  static SCM equal_p (SCM, SCM);
  static const char *const type_p_name_;
  virtual ~Prob ();

private:
  VIRTUAL_CLASS_NAME (Prob);

  void init_vars ();

protected:
  SCM mutable_property_alist_;
  SCM immutable_property_alist_;
  SCM type_;

  virtual void derived_mark () const;
  virtual SCM copy_mutable_properties () const;
  virtual void type_check_assignment (SCM, SCM) const;

public:
  Prob (SCM, SCM);
  Prob (Prob const &);
  virtual std::string name () const;
  SCM type () const { return type_; }
  SCM get_property_alist (bool _mutable) const;
  SCM internal_get_property (SCM sym) const;
  void instrumented_set_property (SCM, SCM, const char *, int, const char *);
  void internal_set_property (SCM sym, SCM val);

  // Needed in both Music and Stream_event
  // For technical reasons defined in lily/music.cc
  //
  /// Transpose, with the interval central C to #p#
  void transpose (Pitch p);
};

SCM ly_prob_set_property_x (SCM system, SCM sym, SCM value);
SCM ly_prob_property (SCM prob, SCM sym, SCM val);

SCM ly_prob_type_p (SCM obj, SCM sym);

#endif /* PROPERTY_OBJECT_HH */
