/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2010 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef SEQUENTIAL_ITERATOR_HH
#define SEQUENTIAL_ITERATOR_HH

#include "music-iterator.hh"
#include "protected-scm.hh"

/** Sequential_music iteration: walk each element in turn, and
    construct an iterator for every element.
*/
class Sequential_iterator : public Music_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  DECLARE_CLASSNAME(Sequential_iterator);
  Sequential_iterator ();
  Sequential_iterator (Sequential_iterator const &);
  virtual void derived_substitute (Context *f, Context *t);

  virtual void derived_mark () const;

  virtual void construct_children ();
  virtual Moment pending_moment () const;
  virtual void do_quit ();
  virtual bool ok () const;

protected:
  virtual void process (Moment);
  virtual bool run_always () const;
  
protected:
  Music_iterator *iter_;
  
  virtual SCM get_music_list () const;
  virtual void next_element (bool side_effect);

  Grace_fixup *get_grace_fixup () const;
  void next_grace_fixup ();

private:
  Moment last_mom_;
  Moment here_mom_;
  SCM cursor_;
  Grace_fixup *grace_fixups_;
};

#endif /* SEQUENTIAL_ITERATOR_HH */
