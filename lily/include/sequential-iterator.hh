/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "grace-fixup.hh"
#include "protected-scm.hh"

/** Sequential_music iteration: walk each element in turn, and
    construct an iterator for every element.
*/
class Sequential_iterator : public Music_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  OVERRIDE_CLASS_NAME (Sequential_iterator);
  Sequential_iterator ();
  Sequential_iterator (Sequential_iterator const &);
  void derived_substitute (Context *f, Context *t) override;

  void derived_mark () const override;

  void construct_children () override;
  Moment pending_moment () const override;
  void do_quit () override;

protected:
  void process (Moment) override;
  bool run_always () const override;

protected:
  virtual SCM get_music_list () const;
  virtual void next_element ();

private:
  class Lookahead
  {
  private:
    Moment here_;
    Moment last_ = -1;
    SCM cursor_;

    Grace_fixup grace_fixup_;
    bool has_grace_fixup_ = false;

  public:
    Lookahead () = default;

    void init (SCM cursor) { cursor_ = cursor; }
    void look_ahead ();

    const Grace_fixup *get_grace_fixup (const Moment &) const;

    bool is_grace_fixup_sane (const Moment &m) const
    {
      return !has_grace_fixup_ || (grace_fixup_.start_ >= m);
    }
  };

private:
  Music_iterator *iter_;
  Moment here_mom_;
  SCM cursor_;
  Lookahead la_;
};

#endif /* SEQUENTIAL_ITERATOR_HH */
