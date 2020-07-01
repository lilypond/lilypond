/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "sequential-iterator.hh"
#include "music.hh"
#include "translator-group.hh"
#include "context.hh"
#include "grace-fixup.hh"

/*
  TODO: handling of grace notes is exquisite pain.  This handling
  should be formally specified and then the implementation verified.
*/

/*
  Invariant for the data structure.


  if (scm_is_pair (cursor_))
  iter_->music_ == unsmob<Music> (scm_car (cursor_))
  else
  iter_ == 0;

  The length of musiclist from start to up to cursor_ (cursor_ not
  including), is summed

  here_mom_  = sum (length (musiclist [start ... cursor>))  %)
*/
Sequential_iterator::Sequential_iterator ()
{
  here_mom_ = Moment (0);
  cursor_ = SCM_EOL;
  iter_ = 0;
}

// Sequential music uses elements-callback to just read out the
// elements property and return it.  Overriding get_music_list rather
// than having a different elements-callback default allows action
// that is not exposed to music expression processing since it only
// happens during iteration.
//
// One such action is the use of iterator callbacks: if a function is
// found in the tail of the music list as it is being consumed (like
// when the whole "list" is just a function), it is called with the
// iterator as its argument as a means for getting the actual
// remaining music list to process.
SCM
Sequential_iterator::get_music_list () const
{
  Music *m = get_music ();
  SCM proc = get_property (m, "elements-callback");
  if (ly_is_procedure (proc))
    return scm_call_1 (proc, m->self_scm ());
  else
    return SCM_EOL;
}

void
Sequential_iterator::do_quit ()
{
  if (iter_)
    iter_->quit ();
}

void
Sequential_iterator::derived_mark () const
{
  if (iter_)
    scm_gc_mark (iter_->self_scm ());
  scm_gc_mark (cursor_);
}

void
Sequential_iterator::derived_substitute (Context *f, Context *t)
{
  if (iter_)
    iter_->substitute_outlet (f, t);
}

void Sequential_iterator::Lookahead::look_ahead ()
{
  has_grace_fixup_ = false;

  for (; !has_grace_fixup_ && scm_is_pair (cursor_);
       cursor_ = scm_cdr (cursor_))
    {
      Music *mus = unsmob<Music> (scm_car (cursor_));
      Moment s = mus->start_mom ();
      Moment l = mus->get_length () - s;

      if (s.grace_part_)
        {
          if (last_ != Moment (-1))
            {
              grace_fixup_.start_ = last_;
              grace_fixup_.length_ = here_ - last_;
              grace_fixup_.grace_start_ = s.grace_part_;

              has_grace_fixup_ = true;
            }

          here_.grace_part_ = s.grace_part_;
        }

      if (l.to_bool ())
        {
          last_ = here_;
          here_ += l;
        }
    }
}

void
Sequential_iterator::construct_children ()
{
  Music_iterator::construct_children ();

  cursor_ = get_music_list ();

  iter_ = 0;
  if (ly_is_procedure (cursor_))
    cursor_ = scm_call_1 (cursor_, self_scm ());

  if (scm_is_pair (cursor_))
    {
      Music *m = unsmob<Music> (scm_car (cursor_));
      iter_ = unsmob<Music_iterator> (get_iterator (m));
    }

  while (iter_ && !iter_->ok ())
    next_element ();

  here_mom_ = get_music ()->start_mom ();
  la_.init (cursor_);
  la_.look_ahead ();

  /*
    iter_->ok () is tautology, but what the heck.
  */
  if (iter_ && iter_->ok ())
    descend_to_child (iter_->get_outlet ());
}

/*
  maintain invariants: change cursor, iter and here_mom_ in one fell
  swoop.
*/
void
Sequential_iterator::next_element ()
{
  Moment len = iter_->music_get_length () - iter_->music_start_mom ();
  assert (la_.is_grace_fixup_sane (here_mom_));

  if (auto gf = len.main_part_ ? la_.get_grace_fixup (here_mom_) : nullptr)
    {
      here_mom_ += gf->length_;
      here_mom_.grace_part_ += gf->grace_start_;

      la_.look_ahead ();
    }
  else if (len.grace_part_ && !len.main_part_)
    {
      here_mom_.grace_part_ = 0;
    }
  else
    {
      /*
        !len.grace_part_ || len.main_part_

        We skip over a big chunk (mainpart != 0). Any starting graces
        in that chunk should be in len.grace_part_

      */
      here_mom_ += len;
    }

  cursor_ = scm_cdr (cursor_);

  iter_->quit ();

  if (ly_is_procedure (cursor_))
    cursor_ = scm_call_1 (cursor_, self_scm ());

  if (scm_is_pair (cursor_))
    iter_ = unsmob<Music_iterator> (get_iterator (unsmob<Music> (scm_car (cursor_))));
  else
    iter_ = 0;
}

void
Sequential_iterator::process (Moment until)
{
  while (iter_)
    {
      const Grace_fixup *gf = la_.get_grace_fixup (here_mom_);
      if (gf
          && gf->start_ + gf->length_
          + Moment (Rational (0), gf->grace_start_) == until)
        {
          /*
            do the stuff/note/rest preceding a grace.
          */
          iter_->process (iter_->music_get_length ());
        }
      else
        {
          Moment w = until - here_mom_ + iter_->music_start_mom ();
          iter_->process (w);
        }

      /*
        if the iter is still OK, there must be events left that have

        TIME > LEFT

      */
      if (iter_->ok ())
        return;

      descend_to_child (iter_->get_outlet ());
      next_element ();
    }
}

Moment
Sequential_iterator::pending_moment () const
{
  if (!iter_)
    return Moment (Rational::infinity ());

  Moment cp = iter_->pending_moment ();

  /*
    Fix-up a grace note halfway in the music.
  */
  const Grace_fixup *gf = la_.get_grace_fixup (here_mom_);
  if (gf
      && gf->length_ + iter_->music_start_mom () == cp)
    return here_mom_ + gf->length_ + Moment (0, gf->grace_start_);

  /*
    Fix-up a grace note at  the start of the music.
  */
  return cp + here_mom_ - iter_->music_start_mom ();
}

IMPLEMENT_CTOR_CALLBACK (Sequential_iterator);

bool
Sequential_iterator::run_always () const
{
  return iter_ ? iter_->run_always () : false;
}

const Grace_fixup *
Sequential_iterator::Lookahead::get_grace_fixup (const Moment &m) const
{
  if (has_grace_fixup_ && grace_fixup_.start_ == m)
    return &grace_fixup_;
  else
    return nullptr;
}
