/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2002--2015 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "music.hh"
#include "sequential-iterator.hh"
#include "context.hh"
#include "lily-imports.hh"

class Volta_repeat_iterator : public Sequential_iterator
{
public:
  DECLARE_SCHEME_CALLBACK (constructor, ());
  Volta_repeat_iterator ();

  void add_repeat_command (SCM);
protected:
  virtual SCM get_music_list () const;
  virtual void next_element (bool);
  virtual void construct_children ();
  virtual void process (Moment);
  virtual void derived_mark () const;

  bool first_time_;
  int alt_count_;
  int rep_count_;
  int done_count_;
  SCM alt_restores_;
};

Volta_repeat_iterator::Volta_repeat_iterator ()
{
  done_count_ = alt_count_ = rep_count_ = 0;
  first_time_ = true;
  alt_restores_ = SCM_EOL;
}

void
Volta_repeat_iterator::derived_mark () const
{
  scm_gc_mark (alt_restores_);
  Sequential_iterator::derived_mark ();
}

SCM
Volta_repeat_iterator::get_music_list ()const
{
  return scm_cons (get_music ()->get_property ("element"),
                   Sequential_iterator::get_music_list ());
}

void
Volta_repeat_iterator::construct_children ()
{
  Sequential_iterator::construct_children ();

  SCM alts = get_music ()->get_property ("elements");

  alt_count_ = int (scm_ilength (alts));
  rep_count_ = scm_to_int (get_music ()->get_property ("repeat-count"));
  done_count_ = 0;
}

/*
  TODO: add source information for debugging
*/
void
Volta_repeat_iterator::add_repeat_command (SCM what)
{
  SCM reps = ly_symbol2scm ("repeatCommands");
  SCM current_reps = SCM_EOL;
  Context *where = get_outlet ()->where_defined (reps, &current_reps);

  if (where && ly_cheap_is_list (current_reps))
    {
      current_reps = scm_cons (what, current_reps);
      where->set_property (reps, current_reps);
    }
}

void
Volta_repeat_iterator::next_element (bool side_effect)
{
  done_count_++;

  Sequential_iterator::next_element (side_effect);

  if (side_effect)
    {
      if (alt_count_)
        {
          string repstr = ::to_string (rep_count_ - alt_count_ + done_count_) + ".";
          if (done_count_ <= 1)
            {
              alt_restores_ = SCM_EOL;
              if (to_boolean (get_outlet ()->get_property ("timing")))
                {
                  for (SCM lst = get_outlet ()->get_property ("alternativeRestores");
                       scm_is_pair (lst);
                       lst = scm_cdr (lst))
                    {
                      SCM res = SCM_EOL;
                      Context *t = get_outlet ()->where_defined (scm_car (lst),
                                                                 &res);
                      if (t)
                        {
                          alt_restores_ = scm_cons
                            (scm_list_3 (t->self_scm (), scm_car (lst), res),
                             alt_restores_);
                        }
                    }
                }
            }
          else
            {

              add_repeat_command (scm_list_2 (ly_symbol2scm ("volta"), SCM_BOOL_F));

              if (done_count_ - 1 < alt_count_)
                {
                  add_repeat_command (ly_symbol2scm ("end-repeat"));

                  if (to_boolean (get_outlet ()->get_property ("timing")))
                    {
                      for (SCM p = alt_restores_; scm_is_pair (p); p = scm_cdr (p))
                        scm_apply_0 (Lily::ly_context_set_property_x,
                                     scm_car (p));
                    }
                }
            }

          if (done_count_ == 1 && alt_count_ < rep_count_)
            repstr = "1.--" + ::to_string (rep_count_ - alt_count_ + done_count_) + ".";

          if (done_count_ <= alt_count_)
            add_repeat_command (scm_list_2 (ly_symbol2scm ("volta"),
                                            ly_string2scm (repstr)));
        }
      else
        add_repeat_command (ly_symbol2scm ("end-repeat"));
    }
}

void
Volta_repeat_iterator::process (Moment m)
{
  if (first_time_)
    {
      add_repeat_command (ly_symbol2scm ("start-repeat"));
      first_time_ = false;
    }
  Sequential_iterator::process (m);
}

IMPLEMENT_CTOR_CALLBACK (Volta_repeat_iterator);
