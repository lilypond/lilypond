/*   
  grace-performer-group.cc -- implement Grace_performer_group
  
  source file of the GNU LilyPond music playter
  
  (c) 1999--2000 Jan Nieuwenhuizen <janneke@gnu.org>
  
 */
#include "grace-performer-group.hh"
#include "lily-guile.hh"

#include "audio-element.hh"

ADD_THIS_TRANSLATOR (Grace_performer_group);

void
Grace_performer_group::start ()
{
}
/*
  We're really finished with this context. Get rid of everything.
 */
void
Grace_performer_group::finish ()
{
  calling_self_b_ = true;
  removal_processing ();	// ugr. We'd want to have this done by our parents.g
  for (int i=0; i < announce_to_top_.size (); i++)
    {
      Performer::announce_element (announce_to_top_[i]);
    }

  for (int i=0; i < play_us_.size (); i++)
    {
      Performer::play_element (play_us_[i]);
    }
  play_us_.clear ();
  calling_self_b_ = false;
}

void
Grace_performer_group::do_removal_processing ()
{
  Performer_group_performer::do_removal_processing ();
}

void
Grace_performer_group::announce_element (Audio_element_info info)
{
  announce_info_arr_.push (info);
  // do not propagate to top
  announce_to_top_.push (info);

  //inf.elem_l_->set_grob_property ("grace", SCM_BOOL_T);
  info.elem_l_->grace_b_ = true;
}

void
Grace_performer_group::play_element (Audio_element*e)
{
  play_us_.push (e);
}


Grace_performer_group::Grace_performer_group()
{
  calling_self_b_ = false;
}

void
Grace_performer_group::process ()
{
  calling_self_b_  = true;
  //process_music ();
  announces();
  pre_move_processing();
  check_removal();
  calling_self_b_ = false;
}


void
Grace_performer_group::each (Method_pointer method)
{
  if (calling_self_b_)
    Performer_group_performer::each (method);
}




/*
  don't let the commands trickle up.
 */
bool
Grace_performer_group::try_music (Music *m)
{
  return try_music_on_nongroup_children (m);
}

