/*   
  grace-engraver-group.cc -- implement Grace_engraver_group
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */
#include "grace-engraver-group.hh"
#include "lily-guile.hh"
#include "ly-symbols.hh"
#include "score-element.hh"
#include "musical-request.hh"

void
Grace_engraver_group::start ()
{
}
/*
  We're really finished with this context. Get rid of everything.
 */
void
Grace_engraver_group::finish ()
{
  calling_self_b_ = true;
  removal_processing ();	// ugr. We'd want to have this done by our parents.g
  for (int i=0; i < announce_to_top_.size (); i++)
    {
      Engraver::announce_element (announce_to_top_[i]);
    }

  for (int i=0; i < typeset_us_.size (); i++)
    {
      Engraver::typeset_element (typeset_us_[i]);
    }
  typeset_us_.clear ();
  calling_self_b_ = false;
}

void
Grace_engraver_group::do_removal_processing ()
{
  Engraver_group_engraver::do_removal_processing ();
}

void
Grace_engraver_group::announce_element (Score_element_info inf)
{
  announce_info_arr_.push (inf);
  // do not propagate to top
  announce_to_top_.push (inf);

  inf.elem_l_->set_elt_property (grace_scm_sym, SCM_BOOL_T);
}

void
Grace_engraver_group::typeset_element (Score_element*e)
{
  typeset_us_.push (e);
}


Grace_engraver_group::Grace_engraver_group()
{
  calling_self_b_ = false;
}

void
Grace_engraver_group::process ()
{
  calling_self_b_  = true;
  process_requests ();
  do_announces();
  pre_move_processing();
  check_removal();
  calling_self_b_ = false;
}


void
Grace_engraver_group::each (Method_pointer method)
{
  if (calling_self_b_)
    Engraver_group_engraver::each (method);
}


void
Grace_engraver_group::each (Const_method_pointer method) const
{
 if (calling_self_b_)
    Engraver_group_engraver::each (method);
}
ADD_THIS_TRANSLATOR(Grace_engraver_group);


/*
  don't let the commands trickle up.
 */
bool
Grace_engraver_group::do_try_music (Music *m)
{
  bool hebbes_b = try_music_on_nongroup_children (m);

  if (!hebbes_b && pass_to_top_b (m))
    {
      hebbes_b = daddy_trans_l_->try_music (m);
    }

  return hebbes_b;
}

bool
Grace_engraver_group::pass_to_top_b (Music *m) const
{
  if (Span_req * sp = dynamic_cast<Span_req*> (m))
    {
      if (sp->span_type_str_ == "slur")
	return true;
    }
  return false;
}
