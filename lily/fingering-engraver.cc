/*   
  fingering-engraver.cc --  implement Fingering_engraver
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1998--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */


#include "engraver.hh"
#include "side-position-interface.hh"
#include "item.hh"
#include "musical-request.hh"
#include "stem.hh"
#include "rhythmic-head.hh"

class Fingering_engraver : public Engraver
{
  Link_array<Music> reqs_;
  Link_array<Item> fingerings_;

  Link_array<Music> up_reqs_;
  Link_array<Music> hor_reqs_;
  Link_array<Music> down_reqs_;
    
public:
  TRANSLATOR_DECLARATIONS(Fingering_engraver);
protected:
  virtual bool try_music (Music* m);
  virtual void stop_translation_timestep ();
  virtual void start_translation_timestep ();
  virtual void process_music ();
  virtual void acknowledge_grob (Grob_info);

  void make_script (Direction, Music*,Axis, int);
};

bool
Fingering_engraver::try_music (Music *m)
{
  if (dynamic_cast<Text_script_req*> (m))
    {
      if (m->get_mus_property ("text-type") != ly_symbol2scm ("finger"))
	return false;
      
      reqs_.push (m);
      return true;
    }
  return false;
}

void
Fingering_engraver::acknowledge_grob (Grob_info inf)
{
  
  if (Stem::has_interface (inf.grob_l_))
    {
      for (int i=0; i < fingerings_.size (); i++)
	{
	  Side_position_interface::add_support (fingerings_[i],inf.grob_l_);
	}
    }
  else if (Rhythmic_head::has_interface (inf.grob_l_))
    {
      Music * mc =inf.music_cause ();
      Pitch * mp = mc? unsmob_pitch (mc->get_mus_property ("pitch")) :0;
      for (int i=0; i < fingerings_.size (); i++)
	{
	  Grob*t = fingerings_[i];
	  Side_position_interface::add_support (t,inf.grob_l_);
	  Pitch *fp = unsmob_pitch (t->get_grob_property ("pitch"));
	  if (fp)
	    {
	      if (!mp)
		continue;
	
	      if (*fp == *mp)
		{
		  Axis other = other_axis (Side_position_interface::get_axis (t));
		  t->set_parent (inf.grob_l_, other);
		}
	    }
	  else
	    {
	      if (!t->get_parent (X_AXIS))
		t->set_parent (inf.grob_l_, X_AXIS);
	    }
	}
    }
}

static int
req_compare (Music * const &a,Music * const &b)
{
  Pitch *pa  = unsmob_pitch (a->get_mus_property ("pitch"));
  Pitch *pb  = unsmob_pitch (b->get_mus_property ("pitch"));

  if (!pa && !pb)
    return 0;
  if (pa && !pb)
    return 1;
  if (!pa && pb)
    return -1;

  return Pitch::compare (*pa, *pb);
}

void
Fingering_engraver::process_music ()
{
  if (!reqs_.size())
    return ;
  
  Link_array<Music> pitch_sorted_reqs = reqs_;
  for (int i= pitch_sorted_reqs.size(); i--;)
    {
      SCM dir = pitch_sorted_reqs[i]->get_mus_property ("direction");
      if (ly_dir_p (dir) && to_dir (dir)) {
	if (to_dir (dir) == UP)
	  up_reqs_.push (pitch_sorted_reqs[i]);
	else if (to_dir (dir) == DOWN)
	  down_reqs_ .push (pitch_sorted_reqs[i]);
	pitch_sorted_reqs.del(i);

	continue ; 
      }
      else if (!unsmob_pitch (pitch_sorted_reqs[i]->get_mus_property ("pitch")))
	{
	  /*
	    chuck out reqs that have no pitch.  We put them over the note by default.
	  */
	  up_reqs_.push (pitch_sorted_reqs [i]);
	  pitch_sorted_reqs.del (i);
	}
    }  
  up_reqs_.reverse ();
  down_reqs_.reverse ();
  
  pitch_sorted_reqs.sort (&req_compare);

  if (to_boolean (get_property ("scriptHorizontal")))
    {
#if 1 // -> 0 for testing horizontal fingerings.
      
      down_reqs_.push  ( pitch_sorted_reqs[0]);
      pitch_sorted_reqs.del (0);

      if (pitch_sorted_reqs.size())
	{
	  up_reqs_.push (pitch_sorted_reqs.top ());
	  pitch_sorted_reqs.pop();
	}
#endif
      hor_reqs_ = pitch_sorted_reqs;
    }
  else
    {
      int sz = pitch_sorted_reqs.size ();
      down_reqs_.concat (pitch_sorted_reqs.slice(0, (sz + sz%2)/2 ));
      up_reqs_.concat (pitch_sorted_reqs.slice((sz + sz%2)/2, sz));
      hor_reqs_ .clear ();
    }

  for (int i = 0; i < down_reqs_.size();i++)
    make_script (DOWN,  down_reqs_[i], Y_AXIS, i);
  for (int i = 0; i < up_reqs_.size();i++)
    make_script (UP,   up_reqs_[i], Y_AXIS, i);
  for (int i = 0; i < hor_reqs_.size();i++)
    make_script (CENTER,  hor_reqs_[i],X_AXIS, i);
}

void
Fingering_engraver::make_script (Direction d, Music *r,Axis a,  int i)
{
  Item *fingering = new Item (get_property ("Fingering"));

  Axis other = other_axis (a);

  SCM pitch = r->get_mus_property ("pitch");
  if (unsmob_pitch (pitch))
    fingering->set_grob_property ("pitch", pitch);
  
  Side_position_interface::set_axis (fingering, a);
      
  fingering->add_offset_callback (Side_position_interface::aligned_on_self_proc, other);
  fingering->add_offset_callback (Side_position_interface::centered_on_parent_proc, other);
  fingering->set_grob_property ("script-priority",
				gh_int2scm (100 + d* i));


  if (!ly_dir_p (fingering->get_grob_property ("direction")))
    {
      if (d)
	fingering->set_grob_property ("direction", gh_int2scm (d));
      else
	fingering->set_grob_property ("direction",  gh_int2scm (RIGHT));
    }
  
  fingering->set_grob_property ("text", r->get_mus_property ("text"));
  
  SCM nonempty = get_property ("textNonEmpty");
  if (to_boolean (nonempty))
    /*
      empty fingering: signal that no rods should be applied.  
    */
    fingering->set_grob_property ("no-spacing-rods" , SCM_BOOL_F);
		
  announce_grob (fingering, r->self_scm());
  fingerings_.push (fingering);
}

void
Fingering_engraver::stop_translation_timestep ()
{
  if (!fingerings_.size ())
    return;
  
  for (int i=0; i < fingerings_.size (); i++)
    {
      Item *ti = fingerings_[i];
      Side_position_interface::add_staff_support (ti);
      typeset_grob (ti);
    }
  fingerings_.clear ();
}

void
Fingering_engraver::start_translation_timestep ()
{
  reqs_.clear ();
  up_reqs_.clear ();
  down_reqs_.clear ();
  hor_reqs_.clear ();
}

Fingering_engraver::Fingering_engraver()
{

}

ENTER_DESCRIPTION(Fingering_engraver,
/* descr */       "Create fingering-scripts",
/* creats*/       "Fingering",
/* acks  */       "rhythmic-head-interface stem-interface",
/* reads */       "scriptHorizontal textNonEmpty",
/* write */       "");
