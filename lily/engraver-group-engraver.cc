/*
  engravergroup.cc -- implement Engraver_group_engraver

  source file of the GNU LilyPond music typesetter

  (c)  1997--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "flower-proto.hh"
#include "engraver-group-engraver.hh"
#include "engraver.hh"
#include "debug.hh"
#include "paper-score.hh"
#include "grob.hh"

void
Engraver_group_engraver::announce_grob (Grob_info info)
{
  announce_info_arr_.push (info);
  Engraver::announce_grob (info);
}


void
Engraver_group_engraver::process_acknowledged_grobs_in_simple_children ()
{
  for (SCM p = simple_trans_list_; gh_pair_p (p); p = ly_cdr (p))
    {
      Translator * t = unsmob_translator (ly_car (p));
      Engraver * eng = dynamic_cast<Engraver*> (t);
      if (eng)
	eng->process_acknowledged_grobs ();
    }
}

/*

  Done: eliminating useless broadcast/acknowledge


One cause for translation slowness: grob broadcasted/acknowledges
 (b/a): every grob is b/a-ed to all peer-engravers and all
parent-engravers. This means that lots of (often) useless b/a is done
for large scores (the top-level engravers gets to know every detail of
every voice, thread, etc. Measurements indicate this is 10% of the
 interpretation time:

 standchen

old: (pre 1.5.13)  10.28
new: 8.73
speedup: 15 %

Coriolan:

new: 197.59
old: 219.12 seconds
speedup: 10%


The cost of this B/A is # of useless engravers * cost of one ack,
which is rather low, since cost of one ack is only an interface check.
The cost of precomputing engraver lists has two elts: computing the
list itself, GC for the structure, looking up the list during the
acks.

*/
SCM find_acknowledge_engravers (SCM gravlist, SCM meta);
void
Engraver_group_engraver::acknowledge_grobs ()
{
  if (!announce_info_arr_.size ())
    return ;
  
  SCM tab = get_property ("acknowledgeHashTable");
  SCM name_sym = ly_symbol2scm ("name");
  SCM meta_sym = ly_symbol2scm ("meta");  

  
  for (int j =0; j < announce_info_arr_.size (); j++)
    {
      Grob_info info = announce_info_arr_[j];
      
      SCM meta = info.grob_l_->internal_get_grob_property (meta_sym);
      SCM nm = scm_assoc (name_sym, meta);
      if (gh_pair_p (nm))
	nm = ly_cdr (nm);
      else
	{
	  /*
	    it's tempting to put an assert for
	    immutable_property_alist_ == '(), but in fact, some
	    engravers (clef-engraver) add some more information to the
	    immutable_property_alist_ (after it has been '()-ed).

	    We ignore the grob anyway. He who has no name, shall not
	    be helped.  */
	  
	  continue;
	}
 
      SCM acklist = scm_hashq_ref (tab, nm, SCM_UNDEFINED);
      if (acklist == SCM_BOOL_F)
	{
	  acklist= find_acknowledge_engravers (gh_cons (self_scm (), simple_trans_list_), meta);
	  scm_hashq_set_x (tab, nm, acklist);
	}

      for (SCM p = acklist; gh_pair_p (p); p = ly_cdr (p))
	{
	  Translator * t = unsmob_translator (ly_car (p));
	  Engraver * eng = dynamic_cast<Engraver*> (t);
	  if (eng && eng!= info.origin_trans_l_)
	    eng->acknowledge_grob (info);
	}
    }
}

void
Engraver_group_engraver::do_announces ()
{
  for (SCM p = trans_group_list_; gh_pair_p (p); p =ly_cdr (p))
    {
      Translator * t = unsmob_translator (ly_car (p));
      dynamic_cast<Engraver_group_engraver*> (t)->do_announces ();
    }

   process_acknowledged_grobs_in_simple_children ();
    
   do
    {
      acknowledge_grobs ();
      announce_info_arr_.clear ();
      process_acknowledged_grobs_in_simple_children ();
    }
   while (announce_info_arr_.size ());
}



/*
  order is : top to bottom (as opposed to do_announces)
 */
void
Engraver_group_engraver::process_music ()
{
   for (SCM p = simple_trans_list_; gh_pair_p (p); p =ly_cdr (p))
    {
      Translator * t = unsmob_translator (ly_car (p));
      Engraver * eng = dynamic_cast<Engraver*> (t);

      if (eng)
	eng->process_music ();
    }
   for (SCM p = trans_group_list_; gh_pair_p (p); p =ly_cdr (p))
    {
      Translator * t = unsmob_translator (ly_car (p));
      Engraver*eng = dynamic_cast<Engraver*> (t);
      if (eng)
	eng->process_music ();
    }
}

void
Engraver_group_engraver::initialize ()
{
  SCM tab = scm_make_vector (gh_int2scm (61), SCM_BOOL_F); // magic ->
  set_property ("acknowledgeHashTable", tab);

  Translator_group::initialize ();
}

Engraver_group_engraver::Engraver_group_engraver() {}

ENTER_DESCRIPTION(Engraver_group_engraver,
/* descr */       "A group of engravers taken together",
/* creats*/       "",
/* acks  */       "",
/* reads */       "",
/* write */       "");



/*****************/


bool engraver_valid (Translator*tr, SCM ifaces)
{
  SCM ack_ifs = scm_assoc (ly_symbol2scm ("interfaces-acked"), tr->translator_description());
  ack_ifs = gh_cdr (ack_ifs);
  for (SCM s = ifaces; ly_pair_p (s); s = ly_cdr (s))
    if (scm_memq (ly_car (s), ack_ifs) != SCM_BOOL_F)
      return true;
  return false;
}


SCM
find_acknowledge_engravers (SCM gravlist, SCM meta_alist)
{
  SCM ifaces = gh_cdr (scm_assoc (ly_symbol2scm ("interfaces"), meta_alist));

  SCM l = SCM_EOL;
  for (SCM s = gravlist; ly_pair_p (s);  s = ly_cdr (s))
    {
      Translator* tr = unsmob_translator (ly_car (s));
      if (engraver_valid (tr, ifaces))
	l = scm_cons (tr->self_scm (), l); 
    }
  l = scm_reverse_x (l, SCM_EOL);

  return l;
}
