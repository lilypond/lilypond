/*
  engraver-group-engraver.cc -- implement Engraver_group_engraver
  
  source file of the GNU LilyPond music typesetter

  (c) 1997--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "flower-proto.hh"
#include "engraver-group-engraver.hh"
#include "engraver.hh"
#include "warn.hh"
#include "paper-score.hh"
#include "grob.hh"
#include "context.hh"



void
Engraver_group_engraver::announce_grob (Grob_info info)
{
  announce_infos_.push (info);
  get_daddy_engraver ()->announce_grob (info);
}


SCM find_acknowledge_engravers (SCM gravlist, SCM meta);
SCM find_accept_engravers (SCM gravlist, SCM music_descr);

void
Engraver_group_engraver::acknowledge_grobs ()
{
  if (!announce_infos_.size ())
    return ;
  
  SCM tab = get_property ("acknowledgeHashTable");
  SCM name_sym = ly_symbol2scm ("name");
  SCM meta_sym = ly_symbol2scm ("meta");  

  
  for (int j =0; j < announce_infos_.size (); j++)
    {
      Grob_info info = announce_infos_[j];
      
      SCM meta = info.grob_->internal_get_property (meta_sym);
      SCM nm = scm_assoc (name_sym, meta);
      if (ly_c_pair_p (nm))
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
	  acklist = find_acknowledge_engravers (scm_cons (self_scm (), get_simple_trans_list ()), meta);
	  scm_hashq_set_x (tab, nm, acklist);
	}

      for (SCM p = acklist; ly_c_pair_p (p); p = ly_cdr (p))
	{
	  Translator * t = unsmob_translator (ly_car (p));
	  Engraver * eng = dynamic_cast<Engraver*> (t);
	  if (eng && eng != info.origin_trans_)
	    eng->acknowledge_grob (info);
	}
    }
}


void
Engraver_group_engraver::do_announces ()
{
  do
    {
      engraver_each (get_simple_trans_list (),
		     &Engraver::process_acknowledged_grobs);

      if (!announce_infos_.size ())
	break;

      acknowledge_grobs ();
      announce_infos_.clear ();
    }
  while (1);
}



void
Engraver_group_engraver::initialize ()
{
  SCM tab = scm_make_vector (scm_int2num (61), SCM_BOOL_F);
  context ()->set_property ("acknowledgeHashTable", tab);

  Translator_group::initialize ();
}

Engraver_group_engraver::Engraver_group_engraver () {}

ENTER_DESCRIPTION (Engraver_group_engraver,
/* descr */       "A group of engravers taken together",
/* creats*/       "",
/* accepts */     "",
/* acks  */      "",
/* reads */       "",
/* write */       "");



/*****************/


bool
engraver_valid (Translator*tr, SCM ifaces)
{
  SCM ack_ifs = scm_assoc (ly_symbol2scm ("interfaces-acked"), tr->translator_description ());
  ack_ifs = ly_cdr (ack_ifs);
  for (SCM s = ifaces; ly_c_pair_p (s); s = ly_cdr (s))
    if (scm_c_memq (ly_car (s), ack_ifs) != SCM_BOOL_F)
      return true;
  return false;
}



SCM
find_acknowledge_engravers (SCM gravlist, SCM meta_alist)
{
  SCM ifaces = ly_cdr (scm_assoc (ly_symbol2scm ("interfaces"), meta_alist));

  SCM l = SCM_EOL;
  for (SCM s = gravlist; ly_c_pair_p (s);  s = ly_cdr (s))
    {
      Translator* tr = unsmob_translator (ly_car (s));
      if (engraver_valid (tr, ifaces))
	l = scm_cons (tr->self_scm (), l); 
    }
  l = scm_reverse_x (l, SCM_EOL);

  return l;
}


/* c&p engraver-group.cc */
void
recurse_down_engravers (Context * c, Engraver_method ptr, bool context_first)
{
  Engraver_group_engraver * tg
    = dynamic_cast<Engraver_group_engraver*> (c->implementation ());


  if (!context_first)
    {
      engraver_each (tg->get_simple_trans_list (),
		     ptr);

      (tg->*ptr) ();
    }

  for (SCM s = c->children_contexts () ; ly_c_pair_p (s);
       s =ly_cdr (s))
    {
      recurse_down_engravers (unsmob_context (ly_car (s)), ptr, context_first);
    }

  if (context_first)
    {
      engraver_each (tg->get_simple_trans_list (),
		     ptr);
      (tg->*ptr) ();
    }
}


void
engraver_each (SCM list, Engraver_method method)
{
  for (SCM p = list; ly_c_pair_p (p); p = ly_cdr (p))
    {
      Engraver * e = dynamic_cast<Engraver*>(unsmob_translator (ly_car (p)));
      if (e)
	(e->*method) ();
    }
}
