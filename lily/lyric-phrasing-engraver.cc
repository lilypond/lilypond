/*
  lyric-phrasing-engraver.cc -- implement Lyric_phrasing_engraver

  source file of the GNU LilyPond music typesetter

  (c)  2000 Glen Prideaux <glenprideaux@iname.com>
*/
#include <string.h>

#include "lyric-phrasing-engraver.hh"
#include "note-head.hh"
#include "translator-group.hh"
#include "spanner.hh"
#include "warn.hh"


String get_context_id (Translator_group * ancestor, const char * type);
String trim_suffix (String &id);



/*
  TODO: this code is too hairy, and does things that should be in the
  backend. Fixme.
*/

/*
  TODO:

  shared lyrics should be vertically centered:

  

  > About lyrics, it happens that there are common words for many bars, like
  > for a refrain say.  When there is an even number of lyrics lines, I do not
  > know how to force the positioning of the common lyric line in the plain
  > middle of the others, because this is in between lines.  Not a big matter,
  > but it would be a bit nicer if this was doable.

*/

/*
  We find start and end of phrases, and align lyrics of multiple stanzas
  accordingly.

  Also, lyrics at start of melismata should be left aligned.
  (is that only lyrics that are followed by `__'?  Because
  that seems to be the case now -- jcn)


  |        |        |     |      |
  x|       x|       x|    x|     x|

  1:  Start  sentence  melisma      end.
  2:  x         x         x_____       x

  Only lyrics that are followed by '__' while there's a melisma,
  are left-aligned, in this case the third x.

  
  Alignment and melismata

  I've taken [a different] approach:
  |      |
  |      |
  O      O  <-- second note throws a melisma score element
  \____/

  ^      ^
  |      |
  Lyric (None)

  Lyric_phrasing_engraver keeps track of the current and previous notes and
  lyrics for each voice, and when it catches a melisma, it adjusts the
  alignment of the lyrics of the previous note. I hope this isn't
  unnecessarily convoluted.
*/

Lyric_phrasing_engraver::Lyric_phrasing_engraver ()
{
  voice_alist_ = SCM_EOL;
  any_notehead_ = 0;
}

Lyric_phrasing_engraver::~Lyric_phrasing_engraver ()
{
  /*
    No need to delete alist_; that's what Garbage collection is for.
  */
}

void
Lyric_phrasing_engraver::finalize ()
{
  /*
    but do need to unprotect alist_, since Engravers are gc'd now.
  */

  voice_alist_ = SCM_EOL;
}


Syllable_group * 
Lyric_phrasing_engraver::lookup_context_id (const String &context_id)
{
  SCM key = scm_makfrom0str (context_id.to_str0 ());
  if (! gh_null_p (voice_alist_))
    {
      SCM s = scm_assoc (key, voice_alist_);
      if (! (gh_boolean_p (s) && !to_boolean (s)))
	{
	  /* match found */
	  // (key . ((alist_entry . old_entry) . previous_entry))
	  if (to_boolean (ly_cdadr (s)))
	    {
	      // it's an old entry ... make it a new one
	      SCM val = gh_cons (gh_cons (ly_caadr (s), SCM_BOOL_F), ly_cddr (s)); 
	      voice_alist_ = scm_assoc_set_x (voice_alist_, ly_car (s), val);
	      return unsmob_voice_entry (ly_caar (val));
	    }
	  else
	    {
	      // the entry is current ... return it.
	    SCM entry_scm = ly_caadr (s);
	    return unsmob_voice_entry (entry_scm);
	  }
	}
    }
  // ((alist_entry . old_entry) . previous_entry)
  SCM val = gh_cons (gh_cons (Syllable_group::make_entry (), SCM_BOOL_F), 
		     Syllable_group::make_entry ()); 

  voice_alist_ = scm_acons (key, val, voice_alist_);
  return unsmob_voice_entry (ly_caar (val));
}


void 
Lyric_phrasing_engraver::record_notehead (const String &context_id, 
					  Grob * notehead)
{
  Syllable_group * v = lookup_context_id (context_id);
  v->set_notehead (notehead);
  if (!any_notehead_)
    any_notehead_ = notehead;
}
  
void 
Lyric_phrasing_engraver::record_lyric (const String &context_id, Grob * lyric)
{
  Syllable_group * v = lookup_context_id (context_id);
  v->add_lyric (lyric);
}

void 
Lyric_phrasing_engraver::record_extender (const String &context_id, Grob * extender)
{
  SCM key = scm_makfrom0str (context_id.to_str0 ());
  if (! gh_null_p (voice_alist_))
    {
      SCM s = scm_assoc (key, voice_alist_);
      if (! (gh_boolean_p (s) && !to_boolean (s)))
	{
	  /* match found */
	  // (key . ((alist_entry . old_entry) . previous_entry))
	  SCM previous_scm = ly_cddr (s);
	  if (previous_scm != SCM_EOL)
	    {
	      Syllable_group * v = unsmob_voice_entry (previous_scm);
	      v->add_extender (extender);
	    }
	}
    }
}

void 
Lyric_phrasing_engraver::record_melisma (const String &context_id)
{
  Syllable_group * v = lookup_context_id (context_id);
  v->set_melisma ();
}

/*
  TODO: this engraver is always on, also for orchestral scores. That
  is a waste of time and space. This should be switched on
  automatically at the first Lyrics found.
 */
void
Lyric_phrasing_engraver::acknowledge_grob (Grob_info i)
{
  SCM p = get_property ("automaticPhrasing");
  if (!to_boolean (p))
    return;


  Grob *h = i.grob_;

  if (Note_head::has_interface (h))
    {
      /* caught a note head ... do something with it */

      /* what's its Voice context name? */
      String voice_context_id = get_context_id (i.origin_trans_->daddy_trans_, "Voice");
      record_notehead (voice_context_id, h);

      /* is it in a melisma ? */
      if (to_boolean (i.origin_trans_->get_property ("melismaEngraverBusy")))
	{
	  record_melisma (voice_context_id);
	}
      return;
    }

  /* now try for a lyric */
  if (h->internal_has_interface (ly_symbol2scm ("lyric-syllable-interface")))
    {

      /* what's its LyricsVoice context name? */
      String voice_context_id;
      SCM voice_context_scm = i.origin_trans_->get_property ("associatedVoice");
      if (gh_string_p (voice_context_scm))
	{
	  voice_context_id = ly_scm2string (voice_context_scm);
	}
      else
	{
	  voice_context_id = get_context_id (i.origin_trans_->daddy_trans_, "LyricsVoice");
	  voice_context_id = trim_suffix (voice_context_id);
	}
      record_lyric (voice_context_id, h);
      return;
    }

  /* Catch any extender items and then if we have a melisma, 
     set the RIGHT item of the extender spanner to the melismatic note in 
     the corresponding context (if any).
     This has the effect of finishing the extender under the last note
     of the melisma, instead of extending it to the next lyric.
     
     Problem: the extender request is thrown at the same moment as the next lyric,
     by which time we have already passed the last note of the melisma.
     However, the Lyric_phrasing_engraver remembers the last note, so just 
     attach it to that, provided it was melismatic. If it was not melismatic, 
     then ignore it and let the Extender_engraver take care of it (i.e. finish at next
     lyric).
  */
  if (h->internal_has_interface (ly_symbol2scm ("lyric-extender-interface")))
    {
      String voice_context_id = get_context_id (i.origin_trans_->daddy_trans_, "LyricsVoice");
      record_extender (trim_suffix (voice_context_id), h);
      return;
    }
}

String 
get_context_id (Translator_group * ancestor, const char *type)
{
  while (ancestor != 0 && ancestor->type_string_ != type)
    {
      ancestor = ancestor->daddy_trans_;
    }

  if (ancestor != 0)
    {
      return ancestor->id_string_;
    }

  return "";
}

String 
trim_suffix (String &id)
{
  int index = id.index ('-');
  if (index >= 0)
    {
      return id.left_string (index);
    }
  return id;
}


void
Lyric_phrasing_engraver::process_acknowledged_grobs () 
{
  SCM p = get_property ("automaticPhrasing");
  if (!to_boolean (p))
    return;

  
  /* iterate through entries in voice_alist_
     for each, call set_lyric_align (alignment). Issue a warning if this returns false.
  */
  String punc;
  SCM sp = get_property ("phrasingPunctuation");
  punc = gh_string_p (sp) ? ly_scm2string (sp) : ".,;:?!\""; 
  
  for (SCM v=voice_alist_; gh_pair_p (v); v = ly_cdr (v))
    {
      SCM v_entry = ly_cdar (v);
      // ((current . oldflag) . previous)
      if (!to_boolean (ly_cdar (v_entry)))
	{ 
	  // not an old entry left over from a prior note ...
	  Syllable_group *entry = unsmob_voice_entry (ly_caar (v_entry));

	  /*
	    TODO: give context for warning.
	  */
	  if (! entry->set_lyric_align (punc.to_str0 (), any_notehead_))
	    warning (_ ("lyrics found without any matching notehead"));

	  // is this note melismatic? If so adjust alignment of previous one.
	  if (entry->get_melisma ())
	    {
	      if (entry->lyric_count ())
		warning (_ ("Huh? Melismatic note found to have associated lyrics."));
	      SCM previous_scm = ly_cdr (v_entry);
	      if (previous_scm != SCM_EOL)
		{
		  Syllable_group *previous = unsmob_voice_entry (previous_scm);
		  if (previous->lyric_count ())
		    previous->adjust_melisma_align ();
		}
	    }
	}
    }
}


void
Lyric_phrasing_engraver::stop_translation_timestep ()
{
  for (SCM v=voice_alist_; gh_pair_p (v); v = ly_cdr (v))
    {
      SCM entry_scm = ly_cdar (v);
      // ((alist_entry . entry_is_old) . previous_entry)
      Syllable_group * entry = unsmob_voice_entry (ly_caar (entry_scm));

      // set previous_entry, set entry_is_old, and resave it to alist_
      // but only change if this current was not old.
      if (! to_boolean (ly_cdar (entry_scm)))
	{ 
	  Syllable_group * previous_entry = unsmob_voice_entry (ly_cdr (entry_scm));
	  previous_entry->copy (entry);
	  entry_scm = gh_cons (gh_cons (ly_caar (entry_scm), SCM_BOOL_T), ly_cdr (entry_scm));
	  voice_alist_ = scm_assoc_set_x (voice_alist_, ly_caar (v), entry_scm);
	}
      entry->next_lyric ();
    }
  any_notehead_ = 0;
}



ENTER_DESCRIPTION(Lyric_phrasing_engraver,
		  /* descr */       "
This engraver combines note heads and lyrics for alignment.

This engraver is switched on by default. Turn it off for faster
processing of orchestral scores.
",
		  /* creats*/       "",
		  /* accepts */     "general-music",
/* acks  */      "lyric-syllable-interface note-head-interface lyric-extender-interface",
		  /* reads */       "automaticPhrasing melismaEngraverBusy associatedVoice phrasingPunctuation",
		  /* write */       "");
