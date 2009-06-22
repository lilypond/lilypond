/*
  beam-setting-scheme.cc -- Retrieving beam settings

  source file of the GNU LilyPond music typesetter

  Copyright (c) 2009 Carl Sorensen <c_sorensen@byu.edu>
*/

#include "beam-settings.hh"
#include "context.hh"
#include "guile-compatibility.hh"

LY_DEFINE (ly_grouping_rules, "ly:grouping-rules",
	   3, 0, 0, (SCM settings, SCM time_signature, SCM rule_type),
	   "Return grouping rules for @var{time-signature} and"
           " @var{rule-type} from @var{settings}.")
{
  LY_ASSERT_TYPE (ly_cheap_is_list, settings, 1);
  LY_ASSERT_TYPE (scm_is_pair, time_signature, 2);
  LY_ASSERT_TYPE (ly_is_symbol, rule_type, 3);

  SCM grouping_rules = SCM_EOL;
  if (scm_is_pair (settings))
    grouping_rules =
        ly_assoc_get (scm_list_2 (time_signature, rule_type),
                      settings,
                      SCM_EOL);
  return grouping_rules;
}

LY_DEFINE (ly_beam_grouping, "ly:beam-grouping",
	   4, 0, 0, (SCM settings, SCM time_signature, SCM rule_type,
                     SCM beam_type),
	   "Return grouping for beams of @var{beam-type} in"
           " @var{time-signature} for"
           " @var{rule-type} from @var{settings}.")
{
  LY_ASSERT_TYPE (ly_cheap_is_list, settings, 1);
  LY_ASSERT_TYPE (scm_is_pair, time_signature, 2);
  LY_ASSERT_TYPE (ly_is_symbol, rule_type, 3);
  SCM_ASSERT_TYPE (scm_is_symbol(beam_type) || scm_is_pair(beam_type),
                   beam_type, SCM_ARG4, __FUNCTION__, "symbol or pair");
  SCM beam_grouping =
        ly_assoc_get (beam_type,
                      ly_grouping_rules (settings,time_signature,rule_type),
                      SCM_EOL);
  return beam_grouping;
}

LY_DEFINE (ly_beat_grouping, "ly:beat-grouping",
	   1, 0, 0, (SCM context),
	   "Return default beat grouping currently active in @var{context}.")
{
  LY_ASSERT_SMOB (Context, context, 1);
  Context *c = unsmob_context (context);
  SCM time_signature =
        c->get_property ("timeSignatureFraction");
  SCM settings =
        c->get_property("beamSettings");
  SCM beat_grouping =
        ly_beam_grouping (settings,
                          time_signature,
                          ly_symbol2scm ("end"),
                          ly_symbol2scm ("*"));
  return beat_grouping;
}

