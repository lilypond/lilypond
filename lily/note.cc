/*
  could use cleanup
 */
#include <ctype.h>

#include "my-lily-lexer.hh"
#include "string.hh"
#include "real.hh"
#include "debug.hh"
#include "musical-request.hh"
#include "command-request.hh"
#include "music-list.hh"
#include "identifier.hh"
#include "varray.hh"
#include "text-def.hh"
#include "parseconstruct.hh"



String *
get_scriptdef (char c)
{
  String s;
  switch (c) 
    {
  case '^' : s = "marcato";
	break;
  case  '+' : s = "stopped";
	break;
  case '-' : s = "tenuto";
	break;
  case  '|':  s = "staccatissimo";
	break;
  case  'o' : s = "";
	break;
  case '>' : s = "accent";
	break;
  case  'v' : s = ""; 
	break;
  case  '.' : s = "staccato";
	break;
  default:
	assert (false);
    }
  return new String (s);
}

Request*
get_script_req (int d , General_script_def*def)
{
  Musical_script_req* script_req_p = new Musical_script_req;
  script_req_p->dir_ =d;
  script_req_p->scriptdef_p_=def;
  return script_req_p;
}



Request*
get_stemdir_req (int d)
{
  d = sign (long (d));
  Group_feature_req * gfreq_p = new Group_feature_req;
  gfreq_p->type_str_ = "vdir";
  gfreq_p->value_str_ = String (d);
  return gfreq_p;
}
Request*
get_hshift_req (int i)
{
   Group_feature_req * gfreq_p = new Group_feature_req;
  gfreq_p->type_str_ = "hshift";
  gfreq_p->value_str_ = String (i);
  return gfreq_p;
}
  
Request*
get_grouping_req (Array<int> i_arr)
{
  Measure_grouping_req * mr_p = new Measure_grouping_req;
  for (int i=0; i <i_arr.size();) 
    {
	mr_p->elt_length_arr_.push (Moment (1, i_arr[i++]));
	mr_p->beat_i_arr_.push (i_arr[i++]);
    }
  return mr_p;
}
