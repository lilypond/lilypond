/* 
  freetype-error.cc -- implement freetype error messages
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2007--2009 Han-Wen Nienhuys <hanwen@lilypond.org>
  
*/

#include "freetype.hh"

#undef __FTERRORS_H__                                           
#define FT_ERRORDEF(e, v, s)  { e, s },                       
#define FT_ERROR_START_LIST     {                               
#define FT_ERROR_END_LIST       { 0, 0 } };                     
                                                                         
const struct Freetype_error_message
{                                                               
  int          err_code;                                        
  const char*  err_msg;
} ft_errors[] =                                                 
                                                                         
#include <freetype/fterrors.h>

  ;


#include <string>

string
freetype_error_string (int code)
{
  for (Freetype_error_message const *p = ft_errors;
       p->err_msg; p ++)
    {
      if (p->err_code == code)
	return p->err_msg;
    }

  return "<unknown error>"; 
}
  
