
#ifndef PARSE_SCM
#define PARSE_SCM

#include "input.hh"
#include "lily-guile.hh"

struct Parse_start
{
  char const* str;
  int nchars;
  Input start_location;
};

SCM catch_protected_parse_body (void *);
SCM protected_ly_parse_scm (Parse_start *);

SCM ly_parse_scm (char const* s, int *, Input);

#endif
