#include "config.h"

#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#if HAVE_SYS_STAT_H 
#include <sys/stat.h>
#endif
#include <iostream.h>
#include <fstream.h>

#include "stream.hh"
#include "file-path.hh"
#include "warn.hh"
#include "main.hh"

#if __GNUC__ > 2
ostream *
open_file_stream (String filename, std::ios_base::openmode mode)
#else
ostream *
open_file_stream (String filename, int mode)
#endif
{
  ostream *os;
  if ((filename == "-"))
    os = &cout;
  else
    {
      Path p = split_path (filename);
      if (!p.dir.empty_b ())
        if (mkdir (p.dir.ch_C (), 0777) == -1 && errno != EEXIST)
          error (_f ("can't create directory: `%s'", p.dir));
      os = new ofstream (filename.ch_C (), mode);
    }
  if (!*os)
    error (_f ("can't open file: `%s'", filename));
  return os;
}

void
close_file_stream (ostream *os)
{
  *os << flush;
  if (!*os)
    {
      warning (_ ("Error syncing file (disk full?)"));
      exit_status_global = 1;
    }
  if (os != &cout)
    delete os;
  os = 0;
}  
