#include "config.h"

#include <stdlib.h>
#include <errno.h>
#include <sys/types.h>
#if HAVE_SYS_STAT_H 
#include <sys/stat.h>
#endif

#include <iostream>
#include <fstream>

#include "stream.hh"
#include "file-path.hh"
#include "warn.hh"
#include "main.hh"

#if __GNUC__ > 2
std::ostream *
open_file_stream (String filename, std::ios_base::openmode mode)
#else
std::ostream *
open_file_stream (String filename, int mode)
#endif
{
  std::ostream *os;
  if ((filename == "-"))
    os = &std::cout;
  else
    {
      Path p = split_path (filename);
      if (!p.dir.empty_b ())
        if (mkdir (p.dir.ch_C (), 0777) == -1 && errno != EEXIST)
          error (_f ("can't create directory: `%s'", p.dir));
      os = new std::ofstream (filename.ch_C (), mode);
    }
  if (!*os)
    error (_f ("can't open file: `%s'", filename));
  return os;
}

void
close_file_stream (std::ostream *os)
{
  *os << std::flush;
  if (!*os)
    {
      warning (_ ("Error syncing file (disk full?)"));
      exit_status_global = 1;
    }
  if (os != &std::cout)
    delete os;
  os = 0;
}  
