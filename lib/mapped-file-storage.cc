#ifdef HAIRY_STUFF

/*
  file-storage.cc -- implement Mapped_file_storage

  source file of the GNU LilyPond music typesetter

  (c)  1997--2000 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>.

  Nextstep fixes by tiggr@ics.ele.tue.nl
*/

#include <sys/types.h>		// open, mmap
#include <sys/stat.h>		// open
#include <sys/mman.h>		// mmap
#include <limits.h>		// INT_MAX
#include <fcntl.h>		// open
#include <unistd.h>		// close, stat
#include <stdio.h>		// fdopen
#include <string.h>		// strerror
#include <errno.h>		// errno



#ifdef __NeXT__
#include <mach/mach.h>
#include <mach/mach_traps.h>
#include <mach/mach_error.h>
#endif

#include "string.hh"
#include "proto.hh"
#include "warn.hh"
#include "file-storage.hh"

Mapped_file_storage::Mapped_file_storage (String s)
{
  data_caddr_ = 0;
  fildes_i_ = 0;
  size_off_ = 0;
  open (s);
}

char const*
Mapped_file_storage::ch_C () const
{
  return (char const*)data_caddr_;
}

void
Mapped_file_storage::map ()
{
  if (fildes_i_ == -1)
    return;
  
#ifdef __NeXT__
   /* Should be #if !HAVE_MMAP && HAVE_MAP_FD...  */
   {
     vm_offset_t address;
     kern_return_t r;
 
     r = map_fd (fildes_i_, (vm_offset_t) 0, &address, TRUE, size_off_);
     if (r != KERN_SUCCESS)
       warning (String ("map_fd: ") + mach_error_string (r));
     else
       data_caddr_ = (char *) address;
   }
#else

  data_caddr_ = (caddr_t)mmap ((void*)0, size_off_, PROT_READ, MAP_SHARED, fildes_i_, 0);

  if ((int)data_caddr_ == -1)
    warning (_ ("Can't map file") + ": " + strerror (errno));

#endif
}


void
Mapped_file_storage::open (String name_str)
{
  fildes_i_ = ::open (name_str.ch_C (), O_RDONLY);

  if (fildes_i_ == -1)
    {
      warning (_f ("Can't open file: `%s'", name_str)
	+ ": " + strerror (errno));
      return;
    }

  struct stat file_stat;
  fstat (fildes_i_, &file_stat);
  size_off_ = file_stat.st_size;
  map ();
}

void
Mapped_file_storage::unmap ()
{
  if (data_caddr_)
    {
#ifdef __NeXT__
       kern_return_t r;
 
       r = vm_deallocate (task_self (), (vm_address_t) data_caddr_, 
size_off_);
       if (r != KERN_SUCCESS)
       warning (String ("vm_deallocate: ") + mach_error_string (r));
#else
       munmap (data_caddr_, size_off_);
#endif
       
      data_caddr_ = 0;
      size_off_ = 0;
    }
}

void
Mapped_file_storage::close ()
{
  unmap ();
  if (fildes_i_)
    {
      ::close (fildes_i_);
      fildes_i_ = 0;
    }
}

int
Mapped_file_storage::length_i () const
{
  return size_off_;
}

Mapped_file_storage::~Mapped_file_storage ()
{
  close ();
}
#endif
