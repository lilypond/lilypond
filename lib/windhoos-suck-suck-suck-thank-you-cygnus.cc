//
// windhoos.cc
//
// mmap () should work now (cygnus beta 18), but let's keep it here
// for people using old cygnus releases
#if 0 // def _WINDOWS32

#include <sys/types.h>
#include <sys/mman.h>
#include <winbase.h>
#include "windhoos-suck-suck-suck-thank-you-cygnus.hh"

/* 
HANDLE CreateFileMapping (
    HANDLE hFile,	// handle to file to map 
    LPSECURITY_ATTRIBUTES lpFileMappingAttributes,	// optional security attributes 
    DWORD flProtect,	// protection for mapping object 
    DWORD dwMaximumSizeHigh,	// high-order 32 bits of object size  
    DWORD dwMaximumSizeLow,	// low-order 32 bits of object size  
    LPCTSTR lpName 	// name of file-mapping object 
);	
 

LPVOID MapViewOfFile (
    HANDLE hFileMappingObject,	// file-mapping object to map into address space  
    DWORD dwDesiredAccess,	// access mode 
    DWORD dwFileOffsetHigh,	// high-order 32 bits of file offset 
    DWORD dwFileOffsetLow,	// low-order 32 bits of file offset 
    DWORD dwNumberOfBytesToMap 	// number of bytes to map 
);	
 

io.h:
long _get_osfhandle (int filehandle);
*/

// cygnus's gnu-win32-b17.1 does not have _get_osfhandle
// however, after some hacking, it turns out that:

static const int OSF_OFFSET_i = 72;  
static const int OSF_BASE_i = -3;
static const int OSF_FACTOR_i = 8;  
// let-s hope bill doesn-t change his mind any time soon :-)

// so that, while waiting for cygnus's mmap, we can write:

// #define HAVE_GET_OSFHANDLE  // no we still cannot; works only with cl.exe
long
_get_osfhandle (int filedes_i)
{
    return (long)(OSF_OFFSET_i + (filedes_i + OSF_BASE_i) * OSF_FACTOR_i);
}

#ifdef HAVE_GET_OSFHANDLE

#include <iostream.h>

caddr_t
mmap (caddr_t addr, size_t len, int prot, int flags, int fd, off_t offset)
{
    (void)flags;
    (void)prot;
    (void)addr;
    HANDLE osf = (HANDLE)_get_osfhandle (fd);
    HANDLE file_handle = CreateFileMapping (osf, (void*)0, PAGE_READONLY,
	0, len, 0); 
    return (caddr_t)MapViewOfFile (file_handle, FILE_MAP_READ, 0, offset, len);
}


int
munmap (caddr_t addr, size_t len)
{
    (void)len;
    return UnmapViewOfFile (addr);
}

#else // ! HAVE_GET_OSFHANDLE //

caddr_t
mmap (caddr_t addr, size_t len, int prot, int flags, int fd, off_t offset)
{
    (void)flags;
    (void)prot;
    (void)addr;
    (void)offset;
    char* ch_p = new char[ len ];
    if (ch_p)
        read (fd, (void*)ch_p, len);
    return ch_p;
}


int
munmap (caddr_t addr, size_t len)
{
    (void)len;
    delete (char*)addr;
    return 0;
}

#endif // !HAVE_GET_OSFHANDLE //


#endif // _WINDOWS32 //
