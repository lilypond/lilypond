//
// windhoos.cc
//
#ifdef _WIN32

#include <sys/types.h>
#include <winbase.h>

/* 
HANDLE CreateFileMapping(
    HANDLE hFile,	// handle to file to map 
    LPSECURITY_ATTRIBUTES lpFileMappingAttributes,	// optional security attributes 
    DWORD flProtect,	// protection for mapping object 
    DWORD dwMaximumSizeHigh,	// high-order 32 bits of object size  
    DWORD dwMaximumSizeLow,	// low-order 32 bits of object size  
    LPCTSTR lpName 	// name of file-mapping object 
   );	
 

LPVOID MapViewOfFile(
    HANDLE hFileMappingObject,	// file-mapping object to map into address space  
    DWORD dwDesiredAccess,	// access mode 
    DWORD dwFileOffsetHigh,	// high-order 32 bits of file offset 
    DWORD dwFileOffsetLow,	// low-order 32 bits of file offset 
    DWORD dwNumberOfBytesToMap 	// number of bytes to map 
   );	
 
*/

caddr_t
mmap(caddr_t addr, size_t len, int prot, int flags, int fd, off_t offset)
{
    (void)flags;
    (void)prot;
    HANDLE file_handle = CreateFileMapping( fd, (void*)0, PAGE_READONLY,
	0, len, 0 ); 
    return (caddr_t)MapViewOfFile( file_handle, addr, 0, offset, len );
}


int
munmap(caddr_t addr, size_t len)
{
    (void)len;
    return UnmapViewOfFile( addr );
}

#endif // _WIN32 //
