/*
  file-storage.hh -- declare File_storage, Mapped_file_storage, Simple_file_storage

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef FILE_STORAGE_HH
#define FILE_STORAGE_HH

#include "proto.hh"

/**
  store a file in-memory.
 */
class File_storage
{
public:
    virtual char const* ch_C()const=0;
    virtual int length_i()const=0;
    virtual ~File_storage(){}
};

/**
  Use mmap to "copy"  a file into memory
 */
class Mapped_file_storage:public File_storage
{
public:
    Mapped_file_storage(String);    
protected:
    virtual char const* ch_C()const;
    virtual int length_i()const;
    virtual ~Mapped_file_storage();
private:
    void open(String name);
    void close();

    void map();
    void unmap();
    int fildes_i_;
    off_t size_off_;
    caddr_t data_caddr_;
};

/**
  read file char by char and copy into a malloc array.
 */
class Simple_file_storage  : public File_storage
{
    char * data_p_;
    int len_i_;
protected:    
    virtual char const*ch_C()const;
    virtual int length_i()const;
    virtual ~Simple_file_storage();
public:
    Simple_file_storage(String);
};
#endif // FILE_STORAGE_HH
