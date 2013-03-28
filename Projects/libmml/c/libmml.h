#ifndef _LIBMML_H
#define _LIBMML_H

#include <stdint.h>

#define LIBMML_RESULT_OK 0
#define LIBMML_RESULT_FALSE 1
#define LIBMML_RESULT_ERROR -1

int init(void); extern libmml;

char* char_last_error(void);

void* create_client(void);
int destroy_client(void* c);

/* Watch the booleans...
 * XXX: We should probably turn this into ``int'' in mml.lpr; because some
 * languages don't have a ``bool'' type the same as pascal. */
int get_debug(void);
void set_debug(int debug);

void* mml_alloc_mem(long size, long objsize);
void* mml_realloc_mem(void* ptr, long size, long objsize);
int mml_free_ptr(void* ptr);

struct tpoint {
    int x, y;
};

int get_mouse_pos(void* c, struct tpoint* t);


#endif
