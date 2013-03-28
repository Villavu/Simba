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

void* alloc_mem(long size, long objsize);
void* realloc_mem(void* ptr, long size, long objsize);
int free_ptr(void* ptr);

struct tpoint {
    int x, y;
};

/* Mouse functions */

int get_mouse_pos(void *c, struct tpoint* t);
int set_mouse_pos(void *c, struct tpoint* t);

int get_mouse_button_state(void *c, int but);

/* XXX: why x, y here? Why not struct tpoint *t ?*/
int set_mouse_button_state(void *c, int but, int x, int y);



/* Colour functions */

int get_color(void *c, int x, int y, int *col);
int find_color(void *c, int *x, int *y, int col, int x1, int y1, int x2,
        int y2);
int find_color_tolerance(void *c, int *x, int *y, int col, int tol, int x1,
        int y1, int x2, int y2);
int find_colors(void *c, struct tpoint *tpa, int* len, int col, int x1,
        int y1, int x2, int y2);
int find_colors_tolerance(void *c, struct tpoint *tpa, int* len, int col,
        int tol, int x1, int y1, int x2, int y2);

#endif
