#include <stdio.h>

#include "libmml.h"


#define DEBUG_TRUE 1
#define DEBUG_FALSE 0

int main () {
    int res, i, len;
    void *c;
    struct tpoint t;
    struct tpoint *tpa;

    tpa = NULL;
    res = init();

    printf("init() result: %d\n", res);

    set_debug(DEBUG_TRUE);
    printf("get_debug: %d\n", get_debug());

    c = create_client();

    get_mouse_pos(c, &t);
    printf("mouse position: (%d, %d)\n", t.x, t.y);

#define WHITE (0xFF | 0xFF << 8 | 0xFF << 16)
    /* Watch the &tpa -> it changes the value of the ptr */
    find_colors_tolerance(c, &tpa, &len, WHITE, 0, 0, 0, 100, 100);

    for (i = 0; i < len; i++) {
        printf("(%d, %d) ", tpa[i].x, tpa[i].y);
    }
    printf("\n");

    if (tpa) {
        free_ptr(tpa);
    }

    destroy_client(c);

    return 0;
}
