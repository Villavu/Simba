#include <stdio.h>

#include "libmml.h"


int main () {
    long res, i, len;
    void *c;
    struct tpoint t;
    struct tpoint *tpa;

    res = init();

    printf("init() result: %ld\n", res);

    c = create_client();

    get_mouse_pos(c, &t);
    printf("mouse position: (%ld, %ld)\n", t.x, t.y);

#define WHITE (0xFF | 0xFF << 8 | 0xFF << 16)

    /* Watch the &tpa -> it changes the value of the ptr */
    find_colors_tolerance(c, &tpa, &len, WHITE, 0, 0, 0, 100, 100);

    for (i = 0; i < len; i++) {
        printf("(%ld, %ld) ", tpa[i].x, tpa[i].y);
    }
    printf("\n");

    free_ptr(tpa);

    destroy_client(c);

    return 0;
}
