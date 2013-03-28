#include <stdio.h>

#include "libmml.h"

int main () {
    long res;
    void *c;
    struct tpoint t;

    res = init();

    printf("init() result: %ld\n", res);

    c = create_client();

    get_mouse_pos(c, &t);

    printf("mouse position: (%ld, %ld)\n", t.x, t.y);

    destroy_client(c);

    return 0;
}
