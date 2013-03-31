libMML API
==========


Initialisation
--------------

init
~~~~

.. code-block:: c

    int init(void); extern libmml;

create_client
~~~~~~~~~~~~~

.. code-block:: c

    void* create_client(void);


destroy_client
~~~~~~~~~~~~~~
.. code-block:: c

    int destroy_client(void* c);

get_debug
~~~~~~~~~
.. code-block:: c

    int get_debug(void);

set_debug
~~~~~~~~~

.. code-block:: c

    void set_debug(int debug);

alloc_mem
~~~~~~~~~

.. code-block:: c

    void* alloc_mem(long size, long objsize);

realloc_mem
~~~~~~~~~~~
.. code-block:: c

    void* realloc_mem(void* ptr, long size, long objsize);

free_ptr
~~~~~~~~

.. code-block:: c
    int free_ptr(void* ptr);


Input
-----

.. code-block:: c

    struct tpoint {
        int x, y;
    };

.. code-block:: c

    int get_mouse_pos(void * c, struct tpoint* t);
.. code-block:: c

    int set_mouse_pos(void * c, struct tpoint* t);

.. code-block:: c

    int get_mouse_button_state(void * c, int but);

.. code-block:: c

    int set_mouse_button_state(void * c, int but, int x, int y);


Colour
------


.. code-block:: c

    int get_color(void * c, int x, int y, int * col);
.. code-block:: c

    int find_color(void * c, int * x, int * y, int col, int x1, int y1, int x2,
            int y2);
.. code-block:: c

    int find_color_tolerance(void * c, int * x, int * y, int col, int tol, int x1,
            int y1, int x2, int y2);
.. code-block:: c

    int find_colors(void * c, struct tpoint ** tpa, int * len, int col, int x1,
            int y1, int x2, int y2);
.. code-block:: c

    int find_colors_tolerance(void * c, struct tpoint ** tpa, int* len, int col,
            int tol, int x1, int y1, int x2, int y2);

Target
------

.. code-block:: c

    int set_array_target(void * c, void* arr, struct tpoint size);
