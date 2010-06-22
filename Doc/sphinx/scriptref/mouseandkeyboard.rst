Mouse and Keyboard
==================

Types
-----

A few variables are exported for working with the Simba mouse functions.

TClickType, which defines the click type.

.. code-block:: pascal

    const
        mouse_Right = 0
        mouse_Left = 1
        mouse_Middle = 2

TMousePress, which defines if the mouse button is to be down or up.

.. code-block:: pascal

    TMousePress = (mouse_Down, mouse_Up);  

Keyboard functions often use Virtual Keys. See :ref:`virtualkeys` for a complete
list.

Mouse Functions
---------------

MoveMouse
~~~~~~~~~

.. code-block:: pascal
   
    procedure MoveMouse(x, y: integer);

MoveMouse moves the mouse pointer to the specified x and y coordinates.

GetMousePos
~~~~~~~~~~~

.. code-block:: pascal

    procedure GetMousePos(var x, y: integer);

GetMousePos returns the current position of the mouse in x and
y.


HoldMouse
~~~~~~~~~

.. code-block:: pascal

    procedure HoldMouse(x, y: Integer; clickType: TClickType);

HoldMouse holds the given mouse button (clickType) down at the specified
x, y coordinate. If the mouse if not at the given x, y yet, the mouse position
will be set to x, y.

ReleaseMouse
~~~~~~~~~~~~

.. code-block:: pascal

    procedure ReleaseMouse(x, y: Integer; clickType: TClickType);

HoldMouse holds the given mouse button (clickType) down at the specified
x, y coordinate. If the mouse if not at the given x, y yet, the
mouse position will be set to x, y.

ClickMouse
~~~~~~~~~~

.. code-block:: pascal

    procedure ClickMouse(x, y: Integer; clickType: Integer):

ClickMouse performs a click with the given mouse button (clickType) at the
specified x, y coordinate.

Keyboard Functions
------------------

Keyboard functions are obviously used to manipulate the keyboard input. It can
also be used to read states of specific keys.


KeyDown
~~~~~~~


.. code-block:: pascal

    procedure KeyDown(key: Word);

KeyDown sends a request to the Operating System to "fake" an event that
causes the ``key`` to be "down".


KeyUp
~~~~~

.. code-block:: pascal

    procedure KeyUp(key: Word);

KeyDown sends a request to the Operating System to "fake" an event that
causes the ``key`` to be "up".


.. _virtualkeys:

Keyboard Virtual Keys
---------------------


*   UNKNOWN: 0
*   LBUTTON: 1
*   RBUTTON: 2
*   CANCEL: 3
*   MBUTTON: 4
*   XBUTTON1: 5
*   XBUTTON2: 6
*   BACK: 8
*   TAB: 9
*   CLEAR: 12
*   RETURN: 13
*   SHIFT: 16
*   CONTROL: 17
*   MENU: 18
*   PAUSE: 19
*   CAPITAL: 20
*   KANA: 21
*   HANGUL: 21
*   JUNJA: 23
*   FINAL: 24
*   HANJA: 25
*   KANJI: 25
*   ESCAPE: 27
*   CONVERT: 28
*   NONCONVERT: 29
*   ACCEPT: 30
*   MODECHANGE: 31
*   SPACE: 32
*   PRIOR: 33
*   NEXT: 34
*   END: 35
*   HOME: 36
*   LEFT: 37
*   UP: 38
*   RIGHT: 39
*   DOWN: 40
*   SELECT: 41
*   PRINT: 42
*   EXECUTE: 43
*   SNAPSHOT: 44
*   INSERT: 45
*   DELETE: 46
*   HELP: 47
*   0: 30
*   1: 31
*   2: 32
*   3: 33
*   4: 34
*   5: 35
*   6: 36
*   7: 37
*   8: 38
*   9: 39
*   A: 41
*   B: 42
*   C: 43
*   D: 44
*   E: 45
*   F: 46
*   G: 47
*   H: 48
*   I: 49
*   J: 4A
*   K: 4B
*   L: 4C
*   M: 4D
*   N: 4E
*   O: 4F
*   P: 50
*   Q: 51
*   R: 52
*   S: 53
*   T: 54
*   U: 55
*   V: 56
*   W: 57
*   X: 58
*   Y: 59
*   Z: 5A
*   LWIN: 5B
*   RWIN: 5C
*   APPS: 5D
*   SLEEP: 5F
*   NUMPAD0: 96
*   NUMPAD1: 97
*   NUMPAD2: 98
*   NUMPAD3: 99
*   NUMPAD4: 100
*   NUMPAD5: 101
*   NUMPAD6: 102
*   NUMPAD7: 103
*   NUMPAD8: 104
*   NUMPAD9: 105
*   MULTIPLY: 106
*   ADD: 107
*   SEPARATOR: 108
*   SUBTRACT: 109
*   DECIMAL: 110
*   DIVIDE: 111
*   F1: 112
*   F2: 113
*   F3: 114
*   F4: 115
*   F5: 116
*   F6: 117
*   F7: 118
*   F8: 119
*   F9: 120
*   F10: 121
*   F11: 122
*   F12: 123
*   F13: 124
*   F14: 125
*   F15: 126
*   F16: 127
*   F17: 128
*   F18: 129
*   F19: 130
*   F20: 131
*   F21: 132
*   F22: 133
*   F23: 134
*   F24: 135
*   NUMLOCK: 90
*   SCROLL: 91
*   LSHIFT: A0
*   RSHIFT: A1
*   LCONTROL: A2
*   RCONTROL: A3
*   LMENU: A4
*   RMENU: A5
*   BROWSER_BACK: A6
*   BROWSER_FORWARD: A7
*   BROWSER_REFRESH: A8
*   BROWSER_STOP: A9
*   BROWSER_SEARCH: AA
*   BROWSER_FAVORITES: AB
*   BROWSER_HOME: AC
*   VOLUME_MUTE: AD
*   VOLUME_DOWN: AE
*   VOLUME_UP: AF
*   MEDIA_NEXT_TRACK: B0
*   MEDIA_PREV_TRACK: B1
*   MEDIA_STOP: B2
*   MEDIA_PLAY_PAUSE: B3
*   LAUNCH_MAIL: B4
*   LAUNCH_MEDIA_SELECT: B5
*   LAUNCH_APP1: B6
*   LAUNCH_APP2: B7
*   OEM_1: BA
*   OEM_PLUS: BB
*   OEM_COMMA: BC
*   OEM_MINUS: BD
*   OEM_PERIOD: BE
*   OEM_2: BF
*   OEM_3: C0
*   OEM_4: DB
*   OEM_5: DC
*   OEM_6: DD
*   OEM_7: DE
*   OEM_8: DF
*   OEM_102: E2
*   PROCESSKEY: E7
*   ATTN: F6
*   CRSEL: F7
*   EXSEL: F8
*   EREOF: F9
*   PLAY: FA
*   ZOOM: FB
*   NONAME: FC
*   PA1: FD
*   OEM_CLEAR: FE
*   HIGHESTVALUE: FE
*   UNDEFINED: FF
