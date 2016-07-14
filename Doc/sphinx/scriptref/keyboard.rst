Keyboard
========

Simba contains several functions to manipulate the mouse and keyboard.
Features range from clicking and moving the mouse to faking keypresses.

Types
-----

Keyboard functions often use Virtual Keys. See :ref:`virtualkeys` for a complete
list.


Keyboard Functions
------------------

Keyboard functions are obviously used to manipulate the keyboard input. It can
also be used to read states of specific keys.

.. _scriptref-keydown:

KeyDown
~~~~~~~

.. code-block:: pascal

    procedure KeyDown(key: Word);

KeyDown sends a request to the Operating System to "fake" an event that
causes the keyboard ``key`` to be "down".

The following example holds down the "Enter" key. (Note that if you call
KeyDown you must call key up afterwards to release the key.)

.. code-block:: pascal

    program KeyDown;

    begin
      KeyDown(13);
    end.

.. _scriptref-keyup:

KeyUp
~~~~~

.. code-block:: pascal

    procedure KeyUp(key: Word);

KeyDown sends a request to the Operating System to "fake" an event that
causes the ``key`` to be "up".

The following example holds down the "Enter" key and release after pausing to
simulate a human action.

.. code-block:: pascal

    program KeyDownRelative;

    begin
      KeyDown(13);
      wait(RandomRange(50, 100));
      KeyUp(13);
    end.

PressKey
~~~~~~~~

.. code-block:: pascal

    procedure PressKey(key: Word);

PressKey sends a request to the Operating System to "fake" a key event.

The following example simulates pressing the "Enter" key.

.. code-block:: pascal

    program PressKey;

    begin
      PressKey(13);
    end.

IsKeyDown
~~~~~~~~~

.. code-block:: pascal

    function isKeyDown(key: Word): boolean;

Returns *True* if *key* is currently being held.

The following example says "Hello World" if the "F5" key is held down.

.. code-block:: pascal

    program PressKey;

    begin
      while not(isKeyDown(116)) do
        Wait(50);
      writeln('Hello World');
    end.

GetKeyCode
~~~~~~~~~~

.. code-block:: pascal

    function GetKeyCode(c: char) : integer;

Returns the keycode of character *c*.

SendKeys
~~~~~~~~

.. code-block:: pascal

    procedure SendKeys(const s: string; keywait, keymodwait: integer);

Type the contents of the string *s*. While typing, hold the keys for *keywait*.
SendKeys should work with any keyboard layout on Windows.

Example:

.. code-block:: pascal

    SendKeys('Hello, World', 100, 30);

.. note::

    If your goal is to randomly wait a small time per key, you should consider
    something like this:

    .. code-block:: pascal

        s := 'Hello, World';
        for i := 0 to length(s) do
        begin
          SendKeys(s[i], 50+Random(51), 30+Random(30));
          Wait(20+Random(30));
        end;

.. _scriptref-virtualkeys:

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
