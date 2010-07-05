.. settings:

Simba settings
==============

The Simba settings form can be used to tweak various Simba settings. Varying
from how often Simba will check for an update to wether or not you want the tray
icon to show at all times. 

Settings can be changed by going to the menubar titled *Tools* and the
selecting the *Settings* option.


General Settings
----------------

``Settings/General/``

Recent files history
~~~~~~~~~~~~~~~~~~~~

``MaxRecentFiles``

Stores the amount of recent files to remember. *10* is default.


Extensions
----------

``Settings/Extensions``

Settings related to extensions are found here.

Extension directory
~~~~~~~~~~~~~~~~~~~

``Path``

This setting contains the directory Simba searches for its extensions.


Plugins
-------

``Settings/Plugins/``

Plugin directory
~~~~~~~~~~~~~~~~

``Path``

This setting contains the directory Simba searches for its plugins.


Includes
--------

``Settings/Includes/``

Include directory
~~~~~~~~~~~~~~~~~

``Path``

This setting contains the directory Simba searches for its 
global includes.


Fonts
-----

``Settings/Fonts/``

Automatically load fonts on startup
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``LoadOnStartUp``

Default is *True*. If set to *False* Fonts will not be loaded on startup.

Font Directory
~~~~~~~~~~~~~~

``Path``

Contains the directory Simba will look for when searching for fonts.


Updater
------

``Settings/Updater/``

The ``Updater`` refers only to the Simba updater, and not to other updaters like
the Font updater or other third party updaters.

Automatically check for updates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``CheckForUpdates``

Default is *True*. If set to *True*, Simba will automatically check if there are
updates available. If you don't want Simba to check for updates of Simba, set
this to *False*.


How often we check for updates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``CheckEveryXMinutes``

Default is *30*. The number here defines how often Simba will check for Simba
updates, in minutes.


Code Hints
----------

``Settings/CodeHints/``

Automatically pop up Code Hints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``ShowAutomatically``

If set to *True*, code hints will be shown when appropriate.
On *False* code hints will never be shown automatically.


Code Hints
----------

``Settings/CodeCompletion/``

Automatically pop up Code Hints
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``ShowAutomatically``

If set to *True*, code completion will pop up when appropriate.
On *False* code will never be completed automatically.


Function list
-------------

``Settings/FunctionList/``

Show on Start
~~~~~~~~~~~~~

``ShowOnStart``

It true is set to *True*, the function list will be visible on startup. *False*
requires you to enable it yourself.


Colour Picker
-------------

``Settings/ColourPicker``

Show colour history on pick?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``ShowHistoryOnPick``

If set to *True*, the Colour History form will be shown every time the user has
picked a colour.


Script Tabs
-----------

``Settings/Tabs/``

Script opening mode
~~~~~~~~~~~~~~~~~~~

``OpenScriptInNewTab``

Simba can load Scripts in two diffferent ways. It can open every script in a new
tab, or it can override the currently active tab. Setting this to *True* opens
scripts in new tabs; this is also the default.

Behaviour when closing tabs
~~~~~~~~~~~~~~~~~~~~~~~~~~~

``OpenNextOnClose``

Once a tab is closed, Simba can open your most recent tab, or the tab that is
next to the closed tab. Setting this to *False* jumps back to 
your most recently used tab.


Source Editor
-------------

``Settings/SourceEditor/``

Source colouring
~~~~~~~~~~~~~~~~

``LazColors``

The default colour theme is the same as Lazarus' colours. If you prefer another
(more familiar?) theme, set this to *False*.

Default script
~~~~~~~~~~~~~~

This contains the path to the default script. You can change this to set your
own default script. (Opening a blank tab loads up the default script)


Tray Icon
---------

``Settings/Tray``

Visiblity of the tray icon
~~~~~~~~~~~~~~~~~~~~~~~~~~

``AlwaysVisible``

If you want the tray icon only to be visible when Simba is minized, set this to
*False*.

.. note::
    
    Here should be a list of the most important Simba settings; what they
    influence, plus the possible values for them.

