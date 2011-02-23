User Interface
==============

Menu items
----------
(Format)
Main menu item Submenu item, [shortcut, ]description

File Menu
~~~~~~~~~

*   New, Ctrl+N, Opens a new tab with the default script

*   Open, Ctrl+O, Prompted for an existing file to open in a new tab 
    (by default)

*   Open recent, Lists up to the last 10 (by default) files that have been open

*   Save, Ctrl+S, Save the current script - if it hasn't been saved before
    or opened, then prompted for a filename

*   Save as, Save the current script as a new filename

*   Save as Default, Make it so all new files will be the current script.

*   Save All, Shift+Ctrl+S, Saves all currently open scripts

*   New Tab, Ctrl+T, Opens a new tab with the default script

*   Close Tab, Ctrl+W, Closes current tab, prompting for a save if it hasn't
      been already

*   Close all tabs, Closes all tabs, prompting for a save if individual tabs
      haven't been already

*   Exit, Ctrl+Q, Closes Simba, prompting for a save if individual tabs
    haven't been already

Edit Menu
~~~~~~~~~

*   Undo, Ctrl+Z, Changes the script to a previous state where available

*   Redo, Ctrl+Y, Changes the script to a later state where available

*   Cut, Ctrl+X, Removes the currently selected text and makes it 
    available to paste

*   Copy, Ctrl+C, Copies the selected text and makes it available to paste

*   Paste, Ctrl+V, Inserts a copy of previously copied/cut text at 
    the current position

*   Delete, Delete/Backspace, Removes the currently selected text

*   Select All, Ctrl+A, Makes all the text in the open tab selected

*   Find, Ctrl+F, Allows user to search for the first occurrence of a text 
    sequence, making it selected, with or without case matching as well 
    as highlighting other occurrences

*   Find next, F3, Selects next occurrence of text sequence being searched for

*   Replace, Ctrl+R, Opens a find/replace dialogue box, allowing a 
    search string to be specified as well as a replacement string, 
    with options of finding occurrences and replacing a single or all
    occurrences

Script
~~~~~~
*   Run, F9, Compile and start executing the script

*   Compile, Ctrl+F9, Only compile the script without executing

*   Pause, Stop executing the script, but allow it to be resumed 
    from the current place via run

*   Stop, F2, Stop executing the script completely without allowing it 
    to be resumed from the current place


View
~~~~

*   Colour History, Toggles display of the colour picker history form

*   Debug Image, Toggles display of the debug image "orm"

*   Function List, Toggles display of the function list box

*   Extensions, Toggles display of the extensions form


Tools
~~~~~

*   Fill Function List, (Ctrl+Q, which is also assigned to Exit, 
    which takes precedence), Reloads the functions from the script into 
    the function list box

*   Update, Opens the update form

*   Settings, Opens the settings form

*   Bitmap conversion, Opens a form for converting images to bitmap strings

*   Export script as HTML, Prompts for a filename to save HTML file with 
    formatted version of script that has syntax highlighting


Help
~~~~

*   About, Opens about form

*   Handbook, (404 at the moment) Opens up the online
    information resource for Simba in default browser.

*   Report a bug, Opens up the online bugtracker for Simba in default browser

*   Check for new SRL, Sees if local version is the latest version of SRL

SRL 
~~~

(Only exists if the srl.sex/SRL Updater extension is installed and enabled):

*   Update SRL, Downloads new version of SRL if one is available

*   Automatically update, toggles whether or not to update SRL without asking 
    upon starting Simba/enabling SRL Updater extension


Toolbar buttons
----------------
(Format)
  Function (listed from left to right), [shortcut command, ]menu 
  [,description if not present on menu]

*   New, Ctrl+N, File

*   Open, Ctrl+O, File

*   Save, Ctrl+S, File

*   Save All, Shift+Ctrl+S, File

*   Cut, Ctrl+X, Edit

*   Copy, Ctrl+C, Edit

*   Paste, Ctrl+V, Edit

*   Run, F9, Script

*   Pause, Script

*   Stop, F2, Script

*   Add Tab, Ctrl+T, File

*   Close Tab, Ctrl+W, File

*   Clear debug, no menu, Removes all lines in the debug box

*   Colour picker, no shortcut (accessible from View->Colour History), 
    Freezes screen and allows use to pick a colour while displaying extra 
    information

*   Select target, no menu, Allows user to choose an area they would 
    like Simba to perform all actions on (must be held then released over 
    target to select it)

*   Reload plugins, no menu, (Currently unavailable) Allows user to 
    reload plugins after updating them without requiring Simba to be restarted

*   Minimize to tray, no menu, Removes Simba from visible screen and start 
    bar allowing it to only be accessed via the Simba tray icon

*   Toggle console, no menu, Either displays or removes the output console


SyncEdit
--------

Multiple references to an identifier in a section of text can be altered 
at once using SyncEdit.
Select the text in which you wish to alter an identifiers name and press 
Ctrl+J. The highlighted area should turn from a blue colour to a green 
colour with identifiers displayed with grey boxes around them, 
except for the currently selected identifier (which should be in a blue box) 
and other references to the same identifier (which should be in a purple
/pink box). To change to a different identifier, click in a different box.
To edit the identifier, simply edit one of the boxes. When finished, press Esc.
This can be used to change variable names, procedure/function names
(and calls to them), custom type names (and declarations that use them). 
It can also be used to change references to a type as well as any uses of 
that type.
Trying to edit something not in a box causes SyncEdit to exit, 
as it would when pressing Esc.

Read Only / External Editor Mode
--------------------------------

Simba has a read only mode that can be enabled and disabled per tab.
If a tab is read only, it will reload the file the script belongs to every time,
and you're obviously not able to edit the script in Simba. This is particularly
useful if you're editing from a different editor.

Function list
-------------

It can be undocked or moved from one side to the other.
To reset it to its default position, restart Simba.
As of now, it doesn't remember where it was last.
If it doesn't correctly display Script functions/procedures,
it may be that there is a problem with the script before the declaration.
Items are not currently sorted alphabetically at any level, 
however Script and Includes items are added in the order they 
appear in the relevant scripts.
If you close it, it can be displayed again from the View menu.

Extensions
----------

The Extensions form is launched from the View menu via the Extensions item.
To enable or disable an extensions, select it from the list and tick or 
untick the box in the bottom left hand corner appropriately 
(ticked for enabled, unticked for disabled).
Listed extensions are, by default, located in the Extensions folder where 
the Simba executable is.
The file extensions .sex is short for Simba EXtension.
Extensions focus more on the Simba form or similar, general area where as 
plugins focus more on the scripting aspect.

For a list of all Simba extensions, have a look here.

.. note::
    The above sentence will link to a list of Simba extensions, but that list
    stills needs to be created.

Colour Picker History
---------------------

The colour picker history form is launched from the View menu via the 
Colour History item.
It lists all colours that have been picked since launching Simba, 
unless they have been deleted, as well as information on where they 
were picked and RGB values.
Names can be given to colours, to help relate it to what it represents,
by selecting a colour item from the left and changing the top field on 
the right.
It is, by default, displayed after picking a colour using the button on the main Simba toolbar.

Update Form
-----------

The update form is launched from Tools menu via the Update item.
Informs user if no update is currently available; else it will 
displaying the form. Gives the option of updating when the current
version is not the latest.

Settings Form
-------------

The Simba settings form is launched from the Tools menu via the Settings item.
The main tree consists of 3 sections: one for the functionality of 
Simba (Settings), one for the appearance of Simba (LastConfig) 
and one for holding settings relevant to extensions (Extensions).

The following information applies for the main Settings branch
and not LastConfig or Extensions.

The Settings section allows toggling of:

*   Loading fonts on startup (Fonts->LoadOnStartUp)

*   Showing the function list on startup (FunctionList->ShowOnStart)

*   Using Lazarus syntax highlighting (SourceEditor->LazColors) 
    [Requires restarting Simba]

*   Using CPascal or PascalScript interpreter (Interpreter->UseCPascal)

*   Automatically checking for Simba updates (Updater->CheckForUpdates)

*   Opening next tab upon closing current (Tabs->OpenNextOnClose) 
    [Unsure on what it exactly does]

*   Open new scripts in current or new tab (Tabs->OpenScriptInNewTab) 
    [Does prompt for saving if current script is unsaved]

*   Automatically showing code hints (CodeHints->ShowAutomatically)

*   Automatically show code completion (CodeCompletion->ShowAutomatically)

*   Automatically open colour picker history form after picking a colour 
    (ColourPicker->ShowHistoryOnPick)


The Settings section allows changing the path of:

*   Where to load includes from (Includes->Path)

*   Where to load fonts from (Fonts->Path)

*   Where to check latest fonts version (Fonts->VersionLink)

*   Where to download latest fonts from (Fonts->UpdateLink)

*   Path to the default script (SourceEditor->DefScriptPath)

*   Where to load extensions from (Extensions->Path) 
    [The file extension for Simba extensions can also be changed]

*   Where to load plugins from (Plugins->Path)

*   Where to load the news from (News->URL)

*   Where to check latest Simba version (Updater->RemoteVersionLink)

*   Where to download latest Simba from (Updater->RemoteLink)


The Settings section has further settings which do not fall into the 
above two categories of sorts. These are:

*   Version of local fonts (Fonts->Version)

*   How often to check for new version of Simba (Updater->CheckEveryXMinutes)

*   Number of recent files to be remembered (General->MaxRecentFiles)

*   The file extension for Simba Extensions (Extensions->FileExtension)

It is highly suggested that the large majority of settings should not be 
altered unless you know what you are doing.
