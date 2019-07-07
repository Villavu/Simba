# TOSWindow

`TOSWindow` is a simple datatype which stores the handle of a window but has been extended with methods to find and manipulate windows.

Here are the base methods, required to begin searching for a window:
```pascal
// Returns all visible windows and child windows.
function GetVisibleWindows: TOSWindowArray;
// Returns all windows and child windows.
function GetWindows: TOSWindowArray;
// Returns the top level root windows.
function GetTopWindows: TOSWindowArray;
// Returns the currently active window.
function GetActiveWindow: TOSWindow;
// Returns the desktop window.
function GetDesktopWindow: TOSWindow;
```
## TOSWindowArray
Available methods:

```pascal
// Returns the windows which contain `Title` in the windows title.
function GetByTitle(Title: String; out Window: TOSWindow): Boolean; overload;
function GetByTitle(Title: String): TOSWindowArray; overload;

// Returns the windows which contain `ClassName` in the windows classname.
function GetByClass(ClassName: String; out Window: TOSWindow): Boolean; overload;
function GetByClass(ClassName: String): TOSWindowArray; overload;

// Similar to above, but `Title` and `ClassName` must match.
function GetByTitleAndClass(Title, ClassName: String; out Window: TOSWindow): Boolean; overload;
function GetByTitleAndClass(Title, ClassName: String): TOSWindowArray; overload;

// Returns a string containing infomation about each window.
function ToString: String;
```

Examples:

```pascal
 // Returns all visible windows with `Simba` in the title. 
 WriteLn(GetVisibleWindows().GetByTitle('Simba'));
 // A regex (RegExpr) is also accepted. Internally ExecRegExpr is used which is also available in Simba. 
 // The following will return all visible windows which have `Simba` or `Chrome` in the title.
 WriteLn(GetVisibleWindows().GetByTitle('Simba|Chrome'))
 // Debug an array.
 Writeln(GetVisibleWindows().ToString());
 ```
 `ToString` output:
```
 - Window: 6949260
 - Title: Simba - Untitled*
 - Class: Window
 - PID: 13688
 - Root: 6949260
 - Visible: True
 - Bounds: [131, 123, 1382, 861]
 - Children: 21
```
## TOSWindow
Available methods:
```pascal
// Returns true if the window handle still exists.
function IsValid: Boolean;
// Returns true if the window is the active window.
function IsActive: Boolean; overload;
// IsActive, but waits up to `Time` for the window to be active.
function IsActive(Time: Int32): Boolean; overload;
// Returns true if the window has a visible flag. 
// For example this will return true even if the window is obscured by other window.
function IsVisible: Boolean;
// Returns the process ID of the windows process.
function GetPID: UInt32;
// Returns the root window.
// The root window is the main window which has a window frame & titlebar.
function GetRootWindow: TOSWindow;
// Returns the windows class name.
function GetClassName: WideString;
// Returns the windows title.  
function GetTitle: WideString;
// Returns the bounds of the window as a TBox.
function GetBounds: TBox; overload;
// Returns the child windows.
// if `Recursive` is true the children of a child window will also be returned.
function GetChildren(Recursive: Boolean = True): TOSWindowArray;
// Sets the bounds of the window.
procedure SetBounds(Bounds: TBox);
// Activates and focuses the window.
function Activate: Boolean;
// Kills the process that owns the window.   
procedure Kill; 
```
Example:
```pascal
// Find Chrome, resize and activate.
var
  Windows: TOSWindowArray;
  Window: TOSWindow;
begin
  Windows := GetTopWindows();

  if Windows.GetByTitle('Chrome', Window) then
  begin
    WriteLn('Chrome found: ', Window);

    Window.SetBounds([0, 0, 1000, 1000]);
    Window.Activate();
  end else
    Writeln('Chrome not found!');
end.
```
## Additional
`TOSWindow` also adds some simple finder methods:
```pascal
// Search top level windows for `Title`
function FindWindow(Title: String): TOSWindowArray;
function FindWindow(Title: String; out Window: TOSWindow): Boolean;

// Search top level windows for `Title` then search the children for `ClassName`
function FindChildWindow(Title: String; ClassName: String): TOSWindowArray;
function FindChildWindow(Title: String; ClassName: String; out Child: TOSWindow): Boolean;
```
Example:
```pascal
WriteLn(FindChildWindow('RuneScape', 'SunAwtCanvas'));
```

