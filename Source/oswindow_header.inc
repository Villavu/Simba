type
  POSWindow = ^TOSWindow;
  TOSWindow = type PtrUInt;

  POSWindowArray = ^TOSWindowArray;
  TOSWindowArray = array of TOSWindow;

  TOSWindowArray_Helper = type Helper for TOSWindowArray
  public
    function GetByTitle(Title: String; out Window: TOSWindow): Boolean; overload;
    function GetByTitle(Title: String): TOSWindowArray; overload;

    function GetByClass(ClassName: String; out Window: TOSWindow): Boolean; overload;
    function GetByClass(ClassName: String): TOSWindowArray; overload;

    function GetByTitleAndClass(Title, ClassName: String; out Window: TOSWindow): Boolean; overload;
    function GetByTitleAndClass(Title, ClassName: String): TOSWindowArray; overload;

    function ToString: String;
  end;

  TOSWindow_Helper = type Helper for TOSWindow
  public
    function IsValid: Boolean;
    function IsActive: Boolean; overload;
    function IsActive(Time: Int32): Boolean; overload;
    function IsVisible: Boolean;
    function GetPID: UInt32;
    function GetRootWindow: TOSWindow;
    function GetClassName: WideString;
    function GetTitle: WideString;
    function GetBounds(out Bounds: TBox): Boolean; overload;
    function GetBounds: TBox; overload;
    function GetChildren(Recursive: Boolean = True): TOSWindowArray;
    function GetRelativeCursorPos: TPoint;
    procedure SetBounds(Bounds: TBox);
    function Activate: Boolean;
    procedure Kill;
  end;

  function GetVisibleWindows: TOSWindowArray;
  function GetWindows: TOSWindowArray;
  function GetActiveWindow: TOSWindow;
  function GetDesktopWindow: TOSWindow;
  function GetWindowAtCursor: TOSWindow;
  function GetTopWindows: TOSWindowArray;

  function FindWindow(Title: String): TOSWindowArray;
  function FindWindow(Title: String; out Window: TOSWindow): Boolean;
  function FindChildWindow(Title: String; ClassName: String): TOSWindowArray;
  function FindChildWindow(Title: String; ClassName: String; out Child: TOSWindow): Boolean;

