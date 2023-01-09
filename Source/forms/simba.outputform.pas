{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.outputform;

{$i simba.inc}

interface

uses
  classes, sysutils, forms, controls, comctrls, graphics, menus, extctrls, syncobjs,
  synedit, synedittypes, syneditmiscclasses, syneditmousecmds,
  simba.settings;

type
  TSimbaOutputBox = class(TSynEdit)
  protected
    FLock: TCriticalSection;
    FBuffer: TStringList;

    FMouseLink: record
      Quote: record
        URL: String;
        FileName: String;
      end;
      DocPos: record
        Line, Col: Integer;
        FileName: String;
      end;
    end;

    procedure DoOpenLink(Data: PtrInt);
    procedure DoSpecialLineMarkup(Sender: TObject; Line: integer; var Special: Boolean; AMarkup: TSynSelectedColor);
    procedure DoMouseLeave(Sender: TObject);
    procedure DoAllowMouseLink(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
    procedure DoMouseLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    function GetAntialiasing: Boolean;
    procedure SetAntialiasing(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetWordBoundsAtRowCol(const XY: TPoint; out StartX, EndX: integer); override;

    function Add(const S: String): String;
    procedure AddClear;
    procedure Flush;

    property Antialiasing: Boolean read GetAntialiasing write SetAntialiasing;
  end;

  TSimbaOutputForm = class(TForm)
    ContextMenu: TPopupMenu;
    MenuItemCustomize: TMenuItem;
    MenuItemCopyAll: TMenuItem;
    MenuItemCopyLine: TMenuItem;
    MenuItemSeperator: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    PageControl: TPageControl;
    Separator1: TMenuItem;
    TabSimba: TTabSheet;
    TabScript: TTabSheet;
    Timer: TTimer;

    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseLeave(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MenuItemCopyAllClick(Sender: TObject);
    procedure MenuItemCopyClick(Sender: TObject);
    procedure MenuItemCopyLineClick(Sender: TObject);
    procedure MenuItemCustomizeClick(Sender: TObject);
    procedure MenuItemSelectAllClick(Sender: TObject);
    procedure TimerExecute(Sender: TObject);
  protected
    FSimbaOutputBox: TSimbaOutputBox;
    FScriptOutputBox: TSimbaOutputBox;

    procedure SimbaSettingChanged(Setting: TSimbaSetting);
  public
    property ScriptOutputBox: TSimbaOutputBox read FScriptOutputBox;
    property SimbaOutputBox: TSimbaOutputBox read FSimbaOutputBox;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SimbaOutputForm: TSimbaOutputForm;

implementation

{$R *.lfm}

uses
  SynEditMarkupBracket, SynEditMarkupWordGroup,
  lclintf, math,
  simba.dockinghelpers, simba.fonthelpers, simba.scripttabsform,
  simba.nativeinterface, simba.mufasatypes, simba.settingsform;

procedure SimbaDebugLn(const S: String);
begin
  SimbaOutputForm.SimbaOutputBox.Add(S + LineEnding + ToStr(ESimbaDebugLn.SHOW) + LineEnding);
end;

function TSimbaOutputBox.GetAntialiasing: Boolean;
begin
  Result := (Font.Quality = fqCleartypeNatural);
end;

procedure TSimbaOutputBox.SetAntialiasing(Value: Boolean);
begin
  case Value of
    True:  Font.Quality := fqCleartypeNatural;
    False: Font.Quality := fqNonAntialiased;
  end;
end;

procedure TSimbaOutputBox.DoOpenLink(Data: PtrInt);
begin
  if (FMouseLink.DocPos.FileName <> '') then
  begin
    if FileExists(FMouseLink.DocPos.FileName) then
      SimbaScriptTabsForm.Open(FMouseLink.DocPos.FileName);

    with FMouseLink.DocPos do
      SimbaScriptTabsForm.CurrentEditor.FocusLine(Line, Col, $0000A5);

    Exit;
  end;

  if (FMouseLink.Quote.FileName <> '') then
  begin
    if DirectoryExists(FMouseLink.Quote.FileName) then
      SimbaNativeInterface.OpenDirectory(FMouseLink.Quote.FileName)
    else
    if FileExists(FMouseLink.Quote.FileName) and FMouseLink.Quote.FileName.EndsWith('.simba') then
      SimbaScriptTabsForm.Open(FMouseLink.Quote.FileName)
    else
    if FileExists(FMouseLink.Quote.FileName) then
      SimbaNativeInterface.OpenFile(FMouseLink.Quote.FileName);

    Exit;
  end;

  if (FMouseLink.Quote.URL <> '') then
  begin
    OpenURL(FMouseLink.Quote.URL);

    Exit;
  end;
end;

procedure TSimbaOutputBox.DoSpecialLineMarkup(Sender: TObject; Line: integer; var Special: Boolean; AMarkup: TSynSelectedColor);
begin
  Special := Lines.Objects[Line - 1] <> nil;

  if Special then
  begin
    AMarkup.BackAlpha  := 115;
    AMarkup.Background := PtrUInt(Lines.Objects[Line - 1]);
    AMarkup.Foreground := clNone;
  end;
end;

procedure TSimbaOutputBox.DoMouseLeave(Sender: TObject);
begin
  LastMouseCaret := TPoint.Create(-1, -1);
end;

procedure TSimbaOutputBox.DoAllowMouseLink(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
begin
  AllowMouseLink := ((FMouseLink.DocPos.FileName <> '') and (FMouseLink.DocPos.FileName = 'Untitled') or FileExists(FMouseLink.DocPos.FileName)) or
                    ((FMouseLink.Quote.URL <> '') or ((FMouseLink.Quote.FileName <> '') and FileExists(FMouseLink.Quote.FileName)));
end;

procedure TSimbaOutputBox.DoMouseLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Application.QueueAsyncCall(@DoOpenLink, 0);
end;

constructor TSimbaOutputBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FBuffer := TStringList.Create();
  FLock := TCriticalSection.Create();

  OnMouseLeave        := @DoMouseLeave;
  OnMouseLink         := @DoAllowMouseLink;
  OnClickLink         := @DoMouseLinkClick;
  OnSpecialLineMarkup := @DoSpecialLineMarkup;

  TabStop := False;
  BorderStyle := bsNone;
  Options := Options + [eoHideRightMargin];

  Gutter.Visible := False;
  MouseLinkColor.Style := [fsUnderline];

  MarkupByClass[TSynEditMarkupBracket].Enabled := False;
  MarkupByClass[TSynEditMarkupWordGroup].Enabled := False;

  MouseOptions := [emUseMouseActions];
  ResetMouseActions();
  with MouseTextActions.Add() do
    Command := emcMouseLink;
end;

destructor TSimbaOutputBox.Destroy;
begin
  if (FLock <> nil) then
    FreeAndNil(FLock);
  if (FBuffer <> nil) then
    FreeAndNil(FBuffer);

  inherited Destroy();
end;

procedure TSimbaOutputBox.GetWordBoundsAtRowCol(const XY: TPoint; out StartX, EndX: integer);

  // Line 3 in function "cpuuu" in file "Untitled"
  function FindStackTrace(Line: String; var StartPos, EndPos: Integer): Boolean;
  begin
    StartPos := 1;
    EndPos := Length(Line) + 1;

    Result := Line.Contains('Line') and Line.Contains('in') and Line.Contains('in file');
    if Result then
    begin
      FMouseLink.DocPos.FileName := Line.After('file').Between('"', '"');
      FMouseLink.DocPos.Line     := Line.Between('Line', 'in').ExtractInteger();
      FMouseLink.DocPos.Col      := 0;
    end;
  end;

  // at line 3, column 3 in file "C:\Users\OllyC\Desktop\Code\Simba\Scripts\testing.simba"
  function FindDocPos(Line: String; var StartPos, EndPos: Integer): Boolean;
  begin
    StartPos := 1;
    EndPos := Length(Line) + 1;

    Result := Line.Contains('at line') and Line.Contains('column') and Line.Contains('in file');
    if Result then
    begin
      FMouseLink.DocPos.FileName := Line.After('file').Between('"', '"');
      FMouseLink.DocPos.Line     := Line.Between('line', ',').ExtractInteger();
      FMouseLink.DocPos.Col      := Line.Between('column', 'in').ExtractInteger();
    end;
  end;

  // "www.google.com"
  // "directory/file.simba"
  function FindURLOrFile(Line: String; var StartPos, EndPos: Integer): Boolean;
  begin
    StartPos := Line.LastIndexOf('"', XY.X) + 1;
    EndPos   := Line.IndexOf('"', XY.X);

    Result := (StartPos > 1) and (EndPos > 0);
    if Result then
    begin
      Line := Copy(Line, StartPos, EndPos - StartPos);
      if Line.StartsWith('www.') or Line.StartsWith('http://') or Line.StartsWith('https://') then
        FMouseLink.Quote.URL := Line
      else
        FMouseLink.Quote.FileName := Line;
    end;
  end;

var
  Line: String;
  StartPos, EndPos: Integer;
begin
  inherited GetWordBoundsAtRowCol(XY, StartX, EndX);

  FMouseLink.Quote.URL       := '';
  FMouseLink.Quote.FileName  := '';
  FMouseLink.DocPos.FileName := '';

  Line := TextView[XY.Y - 1];
  if FindStackTrace(Line, StartPos, EndPos) or FindDocPos(Line, StartPos, EndPos) or FindURLOrFile(Line, StartPos, EndPos) then
  begin
    StartX := StartPos;
    EndX   := EndPos;
  end;
end;

function TSimbaOutputBox.Add(const S: String): String;

  function ParseDebugLn(var S: String): ESimbaDebugLn;
  var
    IntVal, NewLen: Integer;
  begin
    Result := ESimbaDebugLn.NONE;

    if (Length(S) >= 5) and (S[1] = #0) and (S[2] = #0) and (S[3] = '$') then
    begin
      IntVal := S.CopyRange(3, 5).ToInteger(0);

      if InRange(IntVal, Integer(Low(ESimbaDebugLn)) + 1, Integer(High(ESimbaDebugLn))) then
      begin
        Result := ESimbaDebugLn(IntVal);

        NewLen := Length(S) - 5;
        if (NewLen > 0) then
          Move(S[6], S[1], NewLen * SizeOf(Char));
        SetLength(S, NewLen);
      end;
    end;
  end;

var
  Arr: TStringArray;
  I, H: Integer;
begin
  Arr := S.Split(LineEnding);
  if (Length(Arr) = 0) then
    Exit('');

  FLock.Enter();
  try
    H := High(Arr);
    for I := 0 to H - 1 do
      FBuffer.AddObject(Arr[I], TObject(PtrUInt(ParseDebugLn(Arr[I]))));

    if S.EndsWith(LineEnding) then
    begin
      FBuffer.AddObject(Arr[H], TObject(PtrUInt(ParseDebugLn(Arr[H]))));

      Result := '';
    end else
      Result := Arr[H];
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaOutputBox.AddClear;
begin
  Add(ToStr(ESimbaDebugLn.CLEAR) + LineEnding);
end;

procedure TSimbaOutputBox.Flush;
var
  I, StartIndex: Integer;
  LineType: ESimbaDebugLn;
  NeedVisible, NeedScroll: Boolean;
begin
  FLock.Enter();

  try
    if (FBuffer.Count > 0) then
    begin
      NeedVisible := False;

      StartIndex := 0;
      for I := 0 to FBuffer.Count - 1 do
      begin
        LineType := ESimbaDebugLn(PtrUInt(FBuffer.Objects[I]));
        if (LineType = ESimbaDebugLn.CLEAR) then
          StartIndex := I+1;

        NeedVisible := NeedVisible or (LineType in [ESimbaDebugLn.YELLOW, ESimbaDebugLn.RED, ESimbaDebugLn.GREEN, ESimbaDebugLn.SHOW]);
      end;

      BeginUpdate(False);
      if (StartIndex > 0) then
        Clear();

      // auto scroll if already scrolled to bottom.
      NeedScroll := (Lines.Count < LinesInWindow) or ((Lines.Count + 1) = (TopLine + LinesInWindow));
      for I := StartIndex to FBuffer.Count - 1 do
      begin
        LineType := ESimbaDebugLn(PtrUInt(FBuffer.Objects[I]));

        case LineType of
          ESimbaDebugLn.NONE:   Lines.Add(FBuffer[I]);
          ESimbaDebugLn.YELLOW: Lines.AddObject(FBuffer[I], TObject(PtrUInt($00BFFF)));
          ESimbaDebugLn.RED:    Lines.AddObject(FBuffer[I], TObject(PtrUInt($0000A5)));
          ESimbaDebugLn.GREEN:  Lines.AddObject(FBuffer[I], TObject(PtrUInt($228B22)));
        end;
      end;

      if NeedVisible then
      begin
        if (Parent is TTabSheet) then
          TTabSheet(Parent).PageControl.ActivePage := TTabSheet(Parent);
        TopLine := Lines.Count;
      end else
      if NeedScroll then
        TopLine := Lines.Count;

      EndUpdate();
      Invalidate();

      FBuffer.Clear();
    end;
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaOutputForm.MenuItemSelectAllClick(Sender: TObject);
begin
  if (ContextMenu.PopupComponent is TSynEdit) then
    with TSynEdit(ContextMenu.PopupComponent) do
      SelectAll();
end;

procedure TSimbaOutputForm.TimerExecute(Sender: TObject);
begin
  FScriptOutputBox.Flush();
  FSimbaOutputBox.Flush();
end;

procedure TSimbaOutputForm.SimbaSettingChanged(Setting: TSimbaSetting);
begin
  case Setting.Name of
    'OutputBox.Color':
      begin
        FScriptOutputBox.Color := Setting.Value;
        FSimbaOutputBox.Color  := Setting.Value;
      end;

    'OutputBox.FontColor':
      begin
        FScriptOutputBox.Font.Color := Setting.Value;
        FSimbaOutputBox.Font.Color  := Setting.Value;
      end;

    'OutputBox.FontSize':
      begin
        FScriptOutputBox.Font.Size := Setting.Value;
        FSimbaOutputBox.Font.Size  := Setting.Value;
      end;

    'OutputBox.FontName':
      if IsFontFixed(Setting.Value) then
      begin
        FScriptOutputBox.Font.Name := Setting.Value;
        FSimbaOutputBox.Font.Name  := Setting.Value;
      end;

    'OutputBox.FontAntiAliased':
      begin
        FScriptOutputBox.Antialiasing := Setting.Value;
        FSimbaOutputBox.Antialiasing  := Setting.Value;
      end;
  end;
end;

procedure TSimbaOutputForm.MenuItemCopyClick(Sender: TObject);
begin
  if (ContextMenu.PopupComponent is TSynEdit) then
    with TSynEdit(ContextMenu.PopupComponent) do
      CopyToClipboard();
end;

procedure TSimbaOutputForm.MenuItemCopyLineClick(Sender: TObject);
var
  Line: Integer;
begin
  if (ContextMenu.PopupComponent is TSynEdit) then
    with TSynEdit(ContextMenu.PopupComponent) do
    begin
      Line := FScriptOutputBox.PixelsToRowColumn(ScreenToClient(ContextMenu.PopupPoint), []).Y;
      if (Line > 0) and (Line <= Lines.Count) then
        DoCopyToClipboard(Lines[Line - 1]);
    end;
end;

procedure TSimbaOutputForm.MenuItemCustomizeClick(Sender: TObject);
begin
  SimbaSettingsForm.TreeView.Selected := SimbaSettingsForm.TreeView.Items.FindNodeWithText('Output Box');
  SimbaSettingsForm.ShowModal();
end;

procedure TSimbaOutputForm.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseDown(Button, Shift, X, Y);
end;

procedure TSimbaOutputForm.FormMouseLeave(Sender: TObject);
begin
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseLeave();
end;

procedure TSimbaOutputForm.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if (HostDockSite is TSimbaAnchorDockHostSite) then
    TSimbaAnchorDockHostSite(HostDockSite).Header.MouseMove(Shift, X, Y);
end;

procedure TSimbaOutputForm.MenuItemCopyAllClick(Sender: TObject);
begin
  if (ContextMenu.PopupComponent is TSynEdit) then
    with TSynEdit(ContextMenu.PopupComponent) do
      DoCopyToClipboard(Lines.Text);
end;

constructor TSimbaOutputForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  PageControl.ActivePage := TabScript;

  FSimbaOutputBox := TSimbaOutputBox.Create(Self);
  FSimbaOutputBox.Parent := TabSimba;
  FSimbaOutputBox.Align := alClient;
  FSimbaOutputBox.PopupMenu := ContextMenu;

  FScriptOutputBox := TSimbaOutputBox.Create(Self);
  FScriptOutputBox.Parent := TabScript;
  FScriptOutputBox.Align := alClient;
  FScriptOutputBox.PopupMenu := ContextMenu;

  SimbaSettingChanged(SimbaSettings.OutputBox.Color);
  SimbaSettingChanged(SimbaSettings.OutputBox.FontColor);
  SimbaSettingChanged(SimbaSettings.OutputBox.FontSize);
  SimbaSettingChanged(SimbaSettings.OutputBox.FontName);
  SimbaSettingChanged(SimbaSettings.OutputBox.FontAntiAliased);

  SimbaSettings.RegisterChangeHandler(@SimbaSettingChanged);

  DebugLnFunc := @SimbaDebugLn;
end;

destructor TSimbaOutputForm.Destroy;
begin
  DebugLnFunc := @DebugLn;

  if (SimbaSettings <> nil) then
    SimbaSettings.UnRegisterChangeHandler(@SimbaSettingChanged);

  inherited Destroy();
end;

end.

