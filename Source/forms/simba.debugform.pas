{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.debugform;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, SynEdit, Menus, syncobjs, pipes,
  simba.settings;

type
  TSimbaDebugForm = class(TForm)
    Editor: TSynEdit;
    EditorPopupMenu: TPopupMenu;
    MenuItemSeperator: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    MenuItemDelete: TMenuItem;
    Timer: TTimer;

    procedure MenuItemCopyClick(Sender: TObject);
    procedure MenuItemCutClick(Sender: TObject);
    procedure MenuItemDeleteClick(Sender: TObject);
    procedure MenuItemPasteClick(Sender: TObject);
    procedure MenuItemSelectAllClick(Sender: TObject);
    procedure TimerExecute(Sender: TObject);
  protected
    FLock: TCriticalSection;
    FStrings: TStringList;

    procedure SimbaSettingChanged(Setting: TSimbaSetting);
  public
    procedure Clear;

    function Listen(Pipe: TInputPipeStream): TThread;

    procedure Add(const S: String); overload;
    procedure Add(const Strings: TStrings); overload;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SimbaDebugForm: TSimbaDebugForm;

implementation

{$R *.lfm}

uses
  lazloggerbase,
  simba.fonthelpers;

type
  TListenerThread = class(TThread)
  protected
    FInputPipe: TInputPipeStream;

    procedure Execute; override;
  public
    constructor Create(InputPipe: TInputPipeStream); reintroduce;
    destructor Destroy; override;
  end;

procedure TListenerThread.Execute;

  function Process(const Data: String): String;
  var
    I: Int32;
    Lines: TStringArray;
  begin
    Lines := Data.Split([LineEnding], TStringSplitOptions.ExcludeLastEmpty);

    with SimbaDebugForm do
    begin
      FLock.Enter();

      try
        if Data.EndsWith(LineEnding) then
        begin
          for I := 0 to High(Lines) do
            FStrings.Add(Lines[I]);

          Result := '';
        end else
        begin
          // Pipe buffer is full! Carry over last line.
          for I := 0 to High(Lines) - 1 do
            FStrings.Add(Lines[I]);

          Result := Lines[High(Lines)];
        end;
      finally
        FLock.Leave();
      end;
    end;
  end;

var
  Buffer, Remaining: String;
  Count: Int32;
begin
  Remaining := '';

  SetLength(Buffer, 1024);

  while (not Terminated) or (FInputPipe.NumBytesAvailable > 0) do
  begin
    while (FInputPipe.NumBytesAvailable > 0) do
    begin
      Count := FInputPipe.Read(Buffer[1], Length(Buffer));
      if (Count = 0) then
        Break;
      if (Count = Length(Buffer)) then
        SetLength(Buffer, Round(Length(Buffer) * 1.5));

      Remaining := Process(Remaining + Copy(Buffer, 1, Count));
    end;

    Sleep(500);
  end;

  if (Remaining <> '') then
    with SimbaDebugForm do
    begin
      FLock.Enter();
      try
        FStrings.Add(Remaining);
      finally
        FLock.Leave();
      end;
    end;
end;

constructor TListenerThread.Create(InputPipe: TInputPipeStream);
begin
  inherited Create(False);

  FreeOnTerminate := False;

  FInputPipe := InputPipe;
end;

destructor TListenerThread.Destroy;
begin
  Terminate();
  WaitFor();

  inherited Destroy();
end;

procedure TSimbaDebugForm.Add(const S: String);
var
  Line: String;
begin
  DebugLn(S);

  FLock.Enter();

  try
    for Line in S.Split([LineEnding]) do
      FStrings.Add(Line);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaDebugForm.Add(const Strings: TStrings);
var
  I: Int32;
begin
  FLock.Enter();

  try
    for I := 0 to Strings.Count - 1 do
      FStrings.Add(Strings[I]);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaDebugForm.MenuItemCutClick(Sender: TObject);
begin
  Editor.CutToClipboard();
end;

procedure TSimbaDebugForm.MenuItemDeleteClick(Sender: TObject);
begin
  Editor.ClearSelection();
end;

procedure TSimbaDebugForm.MenuItemPasteClick(Sender: TObject);
begin
  Editor.PasteFromClipboard();
end;

procedure TSimbaDebugForm.MenuItemSelectAllClick(Sender: TObject);
begin
  Editor.SelectAll();
end;

procedure TSimbaDebugForm.TimerExecute(Sender: TObject);
var
  Scroll: Boolean;
  I: Int32;
begin
  FLock.Enter();

  try
    if (FStrings.Count > 0) then
    begin
      Editor.BeginUpdate(False);

      // auto scroll if already scrolled to bottom.
      Scroll := (Editor.Lines.Count < Editor.LinesInWindow) or ((Editor.Lines.Count + 1) = (Editor.TopLine + Editor.LinesInWindow));
      for I := 0 to FStrings.Count - 1 do
        Editor.Lines.Add(FStrings[I]);

      if Scroll then
        Editor.TopLine := Editor.Lines.Count;

      FStrings.Clear();

      Editor.EndUpdate();
      Editor.Invalidate();
    end;
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaDebugForm.SimbaSettingChanged(Setting: TSimbaSetting);
begin
  if (Setting = SimbaSettings.Editor.FontSize) then
    Editor.Font.Size := Setting.Value;

  if (Setting = SimbaSettings.Editor.FontName) then
  begin
    if SimbaFontHelpers.IsFontFixed(Setting.Value) then
      Editor.Font.Name := Setting.Value;
  end;

  if (Setting = SimbaSettings.Editor.AntiAliased) then
  begin
    if Setting.Value then
      Editor.Font.Quality := fqCleartypeNatural
    else
      Editor.Font.Quality := fqNonAntialiased;
  end;
end;

procedure TSimbaDebugForm.MenuItemCopyClick(Sender: TObject);
begin
  Editor.CopyToClipboard();
end;

procedure TSimbaDebugForm.Clear;
begin
  FLock.Enter();

  try
    FStrings.Clear();
  finally
    FLock.Leave();
  end;

  Editor.Clear();
end;

function TSimbaDebugForm.Listen(Pipe: TInputPipeStream): TThread;
begin
  Result := TListenerThread.Create(Pipe);
end;

constructor TSimbaDebugForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FStrings := TStringList.Create();
  FLock := TCriticalSection.Create();

  if SimbaSettings.Editor.AntiAliased.Value then
    Editor.Font.Quality := fqCleartypeNatural
  else
    Editor.Font.Quality := fqNonAntialiased;

  if SimbaFontHelpers.IsFontFixed(SimbaSettings.Editor.FontName.Value) then
    Editor.Font.Name := SimbaSettings.Editor.FontName.Value;

  Editor.Font.Color := clWindowText;
  Editor.Font.Size := SimbaSettings.Editor.FontSize.Value;

  SimbaSettings.RegisterChangeHandler(@SimbaSettingChanged);
end;

destructor TSimbaDebugForm.Destroy;
begin
  if (FLock <> nil) then
    FreeAndNil(FLock);
  if (FStrings <> nil) then
    FreeAndNil(FStrings);

  if (SimbaSettings <> nil) then
    SimbaSettings.UnRegisterChangeHandler(@SimbaSettingChanged);

  inherited Destroy();
end;

end.

