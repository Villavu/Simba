unit simba.debugform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, SynEdit, Menus, syncobjs;

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

    procedure SettingChanged_EditorFont(Value: String);
    procedure SettingChanged_EditorFontHeight(Value: Int64);
  public
    procedure Clear;

    procedure Add(constref S: String); overload;
    procedure Add(Strings: TStrings); overload;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SimbaDebugForm: TSimbaDebugForm;

implementation

uses
  simba.settings;

procedure TSimbaDebugForm.Add(constref S: String);
var
  Line: String;
begin
  FLock.Enter();

  try
    for Line in S.Split([LineEnding]) do
      FStrings.Add(Line);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaDebugForm.Add(Strings: TStrings);
var
  Line: String;
begin
  FLock.Enter();

  try
    for Line in Strings.Text.Split([LineEnding]) do
      FStrings.Add(Line);
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
    if FStrings.Count > 0 then
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
    end;
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaDebugForm.MenuItemCopyClick(Sender: TObject);
begin
  Editor.CopyToClipboard();
end;

procedure TSimbaDebugForm.SettingChanged_EditorFont(Value: String);
begin
  if Value <> '' then
    Editor.Font.Name := Value;
end;

procedure TSimbaDebugForm.SettingChanged_EditorFontHeight(Value: Int64);
begin
  Editor.Font.Height := Value;
end;

procedure TSimbaDebugForm.Clear;
begin
  Editor.Lines.Clear();
end;

constructor TSimbaDebugForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FStrings := TStringList.Create();
  FLock := TCriticalSection.Create();

  Editor.Font.Color := clWindowText;
  {$IFDEF DARWIN}
  Editor.Font.Quality := fqAntialiased;
  {$ELSE}
  Editor.Font.Quality := fqDefault; // weird one, I know
  {$ENDIF}

  SimbaSettings.Editor.FontName.AddHandlerOnChange(@SettingChanged_EditorFont);
  SimbaSettings.Editor.FontHeight.AddHandlerOnChange(@SettingChanged_EditorFontHeight);
end;

destructor TSimbaDebugForm.Destroy;
begin
  FLock.Free();
  FStrings.Free();

  inherited Destroy();
end;

initialization
  {$I simba.debugform.lrs}

end.

