unit simba.debugform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, SynEdit, lcltype, lclintf, syncobjs;

type
  TSimbaDebugForm = class(TForm)
    Editor: TSynEdit;
  protected
    FLock: TCriticalSection;
    FStrings: TStringList;

    procedure InternalAdd; overload;

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
begin
  WriteLn(S);

  FLock.Enter();

  try
    FStrings.Text := S;

    TThread.Synchronize(TThread.CurrentThread, @InternalAdd);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaDebugForm.Add(Strings: TStrings);
begin
  FLock.Enter();

  try
    FStrings.Text := Strings.Text;

    TThread.Synchronize(TThread.CurrentThread, @InternalAdd);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaDebugForm.InternalAdd;
var
  Scroll: Boolean;
  I: Int32;
begin
  Editor.BeginUpdate(False);

  with Editor do
  try
    // auto scroll if already scrolled to bottom.
    Scroll := (Editor.Lines.Count < Editor.LinesInWindow) or ((Editor.Lines.Count + 1) = (Editor.TopLine + Editor.LinesInWindow));
    for I := 0 to FStrings.Count - 1 do
      Lines.Add(FStrings[I]);

    if Scroll then
      Editor.TopLine := Editor.Lines.Count;
  finally
    Editor.EndUpdate();
  end;
end;

procedure TSimbaDebugForm.SettingChanged_EditorFont(Value: String);
begin
  Editor.Font.Name := Value;
end;

procedure TSimbaDebugForm.SettingChanged_EditorFontHeight(Value: Int64);
begin
  Editor.Font.Height := Value;
end;

procedure TSimbaDebugForm.Clear;
begin
  Editor.Lines.Clear;
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

