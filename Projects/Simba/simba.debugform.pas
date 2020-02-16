unit simba.debugform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, SynEdit, syncobjs;

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
  synedittypes,
  simba.settings;

type
  TSynEdit_Helper = class Helper for TSynEdit
    procedure Add(Text: String);
  end;

procedure TSynEdit_Helper.Add(Text: String);
begin
  SetSelTextPrimitive(smNormal, PChar(Text));
end;

procedure TSimbaDebugForm.Add(constref S: String);
begin
  WriteLn(S);

  FLock.Enter();

  try
    FStrings.Clear();
    FStrings.Add(S);

    TThread.Synchronize(TThread.CurrentThread, @InternalAdd);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaDebugForm.Add(Strings: TStrings);
begin
  FLock.Enter();

  try
    FStrings.AddStrings(Strings, True);

    TThread.Synchronize(TThread.CurrentThread, @InternalAdd);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaDebugForm.InternalAdd;
var
  Caret: TPoint;
  Line: Int32;
  Scroll: Boolean;
  I: Int32;
begin
  Editor.BeginUpdate(False);

  with Editor do
  try
    Scroll := TopLine = (Lines.Count - LinesInWindow) + 1;
    Line := TopLine;
    Caret := CaretXY;

    try
      CaretX := 0;
      CaretY := Lines.Count;

      for I := 0 to FStrings.Count - 1 do
        Add(FStrings[I] + LineEnding);
    finally
   //   CaretXY := Caret;
    end;

   Editor.TopLine := Lines.Count;

   // if Scroll then
    //  TopLine := (Lines.Count - LinesInWindow) + 1
   // else
   //   TopLine := Line;
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
  Editor.ClearAll();
end;

constructor TSimbaDebugForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FStrings := TStringList.Create();
  FLock := TCriticalSection.Create();

  Editor.Font.Color := clWindowText;
  Editor.Font.Quality := fqAntialiased;

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

