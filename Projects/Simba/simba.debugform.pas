unit simba.debugform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ATSynEdit, LCLType, syncobjs, ATStringProc;

type
  TSimbaDebugForm = class(TForm)
  protected
    FContentsLength: SizeUInt;
    FEditor: TATSynEdit;
    FLock: TCriticalSection;
    FStrings: TStringList;

    procedure _Add;
    procedure _ScrollToBottom(Data: PtrInt);

    procedure StringsChanged(Sender: TObject; AChange: TATLineChangeKind; ALine, AItemCount: integer);

    procedure SettingChanged_EditorFont(Value: String);
    procedure SettingChanged_EditorFontHeight(Value: Int64);
  public
  const
    CAPACITY = 750000; // Maximum characters the debug can hold. Performance reasons.
    CAPACITY_LINE = 300000; // Maximum characters a line can be. Performance reasons.
  public
    procedure Clear;

    procedure Add(constref S: String); overload;
    procedure Add(Strings: TStrings); overload;

    procedure ScrollToBottom;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  SimbaDebugForm: TSimbaDebugForm;

implementation

uses
  math,
  simba.settings, ATStrings;

procedure TSimbaDebugForm._ScrollToBottom(Data: PtrInt);
begin
  FEditor.DoScrollToBeginOrEnd(False);
  FEditor.Invalidate();
end;

procedure TSimbaDebugForm.StringsChanged(Sender: TObject; AChange: TATLineChangeKind; ALine, AItemCount: integer);
var
  Strings: TATStrings absolute Sender;
begin
  FEditor.Fold.Update(AChange, ALine, AItemCount);

  case AChange of
    cLineChangeDeletedAll:
      FContentsLength := 0;

    cLineChangeDeleted:
      for ALine := ALine to Min(ALine + AItemCount - 1, Strings.Count - 1) do
        FContentsLength -= Length(Strings.Lines[ALine]);

    cLineChangeAdded:
      for ALine := ALine to Min(ALine + AItemCount - 1, Strings.Count - 1) do
        FContentsLength += Length(Strings.Lines[ALine]);
  end;
end;

procedure TSimbaDebugForm.ScrollToBottom;
begin
  Application.QueueAsyncCall(@_ScrollToBottom, 0);
end;

procedure TSimbaDebugForm._Add;
var
  i: Int32;
  P: TPoint;
begin
  FEditor.BeginUpdate();
  if (FEditor.ScrollVert.NPos = FEditor.ScrollVert.NPosLast) then
    ScrollToBottom();

  for i := 0 to FStrings.Count - 1 do
  begin
    if FContentsLength > CAPACITY then
    begin
      WriteLn('Debug exceeds capacity. Removing old lines; Write to a file if this causes issues.');
      while FContentsLength > (CAPACITY * 0.85) do
        FEditor.Strings.LineDelete(0);
    end;

    if Length(FStrings[i]) > CAPACITY_LINE then
      FStrings[i] := 'Line exceeds capacity. Cannot print; you should write to a file.';

    FEditor.Strings.TextAppend(FStrings[i] + LineEnding, P, P);
  end;

  FEditor.Update(True, False);
  FEditor.EndUpdate();
end;

procedure TSimbaDebugForm.Clear;
begin
  FEditor.BeginUpdate();
  FEditor.Strings.Clear();
  FEditor.Strings.ActionAddFakeLineIfNeeded();
  Feditor.DoCaretSingle(0, 0);
  FEditor.Update(True, True);
  FEditor.EndUpdate();
end;

procedure TSimbaDebugForm.Add(constref S: String);
begin
  WriteLn(S);

  FLock.Enter();

  try
    FStrings.Clear();
    FStrings.Add(S);

    TThread.Synchronize(TThread.CurrentThread, @_Add);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaDebugForm.Add(Strings: TStrings);
begin
  FLock.Enter();

  try
    FStrings.AddStrings(Strings, True);

    TThread.Synchronize(TThread.CurrentThread, @_Add);
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaDebugForm.SettingChanged_EditorFont(Value: String);
begin
  FEditor.Font.Name := Value;
end;

procedure TSimbaDebugForm.SettingChanged_EditorFontHeight(Value: Int64);
begin
  FEditor.Font.Height := Value;
end;

constructor TSimbaDebugForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FStrings := TStringList.Create();
  FLock := TCriticalSection.Create();

  FEditor := TATSynEdit.Create(Self);
  with FEditor do
  begin
    Parent := Self;
    Align := alClient;

    OptWrapMode := cWrapOn;
    OptUnprintedVisible := false;
    OptRulerVisible:= false;
    OptGutterVisible := False;
    OptMouseDragDrop := False;
    OptMouseWheelZooms := False;
    OptShowMouseSelFrame := False;
    OptCaretManyAllowed := False;
    OptFoldEnabled := False;
    OptAutoIndent := False;
    OptShowCurLineMinimal := False;
    OptShowURLs := False;
    OptShowIndentLines := False;
    OptMarginRight := $FFFF;
    OptScrollSmooth := False;

    Strings.OnChange := @StringsChanged;
    Strings.UndoLimit := 0;
  end;

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

