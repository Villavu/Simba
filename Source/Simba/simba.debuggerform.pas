unit simba.debuggerform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, Menus, syncobjs,
  simba.script_communication;

type
  TSimbaDebuggerList = class(TScrollBox)
  protected
    FItemHeight: Int32;
    FIndentWidth: Int32;
    FLock: TCriticalSection;
    FCount: Int32;
    FMethods: TStringArray;

    FEvents: TSimbaScriptDebuggerEvents;

    procedure FontChanged(Sender: TObject); override;

    function GetVisibleLines: Int32;

    procedure SetCount(Value: Int32);
    procedure SetItemHeight(Value: Int32);
  public
    procedure CopyToClipboard;

    procedure Clear;
    procedure ClearMethods;

    procedure AddMethod(Method: String);
    procedure AddEvents(Events: TSimbaScriptDebuggerEvents; Length: Int32);

    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: Integer; Raw: Boolean = False; WithThemeSpace: Boolean = True); override;
    procedure Paint; override;

    property Count: Int32 read FCount write SetCount;
    property LineHeight: Int32 read FItemHeight write SetItemHeight;
    property VisibleLines: Int32 read GetVisibleLines;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TSimbaDebuggerForm = class(TForm)
    PopupItemClear: TMenuItem;
    PopupItemCopy: TMenuItem;
    Popup: TPopupMenu;
    Timer: TTimer;

    procedure PopupItemClearClick(Sender: TObject);
    procedure PopupItemCopyClick(Sender: TObject);
    procedure TimerExecute(Sender: TObject);
  protected
    FList: TSimbaDebuggerList;
    FLock: TCriticalSection;
    FBuffer: TSimbaScriptDebuggerEvents;
    FEventCount: Int32;
    FHasEvents: Boolean;
  public
    procedure AddEvents(constref Events: PSimbaScriptDebuggerEvent; constref Count: Int32);
    procedure AddMethod(Method: String);

    procedure Clear;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  math, clipbrd;

procedure TSimbaDebuggerList.AddEvents(Events: TSimbaScriptDebuggerEvents; Length: Int32);
begin
  FLock.Enter();

  try
    FEvents := Events;

    Count := Length;
  finally
    FLock.Leave();
  end;

  Invalidate();
end;

procedure TSimbaDebuggerList.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);

  with TBitmap.Create() do
  try
    Font := Self.Font;

    FItemHeight := Canvas.TextHeight('Fj');
    FIndentWidth := Canvas.TextWidth('Fj');
  finally
    Free();
  end;
end;

function TSimbaDebuggerList.GetVisibleLines: Int32;
begin
  Result := (ClientHeight div FItemHeight) + 1;
end;

procedure TSimbaDebuggerList.SetCount(Value: Int32);
begin
  if (FCount = Value) then
    Exit;

  FCount := Value;

  if (VertScrollBar.Position + VertScrollBar.Page = VertScrollBar.Range) then // Scroll to bottom
  begin
    ComputeScrollbars();

    VertScrollBar.Position := VertScrollBar.Range;
  end else
    ComputeScrollbars();
end;

procedure TSimbaDebuggerList.SetItemHeight(Value: Int32);
begin
  if FItemHeight = Value then
    Exit;

  FItemHeight := Value;

  ComputeScrollbars();
  Invalidate();
end;

procedure TSimbaDebuggerList.CopyToClipboard;
var
  I: Int32;
  S: String;
begin
  S := '';

  FLock.Enter();

  try
    for I := 0 to FCount - 1 do
      S := S + StringOfChar(' ', FEvents[I].Depth * 2) + FMethods[FEvents[I].Method] + LineEnding;
  finally
    FLock.Leave();
  end;

  try
    Clipboard.AsText := S;
  except
  end;
end;

procedure TSimbaDebuggerList.Clear;
begin
  FLock.Enter();

  try
    Count := 0;
    ClearMethods();
  finally
    FLock.Leave();
  end;

  Invalidate();
end;

procedure TSimbaDebuggerList.ClearMethods;
begin
  FMethods := nil;
end;

procedure TSimbaDebuggerList.AddMethod(Method: String);
begin
  SetLength(FMethods, Length(FMethods) + 1);
  FMethods[High(FMethods)] := Method;
end;

procedure TSimbaDebuggerList.GetPreferredSize(var PreferredWidth, PreferredHeight: Integer; Raw: Boolean; WithThemeSpace: Boolean);
begin
  PreferredWidth := 2000;
  PreferredHeight := (FCount * FItemHeight) + HorzScrollBar.Size;
end;

procedure TSimbaDebuggerList.Paint;
var
  Index: Int32;
  I: Int32;
begin
  if FLock.TryEnter() then
  try
    if Count > 0 then
    begin
      Index := VertScrollBar.Position div FItemHeight;

      for I := Index to Min(FCount - 1, Index + VisibleLines) do
      begin
        if FEvents[I].Exception then
          Canvas.Brush.Color := 1446122
        else
          Canvas.Brush.Color := clWindow;

        Canvas.FillRect(0, I * FItemHeight, 2000, (I * FItemHeight)+FItemHeight);
        Canvas.TextOut(2 + (FIndentWidth * (FEvents[I].Depth * 2)), I * FItemHeight, FMethods[FEvents[I].Method]);
      end;
    end;
  finally
    FLock.Leave();
  end;
end;

constructor TSimbaDebuggerList.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLock := TCriticalSection.Create();

  VertScrollBar.Tracking := True;
  HorzScrollBar.Tracking := True;
end;

destructor TSimbaDebuggerList.Destroy;
begin
  FLock.Free();

  inherited Destroy();
end;

procedure TSimbaDebuggerForm.TimerExecute(Sender: TObject);
begin
  FLock.Enter();

  try
    if FHasEvents then
      FList.AddEvents(Copy(FBuffer), FEventCount);

    FHasEvents := False;
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaDebuggerForm.PopupItemCopyClick(Sender: TObject);
begin
  FList.CopyToClipboard();
end;

procedure TSimbaDebuggerForm.PopupItemClearClick(Sender: TObject);
begin
  Clear();
end;

procedure TSimbaDebuggerForm.AddEvents(constref Events: PSimbaScriptDebuggerEvent; constref Count: Int32);
begin
  FLock.Enter();

  try
    if FEventCount + Count < Length(FBuffer) then
    begin
      Move(Events[0], FBuffer[FEventCount], Count * SizeOf(TSimbaScriptDebuggerEvent));

      FEventCount := FEventCount + Count;
    end else
    begin
      if Count >= Length(FBuffer) then
        Move(Events[Count - Length(FBuffer)], FBuffer[0], Length(FBuffer) * SizeOf(TSimbaScriptDebuggerEvent))
      else
      begin
        Move(FBuffer[Count], FBuffer[0], (Length(FBuffer) - Count) * SizeOf(TSimbaScriptDebuggerEvent));
        Move(Events[0], FBuffer[Length(FBuffer) - Count], Count * SizeOf(TSimbaScriptDebuggerEvent));
      end;

      FEventCount := Length(FBuffer);
    end;

    FHasEvents := True;
  finally
    FLock.Leave();
  end;
end;

procedure TSimbaDebuggerForm.AddMethod(Method: String);
begin
  FList.AddMethod(Method);
end;

procedure TSimbaDebuggerForm.Clear;
begin
  FLock.Enter();

  try
    FList.Clear();

    FEventCount := 0;
  finally
    FLock.Leave();
  end;
end;

constructor TSimbaDebuggerForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLock := TCriticalSection.Create();

  FList := TSimbaDebuggerList.Create(Self);
  FList.Parent := Self;
  FList.Align := alClient;
  FList.Color := clWindow;
  FList.Font.Color := clWindowText;
  FList.PopupMenu := Popup;

  SetLength(FBuffer, 32768);
end;

destructor TSimbaDebuggerForm.Destroy;
begin
  FLock.Free();

  inherited Destroy();
end;

initialization
  {$I simba.debuggerform.lrs}

end.

