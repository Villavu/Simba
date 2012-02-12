unit debugger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ActnList, uPSUtils, uPSRuntime, uPSDebugger, mmlpsthread;

type

  { TDebuggerForm }

  TDebuggerForm = class(TForm)
    RefreshAction: TAction;
    PlayAction: TAction;
    PauseAction: TAction;
    StopAction: TAction;
    StepIntoAction: TAction;
    StepOverAction: TAction;
    ActionList: TActionList;
    ImageList: TImageList;
    ToolBar: TToolBar;
    Info: TTreeView;
    PlayButton: TToolButton;
    PauseButton: TToolButton;
    StopButton: TToolButton;
    Sep_1: TToolButton;
    StepIntoButton: TToolButton;
    StepOverButton: TToolButton;
    Sep_2: TToolButton;
    RefreshButton: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PauseActionExecute(Sender: TObject);
    procedure PlayActionExecute(Sender: TObject);
    procedure RefreshActionExecute(Sender: TObject);
    procedure StepIntoActionExecute(Sender: TObject);
    procedure StepOverActionExecute(Sender: TObject);
    procedure StopActionExecute(Sender: TObject);
  private
    FRunning: Boolean;
    FActiveLine: LongInt;

    CurPos: Cardinal;
    ProcNo: Cardinal;
    Exec: string;

    TypeCount: LongInt;
    VarCount: LongInt;
    ProcCount: Cardinal;

    LocalParams: TStringList;
    LocalVars: TStringList;

    GlobalVars: TStringList;

    procedure _UpdateInfo();
    procedure _FillInfo();
    procedure FillInfo();
    procedure LineInfo(Sender: TObject; const FileName: string; Pos, Row, Col: Cardinal);
    procedure Idle(Sender: TObject);
    procedure BreakHandler(Sender: TObject; const FileName: ansistring; Po, Row, Col: Cardinal);
    procedure Load();
    procedure Unload();
    procedure UpdateActiveLine();
  public
    DebugThread: TMThread;
    procedure UpdateInfo();
    procedure ToggleRunning();
    procedure ShowForm();
    property Running: Boolean read FRunning;
  end; 

var
  DebuggerForm: TDebuggerForm;

implementation

uses
  SimbaUnit;

procedure TDebuggerForm.ToggleRunning;
begin
  FRunning := (not (FRunning));

  PlayAction.Enabled := (not (FRunning));
  PauseAction.Enabled := FRunning;
  StepIntoAction.Enabled := (not (FRunning));
  StepOverAction.Enabled := (not (FRunning));
end;

procedure TDebuggerForm.PlayActionExecute(Sender: TObject);
begin
  with TPSThread(DebugThread).PSScript do
    TThread.Synchronize(DebugThread, @Resume);

  ToggleRunning;
end;

procedure TDebuggerForm.RefreshActionExecute(Sender: TObject);
begin
  UpdateInfo();
end;

procedure TDebuggerForm.PauseActionExecute(Sender: TObject);
begin
  with TPSThread(DebugThread).PSScript do
    TThread.Synchronize(DebugThread, @Pause);

  ToggleRunning;
end;

procedure TDebuggerForm.Load();
var
  H, I: LongInt;
begin
  with TPSThread(DebugThread).PSScript do
  begin
    OnLineInfo := @LineInfo;
    OnIdle := @Idle;
    OnBreakPoint := @BreakHandler;

    H := SimbaForm.CurrScript.SynEdit.Marks.Count - 1;
    for I := 0 to H do
      SetBreakPoint(TPSThread(DebugThread).PSScript.MainFileName, SimbaForm.CurrScript.SynEdit.Marks.Items[I].Line + 1);
  end;
end;

procedure TDebuggerForm.FormShow(Sender: TObject);
begin
  FRunning := False;
  ToggleRunning();

  SimbaForm.CurrScript.ActiveLine := 0;

  TThread.Synchronize(DebugThread, @Load);
end;

procedure TDebuggerForm.Unload();
begin
  with TPSThread(DebugThread).PSScript do
  begin
    OnIdle := nil;
    OnBreakPoint := nil;
    OnLineInfo := nil;
  end;
end;

procedure TDebuggerForm.FormHide(Sender: TObject);
begin
  SimbaForm.CurrScript.ActiveLine := 0;

  TThread.Synchronize(DebugThread, @Unload);

  DebugThread := nil;
end;

procedure TDebuggerForm.FormCreate(Sender: TObject);
begin
  LocalParams := TStringList.Create;
  LocalVars := TStringList.Create;
  GlobalVars := TStringList.Create;
end;

procedure TDebuggerForm.FormDestroy(Sender: TObject);
begin
  GlobalVars.Free;
  LocalVars.Free;
  LocalParams.Free;
end;

procedure TDebuggerForm.StopActionExecute(Sender: TObject);
begin
  //with TPSThread(DebugThread).PSScript do
  //  TThread.Synchronize(DebugThread, @Stop); //No Point...
  MessageDlg('Use the normal Stop button to stop the script.', mtInformation, [mbOK], 0);
end;

procedure TDebuggerForm.StepIntoActionExecute(Sender: TObject);
begin
  with TPSThread(DebugThread).PSScript do
    TThread.Synchronize(DebugThread, @StepInto);
  UpdateInfo;
end;

procedure TDebuggerForm.StepOverActionExecute(Sender: TObject);
begin
  with TPSThread(DebugThread).PSScript do
    TThread.Synchronize(DebugThread, @StepOver);
  UpdateInfo;
end;

procedure TDebuggerForm._UpdateInfo();
var
  General, Local, Global, Temp: TTreeNode;
  I, H: LongInt;
begin
  Info.Items.Clear;
  General := Info.Items.AddChildFirst(nil, 'General');
    Info.Items.AddChild(General, 'Current Position: ' + IntToStr(CurPos));
    Info.Items.AddChild(General, 'Procedure Number: ' + IntToStr(ProcNo));
    Info.Items.AddChild(General, 'Last Exception: ' + Exec);
    Info.Items.AddChild(General, 'Type Count: ' + IntToStr(TypeCount));
    Info.Items.AddChild(General, 'Variable Count: ' + IntToStr(VarCount));
    Info.Items.AddChild(General, 'Method Count: ' + IntToStr(ProcCount));

  H := LocalParams.Count + LocalVars.Count;
  if (H > 0) then
  begin
    Local := Info.Items.AddChild(nil, 'Local');
      H := LocalParams.Count - 1;
      if (H > -1) then
      begin
        Temp := Info.Items.AddChild(Local, 'Parameters');
          for I := 0 to H do
            Info.Items.AddChild(Temp, LocalParams.Strings[I]);
      end;
      H := LocalVars.Count - 1;
      if (H > -1) then
      begin
        Temp := Info.Items.AddChild(Local, 'Variables');
          for I := 0 to H do
            Info.Items.AddChild(Temp, LocalVars.Strings[I]);
      end;
  end;

  H := GlobalVars.Count - 1;
  if (H >= 0) then
  begin
    Global := Info.Items.AddChild(nil, 'Global');
      Temp := Info.Items.AddChild(Global, 'Variables');
        for I := 0 to H do
          Info.Items.AddChild(Temp, GlobalVars.Strings[I]);
  end;
end;

procedure TDebuggerForm.UpdateInfo();
begin
  FillInfo();

  if (GetCurrentThreadId = MainThreadID) then
    _UpdateInfo()
  else
    TThread.Synchronize(nil, @_UpdateInfo);
end;

procedure TDebuggerForm._FillInfo();
  function BT2S(P: PIFTypeRec): string;
  var
    i: Longint;
  begin
    case p.BaseType of
      btU8: Result := 'UInt8';
      btS8: Result := 'Int8';
      btU16: Result := 'Uint16';
      btS16: Result := 'Int16';
      btU32: Result := 'UInt32';
      btS32: Result := 'Int32';
      {$IFNDEF PS_NOINT64}bts64: Result := 'Int64'; {$ENDIF}
      btChar: Result := {$IFDEF UNICODE}'AnsiChar'{$ELSE}'Char'{$ENDIF};
      {$IFNDEF PS_NOWIDESTRING}
      btWideChar: Result := 'WideChar';
      btWideString: Result := 'WideString';
      {$ENDIF}
      btSet: Result := 'Set';
      btSingle: Result := 'Single';
      btDouble: Result := 'Double';
      btExtended: Result := 'Extended';
      btString: Result := 'String';
      btRecord:
        begin
          Result := 'Record(';
          for i := 0 to TPSTypeRec_Record(p).FieldTypes.Count-1 do
          begin
            if i <> 0 then Result := Result+',';
            Result := Result + BT2S(PIFTypeRec(TPSTypeRec_Record(p).FieldTypes[i]));
          end;
          Result := Result + ')';
        end;
      btArray: Result := 'Array of '+BT2S(TPSTypeRec_Array(p).ArrayType);
      btResourcePointer: Result := 'ResourcePointer';
      btPointer: Result := 'Pointer';
      btVariant: Result := 'Variant';
      btClass: Result := 'Class';
      btProcPtr: Result := 'ProcPtr';
      btStaticArray: Result := 'StaticArray['+inttostR(TPSTypeRec_StaticArray(p).Size)+'] of '+BT2S(TPSTypeRec_Array(p).ArrayType);
    else
      Result := 'Unknown '+inttostr(p.BaseType);
    end;
  end;

  function ToStr(P: PIFVariant): string;
  var
    I: LongInt;
  begin
    Result := ': ' + BT2S(P^.FType);
    case P^.FType.BaseType of
      btS8: Result += ' = ' + IntToStr(PPSVariantU8(P)^.Data);
      btU8: Result += ' = ' + IntToStr(PPSVariantS8(P)^.Data);
      btS16: Result += ' = ' + IntToStr(PPSVariantU16(P)^.Data);
      btU16: Result += ' = ' + IntToStr(PPSVariantS16(P)^.Data);
      btS32: Result += ' = ' + IntToStr(PPSVariantU32(P)^.Data);
      btU32: Result += ' = ' + IntToStr(PPSVariantS32(P)^.Data);
      bts64: Result += ' = ' + IntToStr(PPSVariantS64(P)^.Data);
      btChar: Result += ' = ' + PPSVariantAChar(P)^.Data;
      {$IFNDEF PS_NOWIDESTRING}
      btWideChar: Result += ' = ' + PPSVariantWChar(P)^.Data;
      btWideString: Result += ' = ' + PPSVariantWString(P)^.Data;
      {$ENDIF}
      btSingle: Result += ' = ' + FloatToStr(PPSVariantSingle(P)^.Data);
      btDouble: Result += ' = ' + FloatToStr(PPSVariantDouble(P)^.Data);
      btExtended: Result += ' = ' + FloatToStr(PPSVariantExtended(P)^.Data);
      btString: Result += ' = ' + PPSVariantAString(P)^.Data;
      btResourcePointer: Result += ' = ' + IntToStr(LongInt(PPSVariantPointer(P)^.DataDest));
      btPointer: Result += ' = ' + IntToStr(LongInt(PPSVariantPointer(P)^.DataDest));
      btProcPtr: Result += ' = ' + IntToStr(LongInt(PPSVariantPointer(P)^.DataDest));
    end;
  end;
var
  I, H: LongInt;
begin
  with TPSThread(DebugThread) do
  begin
    CurPos := PSScript.Exec.GetCurrentPosition;
    ProcNo := PSScript.Exec.GetCurrentProcNo;

    Exec := PSScript.ExecErrorToString;

    TypeCount := PSScript.Exec.GetTypeCount;
    VarCount := PSScript.Exec.GetVarCount;
    ProcCount := PSScript.Exec.GetProcCount;

    LocalParams.Clear;
    H := PSScript.Exec.CurrentProcParams.Count - 1;
    for I := 0 to H do
      LocalParams.Add(PSScript.Exec.CurrentProcParams.Items[I] + ToStr(PSScript.Exec.GetProcParam(I)));

    LocalVars.Clear;
    H := PSScript.Exec.CurrentProcVars.Count - 1;
    for I := 0 to H do
      LocalVars.Add(PSScript.Exec.CurrentProcVars.Items[I] + ToStr(PSScript.Exec.GetProcVar(I)));

    GlobalVars.Clear;
    H := PSScript.Exec.GlobalVarNames.Count - 1;
    for I := 0 to H do
      GlobalVars.Add(PSScript.Exec.GlobalVarNames.Items[I] + ToStr(PSScript.Exec.GetGlobalVar(I)));
  end;
end;

procedure TDebuggerForm.FillInfo();
begin
  if (GetCurrentThreadId = DebugThread.ThreadID) then
    _FillInfo()
  else
    TThread.Synchronize(DebugThread, @_FillInfo);
end;

procedure TDebuggerForm.ShowForm();
begin
  if (not (DebuggerForm.Showing)) then
    DebuggerForm.Show;
end;

procedure TDebuggerForm.UpdateActiveLine();
begin
  SimbaForm.CurrScript.ActiveLine := FActiveLine;
end;

procedure TDebuggerForm.LineInfo(Sender: TObject; const FileName: string; Pos, Row, Col: Cardinal);
begin
  if (TPSThread(DebugThread).PSScript.Exec.DebugMode = dmStepOver) then
    Exit;

  if (TPSThread(DebugThread).PSScript.MainFileName <> FileName) then
  begin
    StepOverAction.Execute; //Hehe hack to skip functions inside includes =P
    Exit;
  end;

  FActiveLine := Row - 1;
  TThread.Synchronize(nil, @UpdateActiveLine);
end;

procedure TDebuggerForm.Idle(Sender: TObject);
begin
  //Idle handler... Needed to Pause.
end;

procedure TDebuggerForm.BreakHandler(Sender: TObject; const FileName: ansistring; Po, Row, Col: Cardinal);
begin
  {$IFDEF SIMBA_VERBOSE}
  mDebugLn(Format('BreakHandler(%d, "%s", %d, %d, %d);', [LongInt(Sender), FileName, Po, Row, Col]));
  {$ENDIF}

  if (FRunning) then
    TThread.Synchronize(nil, @ToggleRunning);
  UpdateInfo();
end;

initialization
  {$I debugger.lrs}

end.

