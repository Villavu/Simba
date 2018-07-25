unit script_import_simba;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

implementation

uses
  script_imports, script_thread, lpcompiler, lptypes, ffi, mufasatypes, simbaunit, debugimage,
  Dialogs, ExtCtrls, Forms, Controls, aca, aca_math, dtm_editor, bitmaps, math;

type
  TBalloonHint = class
    Params: PParamArray;

    procedure Execute;
  end;

procedure TBalloonHint.Execute;
type
  PBalloonFlags = ^TBalloonFlags;
begin
  with SimbaForm.MTrayIcon do
  begin
    BalloonTitle := PString(Params^[1])^;
    BalloonHint := PString(Params^[2])^;
    BalloonFlags := PBalloonFlags(Params^[3])^;
    BalloonTimeout := PInt32(Params^[4])^;

    ShowBalloonHint();
  end;
end;

// procedure ShowBalloonHint(const Title, Hint: string; const Timeout: Integer; const Flag: TBalloonFlags);
procedure Lape_ShowBalloonHint(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  BalloonHint: TBalloonHint;
begin
  BalloonHint := TBalloonHint.Create();
  BalloonHint.Params := Params;

  TThread.Synchronize(nil, @BalloonHint.Execute);

  BalloonHint.Free();
end;

type
  TInputQuery = class
    Params: PParamArray;
    Return: Pointer;

    procedure Execute;
  end;

procedure TInputQuery.Execute;
begin
  PBoolean(Return)^ := InputQuery(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

// function InputQuery(const ACaption, APrompt: String; var Value: String): Boolean
procedure Lape_InputQuery(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  InputQuery: TInputQuery;
begin
  InputQuery := TInputQuery.Create();
  InputQuery.Params := Params;
  InputQuery.Return := Result;

  TThread.Synchronize(nil, @InputQuery.Execute);

  InputQuery.Free();
end;

type
  TMessageDialog = class
    Params: PParamArray;
    Result: Pointer;

    procedure Execute;
  end;

procedure TMessageDialog.Execute;
type
  PMsgDlgType = ^TMsgDlgType;
  PMsgDlgButtons = ^TMsgDlgButtons;
begin
  PInt32(Result)^ := MessageDlg(PString(Params^[1])^, PString(Params^[2])^, PMsgDlgType(Params^[3])^, PMsgDlgButtons(Params^[4])^, '');
end;

// function MessageDlg(const Caption, Message: string; DialogType: TMsgDlgType; Buttons: TMsgDlgButtons): Int32
procedure Lape_MessageDlg(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  MessageDialog: TMessageDialog;
begin
  MessageDialog := TMessageDialog.Create();
  MessageDialog.Params := Params;
  MessageDialog.Result := Result;

  TThread.Synchronize(nil, @MessageDialog.Execute);

  MessageDialog.Free();
end;

type
  TShowMessage = class
    Params: PParamArray;

    procedure Execute;
  end;

procedure TShowMessage.Execute;
begin
  ShowMessage(PString(Params^[1])^);
end;

// procedure ShowMessage(const Message: String);
procedure Lape_ShowMessage(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  ShowMessage: TShowMessage;
begin
  ShowMessage := TShowMessage.Create();
  ShowMessage.Params := Params;

  TThread.Synchronize(nil, @ShowMessage.Execute);

  ShowMessage.Free();
end;

type
  TClearDebug = class
    procedure Execute;
  end;

procedure TClearDebug.Execute;
begin
  SimbaForm.DebugMemo.Clear();
end;

// procedure ClearDebug;
procedure Lape_ClearDebug(const Params: PParamArray);
var
  ClearDebug: TClearDebug;
begin
  ClearDebug := TClearDebug.Create();

  TThread.Synchronize(nil, @ClearDebug.Execute);

  ClearDebug.Free();
end;

type
  TStatus = class
    Params: PParamArray;

    procedure Execute;
  end;

procedure TStatus.Execute;
begin
  SimbaForm.StatusBar.Panels[0].Text := PString(Params^[1])^;
end;

// procedure Status(const Status: String);
procedure Lape_Status(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Status: TStatus;
begin
  Status := TStatus.Create();
  Status.Params := Params;

  TThread.Synchronize(nil, @Status.Execute);

  Status.Free();
end;

type
  TDisguise = class
    Params: PParamArray;

    procedure Execute;
  end;

procedure TDisguise.Execute;
begin
  Application.Title := PString(Params^[1])^;

  SimbaForm.Caption := PString(Params^[1])^;
end;

// procedure Disguise(const Caption: String);
procedure Lape_Disguise(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Disguise: TDisguise;
begin
  Disguise := TDisguise.Create();
  Disguise.Params := Params;

  TThread.Synchronize(nil, @Disguise.Execute);

  Disguise.Free();
end;

type
  TDisplayDebugImgWindow = class
    Params: PParamArray;

    procedure Execute;
  end;

procedure TDisplayDebugImgWindow.Execute;
begin
  DebugImgForm.ShowDebugImgForm(Point(PInt32(Params^[1])^, PInt32(Params^[2])^));
end;

// procedure DisplayDebugImgWindow(W, H: Int32);
procedure Lape_DisplayDebugImgWindow(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  DisplayDebugImgWindow: TDisplayDebugImgWindow;
begin
  DisplayDebugImgWindow := TDisplayDebugImgWindow.Create();
  DisplayDebugImgWindow.Params := Params;

  TThread.Synchronize(nil, @DisplayDebugImgWindow.Execute);

  DisplayDebugImgWindow.Free();
end;

type
  TDrawBitmapDebugImg = class
    Params: PParamArray;

    procedure Execute;
  end;

procedure TDrawBitmapDebugImg.Execute;
begin
  with TMMLScriptThread(Params^[0]).Client do
    DebugImgForm.DrawBitmap(MBitmaps[PInt32(Params^[1])^]);
end;

// procedure DrawBitmapDebugImg(BMP: Int32);
procedure Lape_DrawBitmapDebugImg(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  DrawBitmapDebugImg: TDrawBitmapDebugImg;
begin
  DrawBitmapDebugImg := TDrawBitmapDebugImg.Create();
  DrawBitmapDebugImg.Params := Params;

  TThread.Synchronize(nil, @DrawBitmapDebugImg.Execute);

  DrawBitmapDebugImg.Free();
end;

type
  TGetDebugBitmap = class
    Params: PParamArray;
    Result: Pointer;

    procedure Execute;
  end;

procedure TGetDebugBitmap.Execute;
begin
  with TMMLScriptThread(Params^[0]).Client do
  begin
    PInt32(Result)^ := MBitmaps.CreateBMP(0, 0);

    DebugImgForm.GetDebugImage(MBitmaps[PInt32(Result)^]);
  end;
end;

// function GetDebugBitmap: Int32;
procedure Lape_GetDebugBitmap(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  GetDebugImage: TGetDebugBitmap;
begin
  GetDebugImage := TGetDebugBitmap.Create();
  GetDebugImage.Params := Params;
  GetDebugImage.Result := Result;

  TThread.Synchronize(nil, @GetDebugImage.Execute);

  GetDebugImage.Free();
end;

type
  TClearDebugImg = class
    procedure Execute;
  end;

procedure TClearDebugImg.Execute;
begin
  DebugImgForm.BlackDebugImage();
end;

// procedure ClearDebugImg;
procedure Lape_ClearDebugImg(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  ClearDebugImg: TClearDebugImg;
begin
  ClearDebugImg := TClearDebugImg.Create();

  TThread.Synchronize(nil, @ClearDebugImg.Execute);

  ClearDebugImg.Free();
end;

type
  TSync = class
    Params: PParamArray;

    procedure Execute;
  end;

procedure TSync.Execute;
type
  TSyncProcedure = procedure; {$IF DEFINED(CPU32) and DEFINED(LAPE_CDECL)}cdecl;{$ENDIF}
begin
  TSyncProcedure(PPointer(Params^[1])^)();
end;

// procedure Sync(Method: procedure);
procedure Lape_Sync(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Sync: TSync;
begin
  Sync := TSync.Create();
  Sync.Params := Params;

  TThread.Synchronize(nil, @Sync.Execute);

  Sync.Free();
end;

type
  TSyncEx = class
    Params: PParamArray;

    procedure Execute;
  end;

procedure TSyncEx.Execute;
type
  TSyncProcedure = procedure of object; {$IF DEFINED(CPU32) and DEFINED(LAPE_CDECL)}cdecl;{$ENDIF}
begin
  TSyncProcedure(Params^[1]^)();
end;

// procedure Sync(Method: procedure);
procedure Lape_SyncEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  Sync: TSyncEx;
begin
  Sync := TSyncEx.Create();
  Sync.Params := Params;

  TThread.Synchronize(nil, @Sync.Execute);

  Sync.Free();
end;

type
  TACA = class
    Params: PParamArray;

    procedure GetResult(CTS, Color, Tolerance: Int32; Hue, Sat: Extended);
    procedure Execute;
  end;

procedure TACA.GetResult(CTS, Color, Tolerance: Int32; Hue, Sat: Extended);
begin
  PInt32(Params^[2])^ := CTS;
  PInt32(Params^[3])^ := Color;
  PInt32(Params^[4])^ := Tolerance;

  PExtended(Params^[5])^ := Hue;
  PExtended(Params^[6])^ := Sat;
end;

procedure TACA.Execute;
begin
  with TACAForm.Create(TMMLScriptThread(Params^[0]).Client.IOManager) do
  begin
    OnGetResult := @GetResult;
    if (PString(Params^[1])^ <> '') then
      Caption := 'ACA - ' + PString(Params^[1])^;
    ShowModal();
  end;
end;

procedure Lape_ACA(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  ACA: TACA;
begin
  ACA := TACA.Create();
  ACA.Params := Params;

  TThread.Synchronize(nil, @ACA.Execute);

  ACA.Free();
end;

procedure Lape_ACAEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  case PInt32(Params^[2])^ of
    0: BestColor_CTS0(PIntegerArray(Params^[1])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
    1: BestColor_CTS1(PIntegerArray(Params^[1])^, PInt32(Params^[3])^, PInt32(Params^[4])^);
    2: BestColor_CTS2(PIntegerArray(Params^[1])^, PInt32(Params^[3])^, PInt32(Params^[4])^, PExtended(Params^[5])^, PExtended(Params^[6])^);
  end;
end;

type
  TDTMEditor = class
    Params: PParamArray;

    procedure GetResult(DTM: String);
    procedure Execute;
  end;

procedure TDTMEditor.GetResult(DTM: String);
begin
  PString(Params^[2])^ := DTM;
end;

procedure TDTMEditor.Execute;
begin
  with TDTMForm.Create(TMMLScriptThread(Params^[0]).Client.IOManager, SimbaForm.DebugMemo) do
  begin
    OnGetResult := @GetResult;
    if (PString(Params^[1])^ <> '') then
      Caption := 'DTM Editor - ' + PString(Params^[1])^;
    ShowModal();
  end;
end;

procedure Lape_DTMEditor(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  DTMEditor: TDTMEditor;
begin
  DTMEditor := TDTMEditor.Create();
  DTMEditor.Params := Params;

  TThread.Synchronize(nil, @DTMEditor.Execute);

  DTMEditor.Free();
end;

type
  TShowBitmap = class
    Params: PParamArray;

    procedure Execute;
  end;

procedure TShowBitmap.Execute;
var
  BMP: TMufasaBitmap;
begin
  BMP := PMufasaBitmap(Params^[1])^;

  if (not DebugImgForm.Visible) then
  begin
    with DebugImgForm.Monitor.WorkareaRect do
      DebugImgForm.SetBounds(Max(Left, (Left + Right - BMP.Width) div 2), Max(Top, (Top + Bottom - BMP.Height) div 2), BMP.Width, BMP.Height);
  end else
  begin
    DebugImgForm.Width := BMP.Width;
    DebugImgForm.Height := BMP.Height;
  end;

  DebugImgForm.ShowOnTop();
  DebugImgForm.DrawBitmap(BMP);
end;

procedure Lape_ShowBitmap(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  ShowBitmap: TShowBitmap;
begin
  ShowBitmap := TShowBitmap.Create();
  ShowBitmap.Params := Params;

  TThread.Synchronize(nil, @ShowBitmap.Execute);

  ShowBitmap.Free();
end;

type
  TShowBitmapEx = class
    Params: PParamArray;

    procedure Execute;
  end;

procedure TShowBitmapEx.Execute;
var
  BMP: TMufasaBitmap;
begin
  with TMMLScriptThread(Params^[0]).Client do
    BMP := MBitmaps[PInt32(Params^[1])^];

  if (not DebugImgForm.Visible) then
  begin
    with DebugImgForm.Monitor.WorkareaRect do
      DebugImgForm.SetBounds(Max(Left, (Left + Right - BMP.Width) div 2), Max(Top, (Top + Bottom - BMP.Height) div 2), BMP.Width, BMP.Height);
  end else
  begin
    DebugImgForm.Width := BMP.Width;
    DebugImgForm.Height := BMP.Height;
  end;

  DebugImgForm.ShowOnTop();
  DebugImgForm.DrawBitmap(BMP);
end;

procedure Lape_ShowBitmapEx(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var
  ShowBitmapEx: TShowBitmapEx;
begin
  ShowBitmapEx := TShowBitmapEx.Create();
  ShowBitmapEx.Params := Params;

  TThread.Synchronize(nil, @ShowBitmapEx.Execute);

  ShowBitmapEx.Free();
end;

procedure Lape_Import_Simba(Compiler: TLapeCompiler; Data: Pointer);
var
  AppPath: String = '';
  DocPath: String = '';
  IncludePath: String = '';
  PluginPath: String = '';
  FontPath: String = '';
begin
  if (Data <> nil) then
  begin
    AppPath := TMMLScriptThread(Data).AppPath;
    DocPath := TMMLScriptThread(Data).DocPath;
    IncludePath := TMMLScriptThread(Data).IncludePath;
    PluginPath := TMMLScriptThread(Data).PluginPath;
    FontPath := TMMLScriptThread(Data).FontPath;
  end;

  with Compiler do
  begin
    addGlobalVar(AppPath, 'AppPath').isConstant := True;
    addGlobalVar(DocPath, 'DocPath').isConstant := True;
    addGlobalVar(IncludePath, 'IncludePath').isConstant := True;
    addGlobalVar(PluginPath, 'PluginPath').isConstant := True;
    addGlobalVar(FontPath, 'FontPath').isConstant := True;

    addGlobalVar(mrNone, 'mrNone').isConstant := True;
    addGlobalVar(mrOK, 'mrOK').isConstant := True;
    addGlobalVar(mrCancel, 'mrCancel').isConstant := True;
    addGlobalVar(mrAbort, 'mrAbort').isConstant := True;
    addGlobalVar(mrRetry, 'mrRetry').isConstant := True;
    addGlobalVar(mrIgnore,'mrIgnore').isConstant := True;
    addGlobalVar(mrYes, 'mrYes').isConstant := True;
    addGlobalVar(mrNo, 'mrNo').isConstant := True;
    addGlobalVar(mrAll,'mrAll').isConstant := True;
    addGlobalVar(mrNoToAll, 'mrNoToAll').isConstant := True;
    addGlobalVar(mrYesToAll, 'mrYesToAll').isConstant := True;
    addGlobalVar(mrClose, 'mrClose').isConstant := True;

    addGlobalType('set of (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbAll, mbNoToAll, mbYesToAll, mbHelp, mbClose)', 'TMsgDlgButtons');
    addGlobalType('(mtWarning, mtError, mtInformation, mtConfirmation, mtCustom)', 'TMsgDlgType');
    addGlobalType('(bfNone, bfInfo, bfWarning, bfError)', 'TBalloonFlags');

    {$IF DEFINED(CPU32) and DEFINED(LAPE_CDECL)}
    addGlobalType('procedure();', 'TSyncMethod', FFI_CDECL);
    addGlobalType('procedure() of object;', 'TSyncObjectMethod', FFI_CDECL);
    {$ELSE}
    addGlobalType('procedure();', 'TSyncMethod', FFI_DEFAULT_ABI);
    addGlobalType('procedure() of object;', 'TSyncObjectMethod', FFI_DEFAULT_ABI);
    {$ENDIF}

    addGlobalMethod('procedure ShowBalloonHint(const Title, Hint: String; const Timeout: Int32; const Flag: TBalloonFlags);', @Lape_ShowBalloonHint, Data);
    addGlobalMethod('procedure ShowMessage(const Message: String);', @Lape_ShowMessage, Data);
    addGlobalMethod('function InputQuery(const Caption, Prompt: String; var Value: String): Boolean;', @Lape_InputQuery, Data);
    addGlobalMethod('function MessageDlg(const Caption, Message: string; DialogType: TMsgDlgType; Buttons: TMsgDlgButtons): Int32;', @Lape_MessageDlg, Data);
    addGlobalMethod('procedure ClearDebug;', @Lape_ClearDebug, Data);
    addGlobalMethod('procedure Status(const Status: String);', @Lape_Status, Data);
    addGlobalMethod('procedure Disguise(const Caption: String);', @Lape_Disguise, Data);
    addGlobalMethod('procedure DisplayDebugImgWindow(W, H: Int32);', @Lape_DisplayDebugImgWindow, Data);
    addGlobalMethod('procedure DrawBitmapDebugImg(BMP: Int32);', @Lape_DrawBitmapDebugImg, Data);
    addGlobalMethod('function GetDebugBitmap: Int32;', @Lape_GetDebugBitmap, Data);
    addGlobalMethod('procedure ClearDebugImg;', @Lape_ClearDebugImg, Data);
    addGlobalMethod('procedure ACAGUI(Title: String; out CTS, Color, Tolerance: Int32; out Hue, Sat: Extended);', @Lape_ACA, Data);
    addGlobalMethod('procedure ACA(Colors: TIntegerArray; CTS: Int32; out Color, Tolerance: Int32; out Hue, Sat: Extended);', @Lape_ACAEx, Data);
    addGlobalMethod('procedure Sync(Method: TSyncMethod); overload;', @Lape_Sync, Data);
    addGlobalMethod('procedure Sync(Method: TSyncObjectMethod); overload;', @Lape_SyncEx, Data);
    addGlobalMethod('procedure DTMEditor(Title: String; out DTM: String);', @Lape_DTMEditor, Data);
    addGlobalMethod('procedure ShowBitmap(BMP: TMufasaBitmap); overload;', @Lape_ShowBitmap, Data);
    addGlobalMethod('procedure ShowBitmap(BMP: Int32); overload;', @Lape_ShowBitmapEx, Data);
  end;
end;

initialization
  ScriptImports.Add('Simba', @Lape_Import_Simba);

end.

