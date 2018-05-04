{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    MMLPSThread for the Mufasa Macro Library
}

unit mmlpsthread;

{$mode objfpc}{$H+}

{$I Simba.inc}

interface

uses
  Classes, SysUtils, client,
  MufasaTypes, MufasaBase, web, fontloader,
  bitmaps, plugins, dynlibs, internets,scriptproperties,
  settings, settingssandbox, lcltype, dialogs, ExtCtrls,
  lpparser, lpcompiler, lptypes, lpvartypes, ffi, lpffi, lpffiwrappers,
  lpeval, lpinterpreter, lputils, lpmessages, LPDump;

const
  m_Status = 0; //Data = PChar to new status
  m_Disguise = 1; //Data = PChar to new title
  m_DisplayDebugImgWindow = 2; //Data = PPoint to window size
  m_DrawBitmapDebugImg = 3; //Data = TMufasaBitmap
  m_GetDebugBitmap = 4; //Data = TMufasaBitmap
  m_ClearDebugImg = 5; //Data = nil
  m_ClearDebug = 6; //Data = nil
  m_InputQuery = 7; //Data = PInputQueryData
  m_ShowMessage = 8; //Data = PChar
  m_MessageBox = 9; //Data =  PMessageBoxData
  m_MessageDlg = 10; //Data = PMessageDlg
  m_BalloonHint = 11; //Data = PBalloonHintData

  {$I settings_const.inc}

type
    { TMMLPSThread }
    TCallBackData = record
      FormCallBack : procedure of object;
      cmd : integer;
      data : pointer;
    end;
    PCallBackData = ^TCallBackData;
    TSyncInfo = record
      V : MufasaTypes.PVariantArray;
      MethodName : string;
      Res : ^Variant;
      SyncMethod : procedure of object;
      OldThread : TThread;
    end;

    PSyncInfo = ^TSyncInfo;
    TErrorType = (errRuntime,errCompile);
    TOnError = procedure of object;
    TErrorData = record
      Row,Col,Position : integer;
      Error : string;
      ErrType : TErrorType;
      Module : string;
      IncludePath : string;
    end;
    PErrorData = ^TErrorData;
    TExpMethod = record
      Section : string;
      FuncDecl : string;
      FuncPtr : Pointer;
    end;
    TExpMethodArr = array of TExpMethod;
    TInputQueryData = record
      ACaption, APrompt,Value : String;
      Res : boolean;
    end;
    PInputQueryData = ^TInputQueryData;
    TMessageBoxData = record
      AText, ACaption : PChar;
      AFlags, res : integer;
    end;
    PMessageBoxData = ^TMessageBoxdata;
    TMessageDlgData = record
      ACaption, AMsg : string;
      ADlgType : TMsgDlgType;
      AButtons : TMsgDlgButtons;
      Res : integer;
    end;
    PMessageDlgData = ^TMessageDlgData;

    PBalloonHintData = ^TBalloonHintData;
    TBalloonHintData = record
      ATitle: String;
      AHint: String;
      ATimeout: Integer;
      AFlag: TBalloonFlags;
    end;

    { TMThread }

    TMThread = class(TThread)
    private
      procedure SetOpenConnectionEvent(const AValue: TOpenConnectionEvent);
      procedure SetOpenFileEvent(const AValue: TOpenFileEvent);
      procedure SetWriteFileEvent(const AValue: TWriteFileEvent);
    protected
      AppPath, DocPath, ScriptPath, ScriptFile, IncludePath, PluginPath, FontPath: string;
      DebugTo: TWritelnProc;
      Includes : TStringList;
      FOpenConnectionEvent : TOpenConnectionEvent;
      FWriteFileEvent : TWriteFileEvent;
      FOpenFileEvent : TOpenFileEvent;
      procedure LoadPlugin(plugidx: integer); virtual; abstract;
      function CallMethod(const Method: string; var Args: array of Variant): Variant; virtual; abstract;
      procedure HandleScriptTerminates; virtual;
    public
      Prop: TScriptProperties;
      Client: TClient;
      MInternet: TMInternet;
      Socks: TSocks;
      {$IFDEF USE_SQLITE}SQLite3: TMSQLite3;{$ENDIF}
      StartTime: LongWord;
      Settings: TMMLSettings;
      SimbaSettingsFile: String;
      Sett: TMMLSettingsSandbox;

      CallBackData : PCallBackData; //Handles general callback functions for threadsafety
      SyncInfo : PSyncInfo; //We need this for callthreadsafe
      ErrorData : PErrorData; //We need this for thread-safety etc
      OnError  : TOnError; //Error handeler

      CompileOnly : boolean;

      ExportedMethods: TExpMethodArr;

      procedure FormCallBackEx(cmd : integer; var data : pointer);
      procedure FormCallBack(cmd : integer; data : pointer);
      procedure HandleError(Row, Col, Pos: integer; Error: string; Typ: TErrorType; Filename: string);

      function LoadFile(ParentFile: string; var FileName, Content: string): boolean;

      procedure AddMethod(meth: TExpMethod); virtual;

      procedure SetDebug( writelnProc : TWritelnProc );
      procedure SetPath(ScriptP: string);
      procedure SetSettings(S: TMMLSettings; SimbaSetFile: String);
      procedure SetFonts(Fonts: TMFonts); virtual;

      procedure OnThreadTerminate(Sender: TObject);
      procedure SetScript(script: string); virtual; abstract;
      procedure Execute; override; abstract;
      procedure Terminate; virtual; abstract;

      constructor Create(CreateSuspended: boolean; TheSyncInfo : PSyncInfo; plugin_dir: string);
      destructor Destroy; override;

      class function GetExportedMethods: TExpMethodArr; virtual; abstract;

      property OpenConnectionEvent : TOpenConnectionEvent read FOpenConnectionEvent write SetOpenConnectionEvent;
      property WriteFileEvent : TWriteFileEvent read FWriteFileEvent write SetWriteFileEvent;
      property OpenFileEvent : TOpenFileEvent read FOpenFileEvent write SetOpenFileEvent;
    end;

   TLPThread = class(TMThread)
   protected
     procedure LoadPlugin(plugidx: integer); override;
     function CallMethod(const Method: string; var Args: array of Variant): Variant; override;
   public
     Parser: TLapeTokenizerString;
     Compiler: TLPCompiler;
     Running: TInitBool;
     ImportWrappers: TList;
     ExportWrappers: TList;

     constructor Create(CreateSuspended: Boolean; TheSyncInfo : PSyncInfo; plugin_dir: string);
     destructor Destroy; override;

     procedure HandleError(Row, Col, Pos: integer; Error: string; Typ: TErrorType; Filename: string);

     class function GetExportedMethods: TExpMethodArr; override;

     procedure SetScript(Script: string); override;
     procedure SetFonts(Fonts: TMFonts); override;
     procedure Execute; override;
     procedure Terminate; override;
     procedure OnHint(Sender: TLapeCompilerBase; Msg: lpString);
     function OnFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
     function OnHandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;
   end;

   TSyncMethod = class
   private
     FMethod: Pointer;
   public
     constructor Create(Method: Pointer);
     procedure Call;
   end;

threadvar
  CurrThread : TMThread;
var
  PluginsGlob: TMPlugins;

implementation

uses
  SimbaUnit,
  colour_conv, dtmutil,
  {$ifdef mswindows}windows,  MMSystem,{$endif}//MMSystem -> Sounds
  IOmanager,//TTarget_Exported
  IniFiles,//Silly INI files
  stringutil, //String st00f
  LazUTF8,
  newsimbasettings, // SimbaSettings
  files,
  dtm, //Dtms!
  Graphics, //For Graphics types
  math, //Maths!
  mmath, //Real maths!
  strutils,
  fileutil,
  tpa, //Tpa stuff
  mmltimer,
  forms,//Forms
  RegExpr,
  lclintf,  // for GetTickCount and others.
  Clipbrd,

  DCPcrypt2,
  DCPrc2, DCPrc4, DCPrc5, DCPrc6,
  DCPhaval, DCPmd4, DCPmd5,
  DCPripemd128, DCPripemd160,
  DCPsha1, DCPsha256, DCPsha512,
  DCPtiger;

{$MACRO ON}
{$define extdecl := register}

{Some General PS Functions here}
procedure psWriteln(str : string); extdecl;
begin
  if Assigned(CurrThread) and CurrThread.Prop.WriteTimeStamp then
    str := format('[%s]: %s', [TimeToStr(TimeStampToDateTime(MSecsToTimeStamp(GetTickCount - CurrThread.StartTime))), str]);
  if Assigned(CurrThread) and Assigned(CurrThread.DebugTo) then
    CurrThread.DebugTo(str)
  else
    mDebugLn(str);
end;

procedure ps_DebugLn(str : string); extdecl;
begin
  if Assigned(CurrThread) and CurrThread.Prop.WriteTimeStamp then
    str := format('[%s]: %s', [TimeToStr(TimeStampToDateTime(MSecsToTimeStamp(GetTickCount - CurrThread.StartTime))), str]);
  mDebugLn(str);
end;

{***implementation TMThread***}
constructor TMThread.Create(CreateSuspended: boolean; TheSyncInfo: PSyncInfo; plugin_dir: string);
begin
  inherited Create(CreateSuspended);

  Client := TClient.Create(plugin_dir);
  if Assigned(WriteFileEvent) then
    Client.MFiles.WriteFileEvent := WriteFileEvent;
  if Assigned(OpenFileEvent) then
    Client.MFiles.OpenFileEvent := OpenFileEvent;
  MInternet := TMInternet.Create(Client);
  Socks := TSocks.Create(Client);
  {$IFDEF USE_SQLITE}
  SQLite3 := TMSQLite3.Create(Client);
  {$ENDIF}
  if Assigned(OpenConnectionEvent) then
    MInternet.OpenConnectionEvent := Self.OpenConnectionEvent;
  SyncInfo:= TheSyncInfo;
  ExportedMethods:= GetExportedMethods;
  FreeOnTerminate := True;
  CompileOnly := false;
  OnTerminate := @OnThreadTerminate;
  OnError:= nil;
  Includes := TStringList.Create;
  Includes.CaseSensitive := {$ifdef linux}true{$else}false{$endif};
  Sett := nil;

  Prop := TScriptProperties.Create;

  AppPath := Application.Location;
  DocPath := SimbaSettings.Scripts.Path.Value;
  IncludePath := SimbaSettings.Includes.Path.Value;
  PluginPath := SimbaSettings.Plugins.Path.Value;
  FontPath := SimbaSettings.Fonts.Path.Value;
end;

destructor TMThread.Destroy;
begin
  MInternet.Free;
  Socks.Free;
  {$IFDEF USE_SQLITE}SQLite3.Free;{$ENDIF}
  Client.Free;
  Includes.free;
  Prop.Free;
  if Sett <> nil then
    Sett.Free;
  inherited Destroy;
end;

procedure TMThread.SetOpenConnectionEvent(const AValue: TOpenConnectionEvent);
begin
  FOpenConnectionEvent:= AValue;
  if Assigned(MInternet) then
    self.MInternet.OpenConnectionEvent := AValue;
end;

procedure TMThread.SetOpenFileEvent(const AValue: TOpenFileEvent);
begin
  FOpenFileEvent:= AValue;
  if Assigned(Client) then
    self.Client.MFiles.OpenFileEvent := AValue;
end;

procedure TMThread.SetWriteFileEvent(const AValue: TWriteFileEvent);
begin
  FWriteFileEvent:= AValue;
  if Assigned(Client) then
    self.Client.MFiles.WriteFileEvent := AValue;
end;

procedure TMThread.FormCallBackEx(cmd: integer; var data: pointer);
begin
  if (CallBackData = nil) or not Assigned(CallBackData^.FormCallBack) then
    exit;
  CallBackData^.cmd:= cmd;
  CallBackData^.data:= data;
  Synchronize(CallBackData^.FormCallBack);
  data := CallBackData^.data;
end;

procedure TMThread.FormCallBack(cmd: integer; data: pointer);
begin
  if (CallBackData = nil) or (not Assigned(CallBackData^.FormCallBack)) then
    exit;
  CallBackData^.cmd:= cmd;
  CallBackData^.data:= data;
  Synchronize(CallBackData^.FormCallBack);
end;

procedure TMThread.HandleError(Row, Col, Pos: integer; Error: string; Typ: TErrorType; Filename: string);
begin
  if (OnError = nil) then
    Exit;

  ErrorData^.Row := Row;
  ErrorData^.Col := Col;
  ErrorData^.Position := Pos;
  ErrorData^.Error := Error;
  ErrorData^.ErrType := Typ;
  ErrorData^.Module := Filename;
  ErrorData^.IncludePath := IncludePath;

  CurrThread.Synchronize(OnError);
end;

procedure TMThread.OnThreadTerminate(Sender: TObject);
begin

end;

procedure TMThread.AddMethod(meth: TExpMethod);
begin
  raise Exception.Create('AddMethod not Implememnted!');
end;

function TMThread.LoadFile(ParentFile: string; var FileName, Content: string): boolean;
begin
  Result := FindFile(FileName, [IncludeTrailingPathDelimiter(ExtractFileDir(ParentFile)), IncludePath, ScriptPath]);
  if (not (Result)) then
    Exit;

  if (Includes.IndexOf(FileName) = -1) then
    Includes.Add(FileName);

  try
    with TFileStream.Create(UTF8ToSys(FileName), fmOpenRead) do
    try
      SetLength(Content, Size);
      Read(Content[1], Length(Content));
    finally
      Free;
    end;
  except
    Result := False;
    psWriteln('ERROR in TMThread.LoadFile');
  end;
end;

procedure TMThread.SetDebug(writelnProc: TWritelnProc);
begin
  DebugTo := writelnProc;
  Client.WritelnProc:= writelnProc;
end;

procedure TMThread.SetSettings(S: TMMLSettings; SimbaSetFile: String);
begin
  Self.SimbaSettingsFile := SimbaSetFile;
  Self.Settings := S;
  Self.Sett := TMMLSettingsSandbox.Create(Self.Settings);
  Self.Sett.prefix := 'Scripts/';
end;

procedure TMThread.SetPath(ScriptP: string);
begin
  ScriptPath := IncludeTrailingPathDelimiter(ExpandFileName(ExtractFileDir(ScriptP)));
  ScriptFile := ExtractFileName(ScriptP);
end;

procedure TMThread.SetFonts(Fonts: TMFonts);
begin
  if Assigned(Fonts) then
    Client.MOCR.Fonts := Fonts;
end;

procedure TMThread.HandleScriptTerminates;
var
  V: array of Variant;
  index: integer;
  proc: String;
begin
  if (not (SP_OnTerminate in Prop.Properties)) then
    exit;

  SetLength(V, 0);
  index := 0;
  while (Prop.OnTerminateProcs.Count > 0) do
  begin
    proc := Prop.OnTerminateProcs[0];
    Prop.OnTerminateProcs.Delete(0); //call first incase proc invoke DeleteOnTerminate on itself

    if (not Prop.OnTerminateProcsSkip[index]) or (not TerminatedByUser) then
      CallMethod(proc, V);

    Inc(index);
  end;
end;

{$I PSInc/Wrappers/other.inc}
{$I PSInc/Wrappers/settings.inc}
{$I PSInc/Wrappers/bitmap.inc}

{$I PSInc/Wrappers/window.inc}
{$I PSInc/Wrappers/tpa.inc}
{$I PSInc/Wrappers/strings.inc}

{$I PSInc/Wrappers/crypto.inc}
{$I PSInc/Wrappers/colour.inc}
{$I PSInc/Wrappers/colourconv.inc}
{$I PSInc/Wrappers/math.inc}
{$I PSInc/Wrappers/mouse.inc}
{$I PSInc/Wrappers/file.inc}
{$I PSInc/Wrappers/keyboard.inc}
{$I PSInc/Wrappers/dtm.inc}
{$I PSInc/Wrappers/ocr.inc}
{$I PSInc/Wrappers/internets.inc}

type
  PBoolean = ^Boolean;
  PStringArray = ^TStringArray;
  PBmpMirrorStyle = ^TBmpMirrorStyle;
  PPointArray = ^TPointArray;
  P2DIntArray = ^T2DIntArray;
  PCanvas = ^TCanvas;
  P2DPointArray = ^T2DPointArray;
  PMask = ^TMask;
  PBox = ^TBox;
  PTarget_Exported = ^TTarget_Exported;
  PIntegerArray = ^TIntegerArray;
  PExtendedArray = ^TExtendedArray;
  P2DIntegerArray = ^T2DIntegerArray;
  PFont = ^TFont;
//  PStrExtr = ^TStrExtr;
  PReplaceFlags = ^TReplaceFlags;
  PClickType = ^TClickType;
  P2DExtendedArray = ^T2DExtendedArray;
  PMDTM = ^TMDTM;
  PMDTMPoint = ^TMDTMPoint;
  PSDTM = ^TSDTM;
  PMsgDlgType = ^TMsgDlgType;
  PMsgDlgButtons = ^TMsgDlgButtons;
  PClient = ^TClient;
  PStrings = ^TStrings;

threadvar
  WriteLnStr: string;

procedure lp_Write(Params: PParamArray); lape_extdecl
begin
  WriteLnStr += PlpString(Params^[0])^;
end;

procedure lp_WriteLn(Params: PParamArray); lape_extdecl
begin
  psWriteLn(WriteLnStr);
  WriteLnStr := '';
end;

procedure lp_DebugLn(Params: PParamArray); lape_extdecl
begin
  ps_debugln(PlpString(Params^[0])^);
end;

procedure lp_Sync(Params: PParamArray); lape_extdecl
var
  Method: TSyncMethod;
begin
  Method := TSyncMethod.Create(PPointer(Params^[0])^);
  try
    TThread.Synchronize(CurrThread, @Method.Call);
  finally
    Method.Free();
  end;
end;

procedure lp_CurrThreadID(Params: PParamArray; Result: Pointer); lape_extdecl
begin
  PPtrUInt(Result)^ := GetCurrentThreadID();
end;

{$I LPInc/Wrappers/lp_other.inc}

{$I LPInc/Wrappers/lp_settings.inc}
{$I LPInc/Wrappers/lp_bitmap.inc}

{$I LPInc/Wrappers/lp_window.inc}
{$I LPInc/Wrappers/lp_tpa.inc}
{$I LPInc/Wrappers/lp_strings.inc}
{$I LPInc/Wrappers/lp_colour.inc}
{$I LPInc/Wrappers/lp_colourconv.inc}
{$I LPInc/Wrappers/lp_crypto.inc}
{$I LPInc/Wrappers/lp_math.inc}
{$I LPInc/Wrappers/lp_mouse.inc}
{$I LPInc/Wrappers/lp_file.inc}
{$I LPInc/Wrappers/lp_keyboard.inc}
{$I LPInc/Wrappers/lp_dtm.inc}
{$I LPInc/Wrappers/lp_ocr.inc}
{$I LPInc/Wrappers/lp_internets.inc}

constructor TLPThread.Create(CreateSuspended: Boolean; TheSyncInfo: PSyncInfo; plugin_dir: string);
  procedure SetCurrSection(x: string); begin end;
var
  I: integer;
begin
  inherited Create(CreateSuspended, TheSyncInfo, plugin_dir);

  Parser := TLapeTokenizerString.Create('');
  Compiler := TLPCompiler.Create(Parser);
  Running := bFalse;

  InitializeFFI(Compiler);
  InitializePascalScriptBasics(Compiler);
  ExposeGlobals(Compiler);

  Compiler['Move'].Name := 'MemMove';
  Compiler.OnHint     := @OnHint;
  Compiler.OnFindFile := @OnFindFile;
  Compiler.OnHandleDirective := @OnHandleDirective;

  with Compiler do
  begin
    WriteLnStr := '';

    StartImporting;

    {$I LPInc/lpdefines.inc}
    {$I LPInc/lpcompile.inc}

    addGlobalFunc('procedure _write(s: string); override;', @lp_Write);
    addGlobalFunc('procedure _writeln; override;', @lp_WriteLn);
    addGlobalFunc('procedure DebugLn(s: string);', @lp_DebugLn);

    addNativeGlobalType('procedure();', 'TSyncMethod');
    addGlobalFunc('procedure Sync(Proc: TSyncMethod);', @lp_Sync);
    addGlobalFunc('function GetCurrThreadID(): PtrUInt;', @lp_CurrThreadID);

    for I := 0 to High(VirtualKeys) do
      addGlobalVar(VirtualKeys[I].Key, Format('VK_%S', [VirtualKeys[i].Str])).isConstant := True;

    RegisterLCLClasses(Compiler);
    RegisterMMLClasses(Compiler);

    addGlobalVar('TClient', @Client, 'Client');
    addGlobalVar('TMMLSettingsSandbox', @Sett, 'Settings');

    {$I LPInc/lpexportedmethods.inc}

    EndImporting;
  end;

  ImportWrappers := TList.Create;
  ExportWrappers := TList.Create;
end;

destructor TLPThread.Destroy;
begin
  try
    if Assigned(ImportWrappers) then
    begin
      while ImportWrappers.Count <> 0 Do
      begin
        TImportClosure(ImportWrappers.First).Free;
        ImportWrappers.Delete(0)
      end;
      ImportWrappers.Free;
    end;

    if Assigned(ExportWrappers) then
    begin
      while ExportWrappers.Count <> 0 Do
      begin
        TExportClosure(ExportWrappers.First).Free;
        ExportWrappers.Delete(0)
      end;
      ExportWrappers.Free;
    end;

    if (Assigned(Compiler)) then
      Compiler.Free
    else if (Assigned(Parser)) then
      Parser.Free;
  except
    on E: Exception do
      psWriteln('Exception TLPThread.Destroy: ' + e.message);
  end;

  inherited Destroy;
end;

class function TLPThread.GetExportedMethods: TExpMethodArr;
var
  c : integer;
  CurrSection : string;

procedure SetCurrSection(str : string);
begin;
  CurrSection := Str;
end;

procedure AddGlobalFunc(DeclStr: string; Ptr: Pointer);
begin;
  if c >= 600 then
    raise exception.create('TMThread.LoadMethods: Exported more than 600 functions');

  Result[c].FuncDecl:= DeclStr;
  Result[c].FuncPtr:= Ptr;
  Result[c].Section:= CurrSection;

  Inc(c);
end;

begin
  c := 0;
  CurrSection := 'Other';
  SetLength(Result, 550);

  {$DEFINE FUNC_LIST}
  {$i LPInc/lpexportedmethods.inc}
  {$UNDEF FUNC_LIST}

  SetLength(Result, c);
end;

procedure TLPThread.SetScript(Script: string);
begin
  Parser.Doc := Script;
end;

procedure TLPThread.SetFonts(Fonts: TMFonts);
var
  i: Integer;
begin
  inherited;

  Compiler.StartImporting;

  if Assigned(Fonts) then
    for I := Fonts.Count - 1 downto 0 do
      Compiler.addGlobalVar(Fonts[I].Name, Fonts[I].Name).isConstant := True;

  Compiler.EndImporting;
end;

procedure TLPThread.OnHint(Sender: TLapeCompilerBase; Msg: lpString);
begin
  psWriteLn(Msg);
end;

function TLPThread.OnFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
begin
  Result := nil;
  if (not FindFile(FileName, [IncludeTrailingPathDelimiter(ExtractFileDir(Sender.Tokenizer.FileName)), IncludePath, ScriptPath])) then
    FileName := '';
end;

function TLPThread.OnHandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek, InIgnore: Boolean): Boolean;
var
  plugin_idx: integer;
begin
  Result := False;
  if (not InPeek) and (CompareText(Directive,'LOADLIB') = 0) then
  begin
    if (Argument <> '') then
    begin
      Result := True;
      if (InIgnore) then
        Exit;

      plugin_idx := PluginsGlob.LoadPlugin(Argument);
      if (plugin_idx >= 0) then
        LoadPlugin(plugin_idx)
      else
        psWriteln(Format('Your DLL %s has not been found', [Argument]))
    end else
      psWriteln('Your LoadLib directive has no params, thus cannot find the plugin');
  end;
end;

procedure TLPThread.LoadPlugin(plugidx: integer); 
var
  I: integer;
  Wrapper: TImportClosure;
  method: String;
  
  //Check if the string ends with an `Native`-keyword.
  function isNative(str:String; var Res:String): boolean;
  var len:Int32;
  begin
    Res := Trim(Str);
    Len := Length(Res);
    if (Res[len] = ';') then Dec(Len);
    if not (LowerCase(Copy(Res, len-5, 6)) = 'native') then
      Exit(False);
    Dec(len,6);
    SetLength(Res, len);
    while Res[len] in [#9,#10,#13,#32] do Dec(len);
    Result := (Res[len] = ';');
  end;
  function MufCC2LapeCC(MufCC: Integer): TFFIABI;
  begin
    case MufCC of
      {$IFDEF CPU86}
      cv_default: Result := FFI_CDECL;
      cv_StdCall: Result := FFI_STDCALL;
      cv_Register: Result := FFI_REGISTER;
      {$ENDIF}
      {$IFDEF CPUX86_64}
      {$IFDEF UNIX}
      cv_StdCall, cv_default, cv_Register: Result := FFI_UNIX64;
      {$ENDIF}
      {$IFDEF MSWINDOWS}
      cv_StdCall, cv_default, cv_Register: Result := FFI_WIN64;
      {$ENDIF}
      {$ENDIF}
      {$IFDEF ARM}
      cv_StdCall, cv_default, cv_Register: Result := FFI_CDECL;
      {$ENDIF}
      else
        raise Exception.CreateFmt('Unknown Calling Convention [%d]', [MufCC]);
    end;
  end;
begin
  with PluginsGlob.MPlugins[plugidx] do
  begin
    {$IFDEF CPU32}
    if ABI < 2 then
    begin
      psWriteln('Skipping plugin due to ABI being older than version 2');
      exit; // Can't set result?
    end;
    {$ENDIF}

    if not FFILoaded() then
    begin
      writeln('Not loading plugin for lape - libffi not found');
      raise EAssertionFailed.Create('libffi is not loaded');
    end;

    Compiler.StartImporting;

    for i := 0 to TypesLen -1 do
      with Types[I] do
        Compiler.addGlobalType(TypeDef, TypeName);

    for i := 0 to MethodLen - 1 do
    begin
      if isNative(Methods[i].FuncStr, Method) then
        Compiler.addGlobalFunc(Method, Methods[i].FuncPtr)
      else begin
        Wrapper := LapeImportWrapper(Methods[i].FuncPtr, Compiler, Methods[i].FuncStr, MufCC2LapeCC(Methods[i].FuncConv));
        Compiler.addGlobalFunc(Methods[i].FuncStr, Wrapper.func);
        ImportWrappers.Add(Wrapper);
      end;
    end;

    Compiler.EndImporting;
  end;
end;

function TLPThread.CallMethod(const Method: string; var Args: array of Variant): Variant;
begin
  Result := Unassigned;

  if (not FFILoaded) then
    raise Exception.Create('libffi seems to be missing!');

  if (Length(Args) > 0) then
    raise Exception.Create('Lape''s CallMethod only supports procedures with no arguments.');

  with LapeExportWrapper(Compiler.Globals[Method]) do
  try
    TProcedure(func)();
  finally
    Free;
  end;
end;

procedure TLPThread.Execute;
var
  Failed: boolean;
begin
  CurrThread := Self;
  Starttime := GetTickCount;

  try
    Failed := not Compiler.Compile();
  except
    on e: lpException do
    begin
      HandleError(e.DocPos.Line, e.DocPos.Col, 0, e.OldMsg, errCompile, e.DocPos.FileName);
      Failed := True;
    end;
    on e: Exception do
    begin
      HandleError(0, 0, 0, e.message, errCompile, '');
      Failed := True;
    end;
  end;

  if (not (Failed)) then
  begin
    psWriteln('Compiled successfully in ' + IntToStr(GetTickCount - Starttime) + ' ms.');

    if CompileOnly then
      Exit;

    Running := bTrue;
    try
      RunCode(Compiler.Emitter.Code, Running);
      HandleScriptTerminates();
    except
      on e: lpException do
      begin
        HandleError(e.DocPos.Line, e.DocPos.Col, 0, e.OldMsg, errRuntime, e.DocPos.FileName);
        Failed := True;
      end;
      on e: Exception do
      begin
        HandleError(0, 0, 0, e.message, errRuntime, '');
        Failed := True;
      end;
    end;

    if (not (Failed)) then
      psWriteln('Successfully executed.')
    else
      psWriteLn('Execution failed.');
  end else
    psWriteln('Compiling failed.');
end;

procedure TLPThread.Terminate;
begin
  Running := bFalse;
end;

procedure TLPThread.HandleError(Row, Col, Pos: integer; Error: string; Typ: TErrorType; Filename: string);
begin
  if (PosEx('Runtime error: "', Error, 1) = 1) then
    Error := Copy(Error, 17, Length(Error) - 17);

  inherited HandleError(Row, Col, Pos, Error, Typ, Filename);
end;

constructor TSyncMethod.Create(Method: Pointer);
begin
  FMethod := Method
end;

procedure TSyncMethod.Call();
type
  TProc = procedure; cdecl;
begin
  TProc(FMethod)();
end;

initialization
  PluginsGlob := TMPlugins.Create;

finalization
  //PluginsGlob.Free;
  //Its a nice idea, but it will segfault... the program is closing anyway.
end.
