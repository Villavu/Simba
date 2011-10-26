{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2011 by Raymond van Venetië and Merlijn Wajer

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

{$define PS_USESSUPPORT}
//{$define USE_RUTIS}
{$define USE_LAPE}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, client, uPSComponent,uPSCompiler,
  uPSRuntime, uPSPreProcessor,MufasaTypes,MufasaBase, web,
  bitmaps, plugins, dynlibs,internets,scriptproperties,
  settings,settingssandbox, lcltype, dialogs
  {$IFDEF USE_RUTIS}
  , Rutis_Engine, Rutis_Defs
  {$ENDIF}
  {$IFDEF USE_LAPE}
  , lpparser, lpcompiler, lptypes, lpvartypes,
    lpeval, lpinterpreter, lpdisassembler
  {$ENDIF};

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

    { TMThread }

    TMThread = class(TThread)
    private
      procedure SetOpenConnectionEvent(const AValue: TOpenConnectionEvent);
      procedure SetOpenFileEvent(const AValue: TOpenFileEvent);
      procedure SetWriteFileEvent(const AValue: TWriteFileEvent);
    protected
      ScriptPath, AppPath, IncludePath, PluginPath, FontPath: string;
      DebugTo: TWritelnProc;
      ExportedMethods : TExpMethodArr;
      Includes : TStringList;
      FOpenConnectionEvent : TOpenConnectionEvent;
      FWriteFileEvent : TWriteFileEvent;
      FOpenFileEvent : TOpenFileEvent;
      procedure LoadPlugin(plugidx: integer); virtual; abstract;

    public
      Prop: TScriptProperties;
      Client : TClient;
      MInternet : TMInternet;
      Socks: TSocks;
      StartTime : LongWord;
      Settings: TMMLSettings;
      SimbaSettingsFile: String;
      Sett: TMMLSettingsSandbox;

      CallBackData : PCallBackData; //Handles general callback functions for threadsafety
      SyncInfo : PSyncInfo; //We need this for callthreadsafe
      ErrorData : PErrorData; //We need this for thread-safety etc
      OnError  : TOnError; //Error handeler

      CompileOnly : boolean;

      procedure FormCallBackEx(cmd : integer; var data : pointer);
      procedure FormCallBack(cmd : integer; data : pointer);
      procedure HandleError(ErrorRow,ErrorCol,ErrorPosition : integer; ErrorStr : string; ErrorType : TErrorType; ErrorModule : string);
      function ProcessDirective(Sender: TPSPreProcessor;
                    Parser: TPSPascalPreProcessorParser;
                    DirectiveName, DirectiveArgs: string; Filename:String): boolean;
      function LoadFile(ParentFile : string; var filename, contents: string): boolean;
      procedure AddMethod(meth: TExpMethod); virtual;

      procedure SetDebug( writelnProc : TWritelnProc );
      procedure SetPaths(ScriptP,AppP,IncludeP,PluginP,FontP : string);
      procedure SetSettings(S: TMMLSettings; SimbaSetFile: String);

      procedure OnThreadTerminate(Sender: TObject);
      procedure SetScript(script: string); virtual; abstract;
      procedure Execute; override; abstract;
      procedure Terminate; virtual; abstract;

      constructor Create(CreateSuspended: boolean; TheSyncInfo : PSyncInfo; plugin_dir: string);
      destructor Destroy; override;

      class function GetExportedMethods : TExpMethodArr;

      property OpenConnectionEvent : TOpenConnectionEvent read FOpenConnectionEvent write SetOpenConnectionEvent;
      property WriteFileEvent : TWriteFileEvent read FWriteFileEvent write SetWriteFileEvent;
      property OpenFileEvent : TOpenFileEvent read FOpenFileEvent write SetOpenFileEvent;
    end;

    { TPSThread }

    TPSThread = class(TMThread)
      public
        procedure OnProcessDirective(Sender: TPSPreProcessor;
          Parser: TPSPascalPreProcessorParser; const Active: Boolean;
          const DirectiveName, DirectiveParam: string; var Continue: Boolean;
          Filename: String);
        function PSScriptFindUnknownFile(Sender: TObject;
          const OrginFileName: string; var FileName, Output: string): Boolean;
        procedure PSScriptProcessUnknownDirective(Sender: TPSPreProcessor;
          Parser: TPSPascalPreProcessorParser; const Active: Boolean;
          const DirectiveName, DirectiveParam: string; var Continue: Boolean;
          Filename: string);
      protected
        PluginsToload : array of integer;
        procedure LoadPlugin(plugidx: integer); override;
        procedure OnCompile(Sender: TPSScript);
        function RequireFile(Sender: TObject; const OriginFileName: String;
                            var FileName, OutPut: string): Boolean;
        function FileAlreadyIncluded(Sender: TObject; OrgFileName, FileName: string): Boolean;
        function OnIncludingFile(Sender: TObject; OrgFileName, FileName: string): Boolean;

        procedure OnCompImport(Sender: TObject; x: TPSPascalCompiler);
        procedure OnExecImport(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
        procedure OutputMessages;
        procedure HandleScriptTerminates;
      public
        PSScript : TPSScript;
        constructor Create(CreateSuspended: Boolean; TheSyncInfo : PSyncInfo; plugin_dir: string);
        destructor Destroy; override;
        procedure SetScript(script: string); override;
        procedure Execute; override;
        procedure Terminate; override;
    end;

    TPrecompiler_Callback = function(name, args: PChar): boolean; stdcall;
    TErrorHandeler_Callback = procedure(line, pos: integer; err: PChar; runtime: boolean); stdcall;

    TCPThread = class(TMThread)
      protected
        instance: pointer;
        added_methods: array of TExpMethod;
        procedure LoadPlugin(plugidx: integer); override;
      public
        constructor Create(CreateSuspended: Boolean; TheSyncInfo : PSyncInfo; plugin_dir: string);
        destructor Destroy; override;
        procedure SetScript(script: string); override;
        procedure Execute; override;
        procedure Terminate; override;
        procedure AddMethod(meth: TExpMethod); override;
    end;

    { TRTThread }
    {$IFDEF USE_RUTIS}
    TRTThread = class(TMThread)
    private
      procedure RTOnWrite(s : String);
      procedure RTOnError(s : String; ErrorType : TRutisErrorType);
    public
      RUTIS : TRutisEngine;
      constructor Create(CreateSuspended: Boolean; TheSyncInfo : PSyncInfo; plugin_dir: string);
      destructor Destroy; override;
      procedure SetScript(script: string); override;
      procedure Execute; override;
      procedure Terminate; override;
    end;
   {$ENDIF}

   {$IFDEF USE_LAPE}
   { TLPThread }
   TLPThread = class(TMThread)
   protected
     procedure LoadPlugin(plugidx: integer); override;
   public
     Parser: TLapeTokenizerString;
     Compiler: TLapeCompiler;
     constructor Create(CreateSuspended: Boolean; TheSyncInfo : PSyncInfo; plugin_dir: string);
     destructor Destroy; override;
     procedure SetScript(Script: string); override;
     procedure Execute; override;
     procedure Terminate; override;
     function OnFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
     function OnHandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek: Boolean): Boolean;
   end;
   {$ENDIF}


threadvar
  CurrThread : TMThread;
var
  PluginsGlob: TMPlugins;

  libcpascal: integer;
  interp_init: function(precomp: TPrecompiler_Callback; err: TErrorHandeler_Callback): Pointer; cdecl;
  interp_meth: procedure(interp: Pointer; addr: Pointer; def: PChar); cdecl;
  interp_type: procedure(interp: Pointer; def: PChar); cdecl;
  interp_set: procedure(interp: Pointer; ppg: PChar); cdecl;
  interp_comp: function(interp: Pointer): boolean; cdecl;
  interp_run: function(interp: Pointer): boolean; cdecl;
  interp_free: procedure(interp: Pointer); cdecl;

implementation

uses
  colour_conv,dtmutil,
  {$ifdef mswindows}windows,  MMSystem,{$endif}//MMSystem -> Sounds
  uPSC_std, uPSC_controls,uPSC_classes,uPSC_graphics,uPSC_stdctrls,uPSC_forms, uPSC_menus,
  uPSC_extctrls, uPSC_mml, uPSC_dll, //Compile-libs
  uPSUtils,
  fontloader,
  IOmanager,//TTarget_Exported
  IniFiles,//Silly INI files
  stringutil, //String st00f

  uPSR_std, uPSR_controls,uPSR_classes,uPSR_graphics,uPSR_stdctrls,uPSR_forms, uPSR_mml,
  uPSR_menus, uPSI_ComCtrls, uPSI_Dialogs, uPSR_dll,
  files,
  dtm, //Dtms!
  uPSR_extctrls, //Runtime-libs
  Graphics, //For Graphics types
  math, //Maths!
  mmath, //Real maths!
  strutils,
  fileutil,
  tpa, //Tpa stuff
  forms,//Forms
  SynRegExpr,
  lclintf,  // for GetTickCount and others.
  Clipbrd,
  DCPcrypt2,
  DCPhaval,  
  DCPmd4, DCPmd5,
  DCPripemd128, DCPripemd160,
  DCPsha1, DCPsha256, DCPsha512,
  DCPtiger;
{$ifdef Linux}
  {$define PS_SafeCall}
{$else}
//{$define PS_SafeCall}
{$endif}
{$MACRO ON}
{$ifdef PS_SafeCall}
  {$define extdecl := safecall}
{$else}
  {$define extdecl := register}
{$endif}

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
  if Assigned(OpenConnectionEvent) then
    MInternet.OpenConnectionEvent := Self.OpenConnectionEvent;
  SyncInfo:= TheSyncInfo;
  ExportedMethods:= GetExportedMethods;
  FreeOnTerminate := True;
  CompileOnly := false;
  OnTerminate := @OnThreadTerminate;
  OnError:= nil;
  Includes := TStringList.Create;
  Includes.CaseSensitive:= {$ifdef linux}true{$else}false{$endif};
  Sett := nil;

  Prop := TScriptProperties.Create;
end;

destructor TMThread.Destroy;
begin
  MInternet.Free;
  Socks.Free;
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
    self.Client.MFiles.WriteFileEvent := AValue;;
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

procedure TMThread.HandleError(ErrorRow,ErrorCol, ErrorPosition: integer; ErrorStr: string; ErrorType: TErrorType; ErrorModule : string);
begin
  if OnError = nil then
    exit;
  ErrorData^.Row:= ErrorRow - 1;
  ErrorData^.Col := ErrorCol;
  ErrorData^.Position:= ErrorPosition;
  ErrorData^.Error:= ErrorStr;
  ErrorData^.ErrType:= ErrorType;
  ErrorData^.Module:= ErrorModule;
  ErrorData^.IncludePath:= IncludePath;
  CurrThread.Synchronize(OnError);
end;

procedure TMThread.OnThreadTerminate(Sender: TObject);
begin

end;

procedure TMThread.AddMethod(meth: TExpMethod);
begin
end;

function TMThread.LoadFile(ParentFile : string; var filename, contents: string): boolean;
var
  path: string;
  f: TFileStream;
begin
  path := FindFile(filename,[includepath,ScriptPath,IncludeTrailingPathDelimiter(ExtractFileDir(parentfile))]);
  if path = '' then
  begin
    psWriteln(Path + ' doesn''t exist');
    Result := false;
    Exit;
  end;
  filename := path;//Yeah!

  if Includes.IndexOf(path) = -1 then
    Includes.Add(path);

  try
    f:= TFileStream.Create(UTF8ToSys(Path), fmOpenRead);
    SetLength(contents, f.Size);
    f.Read(contents[1], Length(contents));
    result:= true;
    f.free;
  except
    Result := false;
    psWriteln('ERROR in TMThread.LoadFile');
  end;
end;

function TMThread.ProcessDirective(Sender: TPSPreProcessor;
        Parser: TPSPascalPreProcessorParser;
        DirectiveName, DirectiveArgs: string; FileName: string): boolean;
var
  plugin_idx: integer;

begin
  Result := False;
  if CompareText(DirectiveName,'LOADLIB') = 0 then
  begin
    if DirectiveArgs <> '' then
    begin;
      plugin_idx:= PluginsGlob.LoadPlugin(DirectiveArgs);
      if plugin_idx < 0 then
        psWriteln(Format('Your DLL %s has not been found',[DirectiveArgs]))
      else
      begin
        LoadPlugin(plugin_idx);
        Result:= True;
      end;
    end
    else
      psWriteln('Your LoadLib directive has no params, thus cannot find the plugin');
  end
  else if CompareText(DirectiveName,'WARNING') = 0 then
  begin
    if (sender = nil) or (parser = nil) then
    begin
      psWriteln('ERROR: WARNING directive not supported for this interpreter');
      exit(False);
    end;

    if (DirectiveArgs <> '') then
    begin
      Result := True;
      if FileName = '' then
        psWriteln(format('Warning: In %s: at row: %d, col: %d, pos %d: %s',
                               ['Main script', Parser.row, Parser.col,
                               Parser.pos, DirectiveArgs]))
      else
        psWriteln(format('Warning: In file %s: at row: %d, col: %d, pos %d: %s',
                             [FileName, Parser.row, Parser.col,
                             Parser.pos, DirectiveArgs]));
      //HandleError(Parser.Row + 1, Parser.Col, Parser.Pos, 'Warning at ' + DirectiveArgs, errCompile, FileName);
    end;
  end else if CompareText(DirectiveName,'ERROR') = 0 then
  begin
    if (sender = nil) or (parser = nil) then
    begin
      psWriteln('ERROR: ERROR directive not supported for this interpreter');
      exit(False);
    end;

    if (DirectiveArgs <> '') then
    begin
      Result := True;
      {if FileName = '' then
        psWriteln(format('Error: In %s: at row: %d, col: %d, pos %d: %s',
                               ['Main script', Parser.row, Parser.col,
                               Parser.pos, DirectiveArgs]))
      else
        psWriteln(format('Error: In file %s: at row: %d, col: %d, pos %d: %s',
                             [FileName, Parser.row, Parser.col,
                             Parser.pos, DirectiveArgs])); }
      HandleError(Parser.Row + 1, Parser.Col, Parser.Pos, 'Error: ' + DirectiveArgs, errCompile, FileName);
      raise EPSPreProcessor.Create('ERROR directive found');
    end;
  end else
    Result := False; // If we do not know the directive; return true so Continue
                    // will be false.
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

procedure TMThread.SetPaths(ScriptP, AppP,IncludeP,PluginP,FontP: string);
begin
  AppPath:= AppP;
  ScriptPath:= ScriptP;
  IncludePath:= IncludeP;
  PluginPath:= PluginP;
  FontPath:= FontP;
end;

function ThreadSafeCall(ProcName: string; var V: TVariantArray): Variant; extdecl;
begin
  if GetCurrentThreadId = MainThreadID then
  begin
    with TPSThread(currthread).PSScript do
      Result := Exec.RunProcPVar(V,Exec.GetProc(Procname));
  end else
  begin
    CurrThread.SyncInfo^.MethodName:= ProcName;
    CurrThread.SyncInfo^.V:= @V;
    CurrThread.SyncInfo^.OldThread := CurrThread;
    CurrThread.SyncInfo^.Res := @Result;
    CurrThread.Synchronize(CurrThread.SyncInfo^.SyncMethod);
  end;
end;

function CallProc(ProcName: string; var V: TVariantArray): Variant; extdecl;
begin
  with TPSThread(currthread).PSScript do
    Result := Exec.RunProcPVar(V,Exec.GetProc(Procname));
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
{$I PSInc/psmethods.inc}

class function TMThread.GetExportedMethods: TExpMethodArr;
var
  c : integer;
  CurrSection : string;

procedure SetCurrSection(str : string);
begin;
  CurrSection := Str;
end;

procedure AddFunction( Ptr : Pointer; DeclStr : String);
begin;
  if c >= 500 then
    raise exception.create('PSThread.LoadMethods: Exported more than 500 functions');
  Result[c].FuncDecl:= DeclStr;
  Result[c].FuncPtr:= Ptr;
  Result[c].Section:= CurrSection;
  inc(c);
end;

begin
  c := 0;
  CurrSection := 'Other';
  SetLength(Result, 500);

  {$i PSInc/psexportedmethods.inc}

  SetLength(Result,c);
end;

{***implementation TPSThread***}

constructor TPSThread.Create(CreateSuspended : boolean; TheSyncInfo : PSyncInfo; plugin_dir: string);
var
  I : integer;
begin
  inherited Create(CreateSuspended, TheSyncInfo, plugin_dir);
  PSScript := TPSScript.Create(nil);
  PSScript.UsePreProcessor:= True;
  PSScript.CompilerOptions := PSScript.CompilerOptions + [icBooleanShortCircuit];
  PSScript.OnNeedFile := @RequireFile;
  PSScript.OnIncludingFile := @OnIncludingFile;
  PSScript.OnFileAlreadyIncluded := @FileAlreadyIncluded;
  PSScript.OnProcessDirective:=@OnProcessDirective;
  PSScript.OnProcessUnknowDirective:=@PSScriptProcessUnknownDirective;
  PSScript.OnCompile:= @OnCompile;
  PSScript.OnCompImport:= @OnCompImport;
  PSScript.OnExecImport:= @OnExecImport;
  PSScript.OnFindUnknownFile:= @PSScriptFindUnknownFile;

  with PSScript do
  begin
    // Set some defines
    {$I PSInc/psdefines.inc}
  end;

  for i := 0 to high(ExportedMethods) do
    if pos('Writeln',exportedmethods[i].FuncDecl) > 0 then
    begin
      ExportedMethods[i].FuncPtr := nil;
      break;
    end;
end;


destructor TPSThread.Destroy;
begin
  PSScript.Free;
  inherited;
end;

procedure TPSThread.OnProcessDirective(Sender: TPSPreProcessor;
  Parser: TPSPascalPreProcessorParser; const Active: Boolean;
  const DirectiveName, DirectiveParam: string; var Continue: Boolean; Filename: String);
begin
  if (CompareText(DirectiveName, 'LOADLIB') = 0)
  or (CompareText(DirectiveName, 'WARNING') = 0)
  or (CompareText(DirectiveName, 'ERROR') = 0)  then
    Continue := not ProcessDirective(Sender, Parser, DirectiveName,DirectiveParam,
             FileName);
end;

function TPSThread.PSScriptFindUnknownFile(Sender: TObject;
  const OrginFileName: string; var FileName, Output: string): Boolean;
begin
  mDebugLn(OrginFileName + '-' +  Output + '-' + FileName);
  Result := false;
end;

procedure TPSThread.PSScriptProcessUnknownDirective(Sender: TPSPreProcessor;
  Parser: TPSPascalPreProcessorParser; const Active: Boolean;
  const DirectiveName, DirectiveParam: string; var Continue: Boolean;
  Filename: string);
begin
  Continue:= not ProcessDirective(Sender, Parser, DirectiveName, DirectiveParam,
               FileName);
end;

function Muf_Conv_to_PS_Conv( conv : integer) : TDelphiCallingConvention;
begin
  case conv of
    cv_StdCall : result := cdStdCall;
    cv_Register: result := cdRegister;
  else
    raise exception.createfmt('Unknown Calling Convention[%d]',[conv]);
  end;
end;

procedure TPSThread.LoadPlugin(plugidx: integer);
var
 i: integer;
begin
  for i := High(PluginsToLoad) downto 0 do
    if PluginsToLoad[i] = plugidx then
      Exit;
  SetLength(PluginsToLoad,Length(PluginsToLoad)+1);
  PluginsToLoad[High(PluginsToLoad)]:= plugidx;
end;

procedure TPSThread.OnCompile(Sender: TPSScript);
var
  i,ii : integer;
  Fonts : TMFonts;
begin
  Fonts := Client.MOCR.Fonts;
  for i := fonts.count - 1 downto 0 do
    Sender.Comp.AddConstantN(Fonts[i].Name,'string').SetString(Fonts[i].Name);

  for i := High(PluginsToLoad) downto 0 do
  begin
    for ii := 0 to PluginsGlob.MPlugins[PluginsToLoad[i]].TypesLen - 1 do
      Sender.Comp.AddTypeS(PluginsGlob.MPlugins[PluginsToLoad[i]].Types[ii].TypeName,
                      PluginsGlob.MPlugins[PluginsToLoad[i]].Types[ii].TypeDef);

    for ii := 0 to PluginsGlob.MPlugins[PluginsToLoad[i]].MethodLen - 1 do
      Sender.AddFunctionEx(PluginsGlob.MPlugins[PluginsToLoad[i]].Methods[ii].FuncPtr,
                           PluginsGlob.MPlugins[PluginsToLoad[i]].Methods[ii].FuncStr,
                           Muf_Conv_to_PS_Conv(PluginsGlob.MPlugins[PluginsToLoad[i]].Methods[ii].FuncConv));
  end;
  
  for i := 0 to high(VirtualKeys) do
    Sender.Comp.AddConstantN(Format('VK_%S',[VirtualKeys[i].Str]),'Byte').SetInt(VirtualKeys[i].Key);
  // Here we add all the Consts/Types to the engine.

  //Export all the methods
  for i := 0 to high(ExportedMethods) do
    if ExportedMethods[i].FuncPtr <> nil then
      Sender.AddFunctionEx(ExportedMethods[i].FuncPtr,ExportedMethods[i].FuncDecl,
                             {$ifdef PS_SafeCall}cdSafeCall{$else}cdRegister{$endif});
end;

function TPSThread.RequireFile(Sender: TObject;
  const OriginFileName: String; var FileName, OutPut: string): Boolean;
begin
  Result := LoadFile(OriginFileName,FileName,OutPut);

  if Result then
    Output :=
      '{$IFNDEF IS_INCLUDE}{$DEFINE IS_INCLUDE}{$DEFINE __REMOVE_IS_INCLUDE}{$ENDIF}' + LineEnding +
      Output + LineEnding +
      '{$IFDEF __REMOVE_IS_INCLUDE}{$UNDEF IS_INCLUDE}{$ENDIF}';
end;

function TPSThread.FileAlreadyIncluded(Sender: TObject; OrgFileName, FileName: string): Boolean;
var
  path: string;
  i: integer;
begin
  path := FindFile(filename,[includepath,ScriptPath,IncludeTrailingPathDelimiter(ExtractFileDir(OrgFileName))]);
  if path = '' then
  begin
    Result := True;
    Exit;
  end;
  path := ExpandFileNameUTF8(path);

  if (path <> '') then
    if Includes.IndexOf(path) <> -1 then
    begin
      {$IFDEF SIMBA_VERBOSE}
      writeln('Include_Once file already included:' + Path);
      {$ENDIF}
      Result := True;
      Exit;
    end;

  {$IFDEF SIMBA_VERBOSE}
  writeln('OnFileAlreadyIncluded, Adding: ' + path);
  {$ENDIF}
  Includes.Add(path);
  Result := False;
end;

function TPSThread.OnIncludingFile(Sender: TObject; OrgFileName, FileName: string): Boolean;
var
  path: string;
begin
  path := FindFile(filename,[includepath,ScriptPath,IncludeTrailingPathDelimiter(ExtractFileDir(OrgFileName))]);
  if path = '' then
  begin
    Result := True;
    Exit;
  end;
  path := ExpandFileNameUTF8(path);

  if Includes.IndexOf(path) = -1 then
  begin
    {$IFDEF SIMBA_VERBOSE}
    writeln('OnIncludingFile, Adding: ' + path);
    {$ENDIF}
    Includes.Add(path);
  end;

  Result := True; // Not used
end;

procedure SIRegister_Mufasa(cl: TPSPascalCompiler);
begin
  SIRegister_MML(cl);
end;

procedure TPSThread.OnCompImport(Sender: TObject; x: TPSPascalCompiler);
begin
  SIRegister_Std(x);
  SIRegister_Controls(x);
  SIRegister_Classes(x, true);
  SIRegister_Graphics(x, true);
  SIRegister_stdctrls(x);
  SIRegister_Forms(x);
  SIRegister_ExtCtrls(x);
  SIRegister_Menus(x);
  SIRegister_ComCtrls(x);
  SIRegister_Dialogs(x);
  if self.settings <> nil then
  begin
    if lowercase(self.settings.GetKeyValueDefLoad('Settings/Interpreter/AllowSysCalls',
      'False', Self.SimbaSettingsFile)) = 'true' then
    begin
      { Can remove later }
      psWriteln('Allowing API/SysCalls.');
      RegisterDll_Compiletime(x);
    end;
  end;

  with x do
  begin
    {$I PSInc/pscompile.inc}
  end;

  SIRegister_Mufasa(x);
  with x.AddFunction('procedure writeln;').decl do
    with AddParam do
    begin
      OrgName:= 'x';
      Mode:= pmIn;
    end;
  with x.AddFunction('function ToStr:string').decl do
    with addparam do
    begin
      OrgName:= 'x';
      Mode:= pmIn;
    end;
  with x.AddFunction('procedure swap;').decl do
  begin
    with addparam do
    begin
      OrgName:= 'x';
      Mode:= pmInOut;
    end;
    with addparam do
    begin
      OrgName:= 'y';
      Mode:= pmInOut;
    end;
  end;
end;

function TMufasaBitmapCreate : TMufasaBitmap;
begin
  result := TMufasaBitmap.Create;
  CurrThread.Client.MBitmaps.AddBMP(result);
end;

procedure TMufasaBitmapFree(Self : TMufasaBitmap);
begin
  CurrThread.Client.MBitmaps.FreeBMP(Self.Index);
end;

function TMufasaBitmapCopy(Self : TMufasaBitmap;const xs,ys,xe,ye : integer) : TMufasaBitmap;
begin
  result := Self.Copy(xs,ys,xe,ye);
  CurrThread.Client.MBitmaps.AddBMP(result);
end;
function TMDTMCreate : TMDTM;
begin
  result := TMDTM.Create;
  CurrThread.Client.MDTMs.AddDTM(result);
end;
procedure TMDTMFree(Self : TMDTM);
begin
  CurrThread.Client.MDTMs.FreeDTM(self.Index);
end;

procedure RIRegister_Mufasa(CL: TPSRuntimeClassImporter);
begin
  RIRegister_MML(cl);
 //Overwrites the default stuff
  with cl.FindClass('TMufasaBitmap') do
  begin
    RegisterConstructor(@TMufasaBitmapCreate,'Create');
    RegisterMethod(@TMufasaBitmapFree,'Free');
    RegisterMethod(@TMufasaBitmapCopy,'Copy');
  end;
  With cl.FindClass('TMDTM') do
  begin
    RegisterConstructor(@TMDTMCreate,'Create');
    RegisterMethod(@TMDTMFree,'Free');
  end;
end;

procedure TPSThread.OnExecImport(Sender: TObject; se: TPSExec;
  x: TPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x, True);
  RIRegister_Controls(x);
  RIRegister_Graphics(x, True);
  RIRegister_stdctrls(x);
  RIRegister_Forms(x);
  RIRegister_ExtCtrls(x);
  RIRegister_Menus(x);
  RIRegister_Mufasa(x);
  RIRegister_ComCtrls(x);
  RIRegister_Dialogs(x);
  RegisterDLLRuntime(se);
  se.RegisterFunctionName('WRITELN',@Writeln_,nil,nil);
  se.RegisterFunctionName('TOSTR',@ToStr_,nil,nil);
  se.RegisterFunctionName('SWAP',@swap_,nil,nil);
end;

procedure TPSThread.OutputMessages;
var
  l: Longint;
  b: Boolean;
begin
  b := False;
  for l := 0 to PSScript.CompilerMessageCount - 1 do
  begin
    if (not b) and (PSScript.CompilerMessages[l] is TIFPSPascalCompilerError) then
    begin
      b := True;
      if OnError <> nil then
        with PSScript.CompilerMessages[l] do
          HandleError(Row, Col, Pos, MessageToString,errCompile, ModuleName)
      else
        psWriteln(PSScript.CompilerErrorToStr(l) + ' at line ' + inttostr(PSScript.CompilerMessages[l].Row - 1));
    end else
      psWriteln(PSScript.CompilerErrorToStr(l) + ' at line ' + inttostr(PSScript.CompilerMessages[l].Row - 1));
  end;
end;

procedure TPSThread.HandleScriptTerminates;
var
  I : integer;
begin
  if (PSScript.Exec.ExceptionCode =ErNoError) and  (SP_OnTerminate in Prop.Properties) then
  begin;
    for i := 0 to Prop.OnTerminateProcs.Count - 1 do
    begin
      try
        PSScript.ExecuteFunction([],Prop.OnTerminateProcs[i]);
      finally
      end;
    end;
  end;
end;

procedure TPSThread.Execute;
begin
  CurrThread := Self;
  Starttime := lclintf.GetTickCount;

  try
    if PSScript.Compile then
    begin
      OutputMessages;
      psWriteln('Compiled successfully in ' + IntToStr(GetTickCount - Starttime) + ' ms.');
      if CompileOnly then
        exit;
//      if not (ScriptState = SCompiling) then
        if not PSScript.Execute then
          HandleError(PSScript.ExecErrorRow,PSScript.ExecErrorCol,PSScript.ExecErrorPosition,PSScript.ExecErrorToString,
                      errRuntime, PSScript.ExecErrorFileName)
        else
        begin
          HandleScriptTerminates;
          psWriteln('Successfully executed.');
        end;
    end else
    begin
      OutputMessages;
      psWriteln('Compiling failed.');
    end;
  except
     on E : Exception do
       psWriteln('Exception in Script: ' + e.message);
  end;
end;

procedure TPSThread.Terminate;
begin
  PSScript.Stop;
end;

procedure TPSThread.SetScript(script: string);
begin
   PSScript.Script.Text:= LineEnding+Script; //A LineEnding to create space for future extra's in first line (defines?)
end;

{***implementation TCPThread***}

procedure LoadCPascal(plugin_dir: string);
begin
  libcpascal:= LoadLibrary(PChar(plugin_dir + 'libcpascal' + {$ifdef LINUX} '.so' {$else} '.dll' {$endif}));
  if libcpascal = 0 then
    raise Exception.Create('CPascal library not found');
  Pointer(interp_init):= GetProcAddress(libcpascal, PChar('interp_init'));
  Pointer(interp_meth):= GetProcAddress(libcpascal, PChar('interp_meth'));
  Pointer(interp_type):= GetProcAddress(libcpascal, PChar('interp_type'));
  Pointer(interp_set):= GetProcAddress(libcpascal, PChar('interp_set'));
  Pointer(interp_comp):= GetProcAddress(libcpascal, PChar('interp_comp'));
  Pointer(interp_run):= GetProcAddress(libcpascal, PChar('interp_run'));
  Pointer(interp_free):= GetProcAddress(libcpascal, PChar('interp_free'));
end;

function Interpreter_Precompiler(name, args: PChar): boolean; stdcall;

begin
  result:= CurrThread.ProcessDirective(nil, nil, name, args, '');
end;

procedure Interpreter_ErrorHandler(line, pos: integer; err: PChar; runtime: boolean); stdcall;
begin
  if runtime then
    CurrThread.HandleError(line,-1,pos,err,errRuntime,'')
  else
    CurrThread.HandleError(line,-1,pos,err,errCompile,'')
end;

constructor TCPThread.Create(CreateSuspended: Boolean; TheSyncInfo : PSyncInfo; plugin_dir: string);

begin
  inherited Create(CreateSuspended, TheSyncInfo, plugin_dir);
  if libcpascal = 0 then
    LoadCPascal(plugin_dir);
  instance:= interp_init(@Interpreter_Precompiler, @Interpreter_ErrorHandler);
end;

destructor TCPThread.Destroy;
begin
  interp_free(instance);
  inherited Destroy;
end;

procedure TCPThread.SetScript(script: string);
begin
  interp_set(instance,PChar(script));
end;

procedure TCPThread.AddMethod(meth: TExpMethod);
begin
  interp_meth(instance,meth.FuncPtr,PChar(meth.FuncDecl));
end;

procedure TCPThread.LoadPlugin(plugidx: integer);
var
  i: integer;
begin
  with PluginsGlob.MPlugins[plugidx] do
    for i := 0 to MethodLen - 1 do
      with Methods[i] do
      begin
        pswriteln(FuncStr);
        interp_meth(self.instance,FuncPtr,PChar(FuncStr));
      end;
  pswriteln('done')
end;

procedure TCPThread.Execute;
var
  i: integer;
begin
  CurrThread := Self;
  Starttime := GetTickCount;
  psWriteln('Invoking CPascal Interpreter');
  interp_type(self.instance,'type longword = integer;');
  interp_type(self.instance,'type word = integer;');
  interp_type(self.instance,'type longint = integer;');
  interp_type(self.instance,'type pointer = integer;');
  interp_type(self.instance,'type byte = integer;');
  interp_type(self.instance,'type extended = real;');
  interp_type(self.instance,'type tcolor = integer;');

  interp_type(self.instance,'type TExtendedArray = array of extended;');
  interp_type(self.instance,'type T2DExtendedArray = array of array of extended;');
  interp_type(self.instance,'type TIntegerArray = array of integer;');

  interp_type(self.instance,'type TBox = record X1,Y1,X2,Y2: integer; end;');
  interp_type(self.instance,'type TPoint = record x,y: integer; end;');
  interp_type(self.instance,'type TPointArray = array of TPoint;');   ;
  interp_type(self.instance,'type T2DPointArray = array of array of TPoint;');   ;
  interp_type(self.instance,'type TPointArrayArray = T2DPointArray;');

  interp_type(self.instance,'type TTarget_Exported = record int1,int2,int3,int4,int5,int6,int7,int8,int9,int10,int11,int12,int13,int14, int15:integer; end;');
  interp_type(self.instance,'type TMask = record  White, Black : TPointArray; WhiteHi,BlackHi : integer; W,H : integer;end;');
  interp_type(self.instance,'type TDTMPointDef = record x, y, Color, Tolerance, AreaSize, AreaShape: integer; end;');
  interp_type(self.instance,'type TDTMPointDefArray = Array Of TDTMPointDef;');
  interp_type(self.instance,'type TDTM = record MainPoint: TDTMPointDef; SubPoints: TDTMPointDefArray; end;');
  interp_type(self.instance,'type pDTM = record l: Integer;p: TPointArray;c, t, asz, ash: TIntegerArray; bp: Array Of Boolean; n: String; end;');


  for i := 0 to high(ExportedMethods) do
    if ExportedMethods[i].FuncPtr <> nil then
      with ExportedMethods[i] do
        interp_meth(self.instance,FuncPtr,PChar(FuncDecl));
  if interp_comp(instance) then
  begin
    psWriteln('Compiled Successfully in ' +  IntToStr(GetTickCount - Starttime) + 'ms');
    if CompileOnly then
      exit;
    if interp_run(instance) then
       psWriteln('Executed Successfully')
    else
       psWriteln('Execution Failed');
  end else
     psWriteln('Compile Failed');

end;

procedure TCPThread.Terminate;
begin
  raise Exception.Create('Stopping Interpreter not yet implemented');
end;

{ TRTThread }
{$IFDEF USE_RUTIS}
procedure TRTThread.RTOnWrite(s: String);
begin
  psWriteln(s);
end;

procedure TRTThread.RTOnError(s: String; ErrorType: TRutisErrorType);
begin
  if ErrorType in [etRuntimeError,etCompilerError] then
    psWriteln(s)
  else
    writeln(s);
end;

type
  PBoolean = ^Boolean;
  PStringArray = ^TStringArray;
  PBmpMirrorStyle = ^TBmpMirrorStyle;
  PPointArray = ^TPointArray;
  P2DIntArray = ^T2DIntArray;
  PCanvas = ^TCanvas;
  P2DPointArray = ^T2DPointArray;
  PMask = ^Tmask;
  PBox = ^TBox;
  PTarget_Exported = ^TTarget_Exported;
  PIntegerArray = ^TIntegerArray;
  PExtendedArray = ^TExtendedArray;
//  PStrExtr = ^TStrExtr;
  PReplaceFlags = ^TReplaceFlags;
  PClickType = ^TClickType;
  P2DExtendedArray = ^T2DExtendedArray;
  PMDTM = ^TMDTM;
  PSDTM = ^TSDTM;

{$I RTInc/other.inc}
{$I RTInc/settings.inc}
{$I RTInc/bitmap.inc}
{$I RTInc/window.inc}
{$I RTInc/tpa.inc}
{$I RTInc/strings.inc}
{$I RTInc/colour.inc}
{$I RTInc/colourconv.inc}
{$I RTInc/math.inc}
{$I RTInc/mouse.inc}
{$I RTInc/file.inc}
{$I RTInc/keyboard.inc}
{$I RTInc/dtm.inc}
{$I RTInc/ocr.inc}
{$I RTInc/internets.inc}


constructor TRTThread.Create(CreateSuspended: Boolean; TheSyncInfo: PSyncInfo;
  plugin_dir: string);
var
  RutisEngine  : TRutisEngine;
begin
  inherited Create(CreateSuspended, TheSyncInfo, plugin_dir);
  RUTIS := TRutisEngine.Create;
  RUTIS.OnWrite:= @RTOnWrite;
  RUTIS.OnError:= @RTOnError;
  RUTIS.OptProcessTimer:= false;
  RUTIS.Compiler.optArrayRangeCheck:= true;
  RutisEngine := RUTIS;
  {$I RTInc/rtexportedmethods.inc}
end;

destructor TRTThread.Destroy;
begin
  RUTIS.Free;
  inherited Destroy;
end;

procedure TRTThread.SetScript(script: string);
begin
  RUTIS.ScriptCode.Text:= Script;
end;

procedure TRTThread.Execute;
begin
  CurrThread := self;
  Starttime := lclintf.GetTickCount;
  try
    RUTIS.Compile;
    if not RUTIS.CompilerError then
    begin
      psWriteln('Compiled successfully in ' + IntToStr(GetTickCount - Starttime) + ' ms.');
      if CompileOnly then
        exit;
      RUTIS.Run;
    end else
    begin
      CurrThread.HandleError(RUTIS.Error.ELine + 2,RUTIS.Error.EChrPos,-1,RUTIS.Error.Message,errCompile,'');
      psWriteln('Compiling failed.');
    end;
  except
     on E : Exception do
       psWriteln('Exception in Script: ' + e.message);
  end;
end;

procedure TRTThread.Terminate;
begin
  RUTIS.Stop;
end;
{$ENDIF}

{$IFDEF USE_LAPE}
{ TLPThread }

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
  PFont = ^TFont;
//  PStrExtr = ^TStrExtr;
  PReplaceFlags = ^TReplaceFlags;
  PClickType = ^TClickType;
  P2DExtendedArray = ^T2DExtendedArray;
  PMDTM = ^TMDTM;
  PMDTMPoint = ^TMDTMPoint;
  PSDTM = ^TSDTM;
  
 procedure lp_WriteLn(Params: PParamArray);
 begin
   psWriteLn(PlpString(Params^[0])^);
 end;


{$I LPInc/Wrappers/other.inc}
{$I LPInc/Wrappers/settings.inc}
{$I LPInc/Wrappers/bitmap.inc}
{$I LPInc/Wrappers/window.inc}
{$I LPInc/Wrappers/tpa.inc}
{$I LPInc/Wrappers/strings.inc}
{$I LPInc/Wrappers/colour.inc}
{$I LPInc/Wrappers/colourconv.inc}
{$I LPInc/Wrappers/crypto.inc}
{$I LPInc/Wrappers/math.inc}
{$I LPInc/Wrappers/mouse.inc}
{$I LPInc/Wrappers/file.inc}
{$I LPInc/Wrappers/keyboard.inc}
{$I LPInc/Wrappers/dtm.inc}
{.$I LPInc/Wrappers/extensions.inc} //Doesn't work for me!
{$I LPInc/Wrappers/ocr.inc}
{$I LPInc/Wrappers/internets.inc}

constructor TLPThread.Create(CreateSuspended: Boolean; TheSyncInfo: PSyncInfo; plugin_dir: string);
var
  I: integer;
  Fonts: TMFonts;
begin
  inherited Create(CreateSuspended, TheSyncInfo, plugin_dir);

  Parser := TLapeTokenizerString.Create('');
  Compiler := TLapeCompiler.Create(Parser);
  Compiler.OnFindFile := @OnFindFile;
  Compiler.OnHandleDirective := @OnHandleDirective;
  Fonts := Client.MOCR.Fonts;
  with Compiler do
  begin
    addGlobalFunc('procedure _writeln; override;', @lp_WriteLn);
    
    for I := Fonts.Count - 1 downto 0 do
      addGlobalVar(Fonts[I].Name, Fonts[I].Name);

    for I := 0 to High(VirtualKeys) do
      addGlobalVar(VirtualKeys[I].Key, Format('VK_%S', [VirtualKeys[i].Str]));

    {$I LPInc/lpdefines.inc}
    {$I LPInc/lpcompile.inc}

    {$I LPInc/lpexportedmethods.inc}
  end;
end;

destructor TLPThread.Destroy;
begin
  try
    {if (Compiler <> nil) then
      Compiler.Free;}

    if (Parser <> nil) then
      Parser.Free;
  except
    on E: Exception do
      psWriteln('Exception TLPThread.Destroy: ' + e.message);
  end;

  inherited Destroy;
end;

procedure TLPThread.SetScript(Script: string);
begin
  Parser.Doc := Script;
end;

function TLPThread.OnFindFile(Sender: TLapeCompiler; var FileName: lpString): TLapeTokenizerBase;
begin
  Result := nil;
  FileName := IncludePath + FileName;
end;

function TLPThread.OnHandleDirective(Sender: TLapeCompiler; Directive, Argument: lpString; InPeek: Boolean): Boolean;
var
  plugin_idx: integer;
begin
  Result := False;
  if (Directive = 'loadlib') then
  begin
    if (Argument <> '') then
    begin
      plugin_idx := PluginsGlob.LoadPlugin(Argument);
      if (plugin_idx >= 0) then
      begin
        LoadPlugin(plugin_idx);
        Result := True;
      end else
        psWriteln(Format('Your DLL %s has not been found', [Argument]))
    end else
      psWriteln('Your LoadLib directive has no params, thus cannot find the plugin');
  end;
end;

procedure TLPThread.LoadPlugin(plugidx: integer);
var
  I: integer;
begin
  with PluginsGlob.MPlugins[plugidx] do
  begin
    for i := 0 to TypesLen -1 do
      with Types[I] do
        Compiler.addGlobalType(TypeDef, TypeName);

    for i := 0 to MethodLen - 1 do
      with Methods[i] do
        Compiler.addGlobalFunc(FuncStr, FuncPtr);
  end;
end;

procedure TLPThread.Execute;
  function CombineDeclArray(a, b: TLapeDeclArray): TLapeDeclArray;
  var
    i, l: Integer;
  begin
    Result := a;
    l := Length(a);
    SetLength(Result, l + Length(b));
    for i := High(b) downto 0 do
      Result[l + i] := b[i];
  end;
begin
  CurrThread := self;
  try
    Starttime := lclintf.GetTickCount;
    if Compiler.Compile() then
    begin
      psWriteln('Compiled succesfully in ' + IntToStr(GetTickCount - Starttime) + ' ms.');
      if CompileOnly then
        Exit;
      RunCode(Compiler.Emitter.Code);
      psWriteln('Successfully executed.');
    end else
      psWriteln('Compiling failed.');
  except
     on E : Exception do
       psWriteln('Exception in Script: ' + e.message);
  end;
end;

procedure TLPThread.Terminate;
begin
end;
{$ENDIF}

initialization
  PluginsGlob := TMPlugins.Create;
  libcpascal:= 0;
finalization
  //PluginsGlob.Free;
  //Its a nice idea, but it will segfault... the program is closing anyway.
end.
