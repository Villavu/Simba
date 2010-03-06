{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009 by Raymond van Venetië and Merlijn Wajer

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

{$Define PS_USESSUPPORT}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, client, uPSComponent,uPSCompiler,
  uPSRuntime,stdCtrls, uPSPreProcessor,MufasaTypes, web,
  bitmaps, plugins, libloader, dynlibs,internets;


type
    { TMMLPSThread }
    TSyncInfo = record
      V : MufasaTypes.TVariantArray;
      MethodName : string;
      Res : Variant;
      SyncMethod : procedure of object;
      OldThread : TThread;
    end;

    TWritelnProc = procedure(s: string);
    TClearDebugProc = procedure;
    TDbgImgInfo = record
      DispSize : ^TPoint;
      ShowForm : procedure of object;
      ToDrawBitmap : ^TMufasaBitmap;
      DrawBitmap : procedure of object;
      GetDebugBitmap : ^TMufasaBitmap;
      GetBitmap : procedure of object;
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

    { TMThread }

    TMThread = class(TThread)
    protected
      ScriptPath, AppPath, IncludePath, PluginPath, FontPath: string;
      DebugTo: TWritelnProc;
      DebugClear : TClearDebugProc;
      DebugImg : TDbgImgInfo;
      ExportedMethods : TExpMethodArr;
      Includes : TStringList;
      procedure LoadPlugin(plugidx: integer); virtual; abstract;

    public
      Client : TClient;
      MInternet : TMInternet;
      StartTime : LongWord;

      SyncInfo : PSyncInfo; //We need this for callthreadsafe
      ErrorData : PErrorData; //We need this for thread-safety etc
      OnError  : TOnError; //Error handeler

      CompileOnly : boolean;
      procedure HandleError(ErrorRow,ErrorCol,ErrorPosition : integer; ErrorStr : string; ErrorType : TErrorType; ErrorModule : string);
      function ProcessDirective(DirectiveName, DirectiveArgs: string): boolean;
      function LoadFile(ParentFile : string; var filename, contents: string): boolean;
      procedure AddMethod(meth: TExpMethod); virtual;

      procedure SetDebug( writelnProc : TWritelnProc );
      procedure SetDebugClear( clearProc : TClearDebugProc );
      procedure SetDbgImg( DebugImageInfo : TDbgImgInfo);
      procedure SetPaths(ScriptP,AppP,IncludeP,PluginP,FontP : string);
      procedure OnThreadTerminate(Sender: TObject);
      procedure SetScript(script: string); virtual; abstract;
      procedure Execute; override; abstract;
      procedure Terminate; virtual; abstract;

      constructor Create(CreateSuspended: boolean; TheSyncInfo : PSyncInfo; plugin_dir: string);
      destructor Destroy; override;

      class function GetExportedMethods : TExpMethodArr;
    end;

    TPSThread = class(TMThread)
      public
        procedure OnProcessDirective(Sender: TPSPreProcessor;
          Parser: TPSPascalPreProcessorParser; const Active: Boolean;
          const DirectiveName, DirectiveParam: string; var Continue: Boolean);
        function PSScriptFindUnknownFile(Sender: TObject;
          const OrginFileName: string; var FileName, Output: string): Boolean;
        procedure PSScriptProcessUnknowDirective(Sender: TPSPreProcessor;
          Parser: TPSPascalPreProcessorParser; const Active: Boolean;
          const DirectiveName, DirectiveParam: string; var Continue: Boolean);
      protected
        PluginsToload : array of integer;
        procedure LoadPlugin(plugidx: integer); override;
        procedure OnCompile(Sender: TPSScript);
        function RequireFile(Sender: TObject; const OriginFileName: String;
                            var FileName, OutPut: string): Boolean;
        procedure OnCompImport(Sender: TObject; x: TPSPascalCompiler);
        procedure OnExecImport(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
        procedure OutputMessages;
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
  uPSC_std, uPSC_controls,uPSC_classes,uPSC_graphics,uPSC_stdctrls,uPSC_forms,
  uPSC_extctrls, //Compile-libs
  uPSUtils,
  fontloader,
  IOmanager,//TTarget_Exported
  IniFiles,//Silly INI files
  stringutil, //String st00f
  uPSR_std, uPSR_controls,uPSR_classes,uPSR_graphics,uPSR_stdctrls,uPSR_forms,
  uPSR_extctrls, //Runtime-libs
  Graphics, //For Graphics types
  math, //Maths!
  strutils,
  tpa, //Tpa stuff
  forms,//Forms
  lclintf  // for GetTickCount and others.
  ;
{$ifdef Linux}
{$define PS_StdCall}
{$else}
//{$define PS_StdCall}
{$endif}
{$MACRO ON}
{$ifdef PS_StdCall}
  {$define extdecl := stdcall}
{$else}
  {$define extdecl := REGISTER}
{$endif}

{Some General PS Functions here}
procedure psWriteln(str : string); extdecl;
begin
  if Assigned(CurrThread.DebugTo) then
    CurrThread.DebugTo(str)
  else
    writeln(str);
end;

function MakeString(data : TPSVariantIFC) : string;
begin;
  if data.Dta = nil then
    result := 'Nil'
  else
  if data.aType.basetype in [btString,btChar] then
    result := PSGetAnsiString(Data.Dta,data.aType)
  else if data.aType.ExportName = 'BOOLEAN' then
    result := BoolToStr(PSGetInt(Data.Dta,data.aType) <> 0,true)
  else
    result := PSVariantToString(data,'');
end;

function writeln_(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  Result:=true;
  psWriteln(makeString(NewTPSVariantIFC(Stack[Stack.Count-1],false)));
end;

function swap_(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  Param1,Param2: TPSVariantIFC;
  tempCopy : pointer;
begin
  Result:=true;
  Param1 := NewTPSVariantIFC(Stack[Stack.count-1],true);
  Param2 := NewTPSVariantIFC(Stack[Stack.count-2],true);
  if Param1.aType.BaseType <> Param2.aType.BaseType then
    exit(false)
  else
  begin
    Param1.aType.CalcSize;
    param2.aType.CalcSize;
    if Param1.aType.RealSize <> Param2.aType.RealSize then
      exit(false);
    GetMem(tempcopy,Param1.aType.RealSize);
    Move(Param1.Dta^,tempCopy^,param1.atype.realsize);
    Move(Param2.Dta^,Param1.Dta^,param1.atype.realsize);
    Move(tempCopy^,Param2.Dta^,param1.atype.realsize);
    Freemem(tempcopy);
  end;
end;

function ToStr_(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  result := true;
  Stack.SetAnsiString(-1, MakeString(NewTPSVariantIFC(Stack[Stack.Count-2],false)));
end;

{***implementation TMThread***}
constructor TMThread.Create(CreateSuspended: boolean; TheSyncInfo: PSyncInfo; plugin_dir: string);
begin
  Client := TClient.Create(plugin_dir);
  MInternet := TMInternet.Create(Client);
  SyncInfo:= TheSyncInfo;
  ExportedMethods:= GetExportedMethods;
  FreeOnTerminate := True;
  CompileOnly := false;
  OnTerminate := @OnThreadTerminate;
  OnError:= nil;
  Includes := TStringList.Create;
  Includes.CaseSensitive:= {$ifdef linux}true{$else}false{$endif};
  inherited Create(CreateSuspended);
end;

destructor TMThread.Destroy;
begin
  MInternet.Free;
  Client.Free;
  Includes.free;
  inherited Destroy;
end;

procedure TMThread.HandleError(ErrorRow,ErrorCol, ErrorPosition: integer; ErrorStr: string; ErrorType: TErrorType; ErrorModule : string);
begin
  if OnError = nil then
    exit;
  ErrorData^.Row:= ErrorRow;
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

function FindFile(filename : string; Dirs : array of string) : string; //Results '' if not found
var
  i : integer;
begin;
  if fileexists(filename) then
    result := filename
  else
  begin
    for i := 0 to high(Dirs) do
      if DirectoryExists(dirs[i]) then
        if fileexists(dirs[i] + filename) then
        begin
          result := dirs[i] + filename;
          exit;
        end;
  end;
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
  Includes.Add(path);

  try
    f:= TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
    SetLength(contents, f.Size);
    f.Read(contents[1], Length(contents));
    result:= true;
    f.free;
  except
    Result := false;
    psWriteln('ERROR in TMThread.LoadFile');
  end;
end;

function TMThread.ProcessDirective(DirectiveName, DirectiveArgs: string): boolean;
var
  plugin_idx, i: integer;
  path : string;
begin
  result := false;
  if CompareText(DirectiveName,'LOADDLL') = 0 then
  begin
    if DirectiveArgs <> '' then
    begin;
      plugin_idx:= PluginsGlob.LoadPlugin(DirectiveArgs);
      if plugin_idx < 0 then
        psWriteln(Format('Your DLL %s has not been found',[DirectiveArgs]))
      else begin
        LoadPlugin(plugin_idx);
        result:= True;
      end;
    end else
      psWriteln('Your LoadDLL directive has no params, thus cannot find the plugin');
  end else
  if CompareText(DirectiveName,'INCLUDE_ONCE') = 0 then
  begin
    result := true; //Lets to the actual file checking later on in the preprocessor ;-)
    if DirectiveArgs <> '' then
    begin
      path := FindFile(DirectiveArgs,[ScriptPath,IncludePath]);
      if path <> '' then
        if Includes.Find(path,i) then
        begin
          psWriteln('Include_Once file already included');
          result := false;
        end;
    end;
  end else
    result := true;
end;

procedure TMThread.SetDebug(writelnProc: TWritelnProc);
begin
  DebugTo := writelnProc;
end;

procedure TMThread.SetDebugClear(clearProc: TClearDebugProc);
begin
  DebugClear:= clearProc;
end;

procedure TMThread.SetDbgImg(DebugImageInfo: TDbgImgInfo);
begin
  DebugImg := DebugImageInfo;
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
  CurrThread.SyncInfo^.MethodName:= ProcName;
  CurrThread.SyncInfo^.V:= V;
  CurrThread.SyncInfo^.OldThread := CurrThread;
  CurrThread.Synchronize(CurrThread.SyncInfo^.SyncMethod);
  Result := CurrThread.SyncInfo^.Res;
end;

{$I PSInc/Wrappers/other.inc}
{$I PSInc/Wrappers/bitmap.inc}
{$I PSInc/Wrappers/window.inc}
{$I PSInc/Wrappers/tpa.inc}
{$I PSInc/Wrappers/strings.inc}
{$I PSInc/Wrappers/colour.inc}
{$I PSInc/Wrappers/math.inc}
{$I PSInc/Wrappers/mouse.inc}
{$I PSInc/Wrappers/file.inc}
{$I PSInc/Wrappers/keyboard.inc}
{$I PSInc/Wrappers/dtm.inc}
{$I PSInc/Wrappers/ocr.inc}
{$I PSInc/Wrappers/internets.inc}

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
  if c >= 300 then
    raise exception.create('PSThread.LoadMethods: Exported more than 300 functions');
  Result[c].FuncDecl:= DeclStr;
  Result[c].FuncPtr:= Ptr;
  Result[c].Section:= CurrSection;
  inc(c);
end;

begin
  c := 0;
  CurrSection := 'Other';
  SetLength(Result,300);

  {$i PSInc/psexportedmethods.inc}

  SetLength(Result,c);
end;

{***implementation TPSThread***}

{
  Note to Raymond: For PascalScript, Create it on the .Create,
  Execute it on the .Execute, and don't forget to Destroy it on .Destroy.

  Furthermore, all the wrappers can be in the unit "implementation" section.
  Better still to create an .inc for it, otherwise this unit will become huge.
  (You can even split up the .inc's in stuff like color, bitmap, etc. )

  Also, don't add PS to this unit, but make a seperate unit for it.
  Unit "MMLPSThread", perhaps?

  See the TestUnit for use of this thread, it's pretty straightforward.

  It may also be wise to turn the "Importing of wrappers" into an include as
  well, it will really make the unit more straightforward to use and read.
}

constructor TPSThread.Create(CreateSuspended : boolean; TheSyncInfo : PSyncInfo; plugin_dir: string);
var
  I : integer;
begin
  PSScript := TPSScript.Create(nil);
  PSScript.UsePreProcessor:= True;
  PSScript.OnNeedFile := @RequireFile;
  PSScript.OnProcessDirective:=@OnProcessDirective;
  PSScript.OnProcessUnknowDirective:=@PSScriptProcessUnknowDirective;
  PSScript.OnCompile:= @OnCompile;
  PSScript.OnCompImport:= @OnCompImport;
  PSScript.OnExecImport:= @OnExecImport;
  PSScript.OnFindUnknownFile:=@PSScriptFindUnknownFile;
  // Set some defines
  {$I PSInc/psdefines.inc}
  inherited Create(CreateSuspended, TheSyncInfo, plugin_dir);
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
  const DirectiveName, DirectiveParam: string; var Continue: Boolean);
begin
end;

function TPSThread.PSScriptFindUnknownFile(Sender: TObject;
  const OrginFileName: string; var FileName, Output: string): Boolean;
begin
  Writeln(OrginFileName + '-' +  Output + '-' + FileName);
  Result := false;
end;

procedure TPSThread.PSScriptProcessUnknowDirective(Sender: TPSPreProcessor;
  Parser: TPSPascalPreProcessorParser; const Active: Boolean;
  const DirectiveName, DirectiveParam: string; var Continue: Boolean);
begin
  Continue:= ProcessDirective(DirectiveName, DirectiveParam);
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
  {$I PSInc/pscompile.inc}
  Fonts := Client.MOCR.GetFonts;
  for i := fonts.count - 1 downto 0 do
    PSScript.Comp.AddConstantN(Fonts[i].Name,'string').SetString(Fonts[i].Name);

  for i := high(PluginsToLoad) downto 0 do
    for ii := 0 to PluginsGlob.MPlugins[PluginsToLoad[i]].MethodLen - 1 do
      PSScript.AddFunctionEx(PluginsGlob.MPlugins[PluginsToLoad[i]].Methods[ii].FuncPtr,
                           PluginsGlob.MPlugins[PluginsToLoad[i]].Methods[ii].FuncStr,
                           Muf_Conv_to_PS_Conv(PluginsGlob.MPlugins[PluginsToLoad[i]].Methods[ii].FuncConv));

  for i := 0 to high(VirtualKeys) do
    PSScript.Comp.AddConstantN(Format('VK_%S',[VirtualKeys[i].Str]),'Byte').SetInt(VirtualKeys[i].Key);
  // Here we add all the Consts/Types to the engine.

  //Export all the methods
  for i := 0 to high(ExportedMethods) do
    if ExportedMethods[i].FuncPtr <> nil then
      PSScript.AddFunctionEx(ExportedMethods[i].FuncPtr,ExportedMethods[i].FuncDecl,
                             {$ifdef PS_StdCall}cdStdCall{$else}cdRegister {$endif});
end;

function TPSThread.RequireFile(Sender: TObject;
  const OriginFileName: String; var FileName, OutPut: string): Boolean;
begin
  result:= LoadFile(OriginFileName,FileName,OutPut);
end;

procedure SIRegister_Mufasa(cl: TPSPascalCompiler);
begin
  with cl.AddClassN(cl.FindClass('TObject'),'TMufasaBitmap') do
  begin;
    RegisterMethod('constructor create');
    RegisterMethod('procedure Free');
    RegisterMethod('function SaveToFile(const FileName : string) :boolean;');
    RegisterMethod('procedure LoadFromFile(const FileName : string);');
    RegisterProperty('Index','Integer',iptR);
  end;
end;

function CreateMufasaBitmap : TMufasaBitmap;
begin;
  result := TMufasaBitmap.Create;
  CurrThread.Client.MBitmaps.AddBMP(result);
end;

procedure FreeMufasaBitmap(Self : TMufasaBitmap);
begin;
  CurrThread.Client.MBitmaps.FreeBMP(Self.Index);
end;

procedure MufasaBitmapIndex(self : TMufasaBitmap; var Index : integer);
begin;
  Index := self.Index;
end;

procedure RIRegister_Mufasa(cl: TPSRuntimeClassImporter);
begin;
  with cl.Add(TMufasaBitmap) do
  begin
    RegisterConstructor(@CreateMufasaBitmap,'CREATE');
    RegisterMethod(@FreeMufasaBitmap,'FREE');
    RegisterMethod(@TMufasaBitmap.SaveToFile, 'SAVETOFILE');
    RegisterMethod(@TMufasaBitmap.LoadFromFile, 'LOADFROMFILE');
    RegisterPropertyHelper(@MufasaBitmapIndex,nil,'INDEX');
  end;
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
  RIRegister_Mufasa(x);
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
        psWriteln(PSScript.CompilerErrorToStr(l) + ' at line ' + inttostr(PSScript.CompilerMessages[l].Row));
    end else
      psWriteln(PSScript.CompilerErrorToStr(l) + ' at line ' + inttostr(PSScript.CompilerMessages[l].Row));

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
      psWriteln('Compiled succesfully in ' + IntToStr(GetTickCount - Starttime) + ' ms.');
      if CompileOnly then
        exit;
//      if not (ScriptState = SCompiling) then
        if not PSScript.Execute then
          HandleError(PSScript.ExecErrorRow,PSScript.ExecErrorCol,PSScript.ExecErrorPosition,PSScript.ExecErrorToString,
                      errRuntime, PSScript.ExecErrorFileName)
        else
          psWriteln('Successfully executed.');
    end else
    begin
      OutputMessages;
      psWriteln('Compiling failed.');
    end;
  except
     on E : Exception do
       psWriteln('ERROR IN PSSCRIPT: ' + e.message);
  end;
end;

procedure TPSThread.Terminate;
begin
  PSScript.Stop;
end;

procedure TPSThread.SetScript(script: string);
begin
   PSScript.Script.Text:= Script;
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
  result:= CurrThread.ProcessDirective(name, args);
end;

procedure Interpreter_ErrorHandler(line, pos: integer; err: PChar; runtime: boolean); stdcall;
begin
  if runtime then
    CurrThread.HandleError(line,-1,pos,err,errRuntime,'')
  else
    CurrThread.HandleError(line,-1,pos,err,errCompile,'')
end;

constructor TCPThread.Create(CreateSuspended: Boolean; TheSyncInfo : PSyncInfo; plugin_dir: string);
var
  plugin_idx: integer;
begin
  if libcpascal = 0 then
    LoadCPascal(plugin_dir);
  instance:= interp_init(@Interpreter_Precompiler, @Interpreter_ErrorHandler);
  inherited Create(CreateSuspended, TheSyncInfo, plugin_dir);
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

  interp_type(self.instance,'type TTarget_Exported = record int1,int2,int3,int4,int5,int6,int7,int8,int9,int10,int11,int12,int13,int14:integer; end;');
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

initialization
  PluginsGlob := TMPlugins.Create;
  libcpascal:= 0;
finalization
  //PluginsGlob.Free;
  //Its a nice idea, but it will segfault... the program is closing anyway.
end.
