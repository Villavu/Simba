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
  uPSRuntime,stdCtrls, uPSPreProcessor,MufasaTypes,MufasaBase, web,
  bitmaps, plugins, libloader, dynlibs,internets,scriptproperties,
  settingssandbox;


type
    { TMMLPSThread }
    TSyncInfo = record
      V : MufasaTypes.PVariantArray;
      MethodName : string;
      Res : ^Variant;
      SyncMethod : procedure of object;
      OldThread : TThread;
    end;

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
    TInputQueryData = record
      ACaption, APrompt,Value : String;
      Res : boolean;
    end;
    { TMThread }

    TMThread = class(TThread)
    private
      procedure SetOpenConnectionEvent(const AValue: TOpenConnectionEvent);
      procedure SetOpenFileEvent(const AValue: TOpenFileEvent);
      procedure SetWriteFileEvent(const AValue: TWriteFileEvent);
    protected
      ScriptPath, AppPath, IncludePath, PluginPath, FontPath: string;
      DebugTo: TWritelnProc;
      DebugClear : TClearDebugProc;
      DebugImg : TDbgImgInfo;
      ExportedMethods : TExpMethodArr;
      Includes : TStringList;
      Prop: TScriptProperties;
      FOpenConnectionEvent : TOpenConnectionEvent;
      FWriteFileEvent : TWriteFileEvent;
      FOpenFileEvent : TOpenFileEvent;
      procedure LoadPlugin(plugidx: integer); virtual; abstract;

    public
      Client : TClient;
      MInternet : TMInternet;
      StartTime : LongWord;
      Sett: TMMLSettingsSandbox;

      InputQueryData : TInputQueryData;//We need this for InputQuery
      SyncInfo : PSyncInfo; //We need this for callthreadsafe
      ErrorData : PErrorData; //We need this for thread-safety etc
      OnError  : TOnError; //Error handeler

      CompileOnly : boolean;

      procedure mInputQuery;
      procedure HandleError(ErrorRow,ErrorCol,ErrorPosition : integer; ErrorStr : string; ErrorType : TErrorType; ErrorModule : string);
      function ProcessDirective(DirectiveName, DirectiveArgs: string): boolean;
      function LoadFile(ParentFile : string; var filename, contents: string): boolean;
      procedure AddMethod(meth: TExpMethod); virtual;

      procedure SetDebug( writelnProc : TWritelnProc );
      procedure SetDebugClear( clearProc : TClearDebugProc );
      procedure SetDbgImg( DebugImageInfo : TDbgImgInfo);
      procedure SetPaths(ScriptP,AppP,IncludeP,PluginP,FontP : string);
      procedure SetSettings(S: TMMLSettingsSandbox);

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
  uPSC_extctrls, //Compile-libs
  uPSUtils,
  fontloader,
  IOmanager,//TTarget_Exported
  IniFiles,//Silly INI files
  stringutil, //String st00f

  uPSR_std, uPSR_controls,uPSR_classes,uPSR_graphics,uPSR_stdctrls,uPSR_forms,
  uPSR_menus, uPSI_ComCtrls, uPSI_Dialogs,
  files,
  dialogs,
  uPSR_extctrls, //Runtime-libs
  Graphics, //For Graphics types
  math, //Maths!
  mmath, //Real maths!
  strutils,
  fileutil,
  tpa, //Tpa stuff
  forms,//Forms
  SynRegExpr,
  lclintf  // for GetTickCount and others.
  ;
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
  if CurrThread.Prop.WriteTimeStamp then
    str := format('[%s]: %s', [TimeToStr(TimeStampToDateTime(MSecsToTimeStamp(GetTickCount - CurrThread.StartTime))), str]);
  if Assigned(CurrThread.DebugTo) then
    CurrThread.DebugTo(str)
  else
    mDebugLn(str);
end;

procedure ps_DebugLn(str : string); extdecl;
begin
  if CurrThread.Prop.WriteTimeStamp then
    str := format('[%s]: %s', [TimeToStr(TimeStampToDateTime(MSecsToTimeStamp(GetTickCount - CurrThread.StartTime))), str]);
  mDebugLn(str);
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
  inherited Create(CreateSuspended);
  Client := TClient.Create(plugin_dir);
  if Assigned(WriteFileEvent) then
    Client.MFiles.WriteFileEvent := WriteFileEvent;
  if Assigned(OpenFileEvent) then
    Client.MFiles.OpenFileEvent := OpenFileEvent;
  MInternet := TMInternet.Create(Client);
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

procedure TMThread.mInputQuery;
begin
  InputQueryData.Res:= InputQuery(InputQueryData.ACaption,InputQueryData.APrompt,
                                  InputQueryData.Value);
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
  Includes.Add(path);

  try
    f:= TFileStream.Create(UTF8ToSys(Path), fmOpenRead or fmShareDenyWrite);
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
  Client.WritelnProc:= writelnProc;
end;

procedure TMThread.SetDebugClear(clearProc: TClearDebugProc);
begin
  DebugClear:= clearProc;
end;

procedure TMThread.SetDbgImg(DebugImageInfo: TDbgImgInfo);
begin
  DebugImg := DebugImageInfo;
end;

procedure TMThread.SetSettings(S: TMMLSettingsSandbox);
begin
  Self.Sett := S;
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

{$I PSInc/Wrappers/other.inc}
{$I PSInc/Wrappers/settings.inc}
{$I PSInc/Wrappers/bitmap.inc}
{$I PSInc/Wrappers/window.inc}
{$I PSInc/Wrappers/tpa.inc}
{$I PSInc/Wrappers/strings.inc}
{$I PSInc/Wrappers/colour.inc}
{$I PSInc/Wrappers/colourconv.inc}
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
  if c >= 400 then
    raise exception.create('PSThread.LoadMethods: Exported more than 400 functions');
  Result[c].FuncDecl:= DeclStr;
  Result[c].FuncPtr:= Ptr;
  Result[c].Section:= CurrSection;
  inc(c);
end;

begin
  c := 0;
  CurrSection := 'Other';
  SetLength(Result,400);

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
  PSScript.OnProcessDirective:=@OnProcessDirective;
  PSScript.OnProcessUnknowDirective:=@PSScriptProcessUnknowDirective;
  PSScript.OnCompile:= @OnCompile;
  PSScript.OnCompImport:= @OnCompImport;
  PSScript.OnExecImport:= @OnExecImport;
  PSScript.OnFindUnknownFile:=@PSScriptFindUnknownFile;
  // Set some defines
  {$I PSInc/psdefines.inc}
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
  mDebugLn(OrginFileName + '-' +  Output + '-' + FileName);
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
  Fonts := Client.MOCR.Fonts;
  for i := fonts.count - 1 downto 0 do
    Sender.Comp.AddConstantN(Fonts[i].Name,'string').SetString(Fonts[i].Name);

  for i := high(PluginsToLoad) downto 0 do
    for ii := 0 to PluginsGlob.MPlugins[PluginsToLoad[i]].MethodLen - 1 do
      Sender.AddFunctionEx(PluginsGlob.MPlugins[PluginsToLoad[i]].Methods[ii].FuncPtr,
                           PluginsGlob.MPlugins[PluginsToLoad[i]].Methods[ii].FuncStr,
                           Muf_Conv_to_PS_Conv(PluginsGlob.MPlugins[PluginsToLoad[i]].Methods[ii].FuncConv));

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
  Result:= LoadFile(OriginFileName,FileName,OutPut);
  if Result then
    Output := '{$DEFINE IS_INCLUDE}'+LineEnding+Output+LineEnding+'{$UNDEF IS_INCLUDE}';
end;

procedure SIRegister_Mufasa(cl: TPSPascalCompiler);
var
  PSClass : TPSCompileTimeClass;
begin
  PSClass :=cl.AddClassN(cl.FindClass('TObject'),'TMufasaBitmap');
  with PSClass do
  begin;
    RegisterMethod('procedure SetSize(AWidth,AHeight : integer);');
    RegisterMethod('procedure StretchResize(AWidth,AHeight : integer);');
    RegisterMethod('procedure FastSetPixel(x,y : integer; Color : TColor);');
    RegisterMethod('procedure FastSetPixels(TPA : TPointArray; Colors : TIntegerArray);');
    RegisterMethod('procedure DrawATPA(ATPA : T2DPointArray; Colors : TIntegerArray);');
    RegisterMethod('procedure DrawTPA(TPA : TPointArray; Color : TColor);');
    RegisterMethod('function FastGetPixel(x,y : integer) : TColor;');
    RegisterMethod('procedure Rectangle(const Box : TBox; FillCol : TColor);');
    RegisterMethod('procedure FloodFill(const StartPT : TPoint; const SearchCol,ReplaceCol : TColor);');
//      function FastGetPixels(TPA : TPointArray) : TIntegerArray;
    RegisterMethod('procedure SetTransparentColor(Col : TColor);');
    RegisterMethod('function GetTransparentColor : TColor;');
    RegisterProperty('TransparentColorSet','Boolean',iptR);
    RegisterMethod('procedure FastDrawClear(Color : TColor);');
    RegisterMethod('procedure FastDrawTransparent(x, y: Integer; TargetBitmap: TMufasaBitmap);');
    RegisterMethod('procedure FastReplaceColor(OldColor, NewColor: TColor);');
    RegisterMethod('procedure RotateBitmap(angle: Extended;TargetBitmap : TMufasaBitmap );');
    RegisterMethod('procedure Desaturate(TargetBitmap : TMufasaBitmap);');
    RegisterMethod('procedure GreyScale(TargetBitmap : TMufasaBitmap);');
    RegisterMethod('procedure Brightness(TargetBitmap : TMufasaBitmap; br : integer);');
    RegisterMethod('procedure Contrast(TargetBitmap : TMufasaBitmap; co : Extended);');
    RegisterMethod('procedure Invert(TargetBitmap : TMufasaBitmap);');
    RegisterMethod('procedure Posterize(TargetBitmap : TMufasaBitmap; Po : integer);');
    RegisterMethod('function Copy(const xs,ys,xe,ye : integer) : TMufasaBitmap;');
    RegisterMethod('function ToString : string;');
    RegisterMethod('function ToTBitmap : TBitmap;');
    RegisterMethod('function CreateTMask : TMask;');
    RegisterMethod('constructor create');
    RegisterMethod('procedure Free');
    RegisterMethod('function SaveToFile(const FileName : string) :boolean;');
    RegisterMethod('procedure LoadFromFile(const FileName : string);');
    RegisterProperty('Width','Integer',iptR);
    RegisterProperty('Height','Integer',iptR);
    RegisterProperty('Index','Integer',iptR);
    RegisterProperty('Name','String',iptRW);
  end;
  with CL.AddClassN(CL.FindClass('Exception'),'ERegExpr') do
  begin
    RegisterProperty('ErrorCode', 'integer', iptrw);
    RegisterProperty('CompilerErrorPos', 'integer', iptrw);
  end;
  PSClass :=cl.AddClassN(cl.FindClass('TObject'),'TRegExp');
  with PSClass do
  begin
    RegisterMethod('Constructor Create');
    RegisterMethod('Function VersionMajor : integer');
    RegisterMethod('Function VersionMinor : integer');
    RegisterProperty('Expression', 'String', iptrw);
    RegisterProperty('ModifierStr', 'String', iptrw);
    RegisterProperty('ModifierI', 'boolean', iptrw);
    RegisterProperty('ModifierR', 'boolean', iptrw);
    RegisterProperty('ModifierS', 'boolean', iptrw);
    RegisterProperty('ModifierG', 'boolean', iptrw);
    RegisterProperty('ModifierM', 'boolean', iptrw);
    RegisterProperty('ModifierX', 'boolean', iptrw);
    RegisterMethod('Function Exec( const AInputString : String) : boolean;');
    RegisterMethod('Function ExecNext : boolean');
    RegisterMethod('Function ExecPos( AOffset : integer) : boolean');
    RegisterProperty('InputString', 'String', iptrw);
    RegisterMethod('Function Substitute( const ATemplate : String) : String');
    RegisterMethod('Procedure Split( AInputStr : String; APieces : TStrings)');
    RegisterMethod('Function Replace( AInputStr : String; const AReplaceStr : String; AUseSubstitution : boolean) : String;');
    RegisterProperty('SubExprMatchCount', 'integer', iptr);
    RegisterProperty('MatchPos', 'integer integer', iptr);
    RegisterProperty('MatchLen', 'integer integer', iptr);
    RegisterProperty('Match', 'String integer', iptr);
    RegisterMethod('Function LastError : integer');
    RegisterMethod('Function ErrorMsg( AErrorID : integer) : String');
    RegisterProperty('CompilerErrorPos', 'integer', iptr);
    RegisterProperty('SpaceChars', 'String', iptrw);
    RegisterProperty('WordChars', 'String', iptrw);
    RegisterProperty('LineSeparators', 'String', iptrw);
    RegisterProperty('LinePairedSeparator', 'String', iptrw);
    RegisterMethod('Function InvertCaseFunction( const Ch : Char) : Char');
    RegisterProperty('InvertCase', 'TRegExprInvertCaseFunction', iptrw);
    RegisterMethod('Procedure Compile');
    RegisterMethod('Function Dump : String');
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

function TMufasaBitmapCopy(Self : TMufasaBitmap;const xs,ys,xe,ye : integer) : TMufasaBitmap;
begin
  result := Self.Copy(xs,ys,xe,ye);
  CurrThread.Client.MBitmaps.AddBMP(result);
end;

type
  TRegExp = class(SynRegExpr.TRegExpr);
procedure MBmp_Index_r(self : TMufasaBitmap; var Index : integer);begin;  Index := self.Index; end;
procedure MBmp_Width_r(self : TMufasaBitmap; var Width : integer);begin;  Width := self.Width; end;
procedure MBmp_Height_r(self : TMufasaBitmap; var Height : integer);begin;  Height := self.Height; end;
procedure MBmp_Name_r(self : TMufasaBitmap; var Name : String);begin;  Name := self.Name; end;
procedure MBmp_Name_w(self : TMufasaBitmap; const Name : String);begin; Self.name := name; end;
procedure MBmp_TransColorSet_r(Self : TMufasaBitmap; var IsSet : boolean); begin IsSet := self.TransparentColorSet; end;
procedure ERegExprCompilerErrorPos_W(Self: ERegExpr; const T: integer); Begin Self.CompilerErrorPos := T; end;
procedure ERegExprCompilerErrorPos_R(Self: ERegExpr; var T: integer);Begin T := Self.CompilerErrorPos; end;
procedure ERegExprErrorCode_W(Self: ERegExpr; const T: integer);Begin Self.ErrorCode := T; end;
procedure ERegExprErrorCode_R(Self: ERegExpr; var T: integer);Begin T := Self.ErrorCode; end;
procedure TRegExprInvertCase_W(Self: TRegExp; const T: TRegExprInvertCaseFunction);begin Self.InvertCase := T; end;
procedure TRegExprInvertCase_R(Self: TRegExp; var T: TRegExprInvertCaseFunction);begin T := Self.InvertCase; end;
procedure TRegExprLinePairedSeparator_W(Self: TRegExp; const T: RegExprString);begin Self.LinePairedSeparator := T; end;
procedure TRegExprLinePairedSeparator_R(Self: TRegExp; var T: RegExprString);begin T := Self.LinePairedSeparator; end;
procedure TRegExprLineSeparators_W(Self: TRegExp; const T: RegExprString);begin Self.LineSeparators := T; end;
procedure TRegExprLineSeparators_R(Self: TRegExp; var T: RegExprString);begin T := Self.LineSeparators; end;
procedure TRegExprWordChars_W(Self: TRegExp; const T: RegExprString);begin Self.WordChars := T; end;
procedure TRegExprWordChars_R(Self: TRegExp; var T: RegExprString);begin T := Self.WordChars; end;
procedure TRegExprSpaceChars_W(Self: TRegExp; const T: RegExprString);begin Self.SpaceChars := T; end;
procedure TRegExprSpaceChars_R(Self: TRegExp; var T: RegExprString);begin T := Self.SpaceChars; end;
procedure TRegExprCompilerErrorPos_R(Self: TRegExp; var T: integer);begin T := Self.CompilerErrorPos; end;
procedure TRegExprMatch_R(Self: TRegExp; var T: RegExprString; const t1: integer);begin T := Self.Match[t1]; end;
procedure TRegExprMatchLen_R(Self: TRegExp; var T: integer; const t1: integer);begin T := Self.MatchLen[t1]; end;
procedure TRegExprMatchPos_R(Self: TRegExp; var T: integer; const t1: integer);begin T := Self.MatchPos[t1]; end;
procedure TRegExprSubExprMatchCount_R(Self: TRegExp; var T: integer);begin T := Self.SubExprMatchCount; end;
Function TRegExprReplace2_P(Self: TRegExp;  AInputStr : RegExprString; AReplaceFunc : TRegExprReplaceFunction) : RegExprString;Begin Result := Self.Replace(AInputStr, AReplaceFunc); END;
Function TRegExprReplace_P(Self: TRegExp;  AInputStr : RegExprString; const AReplaceStr : RegExprString; AUseSubstitution : boolean) : RegExprString;Begin Result := Self.Replace(AInputStr, AReplaceStr, AUseSubstitution); END;
procedure TRegExprInputString_W(Self: TRegExp; const T: RegExprString);begin Self.InputString := T; end;
procedure TRegExprInputString_R(Self: TRegExp; var T: RegExprString);begin T := Self.InputString; end;
Function TRegExprExec_P(Self: TRegExp;  const AInputString : RegExprString) : boolean;Begin Result := Self.Exec(AInputString); END;
procedure TRegExprModifierX_W(Self: TRegExp; const T: boolean);begin Self.ModifierX := T; end;
procedure TRegExprModifierX_R(Self: TRegExp; var T: boolean);begin T := Self.ModifierX; end;
procedure TRegExprModifierM_W(Self: TRegExp; const T: boolean);begin Self.ModifierM := T; end;
procedure TRegExprModifierM_R(Self: TRegExp; var T: boolean);begin T := Self.ModifierM; end;
procedure TRegExprModifierG_W(Self: TRegExp; const T: boolean);begin Self.ModifierG := T; end;
procedure TRegExprModifierG_R(Self: TRegExp; var T: boolean);begin T := Self.ModifierG; end;
procedure TRegExprModifierS_W(Self: TRegExp; const T: boolean);begin Self.ModifierS := T; end;
procedure TRegExprModifierS_R(Self: TRegExp; var T: boolean);begin T := Self.ModifierS; end;
procedure TRegExprModifierR_W(Self: TRegExp; const T: boolean);begin Self.ModifierR := T; end;
procedure TRegExprModifierR_R(Self: TRegExp; var T: boolean);begin T := Self.ModifierR; end;
procedure TRegExprModifierI_W(Self: TRegExp; const T: boolean);begin Self.ModifierI := T; end;
procedure TRegExprModifierI_R(Self: TRegExp; var T: boolean);begin T := Self.ModifierI; end;
procedure TRegExprModifierStr_W(Self: TRegExp; const T: RegExprString);begin Self.ModifierStr := T; end;
procedure TRegExprModifierStr_R(Self: TRegExp; var T: RegExprString);begin T := Self.ModifierStr; end;
procedure TRegExprExpression_W(Self: TRegExp; const T: RegExprString);begin Self.Expression := T; end;
procedure TRegExprExpression_R(Self: TRegExp; var T: RegExprString);begin T := Self.Expression; end;

procedure RIRegister_Mufasa(cl: TPSRuntimeClassImporter);
var
  PSClass : TPSRuntimeClass;
begin;
  PSClass :=cl.Add(TMufasaBitmap);
  with PSClass do
  begin
    RegisterMethod(@TMufasaBitmap.ToTBitmap,'ToTBitmap');
    RegisterMethod(@TMufasaBitmap.SetSize,'SETSIZE');
    RegisterMethod(@TMufasaBitmap.StretchResize,'STRETCHRESIZE');
    RegisterMethod(@TMufasaBitmap.FastSetPixel,'FASTSETPIXEL');
    RegisterMethod(@TMufasaBitmap.FastSetPixels,'FASTSETPIXELS');
    RegisterMethod(@TMufasaBitmap.DrawATPA,'DRAWATPA');
    RegisterMethod(@TMufasaBitmap.DrawTPA,'DRAWTPA');
    RegisterMethod(@TMufasaBitmap.FloodFill,'FLOODFILL');
    RegisterMethod(@TMufasaBitmap.Rectangle,'RECTANGLE');
    RegisterMethod(@TMufasaBitmap.FastGetPixel,'FASTGETPIXEL');
    RegisterMethod(@TMufasaBitmap.SetTransparentColor,'SETTRANSPARENTCOLOR');
    RegisterMethod(@TMufasaBitmap.GetTransparentColor,'GETTRANSPARENTCOLOR');
    RegisterMethod(@TMufasaBitmap.FastDrawClear,'FASTDRAWCLEAR');
    RegisterMethod(@TMufasaBitmap.FastDrawTransparent,'FASTDRAWTRANSPARENT');
    RegisterMethod(@TMufasaBitmap.FastReplaceColor,'FASTREPLACECOLOR');
    RegisterMethod(@TMufasaBitmap.RotateBitmap,'ROTATEBITMAP');
    RegisterMethod(@TMufasaBitmap.Desaturate,'DESATURATE');
    RegisterMethod(@TMufasaBitmap.GreyScale,'GREYSCALE');
    RegisterMethod(@TMufasaBitmap.Brightness,'BRIGHTNESS');
    RegisterMethod(@TMufasaBitmap.Contrast,'CONTRAST');
    RegisterMethod(@TMufasaBitmap.Invert,'INVERT');
    RegisterMethod(@TMufasaBitmap.Posterize,'POSTERIZE');
    RegisterMethod(@TMufasaBitmapCopy, 'COPY');
    RegisterMethod(@TMufasaBitmap.ToString,'TOSTRING');
    RegisterMethod(@TMufasaBitmap.CreateTMask,'CREATETMASK');
    RegisterPropertyHelper(@MBmp_TransColorSet_r,nil,'TRANSPARENTCOLORSET');
    RegisterPropertyHelper(@MBmp_Index_r,nil,'INDEX');
    RegisterPropertyHelper(@MBmp_Width_r,nil,'WIDTH');
    RegisterPropertyHelper(@MBmp_Height_r,nil,'HEIGHT');
    RegisterPropertyHelper(@MBmp_Name_r,@MBmp_Name_w,'NAME');
    RegisterConstructor(@CreateMufasaBitmap,'CREATE');
    RegisterMethod(@FreeMufasaBitmap,'FREE');
    RegisterMethod(@TMufasaBitmap.SaveToFile, 'SAVETOFILE');
    RegisterMethod(@TMufasaBitmap.LoadFromFile, 'LOADFROMFILE');
  end;
  with CL.Add(ERegExpr) do
  begin
    RegisterPropertyHelper(@ERegExprErrorCode_R,@ERegExprErrorCode_W,'ErrorCode');
    RegisterPropertyHelper(@ERegExprCompilerErrorPos_R,@ERegExprCompilerErrorPos_W,'CompilerErrorPos');
  end;
  with CL.Add(TRegExp) do
  begin
    RegisterConstructor(@TRegExp.Create, 'Create');
    RegisterMethod(@TRegExp.VersionMajor, 'VersionMajor');
    RegisterMethod(@TRegExp.VersionMinor, 'VersionMinor');
    RegisterPropertyHelper(@TRegExprExpression_R,@TRegExprExpression_W,'Expression');
    RegisterPropertyHelper(@TRegExprModifierStr_R,@TRegExprModifierStr_W,'ModifierStr');
    RegisterPropertyHelper(@TRegExprModifierI_R,@TRegExprModifierI_W,'ModifierI');
    RegisterPropertyHelper(@TRegExprModifierR_R,@TRegExprModifierR_W,'ModifierR');
    RegisterPropertyHelper(@TRegExprModifierS_R,@TRegExprModifierS_W,'ModifierS');
    RegisterPropertyHelper(@TRegExprModifierG_R,@TRegExprModifierG_W,'ModifierG');
    RegisterPropertyHelper(@TRegExprModifierM_R,@TRegExprModifierM_W,'ModifierM');
    RegisterPropertyHelper(@TRegExprModifierX_R,@TRegExprModifierX_W,'ModifierX');
    RegisterMethod(@TRegExprExec_P, 'Exec');
    RegisterMethod(@TRegExp.ExecNext, 'ExecNext');
    RegisterMethod(@TRegExp.ExecPos, 'ExecPos');
    RegisterPropertyHelper(@TRegExprInputString_R,@TRegExprInputString_W,'InputString');
    RegisterMethod(@TRegExp.Substitute, 'Substitute');
    RegisterMethod(@TRegExp.Split, 'Split');
    RegisterMethod(@TRegExprReplace_P, 'Replace');
    RegisterPropertyHelper(@TRegExprSubExprMatchCount_R,nil,'SubExprMatchCount');
    RegisterPropertyHelper(@TRegExprMatchPos_R,nil,'MatchPos');
    RegisterPropertyHelper(@TRegExprMatchLen_R,nil,'MatchLen');
    RegisterPropertyHelper(@TRegExprMatch_R,nil,'Match');
    RegisterMethod(@TRegExp.LastError, 'LastError');
    RegisterVirtualMethod(@TRegExp.ErrorMsg, 'ErrorMsg');
    RegisterPropertyHelper(@TRegExprCompilerErrorPos_R,nil,'CompilerErrorPos');
    RegisterPropertyHelper(@TRegExprSpaceChars_R,@TRegExprSpaceChars_W,'SpaceChars');
    RegisterPropertyHelper(@TRegExprWordChars_R,@TRegExprWordChars_W,'WordChars');
    RegisterPropertyHelper(@TRegExprLineSeparators_R,@TRegExprLineSeparators_W,'LineSeparators');
    RegisterPropertyHelper(@TRegExprLinePairedSeparator_R,@TRegExprLinePairedSeparator_W,'LinePairedSeparator');
    RegisterMethod(@TRegExp.InvertCaseFunction, 'InvertCaseFunction');
    RegisterPropertyHelper(@TRegExprInvertCase_R,@TRegExprInvertCase_W,'InvertCase');
    RegisterMethod(@TRegExp.Compile, 'Compile');
    RegisterMethod(@TRegExp.Dump, 'Dump');
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
  SIRegister_Menus(x);
  SIRegister_ComCtrls(x);
  SIRegister_Dialogs(x);
  {$I PSInc/pscompile.inc}
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
  RIRegister_Menus(x);
  RIRegister_Mufasa(x);
  RIRegister_ComCtrls(x);
  RIRegister_Dialogs(x);
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
      psWriteln('Compiled succesfully in ' + IntToStr(GetTickCount - Starttime) + ' ms.');
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

initialization
  PluginsGlob := TMPlugins.Create;
  libcpascal:= 0;
finalization
  //PluginsGlob.Free;
  //Its a nice idea, but it will segfault... the program is closing anyway.
end.
