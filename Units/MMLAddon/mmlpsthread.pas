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
  Classes, SysUtils, client, uPSComponent,uPSCompiler,uPSRuntime,stdCtrls, uPSPreProcessor,MufasaTypes, web,bitmaps;

type
    { TMMLPSThread }
    TSyncInfo = record
      V : MufasaTypes.TVariantArray;
      MethodName : string;
      Res : Variant;
      SyncMethod : procedure of object;
      OldThread : TThread;
      PSScript : TPSScript;
    end;

    TWritelnProc = procedure(s: string);
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
      Line,Position : integer;
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

    TMMLPSThread = class(TThread)
      procedure OnProcessDirective(Sender: TPSPreProcessor;
        Parser: TPSPascalPreProcessorParser; const Active: Boolean;
        const DirectiveName, DirectiveParam: string; var Continue: Boolean);
      function PSScriptFindUnknownFile(Sender: TObject;
        const OrginFileName: string; var FileName, Output: string
        ): Boolean;
      procedure PSScriptProcessUnknowDirective(Sender: TPSPreProcessor;
        Parser: TPSPascalPreProcessorParser; const Active: Boolean;
        const DirectiveName, DirectiveParam: string; var Continue: Boolean);
    private
      ScriptPath, AppPath, IncludePath, PluginPath, FontPath: string;
      procedure HandleError(ErrorAtLine,ErrorPosition : integer; ErrorStr : string; ErrorType : TErrorType; ErrorModule : string);
    protected
      //DebugTo : TMemo;
      DebugTo: TWritelnProc;
      DebugImg : TDbgImgInfo;
      PluginsToload : Array of integer;
      FOnError  : TOnError;
      procedure OnCompile(Sender: TPSScript);
      function RequireFile(Sender: TObject; const OriginFileName: String;
                          var FileName, OutPut: string): Boolean;
      procedure OnCompImport(Sender: TObject; x: TPSPascalCompiler);
      procedure OnExecImport(Sender: TObject; se: TPSExec; x: TPSRuntimeClassImporter);
      procedure OutputMessages;
      procedure OnThreadTerminate(Sender: TObject);
      procedure Execute; override;
    public
      ExportedMethods : TExpMethodArr;
      PSScript : TPSScript;   // Moved to public, as we can't kill it otherwise.
      Client : TClient;
      StartTime : LongWord;
      DebugMemo : TMemo;
      SyncInfo : PSyncInfo; //We need this for callthreadsafe
      ErrorData : PErrorData; //We need this for thread-safety etc
      property OnError : TOnError read FOnError write FOnError;
      procedure LoadMethods;
      class function GetExportedMethods : TExpMethodArr;
      procedure SetPSScript(Script : string);
      procedure SetDebug( writelnProc : TWritelnProc );
      procedure SetDbgImg( DebugImageInfo : TDbgImgInfo);
      procedure SetPaths(ScriptP,AppP,IncludeP,PluginP,FontP : string);
      constructor Create(CreateSuspended: Boolean; TheSyncInfo : PSyncInfo);
      destructor Destroy; override;
    end;
threadvar
  CurrThread : TMMLPSThread;
implementation
uses
  colour_conv,dtmutil,
  {$ifdef mswindows}windows,{$endif}
  uPSC_std, uPSC_controls,uPSC_classes,uPSC_graphics,uPSC_stdctrls,uPSC_forms,
  uPSC_extctrls, //Compile-libs
  uPSUtils,
  uPSR_std, uPSR_controls,uPSR_classes,uPSR_graphics,uPSR_stdctrls,uPSR_forms,
  uPSR_extctrls, //Runtime-libs
  Graphics, //For Graphics types
  math, //Maths!
  internets, // internets
  strutils,
  input,
  tpa, //Tpa stuff
  forms,//Forms
  lclintf; // for GetTickCount and others.


{Some General PS Functions here}
procedure psWriteln(str : string);
begin
  if Assigned(CurrThread.DebugTo) then
    CurrThread.DebugTo(str)
  else
    writeln(str);
end;

function MakeString(data : TPSVariantIFC) : string;
begin;
  if data.aType.basetype in [btString,btChar] then
    result := PSGetAnsiString(Data.Dta,data.aType)
  else if data.aType.ExportName = 'BOOLEAN' then
    result := BoolToStr(PSGetInt(Data.Dta,data.aType) <> 0,true)
  else
    result := PSVariantToString(data,'');
end;

function writeln_(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  arr: TPSVariantIFC;
begin
  Result:=true;
  psWriteln(makeString(NewTPSVariantIFC(Stack[Stack.Count-1],false)));
end;

function ToStr_(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  data: TPSVariantIFC;
begin
  result := true;
  Stack.SetAnsiString(-1, MakeString(NewTPSVariantIFC(Stack[Stack.Count-2],false)));

end;

function NewThreadCall(Procname : string) : Cardinal;
begin;
  result :=   CurrThread.PSScript.Exec.GetVar(Procname);
end;

function ThreadSafeCall(ProcName: string; var V: TVariantArray): Variant;
begin;
  CurrThread.SyncInfo^.MethodName:= ProcName;
  CurrThread.SyncInfo^.V:= V;
  CurrThread.SyncInfo^.PSScript := CurrThread.PSScript;
  CurrThread.SyncInfo^.OldThread := CurrThread;
  CurrThread.Synchronize(CurrThread.SyncInfo^.SyncMethod);
  Result := CurrThread.SyncInfo^.Res;
{  Writeln('We have a length of: '  + inttostr(length(v)));
  Try
    Result := CurrThread.PSScript.Exec.RunProcPVar(v,CurrThread.PSScript.Exec.GetProc(Procname));
  Except
    Writeln('We has some errors :-(');
  end;}
end;


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


constructor TMMLPSThread.Create(CreateSuspended : boolean; TheSyncInfo : PSyncInfo);
begin
  SyncInfo:= TheSyncInfo;
  SetLength(PluginsToLoad,0);
  Client := TClient.Create;
  PSScript := TPSScript.Create(nil);
  PSScript.UsePreProcessor:= True;
  PSScript.OnNeedFile := @RequireFile;
  PSScript.OnProcessDirective:=@OnProcessDirective;
  PSScript.OnProcessUnknowDirective:=@PSScriptProcessUnknowDirective;
  PSScript.OnCompile:= @OnCompile;
  PSScript.OnCompImport:= @OnCompImport;
  PSScript.OnExecImport:= @OnExecImport;
  PSScript.OnFindUnknownFile:=@PSScriptFindUnknownFile;
  OnError:= nil;
  // Set some defines
  {$I PSInc/psdefines.inc}
  // Load the methods we're going to export
  Self.LoadMethods;

  FreeOnTerminate := True;
  Self.OnTerminate := @Self.OnThreadTerminate;

  inherited Create(CreateSuspended);
end;

procedure TMMLPSThread.OnThreadTerminate(Sender: TObject);
begin
//  Writeln('Terminating the thread');
end;

destructor TMMLPSThread.Destroy;
begin
  SetLength(PluginsToLoad,0);
  Client.Free;
  PSScript.Free;
  inherited;
end;

// include PS wrappers
{$I PSInc/Wrappers/other.inc}
{$I PSInc/Wrappers/bitmap.inc}
{$I PSInc/Wrappers/window.inc}
{$I PSInc/Wrappers/strings.inc}

{$I PSInc/Wrappers/colour.inc}
{$I PSInc/Wrappers/math.inc}
{$I PSInc/Wrappers/mouse.inc}
{$I PSInc/Wrappers/file.inc}

{$I PSInc/Wrappers/keyboard.inc}
{$I PSInc/Wrappers/dtm.inc}
{$I PSInc/Wrappers/ocr.inc}
{$I PSInc/Wrappers/internets.inc}

procedure TMMLPSThread.OnProcessDirective(Sender: TPSPreProcessor;
  Parser: TPSPascalPreProcessorParser; const Active: Boolean;
  const DirectiveName, DirectiveParam: string; var Continue: Boolean);
begin
end;

function TMMLPSThread.PSScriptFindUnknownFile(Sender: TObject;
  const OrginFileName: string; var FileName, Output: string): Boolean;
begin
  Writeln(OrginFileName + '-' +  Output + '-' + FileName);
end;

procedure TMMLPSThread.PSScriptProcessUnknowDirective(Sender: TPSPreProcessor;
  Parser: TPSPascalPreProcessorParser; const Active: Boolean;
  const DirectiveName, DirectiveParam: string; var Continue: Boolean);
var
  TempNum : integer;
  I: integer;
begin
  if DirectiveName= 'LOADDLL' then
    if DirectiveParam <> '' then
    begin;
      TempNum := PluginsGlob.LoadPlugin(DirectiveParam);
      if TempNum < 0 then
        psWriteln(Format('Your DLL %s has not been found',[DirectiveParam]))
      else
      begin;
        for i := High(PluginsToLoad) downto 0 do
          if PluginsToLoad[i] = TempNum then
            Exit;
        SetLength(PluginsToLoad,Length(PluginsToLoad)+1);
        PluginsToLoad[High(PluginsToLoad)] := TempNum;
      end;
    end;
  Continue:= True;
end;

procedure TMMLPSThread.HandleError(ErrorAtLine, ErrorPosition: integer;
  ErrorStr: string; ErrorType: TErrorType; ErrorModule : string);
begin
  if FOnError = nil then
    exit;
  ErrorData^.Line:= ErrorAtLine;
  ErrorData^.Position:= ErrorPosition;
  ErrorData^.Error:= ErrorStr;
  ErrorData^.ErrType:= ErrorType;
  ErrorData^.Module:= ErrorModule;
  ErrorData^.IncludePath:= IncludePath;
  CurrThread.Synchronize(FOnError);
end;



procedure TMMLPSThread.OnCompile(Sender: TPSScript);
var
  i,ii : integer;
begin
  {$I PSInc/pscompile.inc}

  for i := high(PluginsToLoad) downto 0 do
    for ii := 0 to PluginsGlob.MPlugins[PluginsToLoad[i]].MethodLen - 1 do
      PSScript.AddFunctionEx(PluginsGlob.MPlugins[PluginsToLoad[i]].Methods[ii].FuncPtr,
                           PluginsGlob.MPlugins[PluginsToLoad[i]].Methods[ii].FuncStr, cdStdCall);

  for i := 0 to high(VirtualKeys) do
    PSScript.Comp.AddConstantN(Format('VK_%S',[VirtualKeys[i].Str]),'Byte').SetInt(VirtualKeys[i].Key);
  // Here we add all the Consts/Types to the engine.

  //Export all the methods
  for i := 0 to high(ExportedMethods) do
    if ExportedMethods[i].FuncPtr <> nil then
      PSScript.AddFunction(ExportedMethods[i].FuncPtr,ExportedMethods[i].FuncDecl);
end;

function TMMLPSThread.RequireFile(Sender: TObject;
  const OriginFileName: String; var FileName, OutPut: string): Boolean;
var
  path: string;
  f: TFileStream;
begin
  if FileExists(FileName) then
    Path := FileName
  else
    Path :=  IncludePath + Filename;
  if not FileExists(Path) then
  begin;
    psWriteln(Path + ' doesn''t exist');
    Result := false;
    Exit;
  end;
  try
    F := TFileStream.Create(Path, fmOpenRead or fmShareDenyWrite);
  except
    Result := false;
    exit;
  end;
  try
    SetLength(Output, f.Size);
    f.Read(Output[1], Length(Output));
  finally
    f.Free;
  end;
  Result := True;
end;

procedure SIRegister_Mufasa(cl: TPSPascalCompiler);

begin;
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
  result := CurrThread.Client.MBitmaps.Bmp[  CurrThread.Client.MBitmaps.CreateBMP(0,0)];
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

procedure TMMLPSThread.OnCompImport(Sender: TObject; x: TPSPascalCompiler);
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
end;

procedure TMMLPSThread.OnExecImport(Sender: TObject; se: TPSExec;
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
end;

procedure TMMLPSThread.OutputMessages;
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
          HandleError(Row, Pos, MessageToString,errCompile, ModuleName)
      else
        psWriteln(PSScript.CompilerErrorToStr(l) + ' at line ' + inttostr(PSScript.CompilerMessages[l].Row));
    end else
      psWriteln(PSScript.CompilerErrorToStr(l) + ' at line ' + inttostr(PSScript.CompilerMessages[l].Row));

  end;
end;

procedure TMMLPSThread.Execute;
begin
  CurrThread := Self;
  Starttime := lclintf.GetTickCount;

  try
    if PSScript.Compile then
    begin
      OutputMessages;
      psWriteln('Compiled succesfully in ' + IntToStr(GetTickCount - Starttime) + ' ms.');
//      if not (ScriptState = SCompiling) then
        if not PSScript.Execute then
          HandleError(PSScript.ExecErrorRow,PSScript.ExecErrorPosition,PSScript.ExecErrorToString,
                      errRuntime, PSScript.ExecErrorFileName)
        else
          psWriteln('Succesfully executed');
    end else
    begin
      OutputMessages;
      psWriteln('Compiling failed');
    end;
  except
     on E : Exception do
       psWriteln('ERROR IN PSSCRIPT: ' + e.message);
  end;
end;

procedure TMMLPSThread.LoadMethods;
begin
  ExportedMethods:= GetExportedMethods;
end;

class function TMMLPSThread.GetExportedMethods: TExpMethodArr;
var
  c : integer;
  CurrSection : string;
procedure SetCurrSection(str : string);
begin;
  CurrSection := Str;
end;

procedure AddFunction( Ptr : Pointer; DeclStr : String);
begin;
//  SetLength(ExportedMethods,c+1);
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

procedure TMMLPSThread.SetPSScript(Script: string);
begin
   PSScript.Script.Text:= Script;
end;

procedure TMMLPSThread.SetDebug(writelnProc: TWritelnProc);
begin
  DebugTo := writelnProc;
end;

procedure TMMLPSThread.SetDbgImg(DebugImageInfo: TDbgImgInfo);
begin
  DebugImg := DebugImageInfo;
end;

procedure TMMLPSThread.SetPaths(ScriptP, AppP,IncludeP,PluginP,FontP: string);
begin
  AppPath:= AppP;
  ScriptPath:= ScriptP;
  IncludePath:= IncludeP;
  PluginPath:= PluginP;
  FontPath:= FontP;

end;


{ Include stuff here? }

//{$I inc/colors.inc}
//{$I inc/bitmaps.inc}


end.


