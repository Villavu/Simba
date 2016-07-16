unit lpTMFiles;
//Depends: TMFiles, TObject, TOpenFileEvent, TWriteFileEvent, string, Boolean, Integer

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, lpClassHelper;

procedure Register_TMFiles(Compiler: TLapeCompiler);

implementation

uses
  files, MufasaTypes;

type
  PMFiles = ^TMFiles;
  PObject = ^TObject;
  POpenFileEvent = ^TOpenFileEvent;
  PWriteFileEvent = ^TWriteFileEvent;


//Read: OpenFileEvent : TOpenFileEvent;
procedure TMFiles_OpenFileEvent_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  POpenFileEvent(Result)^ := PMFiles(Params^[0])^.OpenFileEvent;
end;

//Write: OpenFileEvent : TOpenFileEvent;
procedure TMFiles_OpenFileEvent_Write(const Params: PParamArray); lape_extdecl
begin
  PMFiles(Params^[0])^.OpenFileEvent := POpenFileEvent(Params^[1])^;
end;

//Read: WriteFileEvent: TWriteFileEvent;
procedure TMFiles_WriteFileEvent_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PWriteFileEvent(Result)^ := PMFiles(Params^[0])^.WriteFileEvent;
end;

//Write: WriteFileEvent: TWriteFileEvent;
procedure TMFiles_WriteFileEvent_Write(const Params: PParamArray); lape_extdecl
begin
  PMFiles(Params^[0])^.WriteFileEvent := PWriteFileEvent(Params^[1])^;
end;

//function CreateFile(Path: string): Integer;
procedure TMFiles_CreateFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMFiles(Params^[0])^.CreateFile(PlpString(Params^[1])^);
end;

//function OpenFile(Path: string; Shared: Boolean): Integer;
procedure TMFiles_OpenFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMFiles(Params^[0])^.OpenFile(PlpString(Params^[1])^, PBoolean(Params^[2])^);
end;

//function RewriteFile(Path: string; Shared: Boolean): Integer;
procedure TMFiles_RewriteFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMFiles(Params^[0])^.RewriteFile(PlpString(Params^[1])^, PBoolean(Params^[2])^);
end;

//function AppendFile(Path: string): Integer;
procedure TMFiles_AppendFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMFiles(Params^[0])^.AppendFile(PlpString(Params^[1])^);
end;

//function DeleteFile(Filename: string): Boolean;
procedure TMFiles_DeleteFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMFiles(Params^[0])^.DeleteFile(PlpString(Params^[1])^);
end;

//function RenameFile(OldName, NewName: string): Boolean;
procedure TMFiles_RenameFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMFiles(Params^[0])^.RenameFile(PlpString(Params^[1])^, PlpString(Params^[2])^);
end;

//procedure CloseFile(FileNum: Integer);
procedure TMFiles_CloseFile(const Params: PParamArray); lape_extdecl
begin
  PMFiles(Params^[0])^.CloseFile(PInteger(Params^[1])^);
end;

//procedure WriteINI(const Section, KeyName, NewString : string; FileName : string);
procedure TMFiles_WriteINI(const Params: PParamArray); lape_extdecl
begin
  PMFiles(Params^[0])^.WriteINI(PlpString(Params^[1])^, PlpString(Params^[2])^, PlpString(Params^[3])^, PlpString(Params^[4])^);
end;

//function ReadINI(const Section, KeyName : string; FileName : string) : string;
procedure TMFiles_ReadINI(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PMFiles(Params^[0])^.ReadINI(PlpString(Params^[1])^, PlpString(Params^[2])^, PlpString(Params^[3])^);
end;

//procedure DeleteINI(const Section, KeyName : string; FileName : string);
procedure TMFiles_DeleteINI(const Params: PParamArray); lape_extdecl
begin
  PMFiles(Params^[0])^.DeleteINI(PlpString(Params^[1])^, PlpString(Params^[2])^, PlpString(Params^[3])^);
end;

//function EndOfFile(FileNum: Integer): Boolean;
procedure TMFiles_EndOfFile(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMFiles(Params^[0])^.EndOfFile(PInteger(Params^[1])^);
end;

//function FileSizeMuf(FileNum: Integer): LongInt;
procedure TMFiles_FileSizeMuf(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongInt(Result)^ := PMFiles(Params^[0])^.FileSizeMuf(PInteger(Params^[1])^);
end;

//function ReadFileString(FileNum: Integer; out s: string; x: Integer): Boolean;
procedure TMFiles_ReadFileString(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMFiles(Params^[0])^.ReadFileString(PInteger(Params^[1])^, PlpString(Params^[2])^, PInteger(Params^[3])^);
end;

//function WriteFileString(FileNum: Integer;const s: string): Boolean;
procedure TMFiles_WriteFileString(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PBoolean(Result)^ := PMFiles(Params^[0])^.WriteFileString(PInteger(Params^[1])^, PlpString(Params^[2])^);
end;

//function SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer;
procedure TMFiles_SetFileCharPointer(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMFiles(Params^[0])^.SetFileCharPointer(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

//function FilePointerPos(FileNum: Integer): Integer;
procedure TMFiles_FilePointerPos(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PMFiles(Params^[0])^.FilePointerPos(PInteger(Params^[1])^);
end;

//constructor Create(Owner : TObject);
procedure TMFiles_Init(const Params: PParamArray); lape_extdecl
begin
  PMFiles(Params^[0])^ := TMFiles.Create(PObject(Params^[1])^);
end;

//procedure Free();
procedure TMFiles_Free(const Params: PParamArray); lape_extdecl
begin
  PMFiles(Params^[0])^.Free();
end;

procedure Register_TMFiles(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TMFiles');

    //addClassVar('TMFiles', 'OpenFileEvent', 'TOpenFileEvent', @TMFiles_OpenFileEvent_Read, @TMFiles_OpenFileEvent_Write);
    //addClassVar('TMFiles', 'WriteFileEvent', 'TWriteFileEvent', @TMFiles_WriteFileEvent_Read, @TMFiles_WriteFileEvent_Write);

    addGlobalFunc('function TMFiles.CreateFile(Path: string): Integer; constref;', @TMFiles_CreateFile);
    addGlobalFunc('function TMFiles.OpenFile(Path: string; Shared: Boolean): Integer; constref;', @TMFiles_OpenFile);
    addGlobalFunc('function TMFiles.RewriteFile(Path: string; Shared: Boolean): Integer; constref;', @TMFiles_RewriteFile);
    addGlobalFunc('function TMFiles.AppendFile(Path: string): Integer; constref;', @TMFiles_AppendFile);
    addGlobalFunc('function TMFiles.DeleteFile(Filename: string): Boolean; constref;', @TMFiles_DeleteFile);
    addGlobalFunc('function TMFiles.RenameFile(OldName, NewName: string): Boolean; constref;', @TMFiles_RenameFile);
    addGlobalFunc('procedure TMFiles.CloseFile(FileNum: Integer); constref;', @TMFiles_CloseFile);
    addGlobalFunc('procedure TMFiles.WriteINI(const Section, KeyName, NewString : string; FileName : string); constref;', @TMFiles_WriteINI);
    addGlobalFunc('function TMFiles.ReadINI(const Section, KeyName : string; FileName : string): string; constref;', @TMFiles_ReadINI);
    addGlobalFunc('procedure TMFiles.DeleteINI(const Section, KeyName : string; FileName : string); constref;', @TMFiles_DeleteINI);
    addGlobalFunc('function TMFiles.EndOfFile(FileNum: Integer): Boolean; constref;', @TMFiles_EndOfFile);
    addGlobalFunc('function TMFiles.FileSizeMuf(FileNum: Integer): LongInt; constref;', @TMFiles_FileSizeMuf);
    addGlobalFunc('function TMFiles.ReadFileString(FileNum: Integer; out s: string; x: Integer): Boolean; constref;', @TMFiles_ReadFileString);
    addGlobalFunc('function TMFiles.WriteFileString(FileNum: Integer;const s: string): Boolean; constref;', @TMFiles_WriteFileString);
    addGlobalFunc('function TMFiles.SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer; constref;', @TMFiles_SetFileCharPointer);
    addGlobalFunc('function TMFiles.FilePointerPos(FileNum: Integer): Integer; constref;', @TMFiles_FilePointerPos);
    addGlobalFunc('procedure TMFiles.Init(Owner : TObject);', @TMFiles_Init);
    addGlobalFunc('procedure TMFiles.Free(); constref;', @TMFiles_Free);
  end;
end;

end.

