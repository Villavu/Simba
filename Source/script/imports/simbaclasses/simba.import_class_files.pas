unit simba.import_class_files;

{$i simba.inc}

interface

implementation

uses
  classes, sysutils, lptypes,
  simba.script_compiler, simba.files;

type
  PObject = ^TObject;

procedure _LapeMFiles_CreateFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMFiles(Params^[0])^.CreateFile(PString(Params^[1])^);
end;

procedure _LapeMFiles_OpenFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMFiles(Params^[0])^.OpenFile(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeMFiles_RewriteFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMFiles(Params^[0])^.RewriteFile(PString(Params^[1])^, PBoolean(Params^[2])^);
end;

procedure _LapeMFiles_AppendFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMFiles(Params^[0])^.AppendFile(PString(Params^[1])^);
end;

procedure _LapeMFiles_DeleteFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFiles(Params^[0])^.DeleteFile(PString(Params^[1])^);
end;

procedure _LapeMFiles_RenameFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFiles(Params^[0])^.RenameFile(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeMFiles_CloseFile(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFiles(Params^[0])^.CloseFile(PInteger(Params^[1])^);
end;

procedure _LapeMFiles_WriteINI(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFiles(Params^[0])^.WriteINI(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^, PString(Params^[4])^);
end;

procedure _LapeMFiles_ReadINI(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := PMFiles(Params^[0])^.ReadINI(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

procedure _LapeMFiles_DeleteINI(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFiles(Params^[0])^.DeleteINI(PString(Params^[1])^, PString(Params^[2])^, PString(Params^[3])^);
end;

procedure _LapeMFiles_EndOfFile(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFiles(Params^[0])^.EndOfFile(PInteger(Params^[1])^);
end;

procedure _LapeMFiles_FileSizeMuf(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PLongInt(Result)^ := PMFiles(Params^[0])^.FileSizeMuf(PInteger(Params^[1])^);
end;

procedure _LapeMFiles_ReadFileString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFiles(Params^[0])^.ReadFileString(PInteger(Params^[1])^, PString(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMFiles_WriteFileString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := PMFiles(Params^[0])^.WriteFileString(PInteger(Params^[1])^, PString(Params^[2])^);
end;

procedure _LapeMFiles_SetFileCharPointer(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMFiles(Params^[0])^.SetFileCharPointer(PInteger(Params^[1])^, PInteger(Params^[2])^, PInteger(Params^[3])^);
end;

procedure _LapeMFiles_FilePointerPos(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PInteger(Result)^ := PMFiles(Params^[0])^.FilePointerPos(PInteger(Params^[1])^);
end;

procedure _LapeMFiles_Init(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFiles(Params^[0])^ := TMFiles.Create(PObject(Params^[1])^);
end;

procedure _LapeMFiles_Free(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PMFiles(Params^[0])^.Free();
end;

procedure ImportFiles(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    pushSection('Classes');

    addClass('TMFiles');
    //addClassVar('TMFiles', 'OpenFileEvent', 'TOpenFileEvent', @_LapeMFiles_OpenFileEvent_Read, @_LapeMFiles_OpenFileEvent_Write);
    //addClassVar('TMFiles', 'WriteFileEvent', 'TWriteFileEvent', @_LapeMFiles_WriteFileEvent_Read, @_LapeMFiles_WriteFileEvent_Write);
    addGlobalFunc('function TMFiles.CreateFile(Path: string): Integer;', @_LapeMFiles_CreateFile);
    addGlobalFunc('function TMFiles.OpenFile(Path: string; Shared: Boolean): Integer;', @_LapeMFiles_OpenFile);
    addGlobalFunc('function TMFiles.RewriteFile(Path: string; Shared: Boolean): Integer;', @_LapeMFiles_RewriteFile);
    addGlobalFunc('function TMFiles.AppendFile(Path: string): Integer;', @_LapeMFiles_AppendFile);
    addGlobalFunc('function TMFiles.DeleteFile(Filename: string): Boolean;', @_LapeMFiles_DeleteFile);
    addGlobalFunc('function TMFiles.RenameFile(OldName, NewName: string): Boolean;', @_LapeMFiles_RenameFile);
    addGlobalFunc('procedure TMFiles.CloseFile(FileNum: Integer);', @_LapeMFiles_CloseFile);
    addGlobalFunc('procedure TMFiles.WriteINI(const Section, KeyName, NewString : string; FileName : string);', @_LapeMFiles_WriteINI);
    addGlobalFunc('function TMFiles.ReadINI(const Section, KeyName : string; FileName : string): string;', @_LapeMFiles_ReadINI);
    addGlobalFunc('procedure TMFiles.DeleteINI(const Section, KeyName : string; FileName : string);', @_LapeMFiles_DeleteINI);
    addGlobalFunc('function TMFiles.EndOfFile(FileNum: Integer): Boolean;', @_LapeMFiles_EndOfFile);
    addGlobalFunc('function TMFiles.FileSizeMuf(FileNum: Integer): Integer;', @_LapeMFiles_FileSizeMuf);
    addGlobalFunc('function TMFiles.ReadFileString(FileNum: Integer; out s: string; x: Integer): Boolean;', @_LapeMFiles_ReadFileString);
    addGlobalFunc('function TMFiles.WriteFileString(FileNum: Integer;const s: string): Boolean;', @_LapeMFiles_WriteFileString);
    addGlobalFunc('function TMFiles.SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer;', @_LapeMFiles_SetFileCharPointer);
    addGlobalFunc('function TMFiles.FilePointerPos(FileNum: Integer): Integer;', @_LapeMFiles_FilePointerPos);
    addGlobalFunc('procedure TMFiles.Init(Owner : TObject)', @_LapeMFiles_Init);
    //addGlobalFunc('procedure TMFiles.Free;', @_LapeMFiles_Free);

    popSection();
  end;
end;

initialization
  TSimbaScript_Compiler.RegisterImport(@ImportFiles);

end.

