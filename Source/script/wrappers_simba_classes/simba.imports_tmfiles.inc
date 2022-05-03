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

