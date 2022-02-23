pushSection('File');

addGlobalFunc('function CreateFile(Path: String): Integer', @_LapeCreateFile);
addGlobalFunc('function OpenFile(Path: String; Shared: Boolean): Integer', @_LapeOpenFile);
addGlobalFunc('function RewriteFile(Path: String; Shared: Boolean): Integer', @_LapeRewriteFile);
addGlobalFunc('function AppendFile(Path: String): Integer', @_LapeAppendFile);
addGlobalFunc('procedure CloseFile(FileNum: Integer)', @_LapeCloseFile);
addGlobalFunc('function EndOfFile(FileNum: Integer): Boolean', @_LapeEndOfFile);
addGlobalFunc('function FileSize(FileNum: Integer): Integer', @_LapeFileSize);
addGlobalFunc('function ReadFileString(FileNum: Integer; var s: String; x: Integer): Boolean', @_LapeReadFileString);
addGlobalFunc('function WriteFileString(FileNum: Integer; s: String): Boolean', @_LapeWriteFileString);
addGlobalFunc('function SetFileCharPointer(FileNum, cChars, Origin: Integer): Integer', @_LapeSetFileCharPointer);
addGlobalFunc('function FilePointerPos(FileNum: Integer): Integer', @_LapeFilePointerPos);
addGlobalFunc('function GetFiles(Path, Ext: String): TStringArray', @_LapeGetFiles);
addGlobalFunc('function GetDirectories(Path: String): TStringArray', @_LapeGetDirectories);
addGlobalFunc('procedure WriteINI(Section, KeyName, NewString, FileName: String)', @_LapeWriteINI);
addGlobalFunc('function ReadINI(Section, KeyName, FileName: String): String', @_LapeReadINI);
addGlobalFunc('procedure DeleteINI(Section, KeyName, FileName: String)', @_LapeDeleteINI);
addGlobalFunc('procedure ZipFiles(const ArchiveFileName: String; const Files: TStringArray)', @_LapeZipFiles);
addGlobalFunc('procedure UnZipFile(const ArchiveFileName, OutputDirectory: String)', @_LapeUnZipFile);
addGlobalFunc('function UnZipOneFile(const ArchiveFileName, FileName, OutputDirectory: String): Boolean', @_LapeUnZipOneFile);
addGlobalFunc('function FindFile(Path, Mask: String; SearchSubDirs: Boolean = True; CaseSenstive: Boolean = False): TStringArray', @_LapeFindFile);

popSection();
