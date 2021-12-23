FSection := 'Process';
addGlobalType('type SizeUInt', 'TProcessID');
addGlobalType('type Integer', 'TProcessExitStatus');
addGlobalFunc('function GetScriptPID: TProcessID', @_LapeGetScriptPID);
addGlobalFunc('function GetScriptParameters: TStringArray', @_LapeGetScriptParameters);
addGlobalFunc('function GetScriptParameter(Name: String): String', @_LapeGetScriptParameter);
addGlobalFunc('function IsProcessRunning(PID: TProcessID): Boolean', @_LapeIsProcessRunning);
addGlobalFunc('function IsProcess64Bit(PID: TProcessID): Boolean', @_LapeIsProcess64Bit);
addGlobalFunc('function GetProcessPath(PID: TProcessID): String', @_LapeGetProcessPath);
addGlobalFunc('procedure TerminateProcess(PID: TProcessID)', @_LapeTerminateProcess);
addGlobalFunc('function RunCommandInDir(Directory, Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus; overload', @_LapeRunCommandInDirOutput);
addGlobalFunc('function RunCommandInDir(Directory, Executable: String; Commands: TStringArray): TProcessID; overload', @_LapeRunCommandInDir);
addGlobalFunc('function RunCommand(Executable: String; Commands: TStringArray; out Output: String): TProcessExitStatus; overload', @_LapeRunCommandOutput);
addGlobalFunc('function RunCommand(Executable: String; Commands: TStringArray): TProcessID; overload', @_LapeRunCommand);
addGlobalFunc('function RunCommandTimeout(Executable: String; Commands: TStringArray; out Output: String; Timeout: Int32): Boolean', @_LapeRunCommandTimeout);
addGlobalFunc('function RunScript(Script: String; Parameters: TStringArray; out Output: String): TProcessExitStatus; overload', @_LapeRunScript);
addGlobalFunc('function RunScript(Script: String; Parameters: TStringArray): TProcessID; overload', @_LapeRunScriptEx);
addGlobalFunc('function RunScriptOutputToFile(Script: String; Parameters: TStringArray; OutputFileName: String): TProcessID', @_LapeRunScriptOutputToFile);

