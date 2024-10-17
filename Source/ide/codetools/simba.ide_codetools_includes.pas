{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Caches an entire include/plugin.
}
unit simba.ide_codetools_includes;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.threading,
  simba.ide_codetools_base, simba.ide_codetools_paslexer, simba.ide_codetools_parser;

const
  PurgeThreshold = 35; // If cache miss reaches of a include reaches this, remove the cache

type
  TCodetoolsIncludes = class(TObject)
  protected
    FLock: TEnterableLock;
    FParsers: TCodeParserList;

    procedure Purge;
  public
    constructor Create;
    destructor Destroy; override;

    function GetInclude(Defines: TSaveDefinesRec; IncludedFiles: TStringList; FileName: String): TCodeParser; overload;
    function GetInclude(Lexer: TPasLexer; IncludedFiles: TStringList): TCodeParser; overload;

    function GetPlugin(FileName: String): TCodeParser; overload;
    function GetPlugin(Lexer: TPasLexer): TCodeParser; overload;

    procedure Release(List: TCodeParserList);
  end;

  TCodetoolsInclude = class(TCodeParser)
  protected
    FIncludedFiles: TStringList;
    FPlugins: TStringList;
    FInDefines: TSaveDefinesRec;
    FRefCount: Integer;
    FLastUsed: Integer;

    procedure OnIncludeDirect(Sender: TPasLexer); override;
    procedure OnLibraryDirect(Sender: TPasLexer); override;

    function GetPlugins: TStringArray;
    function GetHash: String; override; // Add InDefines to hash
  public
    constructor Create(AFileName: String; InDefines: TSaveDefinesRec; IncludedFiles: TStringList); reintroduce;
    destructor Destroy; override;

    property Plugins: TStringArray read GetPlugins;

    function IsOutdated: Boolean;
    function IncRef: TCodetoolsInclude;
    function DecRef: TCodetoolsInclude;
  end;

  TCodetoolsPlugin = class(TCodetoolsInclude)
  public
    constructor Create(AFileName: String); reintroduce;
  end;

var
  CodetoolsIncludes: TCodetoolsIncludes;

implementation

uses
  simba.env, simba.fs, simba.plugin_dump;

procedure TCodetoolsInclude.OnIncludeDirect(Sender: TPasLexer);
var
  FilePath: String;
begin
  inherited OnIncludeDirect(Sender);

  FilePath := SimbaEnv.FindInclude(Sender.DirectiveParamAsFileName, [TSimbaPath.PathExtractDir(Sender.FileName)]);
  if (FilePath = '') then
    Exit;
  if (FIncludedFiles <> nil) and (FIncludedFiles.IndexOf(FilePath) > -1) then
    Exit;

  PushLexer(TPasLexer.CreateFromFile(FilePath));

  if (FIncludedFiles <> nil) then
    FIncludedFiles.Add(FilePath);
end;

procedure TCodetoolsInclude.OnLibraryDirect(Sender: TPasLexer);
var
  FilePath: String;
begin
  inherited OnLibraryDirect(Sender);

  FilePath := SimbaEnv.FindPlugin(Sender.DirectiveParamAsFileName, [TSimbaPath.PathExtractDir(Sender.FileName)]);
  if (FilePath = '') then
    Exit;

  FPlugins.Add(FilePath);
end;

function TCodetoolsInclude.GetPlugins: TStringArray;
begin
  Result := FPlugins.ToStringArray();
end;

function TCodetoolsInclude.GetHash: String;
begin
  if FHash.IsNull then
    FHash := inherited + FInDefines.Defines + IntToStr(FInDefines.Stack) + FPlugins.Text;

  Result := FHash;
end;

constructor TCodetoolsInclude.Create(AFileName: String; InDefines: TSaveDefinesRec; IncludedFiles: TStringList);
begin
  inherited Create();

  FSourceType := EParserSourceType.INCLUDE;

  FPlugins := TStringList.Create();
  FIncludedFiles := IncludedFiles;
  FInDefines := InDefines;

  SetFile(AFileName);
  Lexer.LoadDefines(FInDefines);

  FLastUsed := 0;
  FRefCount := 1;
end;

destructor TCodetoolsInclude.Destroy;
begin
  FreeAndNil(FPlugins);

  inherited Destroy();
end;

function TCodetoolsInclude.IsOutdated: Boolean;
var
  i: Integer;
begin
  for i := 0 to fLexers.Count - 1 do
    if (fLexers[i].FileName <> '') and (FileAge(fLexers[i].FileName) <> fLexers[i].FileAge) then
    begin
      Result := True;
      Exit;
    end;

  Result := False;
end;

function TCodetoolsInclude.IncRef: TCodetoolsInclude;
begin
  Inc(FRefCount);
  Result := Self;

  {$IFDEF PARSER_CACHE_DEBUG}
  DebugLn('IncRef: %s -> %d', [Lexer.FileName, FRefCount]);
  {$ENDIF}
end;

function TCodetoolsInclude.DecRef: TCodetoolsInclude;
begin
  Dec(FRefCount);
  Result := Self;

  {$IFDEF PARSER_CACHE_DEBUG}
  DebugLn('DecRef: %s -> %d', [Lexer.FileName, FRefCount]);
  {$ENDIF}
end;

constructor TCodetoolsPlugin.Create(AFileName: String);
begin
  inherited Create(AFileName, Default(TSaveDefinesRec), nil);

  FSourceType := EParserSourceType.PLUGIN;

  SetScript(DumpPluginInAnotherProcess(AFileName), AFileName);
end;

function TCodetoolsIncludes.GetPlugin(FileName: String): TCodeParser;
var
  I: Integer;
begin
  Result := nil;

  FLock.Enter();
  try
    for I := 0 to FParsers.Count - 1 do
      if (FParsers[I].Lexer.FileName = FileName) then
        with TCodetoolsInclude(FParsers[I]) do
        begin
          if IsOutdated() then
          begin
            {$IFDEF PARSER_CACHE_DEBUG}
            DebugLn('[Codetools]: Cache hit "%s" but is outdated %d', [Lexer.FileName, FRefCount]);
            {$ENDIF}

            FLastUsed := 1000;
            Continue;
          end;

          if (Result = nil) then // Already found, but we're checking above checks
            Result := IncRef();
        end;

    Purge();

    if (Result = nil) then
    begin
      DebugLn('[Codetools] Caching %s', [FileName]);

      Result := TCodetoolsPlugin.Create(FileName);
      Result.Run();

      FParsers.Add(Result);
    end;
  finally
    FLock.Leave();
  end;
end;

function TCodetoolsIncludes.GetPlugin(Lexer: TPasLexer): TCodeParser;
var
  FileName: String;
begin
  FileName := SimbaEnv.FindPlugin(Lexer.DirectiveParamAsFileName, [TSimbaPath.PathExtractDir(Lexer.FileName)]);

  if (FileName <> '') then
    Result := GetPlugin(FileName)
  else
    Result := nil;
end;

procedure TCodetoolsIncludes.Purge;
var
  I: Integer;
begin
  for I := FParsers.Count - 1 downto 0 do
    with TCodetoolsInclude(FParsers[I]) do
    begin
      if (FRefCount > 0) or (FLastUsed < PurgeThreshold) then
        Continue;

      DebugLn('Purge include: %s [%d]', [Lexer.FileName, FLastUsed]);

      FParsers.Delete(I);
    end;
end;

constructor TCodetoolsIncludes.Create;
begin
  inherited Create();

  FParsers := TCodeParserList.Create(True);
end;

destructor TCodetoolsIncludes.Destroy;
begin
  if (FParsers <> nil) then
    FreeAndNil(FParsers);

  inherited Destroy();
end;

function TCodetoolsIncludes.GetInclude(Defines: TSaveDefinesRec; IncludedFiles: TStringList; FileName: String): TCodeParser;
var
  I: Integer;
begin
  Result := nil;

  FLock.Enter();
  try
    for I := 0 to FParsers.Count - 1 do
      if (FParsers[I].Lexer.FileName = FileName) then
        with TCodetoolsInclude(FParsers[I]) do
        begin
          if (FInDefines.Stack <> Defines.Stack) or (FInDefines.Defines <> Defines.Defines) then
          begin
            {$IFDEF PARSER_CACHE_DEBUG}
            DebugLn('[Codetools]: Cache hit "%s" but not used (defines mismatch) %d, %d', [Lexer.FileName, FRefCount, FLastUsed + 1]);
            {$ENDIF}

            FLastUsed := FLastUsed + 1;
            Continue;
          end;

          if IsOutdated() then
          begin
            {$IFDEF PARSER_CACHE_DEBUG}
            DebugLn('[Codetools]: Cache hit "%s" but is outdated %d', [Lexer.FileName, FRefCount]);
            {$ENDIF}

            FLastUsed := 1000;
            Continue;
          end;

        if (Result = nil) then // Already found, but we're checking above checks
          Result := IncRef();
      end;

    Purge();

    if (Result = nil) then
    begin
      DebugLn('[Codetools]: Caching %s', [FileName]);

      Result := TCodetoolsInclude.Create(FileName, Defines, IncludedFiles);
      Result.Run();

      FParsers.Add(Result);
    end;
  finally
    FLock.Leave();
  end;
end;

function TCodetoolsIncludes.GetInclude(Lexer: TPasLexer; IncludedFiles: TStringList): TCodeParser;
var
  FileName: String;
begin
  Result := nil;

  FileName := SimbaEnv.FindInclude(Lexer.DirectiveParamAsFileName, [TSimbaPath.PathExtractDir(Lexer.FileName)]);
  if (FileName = '') or (IncludedFiles.IndexOf(FileName) > -1) then
    Exit;

  Result := GetInclude(Lexer.SaveDefines(), IncludedFiles, FileName);
  if Assigned(Result) then
    IncludedFiles.Add(FileName);
end;

procedure TCodetoolsIncludes.Release(List: TCodeParserList);
var
  I: Integer;
begin
  FLock.Enter();

  try
    for I := 0 to List.Count - 1 do
      if (List[I] is TCodetoolsInclude) then
        TCodetoolsInclude(List[I]).DecRef();
  finally
    FLock.Leave();
  end;
end;

initialization
  CodetoolsIncludes := TCodetoolsIncludes.Create();

finalization
  if Assigned(CodetoolsIncludes) then
    FreeAndNil(CodetoolsIncludes);

end.

