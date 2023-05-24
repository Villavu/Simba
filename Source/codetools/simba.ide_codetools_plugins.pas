{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Cache plugin export dumps for codetools.
}
unit simba.ide_codetools_plugins;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.simplelock, simba.ide_codetools_parser;

const
  PurgeThreshold = 35; // If cache miss reaches of a include reaches this, remove the cache

type
  TCodetoolsPlugins = class(TObject)
  protected
    FLock: TSimpleEnterableLock;
    FParsers: TCodeParserList;

    procedure Purge;
  public
    constructor Create;
    destructor Destroy; override;

    function Get(FileName: String): TCodeParser;
    procedure Release(List: TCodeParserList);
  end;

var
  CodetoolsPlugins: TCodetoolsPlugins;

implementation

uses
  simba.ide_initialization, simba.ide_codetools_utils;

type
  TCachedPlugin = class(TCodeParser)
  public
    RefCount: Integer;
    LastUsed: Integer;

    constructor Create(FileName: String); reintroduce;

    function IsOutdated: Boolean;
    function IncRef: TCachedPlugin;
    function DecRef: TCachedPlugin;
  end;

constructor TCachedPlugin.Create(FileName: String);
begin
  inherited Create();

  LastUsed := 0;
  RefCount := 1;

  SetScript(FindPluginExports(FileName), FileName);
end;

function TCachedPlugin.IsOutdated: Boolean;
begin
  Result := False;
end;

function TCachedPlugin.IncRef: TCachedPlugin;
begin
  Inc(RefCount);
  Result := Self;

  {$IFDEF PARSER_CACHE_DEBUG}
  DebugLn('IncRef: %s -> %d', [Lexer.FileName, RefCount]);
  {$ENDIF}
end;

function TCachedPlugin.DecRef: TCachedPlugin;
begin
  Dec(RefCount);
  Result := Self;

  {$IFDEF PARSER_CACHE_DEBUG}
  DebugLn('DecRef: %s -> %d', [Lexer.FileName, RefCount]);
  {$ENDIF}
end;

procedure TCodetoolsPlugins.Purge;
var
  I: Integer;
begin
  for I := FParsers.Count - 1 downto 0 do
    with TCachedPlugin(FParsers[I]) do
    begin
      if (RefCount > 0) or (LastUsed < PurgeThreshold) then
        Continue;

      DebugLn('Purge plugin: %s [%d]', [Lexer.FileName, LastUsed]);

      FParsers.Delete(I);
    end;
end;

constructor TCodetoolsPlugins.Create;
begin
  inherited Create();

  FParsers := TCodeParserList.Create(True);
end;

destructor TCodetoolsPlugins.Destroy;
begin
  FreeAndNil(FParsers);

  inherited Destroy();
end;

function TCodetoolsPlugins.Get(FileName: String): TCodeParser;
var
  I: Integer;
begin
  Result := nil;

  if FileExists(FileName) then
  begin
    FLock.Enter();

    try
      for I := 0 to FParsers.Count - 1 do
        if (FParsers[I].Lexer.FileName = FileName) then
          with TCachedPlugin(FParsers[I]) do
          begin
            if IsOutdated() then
            begin
              {$IFDEF PARSER_CACHE_DEBUG}
              DebugLn('Cache hit "%s" but is outdated %d', [Lexer.FileName, RefCount]);
              {$ENDIF}

              LastUsed := 1000;
              Continue;
            end;

            if (Result = nil) then // Already found, but we're checking above checks
              Result := IncRef();
          end;

      Purge();

      if (Result = nil) then
      begin
        DebugLn('Caching %s', [FileName]);

        Result := TCachedPlugin.Create(FileName);
        Result.Run();

        FParsers.Add(Result);
      end;
    finally
      FLock.Leave();
    end;
  end;
end;

procedure TCodetoolsPlugins.Release(List: TCodeParserList);
var
  I: Integer;
begin
  FLock.Enter();

  try
    for I := 0 to List.Count - 1 do
      if (List[I] is TCachedPlugin) then
        TCachedPlugin(List[I]).DecRef();
  finally
    FLock.Leave();
  end;
end;

procedure CreateCodetoolsPlugins;
begin
  CodetoolsPlugins := TCodetoolsPlugins.Create();
end;

initialization
  SimbaIDEInitialization.RegisterMethodOnBeforeCreate(@CreateCodetoolsPlugins, 'Create CodetoolsPlugins');

finalization
  if Assigned(CodetoolsPlugins) then
    FreeAndNil(CodetoolsPlugins);

end.

