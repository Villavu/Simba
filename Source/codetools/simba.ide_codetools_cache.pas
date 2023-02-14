{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Caches an entire "top level" include, its not per file.
  So cache for `$i SomeInclude`" will include all the files `SomeInclude` also includes.
}
unit simba.ide_codetools_cache;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  mPasLexTypes, mPasLex,
  simba.mufasatypes, simba.ide_codetools_parser, simba.ide_codetools_utils;

function GetCachedInclude(Sender: TmwBasePasLex): TCodeParser;
procedure ReleaseCachedIncludes(Includes: TCodeParserList);

const
  PurgeThreshold = 20; // If cache miss reaches of a include reaches this, remove the cache

implementation

uses
  Generics.Collections,
  simba.simplelock;

type
  TCachedInclude = class(TCodeParser)
  protected
    FInDefines: TSaveDefinesRec;

    // Add InDefines to hash
    function GetHash: String; override;

    function DoHandleLibrary(Sender: TmwBasePasLex): Boolean;
    function DoFindInclude(Sender: TmwBasePasLex; var FileName: String): Boolean;
  public
    RefCount: Integer;
    LastUsed: Integer;

    constructor Create(Sender: TmwBasePasLex; FileName: String); reintroduce;

    function IsOutdated: Boolean;
    function IncRef: TCachedInclude;
    function DecRef: TCachedInclude;

    property InDefines: TSaveDefinesRec read FInDefines;
  end;

  TIncludeCache = class(TObject)
  protected
  type
    TList = specialize TObjectList<TCachedInclude>;
  protected
    FLock: TSimpleEnterableLock;
    FIncludes: TList;

    procedure Purge;
  public
    constructor Create;
    destructor Destroy; override;

    function Get(Sender: TmwBasePasLex): TCodeParser;
    procedure Release(Includes: TCodeParserList);
  end;

var
  IncludeCache: TIncludeCache;

procedure TIncludeCache.Purge;
var
  I: Integer;
begin
  for I := FIncludes.Count - 1 downto 0 do
  begin
    if (FIncludes[I].RefCount > 0) or (FIncludes[I].LastUsed < PurgeThreshold) then
      Continue;

    DebugLn('Purge include: %s [%d]', [FIncludes[I].Lexer.FileName, FIncludes[I].LastUsed]);

    FIncludes.Delete(I);
  end;
end;

constructor TIncludeCache.Create;
begin
  inherited Create();

  FIncludes := TList.Create();
end;

destructor TIncludeCache.Destroy;
begin
  if (FIncludes <> nil) then
    FreeAndNil(FIncludes);

  inherited Destroy();
end;

function TIncludeCache.Get(Sender: TmwBasePasLex): TCodeParser;
var
  Include: TCachedInclude;
  InDefines: TSaveDefinesRec;
  FileName: String;
begin
  Result := nil;

  FileName := FindInclude(Sender);
  if (FileName <> '') then
  begin
    InDefines := Sender.SaveDefines();

    FLock.Enter();
    try
      for Include in FIncludes do
        if (Include.Lexer.FileName = FileName) then
        begin
          if (Include.InDefines.Stack <> InDefines.Stack) or (Include.InDefines.Defines <> InDefines.Defines) then
          begin
            {$IFDEF PARSER_CACHE_DEBUG}
            DebugLn('Cache hit "%s" but not used %d, %d', [Include.Lexer.FileName, Include.RefCount, Include.LastUsed + 1]);
            {$ENDIF}

            Include.LastUsed := Include.LastUsed + 1;
            Continue;
          end;

          if Include.IsOutdated() then
          begin
            {$IFDEF PARSER_CACHE_DEBUG}
            DebugLn('Cache hit "%s" but is outdated %d', [Include.Lexer.FileName, Include.RefCount]);
            {$ENDIF}

            Include.LastUsed := 1000;
            Continue;
          end;

          if (Result = nil) then // Already found, but we're checking above checks
            Result := Include.IncRef();
        end;

      Purge();

      if (Result = nil) then
      begin
        DebugLn('Caching %s', [FileName]);

        Include := TCachedInclude.Create(Sender, FileName);
        Include.Run();

        Result := FIncludes[FIncludes.Add(Include)];
      end;
    finally
      FLock.Leave();
    end;
  end;
end;

procedure TIncludeCache.Release(Includes: TCodeParserList);
var
  Include: TCodeParser;
begin
  FLock.Enter();

  try
    for Include in Includes do
      if (Include is TCachedInclude) then
        TCachedInclude(Include).DecRef();
  finally
    FLock.Leave();
  end;
end;

function TCachedInclude.GetHash: String;
begin
  if (FHash = '') then
    FHash := inherited + InDefines.Defines + IntToStr(InDefines.Stack);

  Result := FHash;
end;

function TCachedInclude.DoHandleLibrary(Sender: TmwBasePasLex): Boolean;
var
  FileName: String;
begin
  Result := True;

  FileName := FindInclude(Sender);
  if (FileName <> '') then
  begin
    PushLexer(TmwPasLex.Create(FindPluginExports(FileName), FileName));

    Lexer.IsLibrary := True
  end;
end;

function TCachedInclude.DoFindInclude(Sender: TmwBasePasLex; var FileName: String): Boolean;
begin
  FileName := FindInclude(Sender);

  Result := FileName <> '';
end;

function TCachedInclude.IsOutdated: Boolean;
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

function TCachedInclude.IncRef: TCachedInclude;
begin
  Inc(RefCount);
  Result := Self;

  {$IFDEF PARSER_CACHE_DEBUG}
  DebugLn('IncRef: %s -> %d', [Lexer.FileName, RefCount]);
  {$ENDIF}
end;

function TCachedInclude.DecRef: TCachedInclude;
begin
  Dec(RefCount);
  Result := Self;

  {$IFDEF PARSER_CACHE_DEBUG}
  DebugLn('DecRef: %s -> %d', [Lexer.FileName, RefCount]);
  {$ENDIF}
end;

constructor TCachedInclude.Create(Sender: TmwBasePasLex; FileName: String);
begin
  inherited Create();

  LastUsed := 0;
  RefCount := 1;
  FInDefines := Sender.SaveDefines();

  OnFindInclude := @DoFindInclude;
  OnHandleLibrary := @DoHandleLibrary;

  if (Sender.TokenID = tokLibraryDirect) then
    SetScript(FindPluginExports(FileName), FileName)
  else
    SetFile(FileName);

  Lexer.LoadDefines(FInDefines);
  Lexer.IsLibrary := (Sender.TokenID = tokLibraryDirect);
end;

function GetCachedInclude(Sender: TmwBasePasLex): TCodeParser;
begin
  Result := IncludeCache.Get(Sender);
end;

procedure ReleaseCachedIncludes(Includes: TCodeParserList);
begin
  IncludeCache.Release(Includes);
end;

initialization
  IncludeCache := TIncludeCache.Create();

finalization
  if (IncludeCache <> nil) then
    FreeAndNil(IncludeCache);

end.

