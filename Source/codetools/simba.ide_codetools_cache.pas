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
  simba.mufasatypes, simba.ide_codetools_parser;

function GetCachedInclude(Sender: TmwBasePasLex): TCodeParser;
procedure ReleaseCachedIncludes(Includes: TCodeParserList);

const
  PurgeThreshold = 20; // If cache miss reaches of a include reaches this, remove the cache

implementation

uses
  Generics.Collections,
  simba.simplelock, simba.files;

type
  TCachedInclude = class(TCodeParser)
  protected
    FInDefines: TSaveDefinesRec;
    // FHash: String;

    function DoFindInclude(Sender: TmwBasePasLex; var FileName: String): Boolean;
  public
    RefCount: Integer;
    LastUsed: Integer;

    constructor Create(FileName: String; InDefines: TSaveDefinesRec); reintroduce;
    procedure Run; override;

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

    DebugLn('Purge include: ' + FIncludes[I].Lexer.FileName);

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

  FileName := Sender.DirectiveParamAsFileName;
  if (not FindFile(FileName, '', [ExtractFileDir(Sender.FileName), GetIncludePath(), GetSimbaPath()])) then
    Exit;

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

    if (Result = nil) then
    begin
      Include := TCachedInclude.Create(FileName, InDefines);
      Include.Run();

      Result := FIncludes[FIncludes.Add(Include)];
    end;

    Purge();
  finally
    FLock.Leave();
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

function TCachedInclude.DoFindInclude(Sender: TmwBasePasLex; var FileName: string): Boolean;
begin
  Result := FindFile(FileName, '', [ExtractFileDir(Sender.FileName), GetIncludePath(), GetSimbaPath()]);
end;

procedure TCachedInclude.Run;
//var
//  Builder: TStringBuilder;
//  I: Integer;
begin
  inherited Run();

  //Builder := TStringBuilder.Create(512);
  //Builder.Append(FInDefines.Defines + IntToStr(FInDefines.Stack));
  //with Lexer.SaveDefines() do
  //  Builder.Append(Defines + IntToStr(Stack));
  //for I := 0 to fLexers.Count - 1 do
  //  Builder.Append(fLexers[i].FileName + IntToStr(fLexers[i].FileAge));
  //
  //FHash := Builder.ToString();
  //
  //Builder.Free();
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

constructor TCachedInclude.Create(FileName: String; InDefines: TSaveDefinesRec);
begin
  inherited Create();

  LastUsed := 0;
  RefCount := 1;
  FInDefines := InDefines;

  OnFindInclude := @DoFindInclude;

  SetFile(FileName);
  Lexer.LoadDefines(FInDefines);
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
  IncludeCache.Free();

end.

