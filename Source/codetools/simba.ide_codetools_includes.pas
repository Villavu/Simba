{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Caches an entire "top level" include.
  If an include file includes another file, all the items will be stored in the cache item too.
}
unit simba.ide_codetools_includes;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  mPasLexTypes, mPasLex,
  simba.mufasatypes, simba.ide_codetools_parser, simba.ide_codetools_utils, simba.simplelock;

const
  PurgeThreshold = 35; // If cache miss reaches of a include reaches this, remove the cache

type
  TCodetoolsIncludes = class(TObject)
  protected
    FLock: TSimpleEnterableLock;
    FParsers: TCodeParserList;

    procedure Purge;
  public
    constructor Create;
    destructor Destroy; override;

    function Get(Sender: TmwBasePasLex): TCodeParser;
    procedure Release(List: TCodeParserList);
  end;

var
  CodetoolsIncludes: TCodetoolsIncludes;

implementation

uses
  simba.ide_initialization;

type
  TCachedInclude = class(TCodeParser)
  protected
    FInDefines: TSaveDefinesRec;

    // Add InDefines to hash
    function GetHash: String; override;

    function DoFindPlugin(Sender: TmwBasePasLex; var AFileName: String): Boolean;
    function DoFindInclude(Sender: TmwBasePasLex; var AFileName: String; var Handled: Boolean): Boolean;
  public
    RefCount: Integer;
    LastUsed: Integer;

    constructor Create(Sender: TmwBasePasLex; AFileName: String); reintroduce;

    function IsOutdated: Boolean;
    function IncRef: TCachedInclude;
    function DecRef: TCachedInclude;

    property InDefines: TSaveDefinesRec read FInDefines;
  end;

function TCachedInclude.GetHash: String;
begin
  if FHash.IsNull then
    FHash.Value := inherited + FInDefines.Defines + IntToStr(FInDefines.Stack);

  Result := FHash.Value;
end;

function TCachedInclude.DoFindPlugin(Sender: TmwBasePasLex; var AFileName: String): Boolean;
begin
  AFileName := FindInclude(Sender);

  Result := AFileName <> '';
end;

function TCachedInclude.DoFindInclude(Sender: TmwBasePasLex; var AFileName: String; var Handled: Boolean): Boolean;
begin
  AFileName := FindInclude(Sender);

  Result := AFileName <> '';
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

constructor TCachedInclude.Create(Sender: TmwBasePasLex; AFileName: String);
begin
  inherited Create();

  LastUsed := 0;
  RefCount := 1;
  FInDefines := Sender.SaveDefines();

  OnFindInclude := @DoFindInclude;
  OnFindPlugin := @DoFindPlugin;

  SetFile(AFileName);

  Lexer.LoadDefines(FInDefines);
end;

procedure TCodetoolsIncludes.Purge;
var
  I: Integer;
begin
  for I := FParsers.Count - 1 downto 0 do
    with TCachedInclude(FParsers[I]) do
    begin
      if (RefCount > 0) or (LastUsed < PurgeThreshold) then
        Continue;

      DebugLn('Purge include: %s [%d]', [Lexer.FileName, LastUsed]);

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

function TCodetoolsIncludes.Get(Sender: TmwBasePasLex): TCodeParser;
var
  SenderDefines: TSaveDefinesRec;
  FileName: String;
  I: Integer;
begin
  Result := nil;

  FileName := FindInclude(Sender);
  if (FileName <> '') then
  begin
    SenderDefines := Sender.SaveDefines();

    FLock.Enter();
    try
      for I := 0 to FParsers.Count - 1 do
        if (FParsers[I].Lexer.FileName = FileName) then
          with TCachedInclude(FParsers[I]) do
          begin
            if (InDefines.Stack <> SenderDefines.Stack) or (InDefines.Defines <> SenderDefines.Defines) then
            begin
              {$IFDEF PARSER_CACHE_DEBUG}
              DebugLn('Cache hit "%s" but not used (defines mismatch) %d, %d', [Lexer.FileName, RefCount, LastUsed + 1]);
              {$ENDIF}

              LastUsed := LastUsed + 1;
              Continue;
            end;

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

        Result := TCachedInclude.Create(Sender, FileName);
        Result.Run();

        FParsers.Add(Result);
      end;
    finally
      FLock.Leave();
    end;
  end;
end;

procedure TCodetoolsIncludes.Release(List: TCodeParserList);
var
  I: Integer;
begin
  FLock.Enter();

  try
    for I := 0 to List.Count - 1 do
      if (List[I] is TCachedInclude) then
        TCachedInclude(List[I]).DecRef();
  finally
    FLock.Leave();
  end;
end;

procedure CreateCodetoolsIncludes;
begin
  CodetoolsIncludes := TCodetoolsIncludes.Create();
end;

initialization
  SimbaIDEInitialization.RegisterMethodOnBeforeCreate(@CreateCodetoolsIncludes, 'Create CodetoolsIncludes');

finalization
  if Assigned(CodetoolsIncludes) then
    FreeAndNil(CodetoolsIncludes);

end.

