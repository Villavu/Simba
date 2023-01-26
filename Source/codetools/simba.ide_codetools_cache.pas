{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Caches an entire include, its not per file.
  So cache for `$i SomeInclude`" will include all the files `SomeInclude` also includes.
}
unit simba.ide_codetools_cache;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Generics.Collections, syncobjs,
  mPasLexTypes, mPasLex,
  simba.mufasatypes, simba.ide_codetools_parser;

type
  TCodeInsight_Include = class(TCodeParser)
  protected
    FInDefines: TSaveDefinesRec;
    FOutDefines: TSaveDefinesRec;
    FHash: String;

    function GetHash: String;
    function GetOutdated: Boolean;
  public
    RefCount: Int32;
    LastUsed: Int32;

    procedure Assign(From: TObject); override;

    property Outdated: Boolean read GetOutdated;
    property InDefines: TSaveDefinesRec read FInDefines write FInDefines;
    property OutDefines: TSaveDefinesRec read FOutDefines write FOutDefines;

    property Hash: String read GetHash;
  end;

  TCodeInsight_IncludeArray = array of TCodeInsight_Include;
  TCodeInsight_IncludeList = specialize TObjectList<TCodeInsight_Include>;

  TCodeInsight_IncludeCache = class
  protected
    FLock: TCriticalSection;
    FCachedIncludes: TCodeInsight_IncludeList;

    procedure Purge;

    function Find(Sender: TCodeParser; FileName: String): TCodeInsight_Include;
  public
    function GetInclude(Sender: TCodeParser; FileName: String): TCodeInsight_Include;
    function GetLibrary(Sender: TCodeParser; FileName: String): TCodeInsight_Include;

    procedure Release(Include: TCodeInsight_Include);

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  simba.settings;

procedure TCodeInsight_Include.Assign(From: TObject);
begin
  inherited Assign(From);

  FOnInclude := nil; // No include cache
  FOnLibrary := nil; // No library cache
end;

function TCodeInsight_Include.GetHash: String;
var
  I: Int32;
  Builder: TStringBuilder;
begin
  if (FHash = '') then
  begin
    Builder := TStringBuilder.Create(512);
    Builder.Append(InDefines.Defines + InDefines.Stack.ToString());
    Builder.Append(OutDefines.Defines + OutDefines.Stack.ToString());
    for I := 0 to fLexers.Count - 1 do
      Builder.Append(fLexers[i].FileName + IntToStr(fLexers[i].FileAge));

    FHash := Builder.ToString();

    Builder.Free();
  end;

  Result := FHash;
end;

function TCodeInsight_Include.GetOutdated: Boolean;
var
  i: Int32;
begin
  for i := 0 to fLexers.Count - 1 do
    if (fLexers[i].FileName <> '') and (FileAge(fLexers[i].FileName) <> fLexers[i].FileAge) then
      Exit(True);

  Result := False;
end;

procedure TCodeInsight_IncludeCache.Purge;
var
  i: Int32;
  Include: TCodeInsight_Include;
begin
  for i := FCachedIncludes.Count - 1 downto 0 do
  begin
    Include := FCachedIncludes.Items[i];
    if (Include.RefCount > 0) then
      Continue;
    if (Include.LastUsed < 25) and (not Include.Outdated) then
      Continue;

    DebugLn('Purge include: ' + Include.Lexer.FileName);

    FCachedIncludes.Delete(I);
  end;
end;

function TCodeInsight_IncludeCache.Find(Sender: TCodeParser; FileName: String): TCodeInsight_Include;
var
  Include: TCodeInsight_Include;
begin
  Result := nil;

  for Include in FCachedIncludes do
    if Include.Lexer.FileName = FileName then
    begin
      if (Include.InDefines.Defines <> Sender.Lexer.SaveDefines.Defines) or
         (Include.InDefines.Stack <> Sender.Lexer.SaveDefines.Stack) then
      begin
        Include.LastUsed := Include.LastUsed + 1; // When this reaches 25 the include will be destroyed.

        Continue;
      end;

      if Include.Outdated then
        Continue;

      Result := Include;
      Break;
    end;
end;

function TCodeInsight_IncludeCache.GetInclude(Sender: TCodeParser; FileName: String): TCodeInsight_Include;
begin
  Result := nil;

  FLock.Enter();

  try
    Purge();

    Result := Find(Sender, FileName);

    if (Result = nil) then
    begin
      DebugLn('Caching Include: ' + FileName);

      Result := TCodeInsight_Include.Create();
      Result.SetFile(FileName);
      Result.Assign(Sender);
      Result.Run();
      Result.OutDefines := Result.Lexer.SaveDefines;
      Result.InDefines := Sender.Lexer.SaveDefines;

      FCachedIncludes.Add(Result);
    end;

    Sender.Lexer.CloneDefinesFrom(Result.Lexer);

    Result.RefCount := Result.RefCount + 1;
    Result.LastUsed := 0;
  finally
    FLock.Leave();
  end;
end;

procedure TCodeInsight_IncludeCache.Release(Include: TCodeInsight_Include);
begin
  FLock.Enter();

  try
    Include.RefCount := Include.RefCount - 1;
  finally
    FLock.Leave();
  end;
end;

function TCodeInsight_IncludeCache.GetLibrary(Sender: TCodeParser; FileName: String): TCodeInsight_Include;
var
  Contents: String = '';
begin
  Result := nil;

  FLock.Enter();

  try
    Purge();

    Result := Find(Sender, FileName);

    if (Result = nil) then
    begin
      DebugLn('Caching Library: ' + FileName);

      if (Sender.OnLoadLibrary <> nil) then
      begin
        Sender.OnLoadLibrary(Self, FileName, Contents);

        Result := TCodeInsight_Include.Create();
        Result.SetScript(Contents, FileName);
        Result.Assign(Sender);
        Result.Run();
        Result.OutDefines := Result.Lexer.SaveDefines;
        Result.InDefines := Sender.Lexer.SaveDefines;
        Result.Lexer.IsLibrary := True;

        FCachedIncludes.Add(Result);
      end;
    end;

    Sender.Lexer.CloneDefinesFrom(Result.Lexer);

    Result.RefCount := Result.RefCount + 1;
    Result.LastUsed := 0;
  finally
    FLock.Leave();
  end;
end;

constructor TCodeInsight_IncludeCache.Create;
begin
  inherited Create();

  FLock := TCriticalSection.Create();
  FCachedIncludes := TCodeInsight_IncludeList.Create();
end;

destructor TCodeInsight_IncludeCache.Destroy;
begin
  FLock.Free();
  FCachedIncludes.Free();

  inherited Destroy();
end;

end.

