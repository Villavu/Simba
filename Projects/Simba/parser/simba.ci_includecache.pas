unit simba.ci_includecache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs, castaliapaslex,
  simba.generics, simba.codeparser;

type
  TCodeInsight_Include = class(TCodeParser)
  protected
    FInDefines: TSaveDefinesRec;
    FOutDefines: TSaveDefinesRec;

    function GetOutdated: Boolean;
  public
    RefCount: Int32;
    LastUsed: Int32;

    function Equals(Obj: TObject): Boolean; override;

    property Outdated: Boolean read GetOutdated;
    property InDefines: TSaveDefinesRec read FInDefines write FInDefines;
    property OutDefines: TSaveDefinesRec read FOutDefines write FOutDefines;

    constructor Create;
  end;

  TCodeInsight_IncludeArray = array of TCodeInsight_Include;
  TCodeInsight_IncludeMap = specialize TSimbaStringMap<TCodeInsight_Include>;

  TCodeInsight_IncludeCache = class
  protected
    FLock: TCriticalSection;
    FCachedIncludes: TCodeInsight_IncludeMap;

    procedure Purge;

    function Find(Sender: TCodeParser; FileName: String): TCodeInsight_Include;
  public
    function GetInclude(Sender: TCodeParser; FileName: String): TCodeInsight_Include;
    function GetLibrary(Sender: TCodeParser; FileName: String): TCodeInsight_Include;

    procedure Release(Include: TCodeInsight_Include);

    constructor Create;
    destructor Destroy; override;
  end;

operator + (Left: TCodeInsight_IncludeArray; Right: TCodeInsight_Include): TCodeInsight_IncludeArray;
operator + (Left: TCodeInsight_IncludeArray; Right: TCodeInsight_IncludeArray): TCodeInsight_IncludeArray;

implementation

uses
  simba.settings;

operator + (Left: TCodeInsight_IncludeArray; Right: TCodeInsight_Include): TCodeInsight_IncludeArray;
begin
  SetLength(Result, Length(Left) + 1);
  if Length(Left) > 0 then
    Move(Left[0], Result[0], Length(Left) * SizeOf(TCodeInsight_Include));

  Result[High(Result)] := Right;
end;

operator + (Left: TCodeInsight_IncludeArray; Right: TCodeInsight_IncludeArray): TCodeInsight_IncludeArray;
begin
  SetLength(Result, Length(Left) + Length(Right));

  if Length(Result) > 0 then
  begin
    if Length(Left) > 0 then
      Move(Left[0], Result[0], Length(Left) * SizeOf(TCodeInsight_IncludeArray));
    if Length(Right) > 0 then
      Move(Right[0], Result[Length(Left)], Length(Right) * SizeOf(TCodeInsight_IncludeArray));
  end;
end;

function TCodeInsight_Include.GetOutdated: Boolean;
var
  i: Int32;
begin
  Result := False;

  for i := 0 to FFiles.Count-1 do
    if FileAge(FFiles[i]) <> PtrInt(FFiles.Objects[i]) then
    begin
      Result := True;
      Exit;
    end;
end;

function TCodeInsight_Include.Equals(Obj: TObject): Boolean;
var
  I: Int32;
begin
  Result := True;

  if TCodeInsight_Include(Obj).Lexer.UseCodeToolsIDEDirective <> Lexer.UseCodeToolsIDEDirective then
    Exit(False);

  if (TCodeInsight_Include(Obj).InDefines.Defines <> InDefines.Defines) or
     (TCodeInsight_Include(Obj).InDefines.Stack <> InDefines.Stack) then
    Exit(False);

  if (TCodeInsight_Include(Obj).OutDefines.Defines <> OutDefines.Defines) or
     (TCodeInsight_Include(Obj).OutDefines.Defines <> OutDefines.Defines) then
    Exit(False);

  if TCodeInsight_Include(Obj).Files.Count <> FFiles.Count then
    Exit(False);

  for I := 0 to FFiles.Count - 1 do
    if (FFiles[I] <> TCodeInsight_Include(Obj).Files[I]) or
       (FFiles.Objects[I] <> TCodeInsight_Include(Obj).Files.Objects[I]) then
      Exit(False);
end;

constructor TCodeInsight_Include.Create;
begin
  inherited Create();

  FLexer.UseCodeToolsIDEDirective := not SimbaSettings.Editor.IgnoreCodeToolsIDEDirective.Value;
end;

procedure TCodeInsight_IncludeCache.Purge;
var
  i: Int32;
  Include: TCodeInsight_Include;
begin
  for i := FCachedIncludes.Count - 1 downto 0 do
  begin
    Include := FCachedIncludes.ItemsI[i];
    if Include.RefCount > 0 then
      Continue;
    if (Include.LastUsed < 500) and (not Include.Outdated) then
      Continue;

    WriteLn('Purge include "', Include.Lexer.FileName, '"');

    FCachedIncludes.Delete(i).Free();
  end;
end;

function TCodeInsight_IncludeCache.Find(Sender: TCodeParser; FileName: String): TCodeInsight_Include;
var
  Include: TCodeInsight_Include;
begin
  Result := nil;

  for Include in FCachedIncludes.ItemsOfKey(FileName) do
  begin
    if (Include.InDefines.Defines <> Sender.Lexer.SaveDefines.Defines) or
       (Include.InDefines.Stack <> Sender.Lexer.SaveDefines.Stack) or
       (Include.Lexer.UseCodeToolsIDEDirective <> Sender.Lexer.UseCodeToolsIDEDirective) then
    begin
      Include.LastUsed := Include.LastUsed + 3; // When this reaches 500 the include will be destroyed.

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
      WriteLn('Caching Include "' + FileName + '"');

      Result := TCodeInsight_Include.Create();
      Result.Assign(Sender);
      Result.OnInclude := nil; // No include cache
      Result.OnLibrary := nil; // No library cache
      Result.Run(FileName);
      Result.OutDefines := Result.Lexer.SaveDefines;
      Result.InDefines := Sender.Lexer.SaveDefines;

      FCachedIncludes.Add(FileName, Result);
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
      WriteLn('Caching Library "' + FileName + '"');

      if (Sender.OnLoadLibrary <> nil) then
      begin
        Sender.OnLoadLibrary(Self, FileName, Contents);

        Result := TCodeInsight_Include.Create();
        Result.Assign(Sender);
        Result.OnInclude := nil; // No include cache
        Result.OnLibrary := nil; // No library cache
        Result.Run(Contents, FileName);
        Result.OutDefines := Result.Lexer.SaveDefines;
        Result.InDefines := Sender.Lexer.SaveDefines;
        Result.Lexer.IsLibrary := True;

        FCachedIncludes.Add(FileName, Result);
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
  FCachedIncludes := TCodeInsight_IncludeMap.Create();
end;

destructor TCodeInsight_IncludeCache.Destroy;
var
  I: Int32;
begin
  for I := 0 to FCachedIncludes.Count - 1 do
    FCachedIncludes.ItemsI[I].Free();

  FLock.Free();
  FCachedIncludes.Free();

  inherited Destroy();
end;

end.

