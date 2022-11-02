{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ci_includecache;

{$i simba.inc}

interface

uses
  Classes, SysUtils, syncobjs, castaliapaslex,
  generics.collections,
  simba.mufasatypes, simba.codeparser;

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
    function Equals(Obj: TObject): Boolean; override;

    property Outdated: Boolean read GetOutdated;
    property InDefines: TSaveDefinesRec read FInDefines write FInDefines;
    property OutDefines: TSaveDefinesRec read FOutDefines write FOutDefines;

    property Hash: String read GetHash;

    constructor Create;
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
  List: TStringList;
begin
  if (FHash = '') then
  begin
    List := TStringList.Create();
    List.LineBreak := '';
    List.Add(Lexer.UseCodeToolsIDEDirective.ToString());
    List.Add(InDefines.Defines  + InDefines.Stack.ToString());
    List.Add(OutDefines.Defines + OutDefines.Stack.ToString());

    List.AddStrings(FFiles);
    for I := 0 to FFiles.Count - 1 do
      List.Add(IntToStr(PtrUInt(FFiles.Objects[I])));

    FHash := List.Text;

    List.Free();
  end;

  Result := FHash;
end;

function TCodeInsight_Include.GetOutdated: Boolean;
var
  i: Int32;
begin
  Result := False;

  for i := 0 to FFiles.Count - 1 do
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
         (Include.InDefines.Stack <> Sender.Lexer.SaveDefines.Stack) or
         (Include.Lexer.UseCodeToolsIDEDirective <> Sender.Lexer.UseCodeToolsIDEDirective) then
      begin
        Include.LastUsed := Include.LastUsed + 1; // When this reaches 10 the include will be destroyed.

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
      Result.Assign(Sender);
      Result.Run(FileName);
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
        Result.Assign(Sender);
        Result.Run(Contents, FileName);
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

