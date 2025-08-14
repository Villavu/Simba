{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.editor_highlighter;

{$i simba.inc}

interface

uses
  Classes,
  SysUtils,
  SynEditHighlighter,
  SynHighlighterPas,
  simba.base,
  simba.settings;

type
  TSimbaEditorHighlighter = class(TSynFreePascalSyn)
  private
    FOverrides: TStringList;

    procedure Reset;
    procedure addTokenOverride(Text: String; Kind: TtkTokenKind);

    procedure DoSettingChanged(Setting: TSimbaSetting);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AttributeNames: TStringArray;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
  end;

implementation

uses
  simba.vartype_string;

procedure TSimbaEditorHighlighter.Reset;
begin
  FOverrides.Clear();
  addTokenOverride('new', tkKey);
end;

procedure TSimbaEditorHighlighter.addTokenOverride(Text: String; Kind: TtkTokenKind);
begin
  FOverrides.AddObject(Text, TObject(PtrUInt(Kind)));
end;

procedure TSimbaEditorHighlighter.DoSettingChanged(Setting: TSimbaSetting);
var
  Arg, Args, Key, Value: String;
  I: Integer;
begin
  Reset();

  Args := Setting.Value;
  for Arg in Args.Split(',') do
  begin
    Key := Arg.Before('=');
    Value := Arg.After('=');
    for I := 0 to AttrCount - 1 do
      if (Attribute[I].Name = Value) then
      begin
        if (Attribute[I] = CommentAttri) then
          addTokenOverride(Key, tkComment)
        else if (Attribute[I] = IdentifierAttri) then
          addTokenOverride(Key, tkIdentifier)
        else if (Attribute[I] = KeyAttri) then
          addTokenOverride(Key, tkKey)
        else if (Attribute[I] = ModifierAttri) then
          addTokenOverride(Key, tkModifier)
        else if (Attribute[I] = NumberAttri) then
          addTokenOverride(Key, tkNumber)
        else if (Attribute[I] = StringAttri) then
          addTokenOverride(Key, tkString)
        else if (Attribute[I] = SymbolAttri) then
          addTokenOverride(Key, tkSymbol)
        else if (Attribute[I] = DirectiveAttri) then
          addTokenOverride(Key, tkDirective);
      end;
  end;
end;

constructor TSimbaEditorHighlighter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOverrides := TStringList.Create();
  FOverrides.UseLocale := False;
  FOverrides.CaseSensitive := False;
  FOverrides.Sorted := True;

  Reset();

  SimbaSettings.RegisterChangeHandler(Self, SimbaSettings.Editor.CustomTokenAttris, @DoSettingChanged, True);
end;

destructor TSimbaEditorHighlighter.Destroy;
begin
  FreeAndNil(FOverrides);

  inherited Destroy();
end;

function TSimbaEditorHighlighter.AttributeNames: TStringArray;
var
  I, C: Integer;
begin
  C := 0;
  SetLength(Result, AttrCount);
  for I := 0 to AttrCount - 1 do
  begin
    // dont add things we have no use for
    if (Attribute[I].StoredName = 'PasDoc-Keyword') or
       (Attribute[I].StoredName = 'PasDoc-Symbol')  or
       (Attribute[I].StoredName = 'PasDoc-Unknown') or
       (Attribute[I].StoredName = 'Assembler') then
      Continue;

    Result[C] := Attribute[I].Name;
    Inc(C);
  end;
  SetLength(Result, C);
end;

function TSimbaEditorHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
var
  Index: Integer;
begin
  Index := FOverrides.IndexOf(GetToken());
  if (Index > -1) then
  begin
    case TtkTokenKind(PtrUInt(FOverrides.Objects[Index])) of
      tkComment:    Result := CommentAttri;
      tkIdentifier: Result := IdentifierAttri;
      tkKey:        Result := KeyAttri;
      tkModifier:   Result := ModifierAttri;
      tkNumber:     Result := NumberAttri;
      tkSpace:      Result := SpaceAttri;
      tkString:     Result := StringAttri;
      tkSymbol:     Result := SymbolAttri;
      tkDirective:  Result := DirectiveAttri;
    end;
  end else
    Result := inherited;
end;

end.

