{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)
}
unit simba.ide_codetools_keywords;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.mufasatypes, simba.ide_codetools_parser, simba.ide_initialization;

function GetKeywords: TDeclarationArray;

implementation

uses
  lpparser,
  simba.list;

type
  TKeywordList = specialize TSimbaObjectList<TDeclaration>;

var
  KeywordsList: TKeywordList;

procedure CreateKeywords;
var
  Keyword: TLapeKeyword;
begin
  KeywordsList := TKeywordList.Create(True);
  for Keyword in Lape_Keywords do
    KeywordsList.Add(TDeclaration_Keyword.Create(Keyword.Keyword.ToLower()));
end;

function GetKeywords: TDeclarationArray;
begin
  Result := KeywordsList.ToArray();
end;

initialization
  SimbaIDEInitialization_AddBeforeCreate(@CreateKeywords, 'Codetools Create Keywords');

finalization
  if (KeywordsList <> nil) then
    FreeAndNil(KeywordsList);

end.
