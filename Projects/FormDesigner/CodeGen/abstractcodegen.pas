unit AbstractCodeGen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,SCList;
type
  { TAbstractCodeGen }

  TAbstractCodeGen = class
    protected
     CmpList: TSimbaComponentList;
     labels,edits,images,buttons,checkboxes,listboxes,comboboxes,RadBtns: TSimbaComponentList;
     FormCode,LabelsCode,EditsCode,ImagesCode,ButtonsCode,CheckBoxesCode,ListBoxesCode,
     ComboBoxesCode, RadBtnsCode,HeaderCode,ScriptCode,ResultScript: TStringList;
     Stream: TStringStream;
     Img: integer;
    public
     constructor Create;
     Destructor Destroy;override;
     procedure CreateScript(list: TSimbaComponentList);virtual;abstract;
     Procedure GetComponentCode(smbl: TSimbaComponentList);virtual;abstract;
     Procedure SmbToCodeList(smb: TSimbaComponent;list: TStringList);virtual;abstract;
     Procedure GenerateFormCode(smbl: TSimbaComponentList);virtual;abstract;
     Procedure GenerateProgressCode(smbl: TSimbaComponentList);virtual;abstract;
     Procedure CreateFormCode(smb: TSimbaComponent;List: TStringList);virtual;abstract;
     function GetSimbaCType(smb: TSimbaComponent):integer;virtual;abstract;
     procedure GenerateScriptHeader;virtual;abstract;
     procedure GenerateProgressHeader;virtual;abstract;
     function GetScript(const List: TSimbaComponentList): TStrings;virtual;abstract;
  end;

function GenSpaces(c: integer): string;
implementation

function GenSpaces(c: integer): string;
var
  i: integer;
  s: string;
begin
 s:='';
 for i := 0 to c -1 do
  begin
     s:=s+#32;
    end;
  result:=s;
end;
{ TAbstractCodeGen }

constructor TAbstractCodeGen.Create;
begin
  Stream:=TStringStream.Create('');
  Labels:=TSimbaComponentList.Create;
  Edits:=TSimbaComponentList.Create;
  Images:=TSimbaComponentList.Create;
  Buttons:=TSimbaComponentList.Create;
  CheckBoxes:=TSimbaComponentList.Create;
  ListBoxes:=TSimbaComponentList.Create;
  ComboBoxes:=TSimbaComponentList.Create;
  RadBtns:=TSimbaComponentList.Create;
  CmpList:=TSimbaComponentList.Create;
  HeaderCode:=TStringList.Create;
  ScriptCode:=TStringList.Create;
  LabelsCode:=TStringList.Create;
  ButtonsCode:=TStringList.Create;
  EditsCode:=TStringList.Create;
  ImagesCode:=TStringList.Create;
  CheckBoxesCode:=TStringList.Create;
  ListBoxesCode:=TStringList.Create;
  ComboBoxesCode:=TStringList.Create;
  RadBtnsCode:=TStringList.Create;
  FormCode:=TStringList.Create;
  ResultScript:=TStringList.Create;
  img:=0;
end;

destructor TAbstractCodeGen.Destroy;
begin
  Labels.Free;
  Edits.Free;
  Images.Free;
  Buttons.Free;
  CheckBoxes.Free;
  ListBoxes.Free;
  ComboBoxes.Free;
  RadBtns.Free;
  CmpList.Free;
  HeaderCode.Free;
  ScriptCode.Free;
  LabelsCode.Free;
  EditsCode.Free;
  ImagesCode.Free;
  CheckBoxesCode.Free;
  ListBoxesCode.Free;
  ComboBoxesCode.Free;
  RadBtnsCode.Free;
  FormCode.Free;
  Stream.Free;
  ResultScript.Free;
  inherited Destroy;
end;

end.

