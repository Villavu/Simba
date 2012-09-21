unit code;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  SynEdit, SynHighlighterPas, SynGutterBase, SynGutterMarks,
  SynGutterLineNumber, SynGutterChanges, SynGutter, SynGutterCodeFolding,
  sclist, ClipBrd, Menus;

type

  { TCodeGen }

  TCodeGen = class(TForm)
    MenuItem1: TMenuItem;
    PopupMenu1: TPopupMenu;
    memo1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
  private
    CmpList: TSimbaComponentList;
    labels,edits,images,buttons,checkboxes,listboxes,comboboxes,RadBtns: TSimbaComponentList;
    FormCode,LabelsCode,EditsCode,ImagesCode,ButtonsCode,CheckBoxesCode,ListBoxesCode,
    ComboBoxesCode, RadBtnsCode,HeaderCode,ScriptCode: TStringList;
    procedure GenerateScriptHeader;
    { private declarations }
  public
    { public declarations }
    procedure CreateScript(list: TSimbaComponentList);
    Procedure GetComponentCode(smbl: TSimbaComponentList);
    Procedure SmbToCodeList(smb: TSimbaComponent;list: TStringList);
    Procedure GenerateFormCode(smbl: TSimbaComponentList);
    Procedure CreateFormCode(smb: TSimbaComponent;List: TStringList);
    function GetSimbaCType(smb: TSimbaComponent):integer;
  end; 

var
  CodeGen: TCodeGen;
  Stream: TStringStream;
   img: integer;

implementation

{$R *.lfm}
function GenSpaces(c: integer): string;
var
  i: integer;
  s: string;
begin
 s:=#32;
 for i := 0 to c -1 do
  begin
     s:=s+#32;
    end;
  result:=s;
end;


{ TCodeGen }

procedure TCodeGen.FormClose(Sender: TObject; var CloseAction: TCloseAction);
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
end;

procedure TCodeGen.FormCreate(Sender: TObject);
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
  img:=0;
end;

procedure TCodeGen.MenuItem1Click(Sender: TObject);
begin
  ClipBoard.AsText:=memo1.Text;
end;

procedure TCodeGen.CreateScript(list: TSimbaComponentList);
var
  i: integer;
begin
  for i:=0 to list.Count -1 do begin
    CmpList.AddItem(list.GetComponent(i),i);
  end;
  GenerateFormCode(CmpList);
  Stream.Position := 0;
  memo1.Lines.LoadFromStream(Stream);
end;

procedure TCodeGen.GenerateScriptHeader;
var
 i: integer;
 s: string;
 b: integer;
begin
  b:=0;
HeaderCode.Add('var');
HeaderCode.Add(GenSpaces(2)+CmpList.GetComponent(0).compname+':TForm;');
if Labels.count> 0 then
  begin
  s:=GenSpaces(2);
  for i:=0 to labels.count -1 do
  begin
   if i<labels.Count-1 then
   s:=s+Labels.GetComponent(i).compname+','
    else
   s:=s+Labels.GetComponent(i).compname;
  end;
  i:=0;
  HeaderCode.Add(s+': TLabel;');
  end;
  if Edits.count> 0 then
  begin
  s:=GenSpaces(2);
  for i:=0 to Edits.count -1 do
  begin
   if i<Edits.Count-1 then
   s:=s+Edits.GetComponent(i).compname+','
    else
   s:=s+Edits.GetComponent(i).compname;
  end;
  i:=0;
  HeaderCode.Add(s+': TEdit;');
end;
  if Images.count> 0 then
  begin
  s:=GenSpaces(2);
  for i:=0 to Images.count -1 do
  begin
   if Images.GetComponent(i).img.switcher = true then inc(b);
   if i<Images.Count-1 then
   s:=s+Images.GetComponent(i).compname+','
    else
   s:=s+Images.GetComponent(i).compname
  end;
  i:=0;
  HeaderCode.Add(s+': TImage;');
end;
if Buttons.count> 0 then
  begin
  s:=GenSpaces(2);
  for i:=0 to Buttons.count -1 do
  begin
   if i<Buttons.Count-1 then
   s:=s+Buttons.GetComponent(i).compname+','
    else
   s:=s+Buttons.GetComponent(i).compname;
  end;
  i:=0;
  HeaderCode.Add(s+': TButton;');
end;
if CheckBoxes.count> 0 then
  begin
  s:=GenSpaces(2);
  for i:=0 to CheckBoxes.count -1 do
  begin
   if i<CheckBoxes.Count-1 then
   s:=s+CheckBoxes.GetComponent(i).compname+','
    else
   s:=s+CheckBoxes.GetComponent(i).compname;
  end;
  i:=0;
  HeaderCode.Add(s+': TCheckBox;');
end;
if ListBoxes.count> 0 then
  begin
  s:=GenSpaces(2);
  for i:=0 to ListBoxes.count -1 do
  begin
   if i<ListBoxes.Count-1 then
   s:=s+ListBoxes.GetComponent(i).compname+','
    else
   s:=s+ListBoxes.GetComponent(i).compname;
  end;
  i:=0;
  HeaderCode.Add(s+': TListBox;');
end;
if ComboBoxes.count> 0 then
  begin
  s:=GenSpaces(2);
  for i:=0 to ComboBoxes.count -1 do
  begin
   if i<ComboBoxes.Count-1 then
   s:=s+ComboBoxes.GetComponent(i).compname+','
    else
   s:=s+ComboBoxes.GetComponent(i).compname;
  end;
  i:=0;
  HeaderCode.Add(s+': TComboBox;');
end;
if RadBtns.count> 0 then
  begin
  s:=GenSpaces(2);
  for i:=0 to RadBtns.count -1 do
  begin
   if i<RadBtns.Count-1 then
   s:=s+RadBtns.GetComponent(i).compname+','
    else
   s:=s+RadBtns.GetComponent(i).compname;
  end;
  i:=0;
  HeaderCode.Add(s+': TRadioButton;');
end;
if img > 0 then
begin
 s:=GenSpaces(2);
  for i:=0 to img -1 do
  begin
   if i<img-1 then
   s:=s+'bmps'+inttostr(i)+','
    else
   s:=s+'bmps'+inttostr(i);
  end;
  i:=0;
  HeaderCode.Add(s+': TMufasaBitmap;');
end;
if img > 0 then
begin
 s:=GenSpaces(2);
  for i:=0 to img -1 do
  begin
   if i<img-1 then
   s:=s+'bmp'+inttostr(i)+','
    else
   s:=s+'bmp'+inttostr(i);
  end;
  i:=0;
  HeaderCode.Add(s+': TBitmap;');
end;
HeaderCode.Add('const');
HeaderCode.Add(GenSpaces(2)+'default = '+#39+'Comic Sans MS'+#39+';');
HeaderCode.Add('');
HeaderCode.Add('');
HeaderCode.Add('');
HeaderCode.Add('procedure YourClickProcedure(Sender: TObject);');
HeaderCode.Add('begin');
HeaderCode.Add(GenSpaces(2)+'ShowMessage('+#39+'click'+#39+');');
HeaderCode.Add('end;');
end;

function TCodeGen.GetSimbaCType(smb: TSimbaComponent):integer;
begin
  Result := -1;
   if CompareText(smb.classname, 'TDsgnForm') = 0 then
    result:=0;
   if CompareText(smb.classname, 'TLabel') = 0 then
    result:=1;
   if CompareText(smb.classname, 'TEdit') = 0 then
    result:=2;
   if CompareText(smb.classname, 'TImage') = 0 then
    result:=3;
   if CompareText(smb.classname, 'TButton') = 0 then
    result:=4;
   if CompareText(smb.classname, 'TCheckBox') = 0 then
    result:=5;
   if CompareText(smb.classname, 'TListBox') = 0 then
    result:=6;
   if CompareText(smb.classname, 'TComboBox') = 0 then
    result:=7;
   if CompareText(smb.classname, 'TRadioButton') = 0 then
    result:=8;
end;
procedure TCodeGen.GetComponentCode(smbl: TSimbaComponentList);
var
  i,j: integer;
  smb: TSimbaComponent;
begin
  for i:=0 to smbl.count - 1 do
   begin
     smb:=smbl.GetComponent(i);
     j:=GetSimbaCType(smb);
     case j of
     0: begin CreateFormCode(smb,FormCode); end;
     1: begin SmbToCodeList(smb,LabelsCode);Labels.AddItem(smb,i); end;
     2: begin SmbToCodeList(smb,EditsCode);Edits.AddItem(smb,i);end;
     3: begin SmbToCodeList(smb,ImagesCode);Images.AddItem(smb,i);end;
     4: begin SmbToCodeList(smb,ButtonsCode);Buttons.AddItem(smb,i);end;
     5: begin SmbToCodeList(smb,CheckBoxesCode);CheckBoxes.AddItem(smb,i);end;
     6: begin SmbToCodeList(smb,ListBoxesCode);ListBoxes.AddItem(smb,i);end;
     7: begin SmbToCodeList(smb,ComboBoxesCode);ComboBoxes.AddItem(smb,i);end;
     8: begin SmbToCodeList(smb,RadBtnsCode);RadBtns.AddItem(smb,i);end;
     end;
   end;
  GenerateScriptHeader;
end;

procedure TCodeGen.SmbToCodeList(smb: TSimbaComponent; list: TStringList);
var
  i,p,u: integer;
  s,s1: string;//strings for bitmap;
begin
  i:=GetSimbaCtype(smb);
  Case i of
  1: begin
       list.Add('//'+smb.compname+'\\');
       list.Add(GenSpaces(1)+ smb.compname+':='+smb.classname+'.Create('+cmpList.GetComponent(0).compname+');');
       list.Add(GenSpaces(2)+'with'+GenSpaces(1)+smb.compname+GenSpaces(1)+'do');
       list.Add(GenSpaces(4)+'begin');
       list.Add(GenSpaces(6)+'Parent:='+cmpList.GetComponent(0).compname+';');
       list.Add(GenSpaces(6)+'Caption:='+#39+smb.caption+#39+';');
       list.Add(GenSpaces(6)+'Left:='+IntToStr(smb.left)+';');
       list.Add(GenSpaces(6)+'Top:='+IntToStr(smb.top)+';');
       list.Add(GenSpaces(6)+'Width:='+IntToStr(smb.width)+';');
       list.Add(GenSpaces(6)+'Height:='+IntToStr(smb.heigth)+';');
       list.Add(GenSpaces(6)+'Font.Name:='+smb.fontname+';');
       list.Add(GenSpaces(6)+'Font.Color:='+ColorToString(smb.fontcolor)+';');
       list.Add(GenSpaces(6)+'Font.Size:='+IntToStr(smb.fontsize)+';');
       list.Add(GenSpaces(2)+'end;');
  end;
  2: begin
       list.Add('//'+smb.compname+'\\');
       list.Add(GenSpaces(1)+ smb.compname+':='+smb.classname+'.Create('+cmpList.GetComponent(0).compname+');');
       list.Add(GenSpaces(2)+'with'+GenSpaces(1)+smb.compname+GenSpaces(1)+'do');
       list.Add(GenSpaces(4)+'begin');
       list.Add(GenSpaces(6)+'Parent:='+cmpList.GetComponent(0).compname+';');
       list.Add(GenSpaces(6)+'Text:='+#39+'input your text here!'+#39+';');
       list.Add(GenSpaces(6)+'Left:='+IntToStr(smb.left)+';');
       list.Add(GenSpaces(6)+'Top:='+IntToStr(smb.top)+';');
       list.Add(GenSpaces(6)+'Width:='+IntToStr(smb.width)+';');
       list.Add(GenSpaces(6)+'Height:='+IntToStr(smb.heigth)+';');
       list.Add(GenSpaces(6)+'Font.Name:='+smb.fontname+';');
       list.Add(GenSpaces(6)+'Font.Color:='+ColorToString(smb.fontcolor)+';');
       list.Add(GenSpaces(6)+'Font.Size:='+IntToStr(smb.fontsize)+';');
       list.Add('//'+smb.compname+'.MaxLength:='+'x'+';');
       list.Add('//'+smb.compname+'.PasswordChar:='+'*'+';');
       list.Add(GenSpaces(2)+'end;');
  end;
  3: begin
       s:=''; s1:='';
       list.Add('//'+smb.compname+'\\');
       list.Add(GenSpaces(1)+ smb.compname+':='+smb.classname+'.Create('+cmpList.GetComponent(0).compname+');');
       list.Add(GenSpaces(2)+'with'+GenSpaces(1)+smb.compname+GenSpaces(1)+'do');
       list.Add(GenSpaces(4)+'begin');
       list.Add(GenSpaces(6)+'Parent:='+cmpList.GetComponent(0).compname+';');
      // list.Add('Image'+IntToStr(ImageNo)+'.Text:='+''+'input your text here!'+''+';');
       list.Add(GenSpaces(6)+'Left:='+IntToStr(smb.left)+';');
       list.Add(GenSpaces(6)+'Top:='+IntToStr(smb.top)+';');
       list.Add(GenSpaces(6)+'Width:='+IntToStr(smb.width)+';');
       list.Add(GenSpaces(6)+'Height:='+IntToStr(smb.heigth)+';');
       if smb.img.switcher = true then
         begin
         s:=smb.img.imgcode;
         s1:=#13#10+GenSpaces(6)+#39;
          for p:=0 to Length(s)-1 do begin
            SetLength(s1,Length(s1)+1);
            s1[Length(s1)]:=s[p+1];
            if p<>0 then begin
             if ((P/40)=trunc(P/40)) then
              begin
              SetLength(s1,Length(s1)+1);
              s1[Length(s1)]:=#39;
              SetLength(s1,Length(s1)+1);
              s1[Length(s1)]:='+';
              SetLength(s1,Length(s1)+1);
              s1[Length(s1)]:=#13;
              SetLength(s1,Length(s1)+1);
              s1[Length(s1)]:=#10;
              for u:=0 to 6 do
                 begin
                 SetLength(s1,Length(s1)+1);
                 s1[Length(s1)]:=#32;
                 end;
              SetLength(s1,Length(s1)+1);
              s1[Length(s1)]:=#39;
              end;
             if P = Length(s) then
              begin
              SetLength(s1,Length(s1)+1);
              s1[Length(s1)]:=#39;
              end;
            end;
          end;
          List.Add(GenSpaces(6)+'bmp'+IntToStr(img)+':=TBitmap.Create;');
          List.Add(GenSpaces(6)+'bmps'+IntToStr(img)+':=GetMufasaBitmap(BitmapFromString('+IntToStr(smb.width)+','+IntToStr(smb.Heigth)+','+s1+#39+'));');
          List.Add(GenSpaces(6)+'bmp'+IntToStr(img)+':=bmps'+IntToStr(img)+'.ToTBitmap;');
          List.Add(GenSpaces(6)+'Picture.Bitmap.handle:=bmp'+IntToStr(img)+'.handle;');
         // List.Add(GenSpaces(6)+'DrawBitmap(bmps'+IntToStr(img)+',Canvas,'+IntToStr(smb.width)+','+IntToStr(smb.Heigth)+');');
         //list.Add(GenSpaces(6)+'Picture.Bitmap.LoadFromFile('+#39+smb.img.path+#39+');');
         end else
       list.Add(GenSpaces(6)+'//'+'load bitmap to image here');
       list.Add(GenSpaces(2)+'end;');
       img:=img+1;
  end;
  4: begin
       list.Add('//'+smb.compname+'\\');
       list.Add(GenSpaces(1)+ smb.compname+':='+smb.classname+'.Create('+cmpList.GetComponent(0).compname+');');
       list.Add(GenSpaces(2)+'with'+GenSpaces(1)+smb.compname+GenSpaces(1)+'do');
       list.Add(GenSpaces(4)+'begin');
       list.Add(GenSpaces(6)+'Parent:='+cmpList.GetComponent(0).compname+';');
       list.Add(GenSpaces(6)+'Caption:='+#39+smb.caption+#39+';');
       list.Add(GenSpaces(6)+'Left:='+IntToStr(smb.left)+';');
       list.Add(GenSpaces(6)+'Top:='+IntToStr(smb.top)+';');
       list.Add(GenSpaces(6)+'Width:='+IntToStr(smb.width)+';');
       list.Add(GenSpaces(6)+'Height:='+IntToStr(smb.heigth)+';');
       list.Add(GenSpaces(6)+'OnClick:=@YourClickProcedure'+';');
       list.Add(GenSpaces(6)+'Font.Name:='+smb.fontname+';');
       list.Add(GenSpaces(6)+'Font.Color:='+ColorToString(smb.fontcolor)+';');
       list.Add(GenSpaces(6)+'Font.Size:='+IntToStr(smb.fontsize)+';');
       list.Add(GenSpaces(2)+'end;');

  end;
    5: begin
       list.Add('//'+smb.compname+'\\');
       list.Add(GenSpaces(1)+ smb.compname+':='+smb.classname+'.Create('+cmpList.GetComponent(0).compname+');');
       list.Add(GenSpaces(2)+'with'+GenSpaces(1)+smb.compname+GenSpaces(1)+'do');
       list.Add(GenSpaces(4)+'begin');
       list.Add(GenSpaces(6)+'Parent:='+cmpList.GetComponent(0).compname+';');
       list.Add(GenSpaces(6)+'Caption:='+#39+smb.caption+#39+';');
       list.Add(GenSpaces(6)+'Checked:=false'+';');
       list.Add(GenSpaces(6)+'Left:='+IntToStr(smb.left)+';');
       list.Add(GenSpaces(6)+'Top:='+IntToStr(smb.top)+';');
       list.Add(GenSpaces(6)+'Width:='+IntToStr(smb.width)+';');
       list.Add(GenSpaces(6)+'Height:='+IntToStr(smb.heigth)+';');
       list.Add(GenSpaces(6)+'Font.Name:='+smb.fontname+';');
       list.Add(GenSpaces(6)+'Font.Color:='+ColorToString(smb.fontcolor)+';');
       list.Add(GenSpaces(6)+'Font.Size:='+IntToStr(smb.fontsize)+';');
       list.Add(GenSpaces(2)+'end;');
  end;
    6: begin
       list.Add('//'+smb.compname+'\\');
       list.Add(GenSpaces(1)+ smb.compname+':='+smb.classname+'.Create('+cmpList.GetComponent(0).compname+');');
       list.Add(GenSpaces(2)+'with'+GenSpaces(1)+smb.compname+GenSpaces(1)+'do');
       list.Add(GenSpaces(4)+'begin');
       list.Add(GenSpaces(6)+'Parent:='+cmpList.GetComponent(0).compname+';');
       list.Add(GenSpaces(6)+'Left:='+IntToStr(smb.left)+';');
       list.Add(GenSpaces(6)+'Top:='+IntToStr(smb.top)+';');
       list.Add(GenSpaces(6)+'Width:='+IntToStr(smb.width)+';');
       list.Add(GenSpaces(6)+'Height:='+IntToStr(smb.heigth)+';');
       list.Add(GenSpaces(6)+'//add your items here');
       list.Add(GenSpaces(6)+'Items.Add('+#39+'YourItem'+#39+')'+';');
       list.Add(GenSpaces(6)+'//End items');
       list.Add(GenSpaces(6)+'OnClick:=@YourClickProcedure'+';');
       list.Add(GenSpaces(6)+'Font.Name:='+smb.fontname+';');
       list.Add(GenSpaces(6)+'Font.Color:='+ColorToString(smb.fontcolor)+';');
       list.Add(GenSpaces(6)+'Font.Size:='+IntToStr(smb.fontsize)+';');
       list.Add(GenSpaces(2)+'end;');
  end;
    7: begin
       list.Add('//'+smb.compname+'\\');
       list.Add(GenSpaces(1)+ smb.compname+':='+smb.classname+'.Create('+cmpList.GetComponent(0).compname+');');
       list.Add(GenSpaces(2)+'with'+GenSpaces(1)+smb.compname+GenSpaces(1)+'do');
       list.Add(GenSpaces(4)+'begin');
       list.Add(GenSpaces(6)+'Parent:='+cmpList.GetComponent(0).compname+';');
       list.Add(GenSpaces(6)+'Left:='+IntToStr(smb.left)+';');
       list.Add(GenSpaces(6)+'Top:='+IntToStr(smb.top)+';');
       list.Add(GenSpaces(6)+'Width:='+IntToStr(smb.width)+';');
       list.Add(GenSpaces(6)+'Height:='+IntToStr(smb.heigth)+';');
       list.Add(GenSpaces(6)+'//add your items here');
       list.Add(GenSpaces(6)+'Items.Add('+#39+'YourItem'+#39+')'+';');
       list.Add(GenSpaces(6)+'//End items');
       list.Add(GenSpaces(6)+'OnClick:=@YourClickProcedure'+';');
       list.Add(GenSpaces(6)+'Font.Name:='+smb.fontname+';');
       list.Add(GenSpaces(6)+'Font.Color:='+ColorToString(smb.fontcolor)+';');
       list.Add(GenSpaces(6)+'Font.Size:='+IntToStr(smb.fontsize)+';');
       list.Add(GenSpaces(2)+'end;');
  end;
    8: begin
       list.Add('//'+smb.compname+'\\');
       list.Add(GenSpaces(1)+ smb.compname+':='+smb.classname+'.Create('+cmpList.GetComponent(0).compname+');');
       list.Add(GenSpaces(2)+'with'+GenSpaces(1)+smb.compname+GenSpaces(1)+'do');
       list.Add(GenSpaces(4)+'begin');
       list.Add(GenSpaces(6)+'Parent:='+cmpList.GetComponent(0).compname+';');
       list.Add(GenSpaces(6)+'Caption:='+#39+smb.caption+#39+';');
       list.Add(GenSpaces(6)+'Left:='+IntToStr(smb.left)+';');
       list.Add(GenSpaces(6)+'Top:='+IntToStr(smb.top)+';');
       list.Add(GenSpaces(6)+'Width:='+IntToStr(smb.width)+';');
       list.Add(GenSpaces(6)+'Height:='+IntToStr(smb.heigth)+';');
       list.Add(GenSpaces(6)+'Font.Name:='+smb.fontname+';');
       list.Add(GenSpaces(6)+'Font.Color:='+ColorToString(smb.fontcolor)+';');
       list.Add(GenSpaces(6)+'Font.Size:='+IntToStr(smb.fontsize)+';');
       list.Add(GenSpaces(2)+'end;');
  end;
end;

end;

procedure TCodeGen.GenerateFormCode(smbl: TSimbaComponentList);
begin
 GetComponentCode(smbl);
 ScriptCode.AddStrings(HeaderCode);
 ScriptCode.Add('');
 ScriptCode.Add('');
 ScriptCode.Add('procedure InitForm;');
 SCriptCode.Add('begin');
 ScriptCode.AddStrings(FormCode);
 if LabelsCode.Count> 0 then
    ScriptCode.AddStrings(LabelsCode);
 if EditsCode.Count> 0 then
    ScriptCode.AddStrings(EditsCode);
 if ImagesCode.Count> 0 then
    ScriptCode.AddStrings(ImagesCode);
 if ButtonsCode.Count> 0 then
    ScriptCode.AddStrings(ButtonsCode);
 if CheckBoxesCode.Count> 0 then
    ScriptCode.AddStrings(CheckBoxesCode);
 if ListBoxesCode.Count> 0 then
    ScriptCode.AddStrings(ListBoxesCode);
 if ComboBoxesCode.Count> 0 then
    ScriptCode.AddStrings(ComboBoxesCode);
 if RadBtnsCode.Count> 0 then
    ScriptCode.AddStrings(RadBtnsCode);
 ScriptCode.Add('end;');
 ScriptCode.Add('');
 ScriptCode.Add('procedure SafeInitForm;');
 ScriptCode.Add('var');
 ScriptCode.Add(GenSpaces(2)+'v: TVariantArray;');
 ScriptCode.Add('begin');
 ScriptCode.Add(GenSpaces(2)+'setarraylength(V, 0);');
 ScriptCode.Add(GenSpaces(2)+'ThreadSafeCall('+#39+'InitForm'+#39+', v); ');
 ScriptCode.Add('end;');
 ScriptCode.Add('');
 ScriptCode.Add('');
 ScriptCode.Add('procedure ShowFormModal;');
 ScriptCode.Add('begin');
 ScriptCode.Add(GenSpaces(2)+CmpList.GetComponent(0).compname + '.ShowModal;');
 ScriptCode.Add('end;');
 ScriptCode.Add('');
 ScriptCode.Add('');
 ScriptCode.Add('procedure SafeShowFormModal;');
 ScriptCode.Add('var');
 ScriptCode.Add(GenSpaces(2)+'v: TVariantArray;');
 ScriptCode.Add('begin');
 ScriptCode.Add(GenSpaces(2)+'SetArrayLength(V, 0);');
 ScriptCode.Add(GenSpaces(2)+'ThreadSafeCall('+#39+'ShowFormModal'+#39+', v);');
 ScriptCode.Add('end;');
 ScriptCode.Add('');
 ScriptCode.Add('');
 ScriptCode.Add('begin');
 ScriptCode.Add(GenSpaces(2)+'SafeInitForm; ');
 ScriptCode.Add(GenSpaces(2)+'SafeShowFormModal;');
 ScriptCode.Add('end.');
 ScriptCode.SaveToStream(Stream);

end;

procedure TCodeGen.CreateFormCode(smb: TSimbaComponent;list: TStringList);
begin
  list.Add('//'+smb.compname+'\\');
  list.Add(GenSpaces(1)+smb.compname+':=TForm.Create(nil);');
  list.Add(GenSpaces(2)+'with'+GenSpaces(1)+smb.compname+GenSpaces(1)+'do');
  list.Add(GenSpaces(4)+'begin');
  list.Add(GenSpaces(6)+'Caption:='+#39+cmpList.GetComponent(0).caption+#39+';');
  list.Add(GenSpaces(6)+'Left:='+IntToStr(smb.left)+';');
  list.Add(GenSpaces(6)+'Top:='+IntToStr(smb.top)+';');
  list.Add(GenSpaces(6)+'Width:='+IntToStr(smb.width)+';');
  list.Add(GenSpaces(6)+'Height:='+IntToStr(smb.heigth)+';');
  list.Add(GenSpaces(6)+'Font.Name:='+smb.fontname+';');
  list.Add(GenSpaces(6)+'Font.Color:='+ColorToString(smb.fontcolor)+';');
  list.Add(GenSpaces(6)+'Font.Size:='+IntToStr(smb.fontsize)+';');
  list.Add(GenSpaces(2)+'end;');
end;






end.

