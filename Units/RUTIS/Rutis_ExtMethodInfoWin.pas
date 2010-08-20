Unit Rutis_ExtMethodInfoWin;

Interface

Uses
  SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus, Math
  {$IFDEF FPC},LResources{$ENDIF};

Type
  TExtMethodInfo = Record
    Name           : String;
    Category       : String;
    Description    : String;
    IsFunction     : Boolean;
    ExtMethodType  : Pointer;
  End;

  TFExtMethodListInfo = Class(TForm)
    ExtList              : TListBox;
    Panel1               : TPanel;
    BtnSortDefault       : TButton;
    BtnSortAlphabetical  : TButton;
    EdSearch             : TEdit;
    PopupMenu1           : TPopupMenu;
    Insertintocode1      : TMenuItem;
    Procedure ExtListDblClick(Sender : TObject);
    Procedure FormCreate(Sender : TObject);
    Procedure ExtListMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
    Procedure FormHide(Sender : TObject);
    Procedure FormShow(Sender : TObject);
    Procedure BtnSortDefaultClick(Sender : TObject);
    Procedure BtnSortAlphabeticalClick(Sender : TObject);
    Procedure EdSearchChange(Sender : TObject);
    Procedure EdSearchMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
    Procedure Insertintocode1Click(Sender : TObject);
  Private
    LastHoveredRow  : Integer;
    Procedure ShowList;
    { Private-Deklarationen }
  Public
    ResultIndex      : Integer;
    ResultExtMethod  : Pointer;
    //Hints       : TStringList;
    Sorted           : Array Of Integer;
    ExtMethods       : Array Of TExtMethodInfo;
    { Public-Deklarationen }
  End;

Implementation

{$IFNDEF FPC}
  {$R *.dfm}

{$ENDIF}

Function IsStr1LowerStr2(s1, s2 : String) : Boolean;
Var
  i  : Integer;
Begin
  i      := 1;
  s1     := LowerCase(s1) + #255;
  s2     := LowerCase(s2) + #254;
  While s1[i] = s2[i] Do
    Inc(i);
  Result := s1[i] < s2[i];
End;

Procedure TFExtMethodListInfo.ShowList;
Var
  i  : Integer;
  s  : String;
Begin
  ExtList.Clear;
  For i := 0 To high(ExtMethods) Do
  Begin
    If Sorted[i] < 0 Then Continue;
    s := ExtMethods[Sorted[i] - 1].Name;
    //    If ExtMethods[Sorted[i]].IsFunction then
    //      s := 'Function  ' + ExtMethods[Sorted[i]].Name
    //    else
    //      s := 'Procedure ' + ExtMethods[Sorted[i]].Name;
    //    If ExtMethods[Sorted[i]].IsFunction then
    //      s := 'F ' + ExtMethods[Sorted[i]].Name
    //    else
    //      s := 'P ' + ExtMethods[Sorted[i]].Name;
    ExtList.Items.Add(s);
  End;

  Canvas.Font         := ExtList.Font;
  {$ifndef FPC}
  ExtList.ScrollWidth := 0;
  For i := 0 To ExtList.Items.Count - 1 Do
    ExtList.ScrollWidth := Max(ExtList.ScrollWidth, Canvas.TextWidth(ExtList.Items[i]));
  {$endif}
End;

Procedure TFExtMethodListInfo.BtnSortAlphabeticalClick(Sender : TObject);
Var
  i, j       : Integer;
  completed  : Boolean;
Begin
  SetLength(Sorted, length(ExtMethods));
  For i := 0 To high(ExtMethods) Do
    Sorted[i] := i + 1;

  Repeat
    completed := True;
    For i := 0 To high(ExtMethods) - 1 Do
      If not IsStr1LowerStr2(ExtMethods[Sorted[i] - 1].Name, ExtMethods[Sorted[i + 1] - 1].Name) Then
      Begin
        j         := Sorted[i];
        Sorted[i] := Sorted[i + 1];
        Sorted[i + 1] := j;
        completed := False;
      End;
  Until completed;

  ShowList;
End;

Procedure TFExtMethodListInfo.BtnSortDefaultClick(Sender : TObject);
Var
  i  : Integer;
Begin
  SetLength(Sorted, length(ExtMethods));
  For i := 0 To high(ExtMethods) Do
    Sorted[i] := i + 1;

  ShowList;
End;

Procedure TFExtMethodListInfo.EdSearchChange(Sender : TObject);
Var
  i  : Integer;
Begin
  For i := 0 To high(ExtMethods) Do
  Begin
    Sorted[i] := abs(Sorted[i]);
    If length(EdSearch.Text) > 0 Then
      If Pos(LowerCase(EdSearch.Text), LowerCase(ExtMethods[Sorted[i] - 1].Name)) = 0 Then
        Sorted[i] := -Sorted[i];
  End;
  ShowList;
End;

Procedure TFExtMethodListInfo.EdSearchMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
Begin
//  EdSearch.SelStart := 0;
//  EdSearch.SelLength := Length(EdSearch.Text);
End;

Procedure TFExtMethodListInfo.ExtListDblClick(Sender : TObject);
Begin
  Insertintocode1Click(nil);
End;

Procedure TFExtMethodListInfo.ExtListMouseMove(Sender : TObject; Shift : TShiftState; X, Y : Integer);
Var
  HoveredRow  : Integer;
  I  : Integer;
Begin
  HoveredRow := ExtList.ItemAtPos(Point(X, Y), True);
  If (HoveredRow > -1) and
    (HoveredRow < length(ExtMethods)) and
    (HoveredRow <> LastHoveredRow) Then
  Begin
    LastHoveredRow := HoveredRow;
    i := -1;
    Repeat
      Inc(i);
      If Sorted[I] > 0 Then
        Dec(HoveredRow);
    Until HoveredRow < 0;
    ExtList.Hint     := ExtMethods[abs(Sorted[i]) - 1].Description;
    Application.Hint := ExtList.Hint;
    Application.HintPause := 0;
    Application.HintHidePause := 60000;
    {$ifndef FPC}
    Application.ActivateHint(Point(X, Y));
    {$endif}
  End;
End;

Procedure TFExtMethodListInfo.FormCreate(Sender : TObject);
Begin
  ResultIndex     := -1;
  LastHoveredRow  := -1;
  ResultExtMethod := nil;
End;

Procedure TFExtMethodListInfo.FormHide(Sender : TObject);
Begin
  Application.HintPause := 2500;
End;

Procedure TFExtMethodListInfo.FormShow(Sender : TObject);
Begin
  BtnSortDefault.Click;
End;

Procedure TFExtMethodListInfo.Insertintocode1Click(Sender : TObject);
Var i, item  : Integer;
Begin
  item := ExtList.ItemIndex;
  i    := -1;
  Repeat
    Inc(i);
    If Sorted[I] > 0 Then
      Dec(item);
  Until item < 0;

  ResultIndex     := abs(Sorted[i]) - 1;
  ResultExtMethod := ExtMethods[ResultIndex].ExtMethodType;
  Close;
End;

Initialization
  {$IFDEF FPC}
    {$I Rutis_ExtMethodInfoWin.lrs}
  {$ENDIF}
End.

