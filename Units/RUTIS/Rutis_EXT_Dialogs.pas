Unit Rutis_EXT_Dialogs;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils,
  Rutis_Defs,
  {$IfDef WinCE}WinCE_FileDialog,{$EndIf WinCE}
  Dialogs;

Procedure RegisterEXTMethods(MethodList : PExtMethodList);

Implementation

Procedure _OpenFileDialog(Stack : PRutisStack; Param0 : Integer; Result : PRutisStackItem);
Var
  {$IfDef WinCE}
  dlg : TFFileDialog;
  {$Else WinCE}
  dlg  : TOpenDialog;
  {$EndIf WinCE}
Begin
  MakeVType(@Stack^[Param0], VString);
  MakeVType(@Stack^[Param0 + 1], VString);

  Result^.VTyp := VString;
  New(Result^.VString);

  {$IfDef WinCE}
  dlg := TFFileDialog.Create(nil);
  try
    dlg.FileName := Stack^[Param0].VString^;
    dlg.ACaption := 'Open File';
    dlg.Filter   := Stack^[Param0+1].VString^;
    If dlg.Filter = '' then
      dlg.Filter := 'All Files (*.*)|*.*';
    If dlg.Execute then
      Result^.VString^ := dlg.FileName
    else
      Result^.VString^ := '';
  finally
    dlg.Free;
  end;

  {$Else WinCE}

  dlg := TOpenDialog.Create(nil);
  Try
    dlg.FileName := Stack^[Param0].VString^;
    dlg.Filter := Stack^[Param0 + 1].VString^;
    If dlg.Filter = '' Then
      dlg.Filter       := 'All Files (*.*)|*.*';
    If dlg.Execute Then
      Result^.VString^ := dlg.FileName
    Else
      Result^.VString^ := '';
  Finally
    dlg.Free;
  End;

  {$EndIf WinCE}
End;

Procedure _SaveFileDialog(Stack : PRutisStack; Param0 : Integer; Result : PRutisStackItem);
Var
  {$IfDef WinCE}
  dlg: TFFileDialog;
  {$Else WinCE}
  dlg  : TSaveDialog;
  {$EndIf WinCE}
Begin
  MakeVType(@Stack^[Param0], VString);
  MakeVType(@Stack^[Param0 + 1], VString);

  Result^.VTyp := VString;
  New(Result^.VString);
  {$IfDef WinCE}
  dlg := TFFileDialog.Create(nil);
  try
    dlg.FileName := Stack^[Param0].VString^;
    dlg.ACaption := 'Open File';
    dlg.Filter   := Stack^[Param0+1].VString^;
    If dlg.Filter = '' then
      dlg.Filter := 'All Files (*.*)|*.*';
    If dlg.Execute then
      Result^.VString^ := dlg.FileName
    else
      Result^.VString^ := '';
  finally
    dlg.Free;
  end;

  {$Else WinCE}

  dlg := TOpenDialog.Create(nil);
  Try
    dlg.FileName := Stack^[Param0].VString^;
    dlg.Filter := Stack^[Param0 + 1].VString^;
    If dlg.Filter = '' Then
      dlg.Filter       := 'All Files (*.*)|*.*';
    If dlg.Execute Then
      Result^.VString^ := dlg.FileName
    Else
      Result^.VString^ := '';
  Finally
    dlg.Free;
  End;

  {$EndIf WinCE}
End;

//==============================================================================
//==============================================================================

Procedure RegisterEXTMethods(MethodList : PExtMethodList);
Begin
  RegExtMethod('OpenFileDialog',{$IfDef FPC}@{$EndIf}_OpenFileDialog,
    [VString, VString], VString, MethodList);
  RegExtMethod('SaveFileDialog',{$IfDef FPC}@{$EndIf}_SaveFileDialog,
    [VString, VString], VString, MethodList);
End;

End.

