unit simba.import_lcl_misc;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script;

procedure ImportLCLMisc(Script: TSimbaScript);

implementation

uses
  Controls, EditBtn, Spin, Menus, Graphics, ListFilterEdit, StdCtrls, Buttons, ButtonPanel,
  lptypes, ffi;

type
  PBitmap = ^TBitmap;
  PNotifyEvent = ^TNotifyEvent;
  PComponent = ^TComponent;
  PCaption = ^TCaption;

  PCustomFloatSpinEdit = ^TCustomFloatSpinEdit;
  PCustomSpinEdit = ^TCustomSpinEdit;
  PFloatSpinEdit = ^TFloatSpinEdit;
  PSpinEdit = ^TSpinEdit;

  PDateEdit = ^TDateEdit;
  PTimeEdit = ^TTimeEdit;

  PMenu = ^TMenu;
  PPopupMenu = ^TPopupMenu;
  PMainMenu = ^TMainMenu;
  PMenuItem = ^TMenuItem;

  PListFilterEdit = ^TListFilterEdit;
  PListBox = ^TListBox;
  PButtonPanel = ^TButtonPanel;
  PPanelButtons = ^TPanelButtons;
  PControl = ^TControl;

procedure _LapeFloatSpinEdit_DecimalPlaces_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PFloatSpinEdit(Params^[0])^.DecimalPlaces;
end;

procedure _LapeFloatSpinEdit_DecimalPlaces_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFloatSpinEdit(Params^[0])^.DecimalPlaces := PInteger(Params^[1])^;
end;

procedure _LapeFloatSpinEdit_Increment_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PFloatSpinEdit(Params^[0])^.Increment;
end;

procedure _LapeFloatSpinEdit_Increment_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFloatSpinEdit(Params^[0])^.Increment := PDouble(Params^[1])^;
end;

procedure _LapeFloatSpinEdit_MinValue_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PFloatSpinEdit(Params^[0])^.MinValue;
end;

procedure _LapeFloatSpinEdit_MinValue_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFloatSpinEdit(Params^[0])^.MinValue := PDouble(Params^[1])^;
end;

procedure _LapeFloatSpinEdit_MaxValue_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PFloatSpinEdit(Params^[0])^.MaxValue;
end;

procedure _LapeFloatSpinEdit_MaxValue_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFloatSpinEdit(Params^[0])^.MaxValue := PDouble(Params^[1])^;
end;

procedure _LapeFloatSpinEdit_Value_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PFloatSpinEdit(Params^[0])^.Value;
end;

procedure _LapeFloatSpinEdit_Value_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PFloatSpinEdit(Params^[0])^.Value := PDouble(Params^[1])^;
end;

procedure _LapeCustomFloatSpinEdit_ValueEmpty_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PCustomFloatSpinEdit(Params^[0])^.ValueEmpty;
end;

procedure _LapeCustomFloatSpinEdit_ValueEmpty_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomFloatSpinEdit(Params^[0])^.ValueEmpty := PBoolean(Params^[1])^;
end;

procedure _LapeCustomFloatSpinEdit_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomFloatSpinEdit(Result)^ := TCustomFloatSpinEdit.Create(PComponent(Params^[0])^);
end;

procedure _LapeFloatSpinEdit_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PFloatSpinEdit(Result)^ := TFloatSpinEdit.Create(PComponent(Params^[0])^);
end;

procedure _LapeCustomSpinEdit_Value_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.Value;
end;

procedure _LapeCustomSpinEdit_Value_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomSpinEdit(Params^[0])^.Value := Pinteger(Params^[1])^;
end;

procedure _LapeCustomSpinEdit_MinValue_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.MinValue;
end;

procedure _LapeCustomSpinEdit_MinValue_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomSpinEdit(Params^[0])^.MinValue := Pinteger(Params^[1])^;
end;

procedure _LapeCustomSpinEdit_MaxValue_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.MaxValue;
end;

procedure _LapeCustomSpinEdit_MaxValue_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomSpinEdit(Params^[0])^.MaxValue := Pinteger(Params^[1])^;
end;

procedure _LapeCustomSpinEdit_Increment_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PCustomSpinEdit(Params^[0])^.Increment;
end;

procedure _LapeCustomSpinEdit_Increment_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomSpinEdit(Params^[0])^.Increment := Pinteger(Params^[1])^;
end;

procedure _LapeCustomSpinEdit_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomSpinEdit(Result)^ := TCustomSpinEdit.Create(PComponent(Params^[0])^);
end;

procedure _LapeSpinEdit_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PSpinEdit(Result)^ := TSpinEdit.Create(PComponent(Params^[0])^);
end;


//DATE
procedure _LapeDateEdit_Date_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := PDateEdit(Params^[0])^.Date;
end;

procedure _LapeDateEdit_Date_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDateEdit(Params^[0])^.Date := PDouble(Params^[1])^;
end;

procedure _LapeDateEdit_MinDate_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := PDateEdit(Params^[0])^.MinDate;
end;

procedure _LapeDateEdit_MinDate_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDateEdit(Params^[0])^.MinDate := PDouble(Params^[1])^;
end;

procedure _LapeDateEdit_MaxDate_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := PDateEdit(Params^[0])^.MaxDate;
end;

procedure _LapeDateEdit_MaxDate_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDateEdit(Params^[0])^.MaxDate := PDateTime(Params^[1])^;
end;

procedure _LapeDateEdit_DateFormat_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PDateEdit(Params^[0])^.DateFormat;
end;

procedure _LapeDateEdit_DateFormat_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDateEdit(Params^[0])^.DateFormat := PString(Params^[1])^;
end;

procedure _LapeDateEdit_DefaultToday_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PDateEdit(Params^[0])^.DefaultToday;
end;

procedure _LapeDateEdit_DefaultToday_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDateEdit(Params^[0])^.DefaultToday := PBoolean(Params^[1])^;
end;

procedure _LapeDateEdit_ReadOnly_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PDateEdit(Params^[0])^.ReadOnly;
end;

procedure _LapeDateEdit_ReadOnly_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDateEdit(Params^[0])^.ReadOnly := PBoolean(Params^[1])^;
end;

procedure _LapeDateEdit_DirectInput_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PDateEdit(Params^[0])^.DirectInput;
end;

procedure _LapeDateEdit_DirectInput_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDateEdit(Params^[0])^.DirectInput := PBoolean(Params^[1])^;
end;

procedure _LapeDateEdit_NumbersOnly_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PDateEdit(Params^[0])^.NumbersOnly;
end;

procedure _LapeDateEdit_NumbersOnly_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDateEdit(Params^[0])^.NumbersOnly := PBoolean(Params^[1])^;
end;

procedure _LapeDateEdit_Text_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PDateEdit(Params^[0])^.Text;
end;

procedure _LapeDateEdit_Text_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDateEdit(Params^[0])^.Text := PCaption(Params^[1])^;
end;

procedure _LapeDateEdit_OnChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PDateEdit(Params^[0])^.OnChange;
end;

procedure _LapeDateEdit_OnChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PDateEdit(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeDateEdit_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateEdit(Result)^ := TDateEdit.Create(PComponent(Params^[0])^);
end;


//TIME
procedure _LapeTimeEdit_Time_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDateTime(Result)^ := PTimeEdit(Params^[0])^.Time;
end;

procedure _LapeTimeEdit_Time_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTimeEdit(Params^[0])^.Time := PDateTime(Params^[1])^;
end;

procedure _LapeTimeEdit_DefaultNow_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PTimeEdit(Params^[0])^.DefaultNow;
end;

procedure _LapeTimeEdit_DefaultNow_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTimeEdit(Params^[0])^.DefaultNow := PBoolean(Params^[1])^;
end;

procedure _LapeTimeEdit_ReadOnly_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PTimeEdit(Params^[0])^.ReadOnly;
end;

procedure _LapeTimeEdit_ReadOnly_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTimeEdit(Params^[0])^.ReadOnly := PBoolean(Params^[1])^;
end;

procedure _LapeTimeEdit_DirectInput_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PTimeEdit(Params^[0])^.DirectInput;
end;

procedure _LapeTimeEdit_DirectInput_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTimeEdit(Params^[0])^.DirectInput := PBoolean(Params^[1])^;
end;

procedure _LapeTimeEdit_NumbersOnly_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PTimeEdit(Params^[0])^.NumbersOnly;
end;

procedure _LapeTimeEdit_NumbersOnly_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTimeEdit(Params^[0])^.NumbersOnly := PBoolean(Params^[1])^;
end;

procedure _LapeTimeEdit_SimpleLayout_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PTimeEdit(Params^[0])^.SimpleLayout;
end;

procedure _LapeTimeEdit_SimpleLayout_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTimeEdit(Params^[0])^.SimpleLayout := PBoolean(Params^[1])^;
end;

procedure _LapeTimeEdit_Text_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PTimeEdit(Params^[0])^.Text;
end;

procedure _LapeTimeEdit_Text_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTimeEdit(Params^[0])^.Text := PCaption(Params^[1])^;
end;

procedure _LapeTimeEdit_OnChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PTimeEdit(Params^[0])^.OnChange;
end;

procedure _LapeTimeEdit_OnChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PTimeEdit(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeTimeEdit_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PTimeEdit(Result)^ := TTimeEdit.Create(PComponent(Params^[0])^);
end;




procedure _LapeMenuItem_Find(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Result)^ := PMenuItem(Params^[0])^.Find(PString(Params^[1])^);
end;

procedure _LapeMenuItem_GetParentMenu(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMenu(Result)^ := PMenuItem(Params^[0])^.GetParentMenu();
end;

procedure _LapeMenuItem_IndexOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.IndexOf(PMenuItem(Params^[1])^);
end;

procedure _LapeMenuItem_IndexOfCaption(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.IndexOfCaption(PString(Params^[1])^);
end;

procedure _LapeMenuItem_VisibleIndexOf(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.VisibleIndexOf(PMenuItem(Params^[1])^);
end;

procedure _LapeMenuItem_Add(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Params^[0])^.Add(PMenuItem(Params^[1])^);
end;

procedure _LapeMenuItem_AddSeparator(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Params^[0])^.AddSeparator();
end;

procedure _LapeMenuItem_Click(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Params^[0])^.Click();
end;

procedure _LapeMenuItem_Delete(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Params^[0])^.Delete(PInteger(Params^[1])^);
end;

procedure _LapeMenuItem_Insert(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Params^[0])^.Insert(PInteger(Params^[1])^, PMenuItem(Params^[2])^);
end;

procedure _LapeMenuItem_Remove(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Params^[0])^.Remove(PMenuItem(Params^[1])^);
end;

procedure _LapeMenuItem_IsCheckItem(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.IsCheckItem();
end;

procedure _LapeMenuItem_IsLine(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.IsLine();
end;

procedure _LapeMenuItem_IsInMenuBar(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.IsInMenuBar();
end;

procedure _LapeMenuItem_Clear(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Params^[0])^.Clear();
end;

procedure _LapeMenuItem_HasBitmap(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.HasBitmap();
end;

procedure _LapeMenuItem_Count_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.Count;
end;

procedure _LapeMenuItem_Item_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Result)^ := PMenuItem(Params^[0])^.Items[PInteger(Params^[1])^];
end;

procedure _LapeMenuItem_MenuIndex_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PMenuItem(Params^[0])^.MenuIndex;
end;

procedure _LapeMenuItem_MenuIndex_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Params^[0])^.MenuIndex := PInteger(Params^[1])^;
end;

procedure _LapeMenuItem_Menu_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMenu(Result)^ := PMenuItem(Params^[0])^.Menu;
end;

procedure _LapeMenuItem_Parent_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Result)^ := PMenuItem(Params^[0])^.Parent;
end;

procedure _LapeMenuItem_Command_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PWord(Result)^ := PMenuItem(Params^[0])^.Command;
end;

procedure _LapeMenuItem_MenuVisibleIndex(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pinteger(Result)^ := PMenuItem(Params^[0])^.MenuVisibleIndex();
end;

procedure _LapeMenuItem_AutoCheck_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pboolean(Result)^ := PMenuItem(Params^[0])^.AutoCheck;
end;

procedure _LapeMenuItem_AutoCheck_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Params^[0])^.AutoCheck := Pboolean(Params^[1])^;
end;

procedure _LapeMenuItem_Default_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.Default;
end;

procedure _LapeMenuItem_Default_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Params^[0])^.Default := PBoolean(Params^[1])^;
end;

procedure _LapeMenuItem_Bitmap_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBitmap(Result)^ := PMenuItem(Params^[0])^.Bitmap;
end;

procedure _LapeMenuItem_Bitmap_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Params^[0])^.Bitmap := PBitmap(Params^[1])^;
end;

procedure _LapeMenuItem_GroupIndex_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  pbyte(Result)^ := PMenuItem(Params^[0])^.GroupIndex;
end;

procedure _LapeMenuItem_GroupIndex_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Params^[0])^.GroupIndex := pbyte(Params^[1])^;
end;

procedure _LapeMenuItem_Hint_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PMenuItem(Params^[0])^.Hint;
end;

procedure _LapeMenuItem_Hint_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Params^[0])^.Hint := PString(Params^[1])^;
end;

procedure _LapeMenuItem_RadioItem_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.RadioItem;
end;

procedure _LapeMenuItem_RadioItem_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Params^[0])^.RadioItem := PBoolean(Params^[1])^;
end;

procedure _LapeMenuItem_OnClick_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PMenuItem(Params^[0])^.OnClick;
end;

procedure _LapeMenuItem_OnClick_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Params^[0])^.OnClick := PNotifyEvent(Params^[1])^;
end;

procedure _LapeMenuItem_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Result)^ := TMenuItem.Create(PComponent(Params^[0])^);
end;

procedure _LapeMenuItem_Caption_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PMenuItem(Params^[0])^.Caption;
end;

procedure _LapeMenuItem_Caption_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Params^[0])^.Caption := PString(Params^[1])^;
end;

procedure _LapeMenuItem_Checked_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMenuItem(Params^[0])^.Checked;
end;

procedure _LapeMenuItem_Checked_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Params^[0])^.Checked := PBoolean(Params^[1])^;
end;

procedure _LapeMenuItem_AddEx(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
type
  TMenuItemArray = array of TMenuItem;
begin
  PMenuItem(Params^[0])^.Add(TMenuItemArray(Params^[1]^));
end;

procedure _LapeMenuItem_AddMenu(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Result)^ := TMenuItem.Create(PMenu(Params^[0])^);
  PMenuItem(Result)^.Caption := PString(Params^[1])^;
  PMenuItem(Params^[0])^.Add(PMenuItem(Result)^);
end;

procedure _LapeMenu_DispatchCommand(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.DispatchCommand(PWord(Params^[1])^);
end;

procedure _LapeMenu_Parent_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PComponent(Result)^ := PMenu(Params^[0])^.Parent;
end;

procedure _LapeMenu_Parent_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMenu(Params^[0])^.Parent := PComponent(Params^[1])^;
end;

procedure _LapeMenu_Items_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Result)^ := PMenu(Params^[0])^.Items;
end;

procedure _LapeMenu_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMenu(Result)^ := TMenu.Create(PComponent(Params^[0])^);
end;

procedure _LapeMenu_AddMenu(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMenuItem(Result)^ := TMenuItem.Create(PMenu(Params^[0])^);
  PMenuItem(Result)^.Caption := PString(Params^[1])^;
  PMenu(Params^[0])^.Items.Add(PMenuItem(Result)^);
end;

procedure _LapePopupMenu_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPopupMenu(Result)^ := TPopupMenu.Create(PComponent(Params^[0])^);
end;

procedure _LapePopupMenu_PopupComponent_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PComponent(Result)^ := PPopupMenu(Params^[0])^.PopupComponent;
end;

procedure _LapePopupMenu_PopupComponent_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPopupMenu(Params^[0])^.PopupComponent := PComponent(Params^[1])^;
end;

procedure _LapePopupMenu_PopupPoint_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPoint(Result)^ := PPopupMenu(Params^[0])^.PopupPoint;
end;

procedure _LapePopupMenu_OnPopup_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PPopupMenu(Params^[0])^.OnPopup;
end;

procedure _LapePopupMenu_OnPopup_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPopupMenu(Params^[0])^.OnPopup := PNotifyEvent(Params^[1])^;
end;

procedure _LapePopupMenu_OnClose_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PPopupMenu(Params^[0])^.OnClose;
end;

procedure _LapePopupMenu_OnClose_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPopupMenu(Params^[0])^.OnClose := PNotifyEvent(Params^[1])^;
end;

procedure _LapePopupMenu_Popup1(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPopupMenu(Params^[0])^.PopUp();
end;

procedure _LapePopupMenu_Popup2(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPopupMenu(Params^[0])^.Popup(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapePopupMenu_Close(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PPopupMenu(Params^[0])^.Close();
end;

procedure _LapeMainMenu_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PMainMenu(Result)^ := TMainMenu.Create(PComponent(Params^[0])^);
end;

procedure _LapeListFilterEdit_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PListFilterEdit(Result)^ := TListFilterEdit.Create(PComponent(Params^[0])^);
end;

procedure _LapeListFilterEdit_FilteredListBox_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PListBox(Result)^ := TListBox(PListFilterEdit(Params^[0])^.FilteredListbox);
end;

procedure _LapeListFilterEdit_FilteredListBox_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PListFilterEdit(Params^[0])^.FilteredListbox := PListBox(Params^[1])^;
end;

procedure _LapeListFilterEdit_Filter_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PListFilterEdit(Params^[0])^.Filter;
end;

procedure _LapeListFilterEdit_Filter_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PListFilterEdit(Params^[0])^.Filter := PString(Params^[1])^;
end;

procedure _LapeListFilterEdit_Flat_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PListFilterEdit(Params^[0])^.Flat;
end;

procedure _LapeListFilterEdit_Flat_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PListFilterEdit(Params^[0])^.Flat := PBoolean(Params^[1])^;
end;

procedure _LapeListFilterEdit_ButtonCaption_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PListFilterEdit(Params^[0])^.ButtonCaption;
end;

procedure _LapeListFilterEdit_ButtonCaption_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PListFilterEdit(Params^[0])^.ButtonCaption := PString(Params^[1])^;
end;

procedure _LapeListFilterEdit_ButtonWidth_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PListFilterEdit(Params^[0])^.ButtonWidth;
end;

procedure _LapeListFilterEdit_ButtonWidth_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PListFilterEdit(Params^[0])^.ButtonWidth := PInteger(Params^[1])^;
end;

procedure _LapeListFilterEdit_TextHint_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PListFilterEdit(Params^[0])^.TextHint;
end;

procedure _LapeListFilterEdit_TextHint_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PListFilterEdit(Params^[0])^.TextHint := PString(Params^[1])^;
end;

procedure _LapeListFilterEdit_OnAfterFilter_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PListFilterEdit(Params^[0])^.OnAfterFilter;
end;

procedure _LapeListFilterEdit_OnAfterFilter_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PListFilterEdit(Params^[0])^.OnAfterFilter := PNotifyEvent(Params^[1])^;
end;

procedure _LapeListFilterEdit_OnChange_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PNotifyEvent(Result)^ := PListFilterEdit(Params^[0])^.OnChange;
end;

procedure _LapeListFilterEdit_OnChange_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PListFilterEdit(Params^[0])^.OnChange := PNotifyEvent(Params^[1])^;
end;

procedure _LapeButtonPanel_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PButtonPanel(Result)^ := TButtonPanel.Create(PComponent(Params^[0])^);
end;

procedure _LapeButtonPanel_ShowButtons_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPanelButtons(Result)^ := PButtonPanel(Params^[0])^.ShowButtons;
end;

procedure _LapeButtonPanel_ShowButtons_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PButtonPanel(Params^[0])^.ShowButtons := PPanelButtons(Params^[1])^;
end;

procedure _LapeButtonPanel_ShowGlyphs_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPanelButtons(Result)^ := PButtonPanel(Params^[0])^.ShowGlyphs;
end;

procedure _LapeButtonPanel_ShowGlyphs_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PButtonPanel(Params^[0])^.ShowGlyphs := PPanelButtons(Params^[1])^;
end;

procedure _LapeControl_PopupMenu_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PPopupMenu(Result)^ := PControl(Params^[0])^.PopupMenu;
end;

procedure _LapeControl_PopupMenu_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PControl(Params^[0])^.PopupMenu := PPopupMenu(Params^[1])^;
end;

procedure ImportLCLMisc(Script: TSimbaScript);
begin
  with Script.Compiler do
  begin
    addClass('TLazCustomFloatSpinEdit', 'TLazCustomEdit', TCustomFloatSpinEdit);
    addProperty('TLazCustomFloatSpinEdit', 'ValueEmpty', 'Boolean', @_LapeCustomFloatSpinEdit_ValueEmpty_Read, @_LapeCustomFloatSpinEdit_ValueEmpty_Write);
    addClassConstructor('TLazCustomFloatSpinEdit', '(TheOwner: TLazComponent)', @_LapeCustomFloatSpinEdit_Create);

    addClass('TLazFloatSpinEdit', 'TLazCustomFloatSpinEdit', TFloatSpinEdit);
    addProperty('TLazFloatSpinEdit', 'DecimalPlaces', 'Integer', @_LapeFloatSpinEdit_DecimalPlaces_Read, @_LapeFloatSpinEdit_DecimalPlaces_Write);
    addProperty('TLazFloatSpinEdit', 'Increment', 'Double', @_LapeFloatSpinEdit_Increment_Read, @_LapeFloatSpinEdit_Increment_Write);
    addProperty('TLazFloatSpinEdit', 'MinValue', 'Double', @_LapeFloatSpinEdit_MinValue_Read, @_LapeFloatSpinEdit_MinValue_Write);
    addProperty('TLazFloatSpinEdit', 'MaxValue', 'Double', @_LapeFloatSpinEdit_MaxValue_Read, @_LapeFloatSpinEdit_MaxValue_Write);
    addProperty('TLazFloatSpinEdit', 'Value', 'Double', @_LapeFloatSpinEdit_Value_Read, @_LapeFloatSpinEdit_Value_Write);
    addClassConstructor('TLazFloatSpinEdit', '(TheOwner: TLazComponent)', @_LapeFloatSpinEdit_Create);

    addClass('TLazCustomSpinEdit', 'TLazCustomFloatSpinEdit', TCustomSpinEdit);
    addProperty('TLazCustomSpinEdit', 'Value', 'Integer', @_LapeCustomSpinEdit_Value_Read, @_LapeCustomSpinEdit_Value_Write);
    addProperty('TLazCustomSpinEdit', 'MinValue', 'Integer', @_LapeCustomSpinEdit_MinValue_Read, @_LapeCustomSpinEdit_MinValue_Write);
    addProperty('TLazCustomSpinEdit', 'MaxValue', 'Integer', @_LapeCustomSpinEdit_MaxValue_Read, @_LapeCustomSpinEdit_MaxValue_Write);
    addProperty('TLazCustomSpinEdit', 'Increment', 'Integer', @_LapeCustomSpinEdit_Increment_Read, @_LapeCustomSpinEdit_Increment_Write);
    addClassConstructor('TLazCustomSpinEdit', '(TheOwner: TLazComponent)', @_LapeCustomSpinEdit_Create);

    addClass('TLazSpinEdit', 'TLazCustomSpinEdit', TSpinEdit);
    addClassConstructor('TLazSpinEdit', '(TheOwner: TLazComponent)', @_LapeSpinEdit_Create);

    addClass('TLazDateEdit', 'TLazCustomControl', TDateEdit);
    addProperty('TLazDateEdit', 'Date', 'TDateTime', @_LapeDateEdit_Date_Read, @_LapeDateEdit_Date_Write);
    addProperty('TLazDateEdit', 'MinDate', 'TDateTime', @_LapeDateEdit_MinDate_Read, @_LapeDateEdit_MinDate_Write);
    addProperty('TLazDateEdit', 'MaxDate', 'TDateTime', @_LapeDateEdit_MaxDate_Read, @_LapeDateEdit_MaxDate_Write);
    addProperty('TLazDateEdit', 'DateFormat', 'String', @_LapeDateEdit_DateFormat_Read, @_LapeDateEdit_DateFormat_Write);
    addProperty('TLazDateEdit', 'DefaultToday', 'Boolean', @_LapeDateEdit_DefaultToday_Read, @_LapeDateEdit_DefaultToday_Write);
    addProperty('TLazDateEdit', 'ReadOnly', 'Boolean', @_LapeDateEdit_ReadOnly_Read, @_LapeDateEdit_ReadOnly_Write);
    addProperty('TLazDateEdit', 'DirectInput', 'Boolean', @_LapeDateEdit_DirectInput_Read, @_LapeDateEdit_DirectInput_Write);
    addProperty('TLazDateEdit', 'NumbersOnly', 'Boolean', @_LapeDateEdit_NumbersOnly_Read, @_LapeDateEdit_NumbersOnly_Write);
    addProperty('TLazDateEdit', 'Text', 'String', @_LapeDateEdit_Text_Read, @_LapeDateEdit_Text_Write);
    addProperty('TLazDateEdit', 'OnChange', 'TLazNotifyEvent', @_LapeDateEdit_OnChange_Read, @_LapeDateEdit_OnChange_Write);
    addClassConstructor('TLazDateEdit', '(TheOwner: TLazComponent)', @_LapeDateEdit_Create);

    addClass('TLazTimeEdit', 'TLazCustomControl', TTimeEdit);
    addProperty('TLazTimeEdit', 'Time', 'TDateTime', @_LapeTimeEdit_Time_Read, @_LapeTimeEdit_Time_Write);
    addProperty('TLazTimeEdit', 'DefaultNow', 'Boolean', @_LapeTimeEdit_DefaultNow_Read, @_LapeTimeEdit_DefaultNow_Write);
    addProperty('TLazTimeEdit', 'ReadOnly', 'Boolean', @_LapeTimeEdit_ReadOnly_Read, @_LapeTimeEdit_ReadOnly_Write);
    addProperty('TLazTimeEdit', 'DirectInput', 'Boolean', @_LapeTimeEdit_DirectInput_Read, @_LapeTimeEdit_DirectInput_Write);
    addProperty('TLazTimeEdit', 'NumbersOnly', 'Boolean', @_LapeTimeEdit_NumbersOnly_Read, @_LapeTimeEdit_NumbersOnly_Write);
    addProperty('TLazTimeEdit', 'SimpleLayout', 'Boolean', @_LapeTimeEdit_SimpleLayout_Read, @_LapeTimeEdit_SimpleLayout_Write);
    addProperty('TLazTimeEdit', 'Text', 'String', @_LapeTimeEdit_Text_Read, @_LapeTimeEdit_Text_Write);
    addProperty('TLazTimeEdit', 'OnChange', 'TLazNotifyEvent', @_LapeTimeEdit_OnChange_Read, @_LapeTimeEdit_OnChange_Write);
    addClassConstructor('TLazTimeEdit', '(TheOwner: TLazComponent)', @_LapeTimeEdit_Create);


    addClass('TLazMenu', 'TLazComponent', TMenu);
    addClass('TLazMenuItem', 'TLazComponent', TMenuItem);
    addGlobalFunc('function TLazMenuItem.Find(const ACaption: string): TLazMenuItem;', @_LapeMenuItem_Find);
    addGlobalFunc('function TLazMenuItem.GetParentMenu: TLazMenu;', @_LapeMenuItem_GetParentMenu);
    addGlobalFunc('function TLazMenuItem.IndexOf(Item: TLazMenuItem): Integer;', @_LapeMenuItem_IndexOf);
    addGlobalFunc('function TLazMenuItem.IndexOfCaption(const ACaption: string): Integer;', @_LapeMenuItem_IndexOfCaption);
    addGlobalFunc('function TLazMenuItem.VisibleIndexOf(Item: TLazMenuItem): Integer;', @_LapeMenuItem_VisibleIndexOf);
    addGlobalFunc('procedure TLazMenuItem.Add(Item: TLazMenuItem);', @_LapeMenuItem_Add);
    addGlobalFunc('procedure TLazMenuItem.AddEx(Items: array of TLazMenuItem);', @_LapeMenuItem_AddEx);
    addGlobalFunc('procedure TLazMenuItem.AddSeparator;', @_LapeMenuItem_AddSeparator);
    addGlobalFunc('procedure TLazMenuItem.Click;', @_LapeMenuItem_Click);
    addGlobalFunc('procedure TLazMenuItem.Delete(Index: Integer);', @_LapeMenuItem_Delete);
    addGlobalFunc('procedure TLazMenuItem.Insert(Index: Integer; Item: TLazMenuItem);', @_LapeMenuItem_Insert);
    addGlobalFunc('procedure TLazMenuItem.Remove(Item: TLazMenuItem);', @_LapeMenuItem_Remove);
    addGlobalFunc('function TLazMenuItem.IsCheckItem: Boolean;', @_LapeMenuItem_IsCheckItem);
    addGlobalFunc('function TLazMenuItem.IsLine: Boolean;', @_LapeMenuItem_IsLine);
    addGlobalFunc('function TLazMenuItem.IsInMenuBar: Boolean;', @_LapeMenuItem_IsInMenuBar);
    addGlobalFunc('procedure TLazMenuItem.Clear;', @_LapeMenuItem_Clear);
    addGlobalFunc('function TLazMenuItem.HasBitmap: Boolean;', @_LapeMenuItem_HasBitmap);
    addGlobalFunc('function TLazMenuItem.AddMenu(s: string): TLazMenuItem;', @_LapeMenuItem_AddMenu);
    addProperty('TLazMenuItem', 'Count', 'Integer', @_LapeMenuItem_Count_Read);
    addPropertyIndexed('TLazMenuItem', 'Item', 'Index: Integer', 'TLazMenuItem', @_LapeMenuItem_Item_Read);
    addProperty('TLazMenuItem', 'Hint', 'String', @_LapeMenuItem_Hint_Read, @_LapeMenuItem_Hint_Write);
    addProperty('TLazMenuItem', 'Checked', 'Boolean', @_LapeMenuItem_Checked_Read, @_LapeMenuItem_Checked_Write);
    addProperty('TLazMenuItem', 'MenuIndex', 'Integer', @_LapeMenuItem_MenuIndex_Read, @_LapeMenuItem_MenuIndex_Write);
    addProperty('TLazMenuItem', 'Menu', 'TLazMenu', @_LapeMenuItem_Menu_Read);
    addProperty('TLazMenuItem', 'Parent', 'TLazMenuItem', @_LapeMenuItem_Parent_Read);
    addProperty('TLazMenuItem', 'Command', 'Int16', @_LapeMenuItem_Command_Read);
    addProperty('TLazMenuItem', 'AutoCheck', 'Boolean', @_LapeMenuItem_AutoCheck_Read, @_LapeMenuItem_AutoCheck_Write);
    addProperty('TLazMenuItem', 'Default', 'Boolean', @_LapeMenuItem_Default_Read, @_LapeMenuItem_Default_Write);
    addProperty('TLazMenuItem', 'Bitmap', 'TLazBitmap', @_LapeMenuItem_Bitmap_Read, @_LapeMenuItem_Bitmap_Write);
    addProperty('TLazMenuItem', 'GroupIndex', 'Byte', @_LapeMenuItem_GroupIndex_Read, @_LapeMenuItem_GroupIndex_Write);
    addProperty('TLazMenuItem', 'RadioItem', 'Boolean', @_LapeMenuItem_RadioItem_Read, @_LapeMenuItem_RadioItem_Write);
    addProperty('TLazMenuItem', 'OnClick', 'TLazNotifyEvent', @_LapeMenuItem_OnClick_Read, @_LapeMenuItem_OnClick_Write);
    addProperty('TLazMenuItem', 'Caption', 'String', @_LapeMenuItem_Caption_Read, @_LapeMenuItem_Caption_Write);
    addClassConstructor('TLazMenuItem', '(AOwner: TLazComponent)', @_LapeMenuItem_Create);

    addGlobalFunc('function TLazMenu.DispatchCommand(ACommand: Int16): Boolean;', @_LapeMenu_DispatchCommand);
    addGlobalFunc('function TLazMenu.AddMenu(Name: string): TLazMenuItem;', @_LapeMenu_AddMenu);
    addProperty('TLazMenu', 'Parent', 'TLazComponent', @_LapeMenu_Parent_Read, @_LapeMenu_Parent_Write);
    addProperty('TLazMenu', 'Items', 'TLazMenuItem', @_LapeMenu_Items_Read);
    addClassConstructor('TLazMenu', '(AOwner: TLazComponent)', @_LapeMenu_Create);

    addClass('TLazPopupMenu', 'TLazMenu', TPopupMenu);
    addClassConstructor('TLazPopupMenu', '(AOwner: TLazComponent)', @_LapePopupMenu_Create);
    addProperty('TLazPopupMenu', 'PopupComponent', 'TLazComponent', @_LapePopupMenu_PopupComponent_Read, @_LapePopupMenu_PopupComponent_Write);
    addProperty('TLazPopupMenu', 'PopupPoint', 'TPoint', @_LapePopupMenu_PopupPoint_Read);
    addProperty('TLazPopupMenu', 'OnPopup', 'TLazNotifyEvent', @_LapePopupMenu_OnPopup_Read, @_LapePopupMenu_OnPopup_Write);
    addProperty('TLazPopupMenu', 'OnClose', 'TLazNotifyEvent', @_LapePopupMenu_OnClose_Read, @_LapePopupMenu_OnClose_Write);
    addGlobalFunc('procedure TLazPopupMenu.PopUp; overload', @_LapePopupMenu_Popup1);
    addGlobalFunc('procedure TLazPopupMenu.PopUp(X, Y: Integer); overload', @_LapePopupMenu_Popup2);
    addGlobalFunc('procedure TLazPopupMenu.Close', @_LapePopupMenu_Close);

    addClass('TLazMainMenu', 'TLazMenu', TMainMenu);
    addClassConstructor('TLazMainMenu', '(AOwner: TLazComponent)', @_LapeMainMenu_Create);

    addClass('TLazListFilterEdit', 'TLazCustomControl', TListFilterEdit);
    addClassConstructor('TLazListFilterEdit', '(AOwner: TLazComponent)', @_LapeListFilterEdit_Create);
    addProperty('TLazListFilterEdit', 'FilteredListBox', 'TLazListBox', @_LapeListFilterEdit_FilteredListBox_Read, @_LapeListFilterEdit_FilteredListBox_Write);
    addProperty('TLazListFilterEdit', 'Filter', 'String', @_LapeListFilterEdit_Filter_Read, @_LapeListFilterEdit_Filter_Write);
    addProperty('TLazListFilterEdit', 'Flat', 'Boolean', @_LapeListFilterEdit_Flat_Read, @_LapeListFilterEdit_Flat_Write);
    addProperty('TLazListFilterEdit', 'ButtonCaption', 'String', @_LapeListFilterEdit_ButtonCaption_Read, @_LapeListFilterEdit_ButtonCaption_Write);
    addProperty('TLazListFilterEdit', 'ButtonWidth', 'Integer', @_LapeListFilterEdit_ButtonWidth_Read, @_LapeListFilterEdit_ButtonWidth_Write);
    addProperty('TLazListFilterEdit', 'TextHint', 'String', @_LapeListFilterEdit_TextHint_Read, @_LapeListFilterEdit_TextHint_Write);
    addProperty('TLazListFilterEdit', 'OnAfterFilter', 'TLazNotifyEvent', @_LapeListFilterEdit_OnAfterFilter_Read, @_LapeListFilterEdit_OnAfterFilter_Write);
    addProperty('TLazListFilterEdit', 'OnChange', 'TLazNotifyEvent', @_LapeListFilterEdit_OnChange_Read, @_LapeListFilterEdit_OnChange_Write);

    addClass('TLazButtonPanel', 'TLazCustomPanel', TButtonPanel);
    addGlobalType('set of enum(OK, Cancel, Close, Help)', 'ELazButtonPanelButtons');
    addClassConstructor('TLazButtonPanel', '(AOwner: TLazComponent)', @_LapeButtonPanel_Create);
    addProperty('TLazButtonPanel', 'ShowButtons', 'ELazButtonPanelButtons', @_LapeButtonPanel_ShowButtons_Read, @_LapeButtonPanel_ShowButtons_Write);
    addProperty('TLazButtonPanel', 'ShowGlyphs', 'ELazButtonPanelButtons', @_LapeButtonPanel_ShowGlyphs_Read, @_LapeButtonPanel_ShowGlyphs_Write);

    addProperty('TLazControl', 'PopupMenu', 'TLazPopupMenu', @_LapeControl_PopupMenu_Read, @_LapeControl_PopupMenu_Write);
  end;
end;

end.

