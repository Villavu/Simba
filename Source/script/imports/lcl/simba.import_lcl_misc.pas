unit simba.import_lcl_misc;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.base, simba.script_compiler;

procedure ImportLCLMisc(Compiler: TSimbaScript_Compiler);

implementation

uses
  process, spin, pipes, menus, graphics, ListFilterEdit, StdCtrls, Buttons, ButtonPanel,
  lptypes, ffi;

type
  PBitmap = ^TBitmap;
  PNotifyEvent = ^TNotifyEvent;
  PRect = ^TRect;
  PHandle = ^THandle;
  PComponent = ^TComponent;
  PSeekOrigin = ^TSeekOrigin;
  PStrings = ^TStrings;
  PInputPipeStream = ^TInputPipeStream;
  POutputPipeStream = ^TOutputPipeStream;
  PProcess = ^TProcess;
  PProcessOptions = ^TProcessOptions;
  PProcessPriority = ^TProcessPriority;
  PStartupOptions = ^TStartupOptions;

  PCustomFloatSpinEdit = ^TCustomFloatSpinEdit;
  PCustomSpinEdit = ^TCustomSpinEdit;
  PFloatSpinEdit = ^TFloatSpinEdit;
  PSpinEdit = ^TSpinEdit;

  PMenu = ^TMenu;
  PMainMenu = ^TMainMenu;
  PMenuItem = ^TMenuItem;

  PListFilterEdit = ^TListFilterEdit;
  PListBox = ^TListBox;
  PButtonPanel = ^TButtonPanel;
  PPanelButtons = ^TPanelButtons;

procedure _LapeOutputPipeStream_Seek(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pint64(Result)^ := POutputPipeStream(Params^[0])^.Seek(Pint64(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

procedure _LapeOutputPipeStream_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := POutputPipeStream(Params^[0])^.Read(PPointer(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeOutputPipeStream_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  POutputPipeStream(Result)^ := TOutputPipeStream.Create(PHandle(Params^[0])^);
end;

procedure _LapeInputPipeStream_Write(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PInputPipeStream(Params^[0])^.Write(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeInputPipeStream_Seek(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pint64(Result)^ := PInputPipeStream(Params^[0])^.Seek(Pint64(Params^[1])^, PSeekOrigin(Params^[2])^);
end;

procedure _LapeInputPipeStream_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PInputPipeStream(Params^[0])^.Read(PInteger(Params^[1])^, PInteger(Params^[2])^);
end;

procedure _LapeInputPipeStream_NumBytesAvailable_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PUInt32(Result)^ := PInputPipeStream(Params^[0])^.NumBytesAvailable;
end;

procedure _LapeInputPipeStream_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInputPipeStream(Result)^ := TInputPipeStream.Create(PHandle(Params^[0])^);
end;

procedure _LapeProcess_Execute(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProcess(Params^[0])^.Execute();
end;

procedure _LapeProcess_Resume(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PProcess(Params^[0])^.Resume();
end;

procedure _LapeProcess_Suspend(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PProcess(Params^[0])^.Suspend();
end;

procedure _LapeProcess_Terminate(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.Terminate(PInteger(Params^[1])^);
end;

procedure _LapeProcess_WaitOnExit(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.WaitOnExit();
end;

procedure _LapeProcess_WindowRect_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PRect(Result)^ := PProcess(Params^[0])^.WindowRect;
end;

procedure _LapeProcess_WindowRect_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProcess(Params^[0])^.WindowRect := PRect(Params^[1])^;
end;

procedure _LapeProcess_ProcessHandle_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PHandle(Result)^ := PProcess(Params^[0])^.ProcessHandle;
end;

procedure _LapeProcess_ThreadHandle_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PHandle(Result)^ := PProcess(Params^[0])^.ThreadHandle;
end;

procedure _LapeProcess_ProcessID_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PProcess(Params^[0])^.ProcessID;
end;

procedure _LapeProcess_ThreadID_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PProcess(Params^[0])^.ThreadID;
end;

procedure _LapeProcess_ExitStatus_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PProcess(Params^[0])^.ExitStatus;
end;

procedure _LapeProcess_InheritHandles_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.InheritHandles;
end;

procedure _LapeProcess_InheritHandles_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProcess(Params^[0])^.InheritHandles := PBoolean(Params^[1])^;
end;

procedure _LapeProcess_PipeBufferSize_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  Pcardinal(Result)^ := PProcess(Params^[0])^.PipeBufferSize;
end;

procedure _LapeProcess_PipeBufferSize_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProcess(Params^[0])^.PipeBufferSize := Pcardinal(Params^[1])^;
end;

procedure _LapeProcess_Active_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.Active;
end;

procedure _LapeProcess_Active_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProcess(Params^[0])^.Active := PBoolean(Params^[1])^;
end;

procedure _LapeProcess_CommandLine_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PProcess(Params^[0])^.CommandLine;
end;

procedure _LapeProcess_CommandLine_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProcess(Params^[0])^.CommandLine := PString(Params^[1])^;
end;

procedure _LapeProcess_Executable_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PProcess(Params^[0])^.Executable;
end;

procedure _LapeProcess_Executable_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProcess(Params^[0])^.Executable := PString(Params^[1])^;
end;

procedure _LapeProcess_Parameters_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Result)^ := PProcess(Params^[0])^.Parameters;
end;

procedure _LapeProcess_Parameters_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProcess(Params^[0])^.Parameters := PStrings(Params^[1])^;
end;

procedure _LapeProcess_CurrentDirectory_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PString(Result)^ := PProcess(Params^[0])^.CurrentDirectory;
end;

procedure _LapeProcess_CurrentDirectory_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProcess(Params^[0])^.CurrentDirectory := PString(Params^[1])^;
end;

procedure _LapeProcess_Environment_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStrings(Result)^ := PProcess(Params^[0])^.Environment;
end;

procedure _LapeProcess_Environment_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProcess(Params^[0])^.Environment := PStrings(Params^[1])^;
end;

procedure _LapeProcess_Options_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PProcessOptions(Result)^ := PProcess(Params^[0])^.Options;
end;

procedure _LapeProcess_Options_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProcess(Params^[0])^.Options := PProcessOptions(Params^[1])^;
end;

procedure _LapeProcess_Priority_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PProcessPriority(Result)^ := PProcess(Params^[0])^.Priority;
end;

procedure _LapeProcess_Priority_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProcess(Params^[0])^.Priority := PProcessPriority(Params^[1])^;
end;

procedure _LapeProcess_StartupOptions_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PStartupOptions(Result)^ := PProcess(Params^[0])^.StartupOptions;
end;

procedure _LapeProcess_StartupOptions_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PProcess(Params^[0])^.StartupOptions := PStartupOptions(Params^[1])^;
end;

procedure _LapeProcess_Running_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PProcess(Params^[0])^.Running;
end;

procedure _LapeProcess_Create(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PProcess(Result)^ := TProcess.Create(PComponent(Params^[0])^);
end;

procedure _LapeProcess_Input_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  POutputPipeStream(Result)^ := PProcess(Params^[0])^.Input;
end;

procedure _LapeProcess_Output_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInputPipeStream(Result)^ := PProcess(Params^[0])^.Output;
end;

procedure _LapeProcess_Stderr_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInputPipeStream(Result)^ := PProcess(Params^[0])^.Stderr;
end;

procedure _LapeProcess_ExitCode_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PProcess(Params^[0])^.ExitCode;
end;

procedure _LapeCustomFloatSpinEdit_DecimalPlaces_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PInteger(Result)^ := PCustomFloatSpinEdit(Params^[0])^.DecimalPlaces;
end;

procedure _LapeCustomFloatSpinEdit_DecimalPlaces_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomFloatSpinEdit(Params^[0])^.DecimalPlaces := PInteger(Params^[1])^;
end;

procedure _LapeCustomFloatSpinEdit_Increment_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.Increment;
end;

procedure _LapeCustomFloatSpinEdit_Increment_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomFloatSpinEdit(Params^[0])^.Increment := PDouble(Params^[1])^;
end;

procedure _LapeCustomFloatSpinEdit_MinValue_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.MinValue;
end;

procedure _LapeCustomFloatSpinEdit_MinValue_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomFloatSpinEdit(Params^[0])^.MinValue := PDouble(Params^[1])^;
end;

procedure _LapeCustomFloatSpinEdit_MaxValue_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.MaxValue;
end;

procedure _LapeCustomFloatSpinEdit_MaxValue_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomFloatSpinEdit(Params^[0])^.MaxValue := PDouble(Params^[1])^;
end;

procedure _LapeCustomFloatSpinEdit_Value_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PDouble(Result)^ := PCustomFloatSpinEdit(Params^[0])^.Value;
end;

procedure _LapeCustomFloatSpinEdit_Value_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PCustomFloatSpinEdit(Params^[0])^.Value := PDouble(Params^[1])^;
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

procedure _LapeMenuItem_Items_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
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

procedure _LapeMenu_ParentBidiMode_Read(const Params: PParamArray; const Result: Pointer); LAPE_WRAPPER_CALLING_CONV
begin
  PBoolean(Result)^ := PMenu(Params^[0])^.ParentBidiMode;
end;

procedure _LapeMenu_ParentBidiMode_Write(const Params: PParamArray); LAPE_WRAPPER_CALLING_CONV
begin
  PMenu(Params^[0])^.ParentBidiMode := PBoolean(Params^[1])^;
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

procedure ImportLCLMisc(Compiler: TSimbaScript_Compiler);
begin
  with Compiler do
  begin
    addClass('TLazCustomFloatSpinEdit', 'TLazCustomEdit');
    addProperty('TLazCustomFloatSpinEdit', 'DecimalPlaces', 'Integer', @_LapeCustomFloatSpinEdit_DecimalPlaces_Read, @_LapeCustomFloatSpinEdit_DecimalPlaces_Write);
    addProperty('TLazCustomFloatSpinEdit', 'Increment', 'Double', @_LapeCustomFloatSpinEdit_Increment_Read, @_LapeCustomFloatSpinEdit_Increment_Write);
    addProperty('TLazCustomFloatSpinEdit', 'MinValue', 'Double', @_LapeCustomFloatSpinEdit_MinValue_Read, @_LapeCustomFloatSpinEdit_MinValue_Write);
    addProperty('TLazCustomFloatSpinEdit', 'MaxValue', 'Double', @_LapeCustomFloatSpinEdit_MaxValue_Read, @_LapeCustomFloatSpinEdit_MaxValue_Write);
    addProperty('TLazCustomFloatSpinEdit', 'Value', 'Double', @_LapeCustomFloatSpinEdit_Value_Read, @_LapeCustomFloatSpinEdit_Value_Write);
    addProperty('TLazCustomFloatSpinEdit', 'ValueEmpty', 'Boolean', @_LapeCustomFloatSpinEdit_ValueEmpty_Read, @_LapeCustomFloatSpinEdit_ValueEmpty_Write);
    addClassConstructor('TLazCustomFloatSpinEdit', '(TheOwner: TLazComponent)', @_LapeCustomFloatSpinEdit_Create);

    addClass('TLazFloatSpinEdit', 'TLazCustomFloatSpinEdit');
    addClassConstructor('TLazFloatSpinEdit', '(TheOwner: TLazComponent)', @_LapeFloatSpinEdit_Create);

    addClass('TLazCustomSpinEdit', 'TLazCustomFloatSpinEdit');
    addProperty('TLazCustomSpinEdit', 'Value', 'Integer', @_LapeCustomSpinEdit_Value_Read, @_LapeCustomSpinEdit_Value_Write);
    addProperty('TLazCustomSpinEdit', 'MinValue', 'Integer', @_LapeCustomSpinEdit_MinValue_Read, @_LapeCustomSpinEdit_MinValue_Write);
    addProperty('TLazCustomSpinEdit', 'MaxValue', 'Integer', @_LapeCustomSpinEdit_MaxValue_Read, @_LapeCustomSpinEdit_MaxValue_Write);
    addProperty('TLazCustomSpinEdit', 'Increment', 'Integer', @_LapeCustomSpinEdit_Increment_Read, @_LapeCustomSpinEdit_Increment_Write);
    addClassConstructor('TLazCustomSpinEdit', '(TheOwner: TLazComponent)', @_LapeCustomSpinEdit_Create);

    addClass('TLazSpinEdit', 'TLazCustomSpinEdit');
    addClassConstructor('TLazSpinEdit', '(TheOwner: TLazComponent)', @_LapeSpinEdit_Create);

    addGlobalType('(poRunSuspended,poWaitOnExit,poUsePipes,poStderrToOutPut,poNoConsole,poNewConsole,poDefaultErrorMode,poNewProcessGroup,poDebugProcess,poDebugOnlyThisProcess,poDetached,poPassInput,poRunIdle)', 'TLazProcessOption');
    addGlobalType('(ppHigh,ppIdle,ppNormal,ppRealTime)', 'TLazProcessPriority');
    addGlobalType('set of TLazProcessOption', 'TLazProcessOptions');

    addClass('TLazOutputPipeStream', 'TLazHandleStream');
    addClassConstructor('TLazOutputPipeStream', '(AHandle: TLazHandle)', @_LapeOutputPipeStream_Create);

    addClass('TLazInputPipeStream', 'TLazHandleStream');
    addProperty('TLazInputPipeStream', 'NumBytesAvailable', 'UInt32', @_LapeInputPipeStream_NumBytesAvailable_Read);
    addClassConstructor('TLazInputPipeStream', '(AHandle: TLazHandle)', @_LapeInputPipeStream_Create);

    addClass('TLazProcess', 'TLazComponent');
    addGlobalFunc('procedure TLazProcess.Execute;', @_LapeProcess_Execute);
    addGlobalFunc('function TLazProcess.Resume: Integer;', @_LapeProcess_Resume);
    addGlobalFunc('function TLazProcess.Suspend: Integer;', @_LapeProcess_Suspend);
    addGlobalFunc('function TLazProcess.Terminate(AExitCode : Integer): Boolean;', @_LapeProcess_Terminate);
    addGlobalFunc('function TLazProcess.WaitOnExit: Boolean;', @_LapeProcess_WaitOnExit);
    addProperty('TLazProcess', 'WindowRect', 'TLazRect', @_LapeProcess_WindowRect_Read, @_LapeProcess_WindowRect_Write);
    addProperty('TLazProcess', 'ProcessHandle', 'TLazHandle', @_LapeProcess_ProcessHandle_Read);
    addProperty('TLazProcess', 'ThreadHandle', 'TLazHandle', @_LapeProcess_ThreadHandle_Read);
    addProperty('TLazProcess', 'ProcessID', 'Integer', @_LapeProcess_ProcessID_Read);
    addProperty('TLazProcess', 'ThreadID', 'Integer', @_LapeProcess_ThreadID_Read);
    addProperty('TLazProcess', 'Input', 'TLazOutputPipeStream', @_LapeProcess_Input_Read);
    addProperty('TLazProcess', 'Output', 'TLazInputPipeStream', @_LapeProcess_Output_Read);
    addProperty('TLazProcess', 'Stderr', 'TLazInputPipeStream', @_LapeProcess_Stderr_Read);
    addProperty('TLazProcess', 'ExitStatus', 'Integer', @_LapeProcess_ExitStatus_Read);
    addProperty('TLazProcess', 'ExitCode', 'Integer', @_LapeProcess_ExitCode_Read);
    addProperty('TLazProcess', 'InheritHandles', 'Boolean', @_LapeProcess_InheritHandles_Read, @_LapeProcess_InheritHandles_Write);
    addProperty('TLazProcess', 'PipeBufferSize', 'UInt32', @_LapeProcess_PipeBufferSize_Read, @_LapeProcess_PipeBufferSize_Write);
    addProperty('TLazProcess', 'Active', 'Boolean', @_LapeProcess_Active_Read, @_LapeProcess_Active_Write);
    addProperty('TLazProcess', 'CommandLine', 'String', @_LapeProcess_CommandLine_Read, @_LapeProcess_CommandLine_Write);
    addProperty('TLazProcess', 'Executable', 'String', @_LapeProcess_Executable_Read, @_LapeProcess_Executable_Write);
    addProperty('TLazProcess', 'Parameters', 'TLazStrings', @_LapeProcess_Parameters_Read, @_LapeProcess_Parameters_Write);
    addProperty('TLazProcess', 'CurrentDirectory', 'String', @_LapeProcess_CurrentDirectory_Read, @_LapeProcess_CurrentDirectory_Write);
    addProperty('TLazProcess', 'Environment', 'TLazStrings', @_LapeProcess_Environment_Read, @_LapeProcess_Environment_Write);
    addProperty('TLazProcess', 'Options', 'TLazProcessOptions', @_LapeProcess_Options_Read, @_LapeProcess_Options_Write);
    addProperty('TLazProcess', 'Priority', 'TLazProcessPriority', @_LapeProcess_Priority_Read, @_LapeProcess_Priority_Write);
    addProperty('TLazProcess', 'Running', 'Boolean', @_LapeProcess_Running_Read);
    addClassConstructor('TLazProcess', '(AOwner : TLazComponent)', @_LapeProcess_Create);

    addClass('TLazMenu', 'TLazComponent');
    addClass('TLazMenuItem', 'TLazComponent');
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
    addGlobalFunc('function TLazMenuItem.IsCheckItem: boolean;', @_LapeMenuItem_IsCheckItem);
    addGlobalFunc('function TLazMenuItem.IsLine: Boolean;', @_LapeMenuItem_IsLine);
    addGlobalFunc('function TLazMenuItem.IsInMenuBar: boolean;', @_LapeMenuItem_IsInMenuBar);
    addGlobalFunc('procedure TLazMenuItem.Clear;', @_LapeMenuItem_Clear);
    addGlobalFunc('function TLazMenuItem.HasBitmap: boolean;', @_LapeMenuItem_HasBitmap);
    addGlobalFunc('function TLazMenuItem.AddMenu(s: string): TLazMenuItem;', @_LapeMenuItem_AddMenu);
    addProperty('TLazMenuItem', 'Count', 'Integer', @_LapeMenuItem_Count_Read);
    addProperty('TLazMenuItem', 'Items', 'TLazMenuItem', @_LapeMenuItem_Items_Read);
    addProperty('TLazMenuItem', 'Hint', 'String', @_LapeMenuItem_Hint_Read, @_LapeMenuItem_Hint_Write);
    addProperty('TLazMenuItem', 'Checked', 'Boolean', @_LapeMenuItem_Checked_Read, @_LapeMenuItem_Checked_Write);
    addProperty('TLazMenuItem', 'MenuIndex', 'Integer', @_LapeMenuItem_MenuIndex_Read, @_LapeMenuItem_MenuIndex_Write);
    addProperty('TLazMenuItem', 'Menu', 'TLazMenu', @_LapeMenuItem_Menu_Read);
    addProperty('TLazMenuItem', 'Parent', 'TLazMenuItem', @_LapeMenuItem_Parent_Read);
    addProperty('TLazMenuItem', 'Command', 'Int16', @_LapeMenuItem_Command_Read);
    addProperty('TLazMenuItem', 'AutoCheck', 'boolean', @_LapeMenuItem_AutoCheck_Read, @_LapeMenuItem_AutoCheck_Write);
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

    addClass('TLazMainMenu', 'TLazMenu');
    addClassConstructor('TLazMainMenu', '(AOwner: TLazComponent)', @_LapeMainMenu_Create);

    addClass('TLazListFilterEdit', 'TLazCustomControl');
    addClassConstructor('TLazListFilterEdit', '(AOwner: TLazComponent)', @_LapeListFilterEdit_Create);
    addProperty('TLazListFilterEdit', 'FilteredListBox', 'TLazListBox', @_LapeListFilterEdit_FilteredListBox_Read, @_LapeListFilterEdit_FilteredListBox_Write);
    addProperty('TLazListFilterEdit', 'Filter', 'String', @_LapeListFilterEdit_Filter_Read, @_LapeListFilterEdit_Filter_Write);
    addProperty('TLazListFilterEdit', 'Flat', 'Boolean', @_LapeListFilterEdit_Flat_Read, @_LapeListFilterEdit_Flat_Write);
    addProperty('TLazListFilterEdit', 'ButtonCaption', 'String', @_LapeListFilterEdit_ButtonCaption_Read, @_LapeListFilterEdit_ButtonCaption_Write);
    addProperty('TLazListFilterEdit', 'ButtonWidth', 'Integer', @_LapeListFilterEdit_ButtonWidth_Read, @_LapeListFilterEdit_ButtonWidth_Write);
    addProperty('TLazListFilterEdit', 'TextHint', 'String', @_LapeListFilterEdit_TextHint_Read, @_LapeListFilterEdit_TextHint_Write);
    addProperty('TLazListFilterEdit', 'OnAfterFilter', 'TLazNotifyEvent', @_LapeListFilterEdit_OnAfterFilter_Read, @_LapeListFilterEdit_OnAfterFilter_Write);
    addProperty('TLazListFilterEdit', 'OnChange', 'TLazNotifyEvent', @_LapeListFilterEdit_OnChange_Read, @_LapeListFilterEdit_OnChange_Write);

    addClass('TLazButtonPanel', 'TLazCustomPanel');
    addGlobalType('set of enum(OK, Cancel, Close, Help)', 'ELazButtonPanelButtons');
    addClassConstructor('TLazButtonPanel', '(AOwner: TLazComponent)', @_LapeButtonPanel_Create);
    addProperty('TLazButtonPanel', 'ShowButtons', 'ELazButtonPanelButtons', @_LapeButtonPanel_ShowButtons_Read, @_LapeButtonPanel_ShowButtons_Write);
    addProperty('TLazButtonPanel', 'ShowGlyphs', 'ELazButtonPanelButtons', @_LapeButtonPanel_ShowGlyphs_Read, @_LapeButtonPanel_ShowGlyphs_Write);
  end;
end;

end.
