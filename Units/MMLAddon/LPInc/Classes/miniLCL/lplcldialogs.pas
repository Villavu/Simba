unit lplcldialogs;

{$mode objfpc}{$H+}
{$I Simba.inc}

interface

uses
  Classes, SysUtils, lpcompiler, lptypes, lpClassHelper;

procedure RegisterLCLDialogs(Compiler: TLapeCompiler);

implementation
  uses MufasaTypes, stdctrls, forms, lplclsystem, lplclgraphics, lplclcontrols, ComCtrls, Dialogs, LCLClasses;

type
  PCommonDialog = ^TCommonDialog;
  PCloseQueryEvent = ^TCloseQueryEvent;
  PLCLComponent = ^TLCLComponent;
  PFileDialog = ^TFileDialog;
  POpenDialog = ^TOpenDialog;
  POpenOption = ^TOpenOption;
  POpenOptions = ^TOpenOptions;
  PColorDialog = ^TColorDialog;
  TColor = Integer;
  PColor = ^TColor;

//procedure RemoveAllHandlersOfObject(AnObject: TObject); virtual;
procedure TLCLComponent_RemoveAllHandlersOfObject(const Params: PParamArray); lape_extdecl
begin
  PLCLComponent(Params^[0])^.RemoveAllHandlersOfObject(PObject(Params^[1])^);
end;

//procedure IncLCLRefCount;
procedure TLCLComponent_IncLCLRefCount(const Params: PParamArray); lape_extdecl
begin
  PLCLComponent(Params^[0])^.IncLCLRefCount();
end;

//procedure DecLCLRefCount;
procedure TLCLComponent_DecLCLRefCount(const Params: PParamArray); lape_extdecl
begin
  PLCLComponent(Params^[0])^.DecLCLRefCount();
end;

//Read: property LCLRefCount: integer read FLCLRefCount;
procedure TLCLComponent_LCLRefCount_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PLCLComponent(Params^[0])^.LCLRefCount;
end;

//constructor Create();
procedure TLCLComponent_Init(const Params: PParamArray); lape_extdecl
begin
 PLCLComponent(Params^[0])^ := TLCLComponent.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TLCLComponent_Free(const Params: PParamArray); lape_extdecl
begin
  PLCLComponent(Params^[0])^.Free();
end;

procedure Register_TLCLComponent(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TLCLComponent', 'TComponent');

    addGlobalFunc('procedure TLCLComponent.RemoveAllHandlersOfObject(AnObject: TObject); constref;', @TLCLComponent_RemoveAllHandlersOfObject);
    addGlobalFunc('procedure TLCLComponent.IncLCLRefCount(); constref;', @TLCLComponent_IncLCLRefCount);
    addGlobalFunc('procedure TLCLComponent.DecLCLRefCount(); constref;', @TLCLComponent_DecLCLRefCount);
    addClassVar('TLCLComponent', 'LCLRefCount', 'integer', @TLCLComponent_LCLRefCount_Read, nil);
    addGlobalFunc('procedure TLCLComponent.Init(TheOwner: TComponent);', @TLCLComponent_Init);
    addGlobalFunc('procedure TLCLComponent.Free();', @TLCLComponent_Free);
  end;
end;


//Read: FCompStyle : LongInt;
Procedure TCommonDialog_FCompStyle_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PLongInt(Result)^ := PCommonDialog(Params^[0])^.FCompStyle;
end;

//Write: FCompStyle : LongInt;
procedure TCommonDialog_FCompStyle_Write(const Params: PParamArray); lape_extdecl
begin
  PCommonDialog(Params^[0])^.FCompStyle := PLongInt(Params^[1])^;
end;

//function Execute: boolean; virtual;
procedure TCommonDialog_Execute(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCommonDialog(Params^[0])^.Execute();
end;

//Read: property Handle: THandle read FHandle write SetHandle;
procedure TCommonDialog_Handle_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PHandle(Result)^ := PCommonDialog(Params^[0])^.Handle;
end;

//Write: property Handle: THandle read FHandle write SetHandle;
procedure TCommonDialog_Handle_Write(const Params: PParamArray); lape_extdecl
begin
  PCommonDialog(Params^[0])^.Handle := PHandle(Params^[1])^;
end;

//Read: property UserChoice: integer read FUserChoice write FUserChoice;
procedure TCommonDialog_UserChoice_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCommonDialog(Params^[0])^.UserChoice;
end;

//Write: property UserChoice: integer read FUserChoice write FUserChoice;
procedure TCommonDialog_UserChoice_Write(const Params: PParamArray); lape_extdecl
begin
  PCommonDialog(Params^[0])^.UserChoice := Pinteger(Params^[1])^;
end;

//procedure Close; virtual;
procedure TCommonDialog_Close(const Params: PParamArray); lape_extdecl
begin
  PCommonDialog(Params^[0])^.Close();
End;

//procedure DoShow; virtual;
procedure TCommonDialog_DoShow(const Params: PParamArray); lape_extdecl
begin
  PCommonDialog(Params^[0])^.DoShow();
end;

//procedure DoCanClose(var CanClose: Boolean); virtual;
procedure TCommonDialog_DoCanClose(const Params: PParamArray); lape_extdecl
begin
  PCommonDialog(Params^[0])^.DoCanClose(PBoolean(Params^[1])^);
end;

//procedure DoClose; virtual;
procedure TCommonDialog_DoClose(const Params: PParamArray); lape_extdecl
begin
  PCommonDialog(Params^[0])^.DoClose();
end;

//function HandleAllocated: boolean;
procedure TCommonDialog_HandleAllocated(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pboolean(Result)^ := PCommonDialog(Params^[0])^.HandleAllocated();
end;

//Read: property OnClose: TNotifyEvent read FOnClose write FOnClose;
procedure TCommonDialog_OnClose_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCommonDialog(Params^[0])^.OnClose;
end;

//Write: property OnClose: TNotifyEvent read FOnClose write FOnClose;
procedure TCommonDialog_OnClose_Write(const Params: PParamArray); lape_extdecl
begin
  PCommonDialog(Params^[0])^.OnClose := PNotifyEvent(Params^[1])^;
end;

//Read: property OnCanClose: TCloseQueryEvent read FOnCanClose write FOnCanClose;
procedure TCommonDialog_OnCanClose_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PCloseQueryEvent(Result)^ := PCommonDialog(Params^[0])^.OnCanClose;
end;

//Write: property OnCanClose: TCloseQueryEvent read FOnCanClose write FOnCanClose;
procedure TCommonDialog_OnCanClose_Write(const Params: PParamArray); lape_extdecl
begin
  PCommonDialog(Params^[0])^.OnCanClose := PCloseQueryEvent(Params^[1])^;
end;

//Read: property OnShow: TNotifyEvent read FOnShow write FOnShow;
procedure TCommonDialog_OnShow_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PCommonDialog(Params^[0])^.OnShow;
end;

//Write: property OnShow: TNotifyEvent read FOnShow write FOnShow;
procedure TCommonDialog_OnShow_Write(const Params: PParamArray); lape_extdecl
begin
  PCommonDialog(Params^[0])^.OnShow := PNotifyEvent(Params^[1])^;
end;

//Read: property Width: integer read FWidth write SetWidth default 0;
procedure TCommonDialog_Width_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCommonDialog(Params^[0])^.Width;
end;

//Write: property Width: integer read FWidth write SetWidth default 0;
procedure TCommonDialog_Width_Write(const Params: PParamArray); lape_extdecl
begin
  PCommonDialog(Params^[0])^.Width := Pinteger(Params^[1])^;
end;

//Read: property Height: integer read FHeight write SetHeight default 0;
procedure TCommonDialog_Height_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  Pinteger(Result)^ := PCommonDialog(Params^[0])^.Height;
end;

//Write: property Height: integer read FHeight write SetHeight default 0;
procedure TCommonDialog_Height_Write(const Params: PParamArray); lape_extdecl
begin
  PCommonDialog(Params^[0])^.Height := Pinteger(Params^[1])^;
end;

//constructor Create();
procedure TCommonDialog_Init(const Params: PParamArray); lape_extdecl
begin
  PCommonDialog(Params^[0])^ := TCommonDialog.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TCommonDialog_Free(const Params: PParamArray); lape_extdecl
begin
  PCommonDialog(Params^[0])^.Free();
end;

procedure Register_TCommonDialog(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TCommonDialog', 'TLCLComponent');

    addClassVar('TCommonDialog', 'FCompStyle', 'LongInt', @TCommonDialog_FCompStyle_Read, @TCommonDialog_FCompStyle_Write);
    addGlobalFunc('function TCommonDialog.Execute(): boolean; constref;', @TCommonDialog_Execute);
    addClassVar('TCommonDialog', 'Handle', 'THandle', @TCommonDialog_Handle_Read, @TCommonDialog_Handle_Write);
    addClassVar('TCommonDialog', 'UserChoice', 'integer', @TCommonDialog_UserChoice_Read, @TCommonDialog_UserChoice_Write);
    addGlobalFunc('procedure TCommonDialog.Close(); constref;', @TCommonDialog_Close);
    addGlobalFunc('procedure TCommonDialog.DoShow(); constref;', @TCommonDialog_DoShow);
    addGlobalFunc('procedure TCommonDialog.DoCanClose(var CanClose: Boolean); constref;', @TCommonDialog_DoCanClose);
    addGlobalFunc('procedure TCommonDialog.DoClose(); constref;', @TCommonDialog_DoClose);
    addGlobalFunc('function TCommonDialog.HandleAllocated(): boolean; constref;', @TCommonDialog_HandleAllocated);
    addClassVar('TCommonDialog', 'OnClose', 'TNotifyEvent', @TCommonDialog_OnClose_Read, @TCommonDialog_OnClose_Write);
    addClassVar('TCommonDialog', 'OnCanClose', 'TCloseQueryEvent', @TCommonDialog_OnCanClose_Read, @TCommonDialog_OnCanClose_Write);
    addClassVar('TCommonDialog', 'OnShow', 'TNotifyEvent', @TCommonDialog_OnShow_Read, @TCommonDialog_OnShow_Write);
    addClassVar('TCommonDialog', 'Width', 'integer', @TCommonDialog_Width_Read, @TCommonDialog_Width_Write);
    addClassVar('TCommonDialog', 'Height', 'integer', @TCommonDialog_Height_Read, @TCommonDialog_Height_Write);
    addGlobalFunc('procedure TCommonDialog.Init(AOwner: TComponent);', @TCommonDialog_Init);
    addGlobalFunc('procedure TCommonDialog.Free();', @TCommonDialog_Free);
  end;
end;

//procedure DoTypeChange; virtual;
procedure TFileDialog_DoTypeChange(const Params: PParamArray); lape_extdecl
begin
  PFileDialog(Params^[0])^.DoTypeChange();
end;

//function Execute: boolean; override;
procedure TFileDialog_Execute(const Params: PParamArray; const Result: Pointer); lape_extdecl
Begin
  Pboolean(Result)^ := PFileDialog(Params^[0])^.Execute();
end;

//Read: property Files: TStrings read FFiles;
procedure TFileDialog_Files_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStrings(Result)^ := PFileDialog(Params^[0])^.Files;
end;

//Read: property HistoryList: TStrings read FHistoryList write SetHistoryList;
procedure TFileDialog_HistoryList_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStrings(Result)^ := PFileDialog(Params^[0])^.HistoryList;
end;

//Write: property HistoryList: TStrings read FHistoryList write SetHistoryList;
procedure TFileDialog_HistoryList_Write(const Params: PParamArray); lape_extdecl
begin
  PFileDialog(Params^[0])^.HistoryList := PStrings(Params^[1])^;
end;

//procedure IntfFileTypeChanged(NewFilterIndex: Integer);
procedure TFileDialog_IntfFileTypeChanged(const Params: PParamArray); lape_extdecl
begin
  PFileDialog(Params^[0])^.IntfFileTypeChanged(PInteger(Params^[1])^);
end;

//Read: property DefaultExt: string read FDefaultExt write SetDefaultExt;
procedure TFileDialog_DefaultExt_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PFileDialog(Params^[0])^.DefaultExt;
end;

//Write: property DefaultExt: string read FDefaultExt write SetDefaultExt;
procedure TFileDialog_DefaultExt_Write(const Params: PParamArray); lape_extdecl
begin
  PFileDialog(Params^[0])^.DefaultExt := PlpString(Params^[1])^;
end;

//Read: property FileName: String read FFileName write SetFileName;
procedure TFileDialog_FileName_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PFileDialog(Params^[0])^.FileName;
end;

//Write: property FileName: String read FFileName write SetFileName;
procedure TFileDialog_FileName_Write(const Params: PParamArray); lape_extdecl
begin
  PFileDialog(Params^[0])^.FileName := PlpString(Params^[1])^;
end;

//Read: property Filter: String read FFilter write SetFilter;
procedure TFileDialog_Filter_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PFileDialog(Params^[0])^.Filter;
end;

//Write: property Filter: String read FFilter write SetFilter;
procedure TFileDialog_Filter_Write(const Params: PParamArray); lape_extdecl
begin
  PFileDialog(Params^[0])^.Filter := PlpString(Params^[1])^;
end;

//Read: property FilterIndex: Integer read GetFilterIndex write SetFilterIndex default 1;
procedure TFileDialog_FilterIndex_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PInteger(Result)^ := PFileDialog(Params^[0])^.FilterIndex;
end;

//Write: property FilterIndex: Integer read GetFilterIndex write SetFilterIndex default 1;
procedure TFileDialog_FilterIndex_Write(const Params: PParamArray); lape_extdecl
begin
  PFileDialog(Params^[0])^.FilterIndex := PInteger(Params^[1])^;
end;

//Read: property InitialDir: string read FInitialDir write FInitialDir;
procedure TFileDialog_InitialDir_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PlpString(Result)^ := PFileDialog(Params^[0])^.InitialDir;
end;

//Write: property InitialDir: string read FInitialDir write FInitialDir;
procedure TFileDialog_InitialDir_Write(const Params: PParamArray); lape_extdecl
begin
  PFileDialog(Params^[0])^.InitialDir := PlpString(Params^[1])^;
end;

//Read: property OnHelpClicked: TNotifyEvent read FOnHelpClicked write FOnHelpClicked;
procedure TFileDialog_OnHelpClicked_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PFileDialog(Params^[0])^.OnHelpClicked;
end;

//Write: property OnHelpClicked: TNotifyEvent read FOnHelpClicked write FOnHelpClicked;
procedure TFileDialog_OnHelpClicked_Write(const Params: PParamArray); lape_extdecl
begin
  PFileDialog(Params^[0])^.OnHelpClicked := PNotifyEvent(Params^[1])^;
end;

//Read: property OnTypeChange: TNotifyEvent read FOnTypeChange write FOnTypeChange;
procedure TFileDialog_OnTypeChange_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := PFileDialog(Params^[0])^.OnTypeChange;
end;

//Write: property OnTypeChange: TNotifyEvent read FOnTypeChange write FOnTypeChange;
procedure TFileDialog_OnTypeChange_Write(const Params: PParamArray); lape_extdecl
begin
  PFileDialog(Params^[0])^.OnTypeChange := PNotifyEvent(Params^[1])^;
end;

//constructor Create();
procedure TFileDialog_Init(const Params: PParamArray); lape_extdecl
begin
  PFileDialog(Params^[0])^ := TFileDialog.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TFileDialog_Free(const Params: PParamArray); lape_extdecl
begin
  PFileDialog(Params^[0])^.Free();
end;

procedure Register_TFileDialog(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    AddClass('TFileDialog', 'TCommonDialog');

    addGlobalFunc('procedure TFileDialog.DoTypeChange(); constref;', @TFileDialog_DoTypeChange);
    addGlobalFunc('function TFileDialog.Execute(): boolean; constref;', @TFileDialog_Execute);
    addClassVar('TFileDialog', 'Files', 'TStrings', @TFileDialog_Files_Read, nil);
    addClassVar('TFileDialog', 'HistoryList', 'TStrings', @TFileDialog_HistoryList_Read, @TFileDialog_HistoryList_Write);
    addGlobalFunc('procedure TFileDialog.IntfFileTypeChanged(NewFilterIndex: Integer); constref;', @TFileDialog_IntfFileTypeChanged);
    addClassVar('TFileDialog', 'DefaultExt', 'string', @TFileDialog_DefaultExt_Read, @TFileDialog_DefaultExt_Write);
    addClassVar('TFileDialog', 'FileName', 'String', @TFileDialog_FileName_Read, @TFileDialog_FileName_Write);
    addClassVar('TFileDialog', 'Filter', 'String', @TFileDialog_Filter_Read, @TFileDialog_Filter_Write);
    addClassVar('TFileDialog', 'FilterIndex', 'Integer', @TFileDialog_FilterIndex_Read, @TFileDialog_FilterIndex_Write);
    addClassVar('TFileDialog', 'InitialDir', 'string', @TFileDialog_InitialDir_Read, @TFileDialog_InitialDir_Write);
    addClassVar('TFileDialog', 'OnHelpClicked', 'TNotifyEvent', @TFileDialog_OnHelpClicked_Read, @TFileDialog_OnHelpClicked_Write);
    addClassVar('TFileDialog', 'OnTypeChange', 'TNotifyEvent', @TFileDialog_OnTypeChange_Read, @TFileDialog_OnTypeChange_Write);
    addGlobalFunc('procedure TFileDialog.Init(AOwner: TComponent);', @TFileDialog_Init);
    addGlobalFunc('procedure TFileDialog.Free();', @TFileDialog_Free);
  end;
end;

//procedure DoFolderChange; virtual;
procedure TOpenDialog_DoFolderChange(const Params: PParamArray); lape_extdecl
begin
  POpenDialog(Params^[0])^.DoFolderChange();
end;

//procedure DoSelectionChange; virtual;
procedure TOpenDialog_DoSelectionChange(const Params: PParamArray); lape_extdecl
begin
  POpenDialog(Params^[0])^.DoSelectionChange();
end;

//procedure IntfSetOption(const AOption: TOpenOption; const AValue: Boolean);
procedure TOpenDialog_IntfSetOption(const Params: PParamArray); lape_extdecl
begin
  POpenDialog(Params^[0])^.IntfSetOption(POpenOption(Params^[1])^, PBoolean(Params^[2])^);
end;

//Read: property Options: TOpenOptions read FOptions write FOptions default DefaultOpenDialogOptions;
procedure TOpenDialog_Options_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  POpenOptions(Result)^ := POpenDialog(Params^[0])^.Options;
end;

//Write: property Options: TOpenOptions read FOptions write FOptions default DefaultOpenDialogOptions;
procedure TOpenDialog_Options_Write(const Params: PParamArray); lape_extdecl
begin
  POpenDialog(Params^[0])^.Options := POpenOptions(Params^[1])^;
end;

//Read: property OnFolderChange: TNotifyEvent read FOnFolderChange write FOnFolderChange;
procedure TOpenDialog_OnFolderChange_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
Begin
  PNotifyEvent(Result)^ := POpenDialog(Params^[0])^.OnFolderChange;
end;

//Write: property OnFolderChange: TNotifyEvent read FOnFolderChange write FOnFolderChange;
procedure TOpenDialog_OnFolderChange_Write(const Params: PParamArray); lape_extdecl
begin
  POpenDialog(Params^[0])^.OnFolderChange := PNotifyEvent(Params^[1])^;
end;

//Read: property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
procedure TOpenDialog_OnSelectionChange_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PNotifyEvent(Result)^ := POpenDialog(Params^[0])^.OnSelectionChange;
end;

//Write: property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
procedure TOpenDialog_OnSelectionChange_Write(const Params: PParamArray); lape_extdecl
begin
  POpenDialog(Params^[0])^.OnSelectionChange := PNotifyEvent(Params^[1])^;
end;

//constructor Create();
procedure TOpenDialog_Init(const Params: PParamArray); lape_extdecl
begin
  POpenDialog(Params^[0])^ := TOpenDialog.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TOpenDialog_Free(const Params: PParamArray); lape_extdecl
begin
  POpenDialog(Params^[0])^.Free();
end;

procedure Register_TOpenDialog(Compiler: TLapeCompiler);
begin
  with Compiler do
  begin
    addClass('TOpenDialog', 'TFileDialog');

    addGlobalFunc('procedure TOpenDialog.DoFolderChange(); constref;', @TOpenDialog_DoFolderChange);
    addGlobalFunc('procedure TOpenDialog.DoSelectionChange(); constref;', @TOpenDialog_DoSelectionChange);
    addClassVar('TOpenDialog', 'Options', 'TOpenOptions', @TOpenDialog_Options_Read, @TOpenDialog_Options_Write);
    addClassVar('TOpenDialog', 'OnFolderChange', 'TNotifyEvent', @TOpenDialog_OnFolderChange_Read, @TOpenDialog_OnFolderChange_Write);
    addClassVar('TOpenDialog', 'OnSelectionChange', 'TNotifyEvent', @TOpenDialog_OnSelectionChange_Read, @TOpenDialog_OnSelectionChange_Write);
    addGlobalFunc('procedure TOpenDialog.Init(TheOwner: TComponent);', @TOpenDialog_Init);
    addGlobalFunc('procedure TOpenDialog.Free();', @TOpenDialog_Free);
  end;
end;

//Read: property Color: TColor read FColor write FColor;
procedure TColorDialog_Color_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PColor(Result)^ := PColorDialog(Params^[0])^.Color;
end;

//Write: property Color: TColor read FColor write FColor;
procedure TColorDialog_Color_Write(const Params: PParamArray); lape_extdecl
begin
  PColorDialog(Params^[0])^.Color := PColor(Params^[1])^;
end;

//Read: property CustomColors: TStrings read FCustomColors write SetCustomColors;
procedure TColorDialog_CustomColors_Read(const Params: PParamArray; const Result: Pointer); lape_extdecl
begin
  PStrings(Result)^ := PColorDialog(Params^[0])^.CustomColors;
end;

//Write: property CustomColors: TStrings read FCustomColors write SetCustomColors;
procedure TColorDialog_CustomColors_Write(const Params: PParamArray); lape_extdecl
begin
  PColorDialog(Params^[0])^.CustomColors := PStrings(Params^[1])^;
end;

//constructor Create();
procedure TColorDialog_Init(const Params: PParamArray); lape_extdecl
begin
  PColorDialog(Params^[0])^ := TColorDialog.Create(PComponent(Params^[1])^);
end;

//procedure Free();
procedure TColorDialog_Free(const Params: PParamArray); lape_extdecl
begin
  PColorDialog(Params^[0])^.Free();
end;

procedure Register_TColorDialog(Compiler: TLapeCompiler);
begin
   with Compiler do
   begin
     addClass('TColorDialog', 'TCommonDialog');

     addClassVar('TColorDialog', 'Color', 'TColor', @TColorDialog_Color_Read, @TColorDialog_Color_Write);
     addClassVar('TColorDialog', 'CustomColors', 'TStrings', @TColorDialog_CustomColors_Read, @TColorDialog_CustomColors_Write);
     addGlobalFunc('procedure TColorDialog.Init(TheOwner: TComponent);', @TColorDialog_Init);
     addGlobalFunc('procedure TColorDialog.Free();', @TColorDialog_Free);
   end;
end;

procedure RegisterLCLDialogs(Compiler: TLapeCompiler);
begin
  with Compiler do
   begin
     addGlobalType('(ofReadOnly, ofOverwritePrompt,ofHideReadOnly,ofNoChangeDir,ofShowHelp,ofNoValidate,ofAllowMultiSelect,ofExtensionDifferent,ofPathMustExist,ofFileMustExist,ofCreatePrompt,ofShareAware,ofNoReadOnlyReturn,ofNoTestFileCreate,ofNoNetworkButton,ofNoLongNames,ofOldStyleDialog,ofNoDereferenceLinks,ofEnableIncludeNotify,ofEnableSizing,ofDontAddToRecent,ofForceShowHidden,ofViewDetail,ofAutoPreview)','TOpenOption');
     addGlobalType('set of TOpenOption', 'TOpenOptions');
   end;
  Register_TLCLComponent(Compiler);
  Register_TCommonDialog(Compiler);
  Register_TFileDialog(Compiler);
  Register_TOpenDialog(Compiler);
  Register_TColorDialog(Compiler);
end;

end.
