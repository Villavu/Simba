{
  Author: Raymond van Venetië and Merlijn Wajer
  Project: Simba (https://github.com/MerlijnWajer/Simba)
  License: GNU General Public License (https://www.gnu.org/licenses/gpl-3.0)

  Find in files.
}
unit simba.form_findinfiles;

{$i simba.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  SynEditMiscClasses, SynEditSearch, SynEditMouseCmds,
  simba.base,
  simba.component_synedit, simba.component_button, simba.component_edit, simba.component_buttonpanel;

type
  TLineInfo = class
  public
    isFile: Boolean;
    isResult: Boolean;
    FileName: String;
    Line: Integer;
  end;

  TResultsMemo = class(TSimbaMemo)
  protected
    function GetLineInfo(Line: Integer): TLineInfo;
  public
    procedure GetWordBoundsAtRowCol(const XY: TPoint; out StartX, EndX: integer); override;

    procedure AddFileLine(FileName: String);
    procedure AddResultLine(FileName: String; Line: Integer; LineStr: String);

    property LineInfo[Line: Integer]: TLineInfo read GetLineInfo;
  end;

  TSimbaFindInFilesForm = class(TForm)
    MatchesLabel: TLabel;
    PanelSearchButton: TPanel;
    PanelDivider: TPanel;
    PanelTop: TPanel;
    PanelSelectLocation: TPanel;
    PanelOptions: TPanel;
    PanelLocation: TPanel;
    PanelMatches: TPanel;
    DialogSelectDir: TSelectDirectoryDialog;

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DialogSelectDirClose(Sender: TObject);
  private
    Searcher: TSynEditSearch;
    ButtonFind: TSimbaButton;
    ButtonOpenAllFiles: TSimbaButton;
    ButtonSelectDir: TSimbaButton;
    EditLocation: TSimbaLabeledEdit;
    MemoResults: TResultsMemo;
    EditSearch: TSimbaLabeledEdit;
    ButtonPanel: TSimbaButtonPanel;
    CheckboxOptions: TSimbaCheckButtonGroup;

    FSearchString: String;
    FSearchLocation: String;
    FSearchSubDirs: Boolean;
    FSearchCaseSens: Boolean;
    FSearchWholeWords: Boolean;

    procedure DoSearching;

    procedure DoButtonOpenAllFiles(Sender: TObject);
    procedure DoButtonFindClick(Sender: TObject);
    procedure DoSelectDirButtonClick(Sender: TObject);
    procedure DoLineMarkup(Sender: TObject; Line: integer; var Special: boolean; Markup: TSynSelectedColor);
    procedure DoAllowMouseLink(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
    procedure DoMouseLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  end;

var
  SimbaFindInFilesForm: TSimbaFindInFilesForm;

implementation

{$R *.lfm}

uses
  LazFileUtils,
  simba.threading, simba.ide_theme, simba.fs, simba.settings, simba.form_tabs;

procedure TResultsMemo.AddFileLine(FileName: String);
var
  Info: TLineInfo;
begin
  Info := TLineInfo.Create();
  Info.isFile := True;
  Info.FileName := FileName;

  Lines.AddObject(FileName, Info);
end;

procedure TResultsMemo.AddResultLine(FileName: String; Line: Integer; LineStr: String);
var
  Info: TLineInfo;
begin
  Info := TLineInfo.Create();
  Info.isResult := True;
  Info.FileName := FileName;
  Info.Line := Line;

  Lines.AddObject('  Line ' + Line.ToString().PadRight(4) + ' -> ' + LineStr.Trim(), Info);
end;

function TResultsMemo.GetLineInfo(Line: Integer): TLineInfo;
begin
  if (Line >= 0) and (Line < Lines.Count) then
    Result := TLineInfo(Lines.Objects[Line])
  else
    Result := nil;
end;

procedure TResultsMemo.GetWordBoundsAtRowCol(const XY: TPoint; out StartX, EndX: integer);
var
  Line: String;
begin
  if (XY.Y >= 1) and (XY.Y <= Lines.Count) then
  begin
    Line := Lines[XY.Y - 1];

    StartX := 1;
    while (StartX < Length(Line)) and (Line[StartX] <= #32) do
      Inc(StartX);
    EndX := Length(Line);
    while (EndX > StartX) and (Line[EndX] <= #32) do
      Dec(EndX);
    Inc(EndX);
  end;
end;

procedure TSimbaFindInFilesForm.DoButtonFindClick(Sender: TObject);
begin
  FSearchString := EditSearch.Edit.Text;
  FSearchLocation := EditLocation.Edit.Text;

  FSearchSubDirs := CheckboxOptions.Checked[0];
  FSearchCaseSens := CheckboxOptions.Checked[1];
  FSearchWholeWords := CheckboxOptions.Checked[2];

  RunInThread(@DoSearching);
end;

procedure TSimbaFindInFilesForm.FormCreate(Sender: TObject);
begin
  Color := SimbaTheme.ColorFrame;
  Font.Color := SimbaTheme.ColorFont;
  PanelDivider.Color := SimbaTheme.ColorScrollBarActive;

  ButtonPanel := TSimbaButtonPanel.Create(Self);
  ButtonPanel.ButtonCancel.Hide();
  ButtonPanel.ButtonOk.Caption := 'Close';
  ButtonPanel.Parent := Self;
  ButtonPanel.BorderSpacing.Around := 5;

  ButtonFind := TSimbaButton.Create(Self);
  ButtonFind.Parent := PanelSearchButton;
  ButtonFind.Caption := 'Find';
  ButtonFind.BorderSpacing.Around := 5;
  ButtonFind.XPadding := 20;
  ButtonFind.OnClick := @DoButtonFindClick;

  CheckboxOptions := TSimbaCheckButtonGroup.Create(Self);
  CheckboxOptions.Parent := PanelOptions;
  CheckboxOptions.Align := alClient;
  CheckboxOptions.Add('Search Subdirectories');
  CheckboxOptions.Add('Case Sensitive');
  CheckboxOptions.Add('Whole Words Only');

  EditLocation := TSimbaLabeledEdit.Create(Self);
  EditLocation.Parent := PanelLocation;
  EditLocation.Align := alClient;
  EditLocation.Caption := 'Where: ';
  EditLocation.Color := SimbaTheme.ColorFrame;
  EditLocation.Edit.ColorBorder := SimbaTheme.ColorScrollBarActive;

  ButtonSelectDir := TSimbaButton.Create(Self);
  ButtonSelectDir.Parent := PanelSelectLocation;
  ButtonSelectDir.BorderSpacing.Around := 5;
  ButtonSelectDir.Align := alClient;
  ButtonSelectDir.Image := ESimbaButtonImage.SELECT_DIR;
  ButtonSelectDir.OnClick := @DoSelectDirButtonClick;

  ButtonOpenAllFiles := TSimbaButton.Create(Self);
  ButtonOpenAllFiles.Parent := ButtonPanel;
  ButtonOpenAllFiles.Caption := 'Open all files';
  ButtonOpenAllFiles.BorderSpacing.Around := 5;
  ButtonOpenAllFiles.XPadding := Scale96ToScreen(10);
  ButtonOpenAllFiles.OnClick := @DoButtonOpenAllFiles;

  EditSearch := TSimbaLabeledEdit.Create(Self);
  EditSearch.Parent := PanelLocation;
  EditSearch.Align := alTop;
  EditSearch.Caption := 'Search: ';
  EditSearch.Color := SimbaTheme.ColorFrame;
  EditSearch.Edit.ColorBorder := SimbaTheme.ColorScrollBarActive;

  MemoResults := TResultsMemo.Create(Self, False);
  MemoResults.ReadOnly := True;
  MemoResults.OnSpecialLineMarkup := @DoLineMarkup;
  MemoResults.OnMouseLink := @DoAllowMouseLink;
  MemoResults.OnClickLink := @DoMouseLinkClick;
  MemoResults.Parent := PanelMatches;
  MemoResults.Align := alClient;
  MemoResults.MaxUndo := 0;
  MemoResults.MouseLinkColor.Style := [];
  MemoResults.MouseLinkColor.Foreground := $CC6600;
  MemoResults.MouseOptions := [emUseMouseActions];
  MemoResults.ResetMouseActions();
  with MemoResults.MouseTextActions.Add() do
    Command := emcMouseLink;

  Searcher := TSynEditSearch.Create();

  Width := Scale96ToScreen(SimbaSettings.General.FindInFilesWidth.Value);
  Height := Scale96ToScreen(SimbaSettings.General.FindInFilesHeight.Value);
  EditSearch.Edit.Text := SimbaSettings.General.FindInFilesSearch.Value;
  EditLocation.Edit.Text := SimbaSettings.General.FindInFilesLocation.Value;

  CheckboxOptions.Checked[0] := SimbaSettings.General.FindInFilesSubDirs.Value;
  CheckboxOptions.Checked[1] := SimbaSettings.General.FindInFilesCaseSens.Value;
  CheckboxOptions.Checked[2] := SimbaSettings.General.FindInFilesWholeWords.Value;

  PanelTop.AutoSize := True;
  PanelOptions.AutoSize := True;
  PanelLocation.AutoSize := True;
  PanelSelectLocation.AutoSize := True;
  PanelSearchButton.AutoSize := True;
end;

procedure TSimbaFindInFilesForm.FormDestroy(Sender: TObject);
begin
  if Assigned(Searcher) then
    FreeAndNil(Searcher);
end;

procedure TSimbaFindInFilesForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SimbaSettings.General.FindInFilesWidth.Value := ScaleScreenTo96(Width);
  SimbaSettings.General.FindInFilesHeight.Value := ScaleScreenTo96(Height);
  SimbaSettings.General.FindInFilesSearch.Value := EditSearch.Edit.Text;
  SimbaSettings.General.FindInFilesLocation.Value := EditLocation.Edit.Text;
  SimbaSettings.General.FindInFilesSubDirs.Value := CheckboxOptions.Checked[0];
  SimbaSettings.General.FindInFilesCaseSens.Value := CheckboxOptions.Checked[1];
  SimbaSettings.General.FindInFilesWholeWords.Value := CheckboxOptions.Checked[2];
end;

procedure TSimbaFindInFilesForm.DialogSelectDirClose(Sender: TObject);
begin
  EditLocation.Edit.Text := DialogSelectDir.FileName;
end;

procedure TSimbaFindInFilesForm.DoSearching;
var
  Total: Integer;

  procedure BeginUpdate;
  var
    I: Integer;
  begin
    Total := 0;

    MatchesLabel.Caption := 'Searching...';
    ButtonFind.Enabled := False;
    MemoResults.Visible := False;
    MemoResults.BeginUpdate(False);
    for I := 0 to MemoResults.Lines.Count - 1 do
      if Assigned(MemoResults.Lines.Objects[I]) then
        MemoResults.Lines.Objects[I].Free();
    MemoResults.Clear();

    Application.ProcessMessages();
  end;

  procedure EndUpdate;
  begin
    MemoResults.EndUpdate();
    ButtonFind.Enabled := True;
    MemoResults.Visible := True;
    MatchesLabel.Caption := 'Matches: ' + IntToStr(Total);
  end;

var
  Lines: TStringList;
  SearchStart, SearchEnd, FoundStart, FoundEnd: TPoint;
  Added: Boolean;
  FileName: String;
begin
  RunInMainThread(@BeginUpdate);

  Searcher.Sensitive := FSearchCaseSens;
  Searcher.Whole := FSearchWholeWords;
  Searcher.Pattern := FSearchString;

  Lines := TStringList.Create();

  for FileName in TSimbaDir.DirListFiles(FSearchLocation, FSearchSubDirs) do
    if FileIsText(FileName) then
    begin
      try
        Lines.LoadFromFile(FileName);
      except
        Lines.Clear();
      end;

      if (Lines.Count > 0) then
      begin
        SearchStart.X := 1;
        SearchStart.Y := 1;

        SearchEnd.Y := Lines.Count;
        SearchEnd.X := Length(Lines[Lines.Count - 1]) + 1;

        Added := False;
        while Searcher.FindNextOne(Lines, SearchStart, SearchEnd, FoundStart, FoundEnd) do
        begin
          Inc(Total);
          if not Added then
          begin
            MemoResults.AddFileLine(FileName);
            Added := True;
          end;
          MemoResults.AddResultLine(FileName, FoundStart.Y, Lines[FoundStart.Y - 1]);

          SearchStart := FoundEnd;
        end;

        if Added then
          MemoResults.Lines.Add('');
      end;
    end;

  Lines.Free();

  RunInMainThread(@EndUpdate);
end;

procedure TSimbaFindInFilesForm.DoButtonOpenAllFiles(Sender: TObject);
var
  Line: Integer;
  LineInfo: TLineInfo;
begin
  for Line := 0 to MemoResults.Lines.Count - 1 do
  begin
    LineInfo := MemoResults.GetLineInfo(Line);
    if Assigned(LineInfo) and LineInfo.isFile then
      SimbaTabsForm.Open(LineInfo.FileName);
  end;
end;

procedure TSimbaFindInFilesForm.DoSelectDirButtonClick(Sender: TObject);
begin
  DialogSelectDir.InitialDir := Application.Location;
  DialogSelectDir.Execute();
end;

procedure TSimbaFindInFilesForm.DoLineMarkup(Sender: TObject; Line: integer; var Special: boolean; Markup: TSynSelectedColor);
begin
  Special := Assigned(MemoResults.LineInfo[Line - 1]) and MemoResults.LineInfo[Line - 1].isFile;

  if Special then
  begin
    Markup.Background := clNone;
    Markup.Foreground := $50D8FB;
  end;
end;

procedure TSimbaFindInFilesForm.DoAllowMouseLink(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
begin
  AllowMouseLink := Assigned(MemoResults.LineInfo[Y - 1]) and MemoResults.LineInfo[Y - 1].isResult;
end;

procedure TSimbaFindInFilesForm.DoMouseLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Line: Integer;
  LineInfo: TLineInfo;
begin
  Line := MemoResults.PixelsToRowColumn(TPoint.Create(X, Y)).Y;

  LineInfo := MemoResults.LineInfo[Line - 1];
  if Assigned(LineInfo) and SimbaTabsForm.Open(LineInfo.FileName) then
    SimbaTabsForm.CurrentTab.GotoLine(LineInfo.Line);
end;

end.

