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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  SynEditMiscClasses, SynEditSearch, SynEditMouseCmds,
  simba.base,
  simba.ide_events,
  simba.component_synedit,
  simba.component_button,
  simba.component_edit,
  simba.component_buttonpanel,
  simba.component_divider;

type
  TResultsMemo = class(TSimbaMemo)
  protected type
    TLineInfo = class
    public
      isFile: Boolean;
      isResult: Boolean;
      FileName: String;
      Line: Integer;
    end;
  protected
    procedure DoLineMarkup(Sender: TObject; Line: integer; var Special: boolean; AMarkup: TSynSelectedColor);
    procedure DoAllowMouseLink(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
    procedure DoMouseLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    function GetLineInfo(Line: Integer): TLineInfo;
  public
    constructor Create(AOwner: TComponent; LineWrapping: Boolean); override;

    procedure GetWordBoundsAtRowCol(const XY: TPoint; out StartX, EndX: integer); override;

    procedure AddFileLine(FileName: String);
    procedure AddResultLine(FileName: String; Line: Integer; LineStr: String);

    property LineInfo[Line: Integer]: TLineInfo read GetLineInfo;
  end;

  TFindInFilesTab = class(TCustomControl)
  protected
    FEditSearch: TSimbaLabeledEdit;
    FEditLocation: TSimbaLabeledEdit;
    FCheckboxOptions: TSimbaCheckButtonGroup;
    FMemoResults: TResultsMemo;
    FButtonFind: TSimbaButton;
    FButtonSelectDir: TSimbaButton;
    FButtonOpenAll: TSimbaButton;
    FMatchesLabel: TLabel;
    FLabelOpenAll: TLAbel;

    FSearchString: String;
    FSearchLocation: String;
    FSearchSubDirs: Boolean;
    FSearchCaseSens: Boolean;
    FSearchWholeWords: Boolean;

    procedure DoSearching;
    procedure DoClickOpenAll(Sender: TObject);
    procedure DoLabelOpenAllHover(Sender: TObject);
    procedure DoButtonFindClick(Sender: TObject);
    procedure DoSelectDirButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;

    procedure OpenAllFiles;
  end;

  TSimbaFindInFilesForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    Tab: TFindInFilesTab;
    ButtonPanel: TSimbaButtonPanel;

    procedure DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
  end;

var
  SimbaFindInFilesForm: TSimbaFindInFilesForm;

implementation

{$R *.lfm}

uses
  Menus,
  AnchorDocking,
  simba.ide_docking,
  simba.ide_controller,
  simba.threading,
  simba.component_theme,
  simba.fs,
  simba.settings;

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

procedure TResultsMemo.DoLineMarkup(Sender: TObject; Line: integer; var Special: boolean; AMarkup: TSynSelectedColor);
begin
  Special := Assigned(LineInfo[Line - 1]) and LineInfo[Line - 1].isFile;

  if Special then
  begin
    AMarkup.Background := clNone;
    AMarkup.Foreground := $50D8FB;
  end;
end;

procedure TResultsMemo.DoAllowMouseLink(Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
begin
  AllowMouseLink := Assigned(LineInfo[Y - 1]) and LineInfo[Y - 1].isResult;
end;

procedure TResultsMemo.DoMouseLinkClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Line: Integer;
begin
  Line := PixelsToRowColumn(TPoint.Create(X, Y)).Y;
  if Assigned(LineInfo[Line - 1]) then
    SimbaController.OpenInTab(
      LineInfo[Line - 1].FileName,
      1,
      LineInfo[Line - 1].Line
    );
end;

function TResultsMemo.GetLineInfo(Line: Integer): TLineInfo;
begin
  if (Line >= 0) and (Line < Lines.Count) then
    Result := TLineInfo(Lines.Objects[Line])
  else
    Result := nil;
end;

constructor TResultsMemo.Create(AOwner: TComponent; LineWrapping: Boolean);
begin
  inherited Create(AOwner, LineWrapping);

  OnSpecialLineMarkup := @DoLineMarkup;
  OnMouseLink := @DoAllowMouseLink;
  OnClickLink := @DoMouseLinkClick;
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

procedure TFindInFilesTab.DoSearching;
var
  Total, Files: Integer;

  procedure BeginUpdate;
  var
    I: Integer;
  begin
    Total := 0;
    Files := 0;

    FMatchesLabel.Caption := 'Searching...';
    FLabelOpenAll.Caption := '';
    FButtonFind.Enabled := False;
    FMemoResults.Visible := False;
    FMemoResults.BeginUpdate(False);
    for I := 0 to FMemoResults.Lines.Count - 1 do
      if Assigned(FMemoResults.Lines.Objects[I]) then
        FMemoResults.Lines.Objects[I].Free();
    FMemoResults.Clear();

    Application.ProcessMessages();
  end;

  procedure EndUpdate;
  begin
    FMemoResults.EndUpdate();
    FButtonFind.Enabled := True;
    FMemoResults.Visible := True;
    if (Total > 0) then
      FMatchesLabel.Caption := 'Matches: ' + IntToStr(Total) + ' in ' + IntToStr(Files) + ' files'
    else
      FMatchesLabel.Caption := 'Matches: 0';
    FLabelOpenAll.Caption := '(click to open all files)';
  end;

var
  Searcher: TSynEditSearch;
  Lines: TStringList;
  SearchStart, SearchEnd, FoundStart, FoundEnd: TPoint;
  Added: Boolean;
  FileName: String;
begin
  RunInMainThread(@BeginUpdate);

  Searcher := TSynEditSearch.Create();
  Searcher.Sensitive := FSearchCaseSens;
  Searcher.Whole := FSearchWholeWords;
  Searcher.Pattern := FSearchString;

  Lines := TStringList.Create();

  for FileName in TSimbaDir.DirListFiles(FSearchLocation, FSearchSubDirs) do
    if TSimbaFile.FileIsText(FileName) then
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
            Inc(Files);
            FMemoResults.AddFileLine(FileName);
            Added := True;
          end;
          FMemoResults.AddResultLine(FileName, FoundStart.Y, Lines[FoundStart.Y - 1]);

          SearchStart := FoundEnd;
        end;

        if Added then
          FMemoResults.Lines.Add('');
      end;
    end;

  Searcher.Free();
  Lines.Free();

  RunInMainThread(@EndUpdate);
end;

procedure TFindInFilesTab.DoClickOpenAll(Sender: TObject);
begin
  OpenAllFiles();
end;

procedure TFindInFilesTab.DoLabelOpenAllHover(Sender: TObject);
begin
  TLabel(Sender).Font.Underline := TLabel(Sender).MouseInClient;
end;

procedure TFindInFilesTab.DoButtonFindClick(Sender: TObject);
begin
  FSearchString := FEditSearch.Edit.Text;
  FSearchLocation := FEditLocation.Edit.Text;

  FSearchSubDirs := FCheckboxOptions.Checked[0];
  FSearchCaseSens := FCheckboxOptions.Checked[1];
  FSearchWholeWords := FCheckboxOptions.Checked[2];

  RunInThread(@DoSearching);
end;

procedure TFindInFilesTab.DoSelectDirButtonClick(Sender: TObject);
var
  InitialDir, Dir: String;
begin
  InitialDir := FEditLocation.Edit.Text;
  if (InitialDir = '') then
    InitialDir := Application.Location;
  if SelectDirectory('Select location', InitialDir, Dir) then
    FEditLocation.Edit.Text := Dir;
end;

constructor TFindInFilesTab.Create(AOwner: TComponent);
var
  LabelContainer: TCustomControl;
  MemoContainer: TCustomControl;
begin
  inherited Create(AOwner);

  LabelContainer := TCustomControl.Create(Self);
  LabelContainer.Parent := Self;
  LabelContainer.Align := alTop;
  LabelContainer.AutoSize := True;

  FLabelOpenAll := TLabel.Create(Self);
  FLabelOpenAll.Parent := LabelContainer;
  FLabelOpenAll.Align := alLeft;
  FLabelOpenAll.Caption := '';
  FLabelOpenAll.OnClick := @DoClickOpenAll;
  FLabelOpenAll.OnMouseEnter := @DoLabelOpenAllHover;
  FLabelOpenAll.OnMouseLeave := @DoLabelOpenAllHover;
  FLabelOpenAll.BorderSpacing.Left := 5;

  FMatchesLabel := TLabel.Create(Self);
  FMatchesLabel.Parent := LabelContainer;
  FMatchesLabel.Align := alLeft;
  FMatchesLabel.Caption := 'Matches:';
  FMatchesLabel.BorderSpacing.Bottom := 5;

  with TSimbaDivider.Create(Self) do
  begin
    Parent := Self;
    Align := alTop;
    BorderSpacing.Top := 10;
    BorderSpacing.Bottom := 10;
  end;

  FButtonFind := TSimbaButton.Create(Self);
  FButtonFind.Parent := Self;
  FButtonFind.Align := alTop;
  FButtonFind.Caption := 'Find';
  FButtonFind.XPadding := 20;
  FButtonFind.Constraints.MaxWidth := 100;
  FButtonFind.OnClick := @DoButtonFindClick;

  FCheckboxOptions := TSimbaCheckButtonGroup.Create(Self);
  FCheckboxOptions.Parent := Self;
  FCheckboxOptions.Align := alTop;
  FCheckboxOptions.Add('Search Subdirectories');
  FCheckboxOptions.Add('Case Sensitive');
  FCheckboxOptions.Add('Whole Words Only');
  FCheckboxOptions.BorderSpacing.Top := 5;

  FButtonSelectDir := TSimbaButton.Create(Self);
  FButtonSelectDir.Image := ESimbaButtonImage.SELECT_DIR;
  FButtonSelectDir.OnClick := @DoSelectDirButtonClick;

  FEditLocation := TSimbaLabeledEdit.Create(Self);
  FEditLocation.Parent := Self;
  FEditLocation.Align := alTop;
  FEditLocation.Caption := 'Location:';
  FEditLocation.LabelMeasure := 'Location:';
  FEditLocation.Color := SimbaComponentTheme.ColorFrame;
  FEditLocation.Edit.ColorBorder := SimbaComponentTheme.ColorScrollBarActive;
  FEditLocation.BorderSpacing.Top := 5;
  FEditLocation.Button := FButtonSelectDir;
  FEditLocation.TabOrder := 1;

  FEditSearch := TSimbaLabeledEdit.Create(Self);
  FEditSearch.Parent := Self;
  FEditSearch.Align := alTop;
  FEditSearch.Caption := 'Search:';
  FEditSearch.LabelMeasure := 'Location:';
  FEditSearch.Color := SimbaComponentTheme.ColorFrame;
  FEditSearch.Edit.ColorBorder := SimbaComponentTheme.ColorScrollBarActive;
  FEditSearch.TabOrder := 0;

  MemoContainer := TCustomControl.Create(Self);
  MemoContainer.Parent := Self;
  MemoContainer.Align := alClient;
  MemoContainer.BorderSpacing.Top := 5;

  FMemoResults := TResultsMemo.Create(Self, False);
  FMemoResults.Parent := MemoContainer;
  FMemoResults.ReadOnly := True;
  FMemoResults.Align := alClient;
  FMemoResults.MaxUndo := 0;
  FMemoResults.MouseLinkColor.Style := [];
  FMemoResults.MouseLinkColor.Foreground := $CC6600;
  FMemoResults.MouseOptions := [emUseMouseActions];
  FMemoResults.ResetMouseActions();
  with FMemoResults.MouseTextActions.Add() do
    Command := emcMouseLink;
end;

procedure TFindInFilesTab.OpenAllFiles;
var
  Line: Integer;
begin
  for Line := 0 to FMemoResults.Lines.Count - 1 do
    if Assigned(FMemoResults.LineInfo[Line]) and FMemoResults.LineInfo[Line].isFile then
      SimbaController.OpenInTab(FMemoResults.LineInfo[Line].FileName);
end;

procedure TSimbaFindInFilesForm.FormCreate(Sender: TObject);
begin
  Color := SimbaComponentTheme.ColorFrame;
  Font.Color := SimbaComponentTheme.ColorFont;

  ButtonPanel := TSimbaButtonPanel.Create(Self);
  ButtonPanel.Parent := Self;
  ButtonPanel.ButtonCancel.Hide();
  ButtonPanel.ButtonOk.Caption := 'Close';
  ButtonPanel.BorderSpacing.Around := 5;

  Tab := TFindInFilesTab.Create(Self);
  Tab.Parent := Self;
  Tab.Align := alClient;
  Tab.BorderSpacing.Around := 5;

  Width  := Scale96ToScreen(SimbaSettings.General.FindInFilesWidth.Value);
  Height := Scale96ToScreen(SimbaSettings.General.FindInFilesHeight.Value);

  Tab.FEditSearch.Edit.Text       := SimbaSettings.General.FindInFilesSearch.Value;
  Tab.FEditLocation.Edit.Text     := SimbaSettings.General.FindInFilesLocation.Value;
  Tab.FCheckboxOptions.Checked[0] := SimbaSettings.General.FindInFilesSubDirs.Value;
  Tab.FCheckboxOptions.Checked[1] := SimbaSettings.General.FindInFilesCaseSens.Value;
  Tab.FCheckboxOptions.Checked[2] := SimbaSettings.General.FindInFilesWholeWords.Value;

  SimbaEvents.Register(Self, @DoSimbaEvent, [ESimbaEvent.ACTION_FIND_IN_FILES]);
end;

procedure TSimbaFindInFilesForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  with Tab do
  begin
    SimbaSettings.General.FindInFilesWidth.Value  := ScaleScreenTo96(Width);
    SimbaSettings.General.FindInFilesHeight.Value := ScaleScreenTo96(Height);

    SimbaSettings.General.FindInFilesSearch.Value     := FEditSearch.Edit.Text;
    SimbaSettings.General.FindInFilesLocation.Value   := FEditLocation.Edit.Text;
    SimbaSettings.General.FindInFilesSubDirs.Value    := FCheckboxOptions.Checked[0];
    SimbaSettings.General.FindInFilesCaseSens.Value   := FCheckboxOptions.Checked[1];
    SimbaSettings.General.FindInFilesWholeWords.Value := FCheckboxOptions.Checked[2];
  end;
end;

procedure TSimbaFindInFilesForm.DoSimbaEvent(Event: ESimbaEvent; Data: Pointer);
begin
  case Event of
    ESimbaEvent.ACTION_FIND_IN_FILES:
      SimbaDocking.Show(Self);
  end;
end;

end.

