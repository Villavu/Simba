{
	This file is part of the Mufasa Macro Library (MML)
	Copyright (c) 2009-2012 by Raymond van Venetië and Merlijn Wajer

    MML is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    MML is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with MML.  If not, see <http://www.gnu.org/licenses/>.

	See the file COPYING, included in this distribution,
	for details about the copyright.

    Frame Function List form for Simba
}
unit framefunctionlist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, MufasaBase,Forms, ComCtrls, StdCtrls, Controls,
  ExtCtrls, Buttons,mmisc,v_ideCodeInsight, newsimbasettings;

type

  { TFillThread }
  TFillThread = class(TThread)
  public
    Analyzer: TCodeInsight;
    MS: TMemoryStream;
    IncludesNode, PluginsNode, ScriptNode: TTreeNode;
    LastIncludeCount: PLongInt;
    procedure Update;
    procedure Execute; override;
  end;

  { TFunctionListFrame }
  TFunctionListFrame = class(TFrame)
    editSearchList: TEdit;
    FunctionList: TTreeView;
    FunctionListLabel: TLabel;
    CloseButton: TSpeedButton;
    ClearSearch: TSpeedButton;
    Panel1: TPanel;
    procedure ClearSearchClick(Sender: TObject);
    procedure editSearchListChange(Sender: TObject);
    procedure FillThreadTerminate(Sender: TObject);
    procedure FrameEndDock(Sender, Target: TObject; X, Y: Integer);
    procedure FunctionListDblClick(Sender: TObject);
    procedure FunctionListDeletion(Sender: TObject; Node: TTreeNode);
    procedure FunctionListLabelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FunctionListMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DockFormOnClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure CloseButtonClick(Sender: TObject);
    procedure FunctionListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FFilterTree: TTreeView;
    FLastScript: string;
    FLastInterp: Integer;
    Filtering: boolean;
    FillThread: TFillThread;
    procedure FilterTreeVis(Vis : boolean);
    function GetFilterTree: TTreeView;
    { private declarations }
  public
    DraggingNode: TTreeNode;
    ScriptNode: TTreeNode;
    PluginsNode: TTreeNode;
    IncludesNode: TTreeNode;
    InCodeCompletion: boolean;
    CompletionCaret: TPoint;
    StartWordCompletion: TPoint;
    CompletionLine: string;
    CompletionStart: string;
    LastIncludeCount: LongInt;
    procedure Terminate;
    property FilterTree: TTreeView read GetFilterTree;
    procedure LoadScriptTree(Script: String; Force: Boolean = False);
    function Find(Next: boolean; backwards: boolean = false) : boolean;
    { public declarations }
  end; 

  TMethodInfo = packed record
    MethodStr, Filename: PChar;
    BeginPos, endpos: integer;
  end;
  PMethodInfo = ^TMethodInfo;

implementation

uses
  SimbaUnit, Graphics, stringutil, simpleanalyzer, v_ideCodeParser, lclintf, dynlibs;

{ TFunctionListFrame }

procedure TFunctionListFrame.editSearchListChange(Sender: TObject);
begin
  Find(false);
end;

procedure TFunctionListFrame.ClearSearchClick(Sender: TObject);
begin
  editSearchList.Text := '';
  Find(false);
end;

procedure TFunctionListFrame.FillThreadTerminate(Sender: TObject);
begin
  FillThread.Analyzer.Free;
  ScriptNode.Expand(true);
  FunctionList.EndUpdate;
  if Filtering then
  begin
    FilterTreeVis(True);
    Find(false,false);
  end;
  FillThread := nil;
end;

procedure TFunctionListFrame.FrameEndDock(Sender, Target: TObject; X, Y: Integer);
begin
  if (Target is TPanel) then
  begin
     SimbaForm.SplitterFunctionList.Visible := true;
     CloseButton.Visible:= true;
  end
  else if (Target is TCustomDockForm) then
  begin
    TCustomDockForm(Target).Caption := 'Functionlist';
    TCustomDockForm(Target).OnClose := @DockFormOnClose;
    SimbaForm.SplitterFunctionList.Visible:= false;
    CloseButton.Visible:= false;
  end;
end;

procedure TFunctionListFrame.FunctionListDblClick(Sender: TObject);
  procedure OpenDocs(MethodInfo: TMethodInfo);
  var
    Arr: array of string;
  begin
    Arr := Explode('/', Copy(MethodInfo.Filename, 6, Length(MethodInfo.Filename) - 5));
    if (Length(Arr) = 2) then
      OpenURL(Format('http://docs.villavu.com/simba/scriptref/%s.html#%s', [Arr[0], Arr[1]]));
  end;
var
  Node: TTreeNode;
  MethodInfo: TMethodInfo;
begin
  if FilterTree.Visible then
    Node := FilterTree.Selected
  else
    node := FunctionList.Selected;

  if (node<> nil) and (node.Level > 0) and (node.Data <> nil) then
    if InCodeCompletion then
    begin
      SimbaForm.CurrScript.SynEdit.InsertTextAtCaret( GetMethodName(PMethodInfo(node.Data)^.MethodStr,true));
      SimbaForm.RefreshTab;
    end
    else
    begin
      MethodInfo := PMethodInfo(node.Data)^;
      if (DraggingNode = node) and (MethodInfo.BeginPos >= 0) then
      begin
        if (MethodInfo.Filename <> nil) and (MethodInfo.Filename <> '') then
        begin
          case Copy(MethodInfo.Filename, 1, 5) of
            'docs:': OpenDocs(MethodInfo);
            else
              if (FileExistsUTF8(MethodInfo.Filename)) then
                SimbaForm.LoadScriptFile(MethodInfo.Filename,true,true)
              else
                Exit;
          end;
        end;


        if (MethodInfo.BeginPos > 0) then
        begin
          SimbaForm.CurrScript.SynEdit.SelStart := MethodInfo.BeginPos + 1;
          SimbaForm.CurrScript.SynEdit.SelEnd := MethodInfo.EndPos + 1;
        end;
      end;
    end;
end;

procedure TFunctionListFrame.FunctionListDeletion(Sender: TObject;
  Node: TTreeNode);
var
  MethodInfo : PMethodInfo;
begin
  if node.data <> nil then
  begin
    MethodInfo := PMethodInfo(Node.data);
    if MethodInfo^.MethodStr <> nil then
      StrDispose(MethodInfo^.MethodStr);
    if MethodInfo^.FileName <> nil then
      StrDispose(MethodInfo^.filename);
    Freemem(node.data,sizeof(TMethodInfo));
  end;
end;

procedure TFunctionListFrame.FunctionListLabelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Self.DragKind := dkDock;
  Self.BeginDrag(false, 40);
end;

procedure TFunctionListFrame.DockFormOnClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
  SimbaForm.MenuItemFunctionList.Checked := False;
end;

procedure TFunctionListFrame.CloseButtonClick(Sender: TObject);
begin
  self.Hide;
  SimbaForm.MenuItemFunctionList.Checked := False;
end;

procedure TFunctionListFrame.FunctionListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   N: TTreeNode;
   MethodInfo : TMethodInfo;
begin
  if InCodeCompletion then
  begin;
    mDebugLn('Not yet implemented');
    exit;
  end;
  if not (Sender is TTreeView) then
    exit;
  N := TTreeView(Sender).GetNodeAt(x, y);
  if(N = nil)then
    exit;
end;

procedure TFunctionListFrame.FilterTreeVis(Vis: boolean);
begin
  FunctionList.Visible := not Vis;
  FilterTree.Visible := Vis;
end;

function TFunctionListFrame.GetFilterTree: TTreeView;
begin
  Result := FFilterTree;

  if Assigned(Result) then
    Exit;

  FFilterTree := TTreeView.Create(Self);
  FFilterTree.Parent := Self;
  FFilterTree.Visible := false;
  FFilterTree.SetBounds(FunctionList.Left,FunctionList.Top,FunctionList.Width,FunctionList.Height);
  FFilterTree.Align := alClient;
  FFilterTree.ReadOnly:= True;
  FFilterTree.ScrollBars:= ssAutoBoth;
  FFilterTree.Images := FunctionList.Images;
  FFilterTree.OnMouseDown := FunctionList.OnMouseDown;
  FFilterTree.OnMouseUp := FunctionList.OnMouseUp;
  FFilterTree.OnChange := FunctionList.OnChange;
  FFilterTree.OnExit := FunctionList.OnExit;
  FFilterTree.OnDblClick := FunctionList.OnDblClick;

  Result := FFilterTree;

  //We do not want to delete the data from the FilterTree
  //FilterTree.OnDeletion := FunctionList.OnDeletion;
end;

procedure TFunctionListFrame.LoadScriptTree(Script: String; Force: Boolean = False);
begin
  if script = '' then
    exit;
  if ScriptNode = nil then
    exit;
  if FillThread <> nil then {Already busy filling!}
    exit;
  if ((FLastScript = Script) and (FLastInterp = SimbaSettings.Interpreter._Type.Value)) and (not Force) then
    exit;
  if SimbaForm.CurrScript = nil then
    exit;
  FLastScript := Script;
  FLastInterp := SimbaSettings.Interpreter._Type.Value;
  Filtering := FilterTree.Visible;
  if FilterTree.Visible then
    FilterTreeVis(false);
  FunctionList.BeginUpdate;
  ScriptNode.DeleteChildren;
  FillThread := TFillThread.Create(true);
  FillThread.Analyzer := TCodeInsight.Create;
  with FillThread,FillThread.Analyzer do
  begin
    OnFindInclude := @SimbaForm.OnCCFindInclude;
    OnLoadLibrary := @SimbaForm.OnCCLoadLibrary;
    FileName := SimbaForm.CurrScript.ScriptFile;
    MS := TMemoryStream.Create;
    MS.Write(Script[1], Length(script));
    OnTerminate := @FillThreadTerminate;
    FreeOnTerminate := True;
    LastIncludeCount := @Self.LastIncludeCount;
    FillThread.ScriptNode := self.ScriptNode;
    FillThread.PluginsNode := self.PluginsNode;
    FillThread.IncludesNode := self.IncludesNode;
  end;
  FillThread.Resume;
 //See FillThreadTerminate for the rest of this procedure
end;

function TFunctionListFrame.Find(Next : boolean; backwards : boolean = false) : boolean;
var
  Start,Len,i,ii,index,posi,c: Integer;
  FoundFunction : boolean;
  LastSection : Array[1..2] of String;
  str : string;
  RootNode : TTreeNode;
  NormalNode,tmpNode : TTreeNode;
  Node : TTreeNode;
  InsertStr : string;
begin
  if(editSearchList.Text = '')then
  begin
    editSearchList.Color := clWhite;
    FunctionList.FullCollapse;
    if InCodeCompletion then
    begin;
      SimbaForm.CurrScript.SynEdit.Lines[CompletionCaret.y - 1] := CompletionStart;
      SimbaForm.CurrScript.SynEdit.LogicalCaretXY:= point(CompletionCaret.x,CompletionCaret.y);
      SimbaForm.CurrScript.SynEdit.SelEnd:= SimbaForm.CurrScript.SynEdit.SelStart;
    end;
    FilterTreeVis(False);
    ScriptNode.Expand(true);
    Exit;
  end;

  //We only have to search the next item in our filter tree.. Fu-king easy!
  if Next then
  begin;
    if FilterTree.Visible = false then
    begin;
      mDebugLn('ERROR: You cannot search next, since the Tree isnt generated yet');
      Find(false);
      Exit;
    end;

    if FilterTree.Selected <> nil then
    begin;
      if backwards then
        start := FilterTree.Selected.AbsoluteIndex - 1
      else
        Start := FilterTree.Selected.AbsoluteIndex + 1;
    end
    else
    begin
      if backwards then
        Start := FilterTree.Items.Count - 1
      else
        Start := 0;
    end;
    Len := FilterTree.Items.Count;
    i := start + len; //This is for the backwards compatibily, we do mod anways.. it just makes sure -1 isn't negative.
    c := 0;
    while c < (len ) do
    begin;
      if (FilterTree.Items[i mod len].HasChildren = false) then
      begin
        FilterTree.Items[i mod len].Selected:= true;
        InsertStr := FilterTree.Items[i mod len].Text;
        Result := true;
        break;
      end;
      if backwards then
        dec(i)
      else
        inc(i);
      inc(c);
    end;
  end else
  begin
    FilterTree.BeginUpdate;
    FilterTree.Items.Clear;

    FoundFunction := False;
    if FunctionList.Selected <> nil then
      Start := FunctionList.Selected.AbsoluteIndex
    else
      Start := 0;
    Len := FunctionList.Items.Count;
    LastSection[1] := '';
    LastSection[2] := '';
    for i := start to start + FunctionList.Items.Count - 1 do
    begin
      Node := FunctionList.Items[i mod FunctionList.Items.Count];
      if(Node.Level >= 1) and (Node.HasChildren = false) then
        if(pos(lowercase(editSearchList.Text), lowercase(Node.Text)) > 0)then
        begin
          if not FoundFunction then
          begin
            FoundFunction := True;
            Index := i mod FunctionList.Items.Count;
            InsertStr := Node.Text;
          end;
          if Node.Level = 2 then
          begin
            if Node.Parent.Text <> lastsection[2] then
            begin
              if Node.Parent.Parent.Text <> lastsection[1] then
              begin
                RootNode := FilterTree.Items.AddChild(nil, Node.Parent.Parent.Text);
                RootNode.ImageIndex := Node.Parent.Parent.ImageIndex;
                RootNode.SelectedIndex := Node.Parent.Parent.SelectedIndex;
                lastsection[1] := RootNode.Text;
                RootNode := FilterTree.Items.AddChild(RootNode, Node.Parent.Text);
                RootNode.ImageIndex := Node.Parent.ImageIndex;
                RootNode.SelectedIndex := Node.Parent.SelectedIndex;
                lastsection[2] := RootNode.Text;
              end else
              begin
                RootNode := FilterTree.Items.AddChild(RootNode.Parent, Node.Parent.Text);
                RootNode.ImageIndex := Node.Parent.ImageIndex;
                RootNode.SelectedIndex := Node.Parent.SelectedIndex;
                lastsection[2] := RootNode.Text;
              end;
            end;
          end else
          begin
            if (Node.Parent.Text <> lastsection[1]) then
            begin
              RootNode := FilterTree.Items.AddChild(nil, Node.Parent.Text);
              RootNode.ImageIndex := Node.Parent.ImageIndex;
              RootNode.SelectedIndex := Node.Parent.SelectedIndex;
              lastsection[1] := Rootnode.text;
            end;
          end;

          with FilterTree.Items.AddChild(RootNode, Node.Text) do
          begin
            Data := Node.Data;
            ImageIndex := Node.ImageIndex;
            SelectedIndex := Node.SelectedIndex;
          end;

  //        break;
        end;
      end;
      Result := FoundFunction;

      if Result then
      begin;
        FilterTreeVis(True);
        FilterTree.FullExpand;
        c := 0;
        while FilterTree.Items[c].HasChildren do
          inc(c);
        FilterTree.Items[c].Selected:= True;
        mDebugLn(FunctionList.Items[Index].Text);
        FunctionList.FullCollapse;
        FunctionList.Items[Index].Selected := true;
        FunctionList.Items[index].ExpandParents;
        editSearchList.Color := clWhite;


      end else
      begin
        FilterTreeVis(false);
        editSearchList.Color := 6711039;
        if InCodeCompletion then
          SimbaForm.CurrScript.SynEdit.Lines[CompletionCaret.y - 1] := CompletionStart;
      end;
    FilterTree.EndUpdate;
  end;

  if result and InCodeCompletion then
    begin;
      str := format(CompletionLine, [InsertStr]);
      with SimbaForm.CurrScript.SynEdit do
      begin;
        Lines[CompletionCaret.y - 1] := str;
        LogicalCaretXY:= StartWordCompletion;
        i := SelStart;
        posi := pos(lowercase(editSearchList.text), lowercase(InsertStr)) + length(editSearchList.text) - 1; //underline the rest of the word
        if Posi = Length(InsertStr) then //Special occasions
        begin;
          if Length(editSearchList.Text) <> Posi then          //We found the last part of the text -> for exmaple when you Search for bitmap, you can find LoadBitmap -> We underline 'Load'
          begin;
            SelStart := i;
            SelEnd := i + pos(lowercase(editSearchList.text), lowercase(InsertStr)) -1;
            Exit;
          end;
          //We searched for the whole text -> for example LoadBitmap, and we found LoadBitmap -> Underline the whole text
          Posi := 0;
        end;
        //Underline the rest of the word
        SelStart := i + posi;
        SelEnd := SelStart + Length(InsertStr) - posi;
      end;
    end;
end;

procedure TFunctionListFrame.FunctionListMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
   N: TTreeNode;
begin
  if button = mbRight then
    exit;
  if InCodeCompletion then
  begin;
    mDebugLn('Not yet implemented');
    exit;
  end;
  if not (Sender is TTreeView) then
    exit;
  N := TTreeView(Sender).GetNodeAt(x, y);
  if(N = nil)then
  begin
    Self.DragKind := dkDock;
    Self.BeginDrag(false, 40);
    exit;
  end;
  Self.DragKind := dkDrag;
  if ((Button = mbLeft) and (N.Level > 0) and (N.Parent <> PluginsNode)) then
    Self.BeginDrag(False, 10);
  DraggingNode := N;
end;

procedure TFunctionListFrame.Terminate;
begin
  if (FillThread <> nil) then
    FillThread.Terminate;
end;

{ TFillThread }

procedure TFillThread.Update;
  procedure AddProcsTree(Node: TTreeNode; Procs: TDeclarationList; Path: string);
    procedure ProcessProcedure(Node: TTreeNode; Proc: TciProcedureDeclaration);
      function findNodeStartingWith(Node: TTreeNode; x: string): TTreeNode;
      var
        x_len: LongInt;
      begin
        x_len := Length(x);

        Result := Node.GetFirstChild();
        while (Result <> nil) and (Copy(Result.Text, 1, x_len) <> x) do
          Result := Result.GetNextSibling();
      end;
    var
      tmpNode: TTreeNode;
      Name, FirstLine: string;
      Index: LongInt;
      ClassName: TDeclaration;
    begin
      if (Assigned(Proc.Name)) then
      begin
        Name := Proc.Name.ShortText + '; ';

        ClassName := Proc.Items.GetFirstItemOfClass(TciProcedureClassName);
        if (Assigned(ClassName)) then
        begin
          tmpNode := findNodeStartingWith(Node, ClassName.ShortText + ' =');
          if (tmpNode = nil) then
            tmpNode := findNodeStartingWith(Node, ClassName.ShortText);

          if (tmpNode = nil) then
          begin
            tmpNode := Node.TreeNodes.AddChild(Node, ClassName.CleanText);
            tmpNode.ImageIndex := 36;
            tmpNode.SelectedIndex := 36;
          end;

          Node := tmpNode;
        end;

        FirstLine := Lowercase(Proc.CleanText);

        Index := Pos(#13, FirstLine);
        if (Index > 0) then
          FirstLine := Copy(FirstLine, 1, Index - 1);

        Index := Node.IndexOfText(Trim(Name));
        if (Index = -1)  then
          Index := Node.IndexOfText(Name + 'override;');
        if (Index = -1)  then
          Index := Node.IndexOfText(Name + 'overload;');
        if (Index > -1) then
          Node := Node.Items[Index];

        if (Pos('override;', FirstLine) > 0) then
          Name += 'override; ';
        if (Pos('overload;', FirstLine) > 0) then
          Name += 'overload; ';

        tmpNode := Node.TreeNodes.AddChild(Node, Trim(Name));
        tmpNode.Data := GetMem(SizeOf(TMethodInfo));

        tmpNode.ImageIndex := 34;
        if (Proc.ProcType = 'procedure') then tmpNode.ImageIndex := 35;
        tmpNode.SelectedIndex := tmpNode.ImageIndex;

        FillChar(PMethodInfo(tmpNode.Data)^, SizeOf(TMethodInfo), 0);

        with PMethodInfo(tmpNode.Data)^ do
        begin
          MethodStr := strnew(Pchar(Proc.CleanDeclaration));
          Filename := strnew(pchar(Path));
          BeginPos := Proc.Name.StartPos;
          EndPos :=  Proc.Name.StartPos + Length(TrimRight(Proc.Name.RawText));
        end;
      end;
    end;
    procedure ProcessDecl(Node: TTreeNode; Decl: TDeclaration);
      function getVarName(Decl: TDeclaration): string;
      begin
        case Decl.ClassName of
          'TciVarDeclaration': Result := Decl.Items.GetFirstItemOfClass(TciVarName).ShortText;
          'TciConstantDeclaration': Result := Decl.Items.GetFirstItemOfClass(TciConstantName).ShortText;
          'TciClassField': Result := Decl.Items.GetFirstItemOfClass(TciFieldName).ShortText;
        end;
      end;
    var
      tmpNode: TTreeNode;
    begin
      tmpNode := Node.TreeNodes.AddChild(Node, Trim(Decl.CleanText));
      tmpNode.Data := GetMem(SizeOf(TMethodInfo));

      if (Decl is TciConstantDeclaration) then tmpNode.ImageIndex := 33;
      if (Decl is TciVarDeclaration) or (Decl is TciClassField) then tmpNode.ImageIndex := 37;
      tmpNode.SelectedIndex := tmpNode.ImageIndex;

      FillChar(PMethodInfo(tmpNode.Data)^, SizeOf(TMethodInfo), 0);

      with PMethodInfo(tmpNode.Data)^ do
      begin
        MethodStr := strnew(Pchar(getVarName(Decl)));
        Filename := strnew(pchar(Path));
        BeginPos := Decl.StartPos;
        EndPos :=  Decl.StartPos + Length(TrimRight(Decl.RawText));
      end;
    end;
    procedure ProcessType(Node: TTreeNode; Decl: TciTypeDeclaration);
    var
      tmpNode: TTreeNode;
      TypeKind: TciTypeKind;
      TypeName: TciTypeName;
      Index, Count: LongInt;
      Str, Txt: String;
      EndOfLine: Integer;
    begin
      TypeKind := TciTypeKind(Decl.Items.GetFirstItemOfClass(TciTypeKind));
      TypeName := TciTypeName(Decl.Items.GetFirstItemOfClass(TciTypeName));

      Txt := Lowercase(TypeKind.ShortText);
      Str := TypeKind.GetRealType.ShortText;
      EndOfLine := Pos(LineEnding, Str);

      if (EndOfLine > 0) then
         Str := Copy(Str, 0, EndOfLine);

      case Txt of
         'record', 'union': tmpNode := Node.TreeNodes.AddChild(Node, Trim(TypeName.CleanText) + ' = ' + Str);
         'packed record': tmpNode := Node.TreeNodes.AddChild(Node, Trim(TypeName.CleanText) + ' = ' + 'packed ' + Str);
       else
          tmpNode := Node.TreeNodes.AddChild(Node, Trim(Decl.CleanText));
      end;

      tmpNode.ImageIndex := 36;
      tmpNode.SelectedIndex := 36;
      tmpNode.Data := GetMem(SizeOf(TMethodInfo));
      FillChar(PMethodInfo(tmpNode.Data)^, SizeOf(TMethodInfo), 0);

      with PMethodInfo(tmpNode.Data)^ do
      begin
        MethodStr := strnew(Pchar(Decl.CleanText));
        Filename := strnew(pchar(Path));
        BeginPos := Decl.StartPos;
        EndPos :=  Decl.StartPos + Length(TrimRight(Decl.RawText));
      end;

      if (Txt = 'record') or (Txt = 'union') or (Txt = 'packed record') then
      begin
        Count := TypeKind.GetRealType.Items.Count - 1;
        for Index := 0 to Count do
          if (not (TypeKind.GetRealType.Items[Index] is TciJunk)) then
            ProcessDecl(tmpNode, TypeKind.GetRealType.Items[Index]);
      end;
    end;

  var
    I: integer;
  begin;
    if (not Assigned(Procs)) then
      Exit;

    for I := 0 to Procs.Count - 1 do
      if (Assigned(Procs[I])) then
        case Procs[I].ClassName of
          'TciProcedureDeclaration': ProcessProcedure(Node, Procs[I] as TciProcedureDeclaration);
          'TciVarDeclaration', 'TciConstantDeclaration': ProcessDecl(Node, Procs[I] as TDeclaration);
          'TciTypeDeclaration': ProcessType(Node, Procs[I] as TciTypeDeclaration);
          'TciJunk', 'TciInclude', 'TciCompoundStatement': ;
          else
            WriteLn('Unknown Class: ', Procs[I].ClassName);
        end;
  end;

  function isLib(ci: TCodeInsight): boolean; inline;
  begin
    Result := (Length(ci.FileName) > 4) and (Copy(ci.FileName, 1, 4) = 'lib:');
  end;

  procedure AddInclude(ParentNode: TTreeNode; Include: TCodeInsight);
  var
    I: integer;
  begin;
    I := ParentNode.IndexOfText(ExtractFileNameOnly(Include.FileName));
    if (I < 0) then
    begin
      ParentNode.TreeNodes.AddChild(ParentNode, ExtractFileNameOnly(Include.FileName));
      I := ParentNode.IndexOfText(ExtractFileNameOnly(Include.FileName));

      with ParentNode.Items[I] do
      begin
        if (isLib(Include)) then
          ImageIndex := 42
        else
          ImageIndex := 38;
        SelectedIndex := ImageIndex;

        Data := GetMem(SizeOf(TMethodInfo));
        FillChar(PMethodInfo(Data)^, SizeOf(TMethodInfo), 0);
      end;
    end;

    ParentNode := ParentNode.Items[I];

    if (PMethodInfo(ParentNode.Data)^.Filename <> nil) and (PMethodInfo(ParentNode.Data)^.Filename = Include.FileName) then
      Exit;

    PMethodInfo(ParentNode.Data)^.Filename := strnew(PChar(Include.FileName));

    AddProcsTree(ParentNode, Include.Items, Include.FileName);

    for I := 0 to High(Include.Includes) do
      if (isLib(Include.Includes[I])) then
        AddInclude(PluginsNode, Include.Includes[I])
      else
        AddInclude(ParentNode.Parent, Include.Includes[I]);
  end;

  function getCount(Analyzer: TCodeInsight): LongWord;
  begin
    Result := Length(Analyzer.Includes) + Analyzer.Lexer.Defines.Count;
  end;

var
  I: LongInt;
begin
  AddProcsTree(ScriptNode, Analyzer.Items, Analyzer.FileName);

  if (LastIncludeCount^ <> getCount(Analyzer)) then
  begin
    PluginsNode.DeleteChildren;
    IncludesNode.DeleteChildren;

    for I := 0 to High(Analyzer.Includes) do
      if (isLib(Analyzer.Includes[I])) then
        AddInclude(PluginsNode, Analyzer.Includes[I])
      else
        AddInclude(IncludesNode, Analyzer.Includes[I]);

    LastIncludeCount^ := getCount(Analyzer);
  end;
end;

procedure TFillThread.execute;
begin
  Analyzer.Run(MS, nil, -1, True);

  Synchronize(@Update);
end;

initialization
  {$R *.lfm}

end.

