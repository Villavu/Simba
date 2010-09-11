unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, SynEdit, SynHighlighterPas,wrapfiles;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnGo: TButton;
    btnAdvanced: TButton;
    pnlMain: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    eIn: TSynEdit;
    eOut: TSynEdit;
    eDebug: TSynEdit;
    PasHL: TSynPasSyn;
    procedure btnAdvancedClick(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 
procedure ConvertRT(Input, Dbg, Output : TStrings; procnames : TStrings = nil);
var
  frmMain: TfrmMain;

implementation

uses
  v_ideCodeParser;

{$R *.lfm}

{ TfrmMain }

procedure ConvertRT(Input, Dbg, Output : TStrings; procnames : TStrings = nil);
  procedure Debug(s: string); overload;
  begin
    if (Trim(Output.Text) <> '') then
      Dbg.Append(s)
    else
      Dbg.Text := s;
  end;

  procedure Debug(v: Variant); overload;
  begin
    Debug(string(v));
  end;

  procedure Write(s: string); overload;
  begin
    if (Trim(Output.Text) <> '') then
      Output.Text := Output.Text + s
    else
      Output.Text := s;
  end;

  procedure Write(v: Variant); overload;
  begin
    Write(string(v));
  end;

  function FixName( str : string) : string;
  begin
    if (length(str) > 3) and (str[1] = 'p') and (str[2] = 's') and (str[3] = '_') then
      result := Copy(str,4,length(str)-3);
  end;

  function PtrName ( str : string) : String;
  begin
    debug(str);
    if (length(str) > 1) and (str[1] in ['T','t']) then
      result := 'P' + copy(str,2,length(str)-1)
    else
      result := 'P' + str;
    debug(result);
  end;

var
  p: TCodeParser;
  m: TMemoryStream;
  a, b, c: TDeclarationArray;
  i, ii, iii, pc: Integer;
  s: string;
  rutiss,tmp : string;
  d: TDeclaration;
  Fail: Boolean;
begin
  p := TCodeParser.Create;
  m := TMemoryStream.Create;

  try
    Output.BeginUpdate;
    Output.Clear;
    Dbg.BeginUpdate;
    Dbg.Clear;

    Input.SaveToStream(m);

    try
      p.Run(m);
    except on E : Exception do
      Debug(e.Message);
    end;

    a := p.Items.GetItemsOfClass(TciProcedureDeclaration);
    Debug('Start converting '+IntToStr(Length(a))+' methods!');
    for i := 0 to High(a) do
      with TciProcedureDeclaration(a[i]) do
      begin
        if (Name = nil) then
        begin
          Debug('No name found, skipping..');
          Continue;
        end;

        s := 'procedure RUTIS_'+Name.ShortText+
          '(Params: PRutisParamInfoArray; Result: PRutisParamInfo);'+LineEnding+
          'begin'+LineEnding+'  ';

        d := Items.GetFirstItemOfClass(TciReturnType);
        if (d <> nil) then
        begin
          s := s+PtrName(d.ShortText)+'(Result^.Data)^ := ';
          rutiss := 'RutisEngine.RegExtMethod(''%s'',%s, [%s], '''+d.CleanText +''');';
        end else
          rutiss := 'RutisEngine.RegExtMethod(''%s'',%s, [%s], '''');';

        s := s+Name.ShortText+'(';

        pc := 0;
        Fail := False;
        b := GetParamDeclarations();
        tmp := '';
        for ii := 0 to High(b) do
        begin
          d := b[ii].Items.GetFirstItemOfClass(TciParameterType);
          if (d = nil) then
          begin
            Debug('No parameter type found in '+Name.ShortText+', skipping..');
            Fail := True;
            Break;
          end;
          c := b[ii].Items.GetItemsOfClass(TciParameterName);
          if (Length(c) < 1) then
          begin
            Debug('No parameter names found in '+Name.ShortText+', skipping..');
            Fail := True;
            Break;
          end;

          for iii := High(c) downto 0 do
          begin
            if (pc > 0) then
              s := s+', ';
            if b[ii] is TciVarParameter then
            begin
              tmp := tmp + #39 + 'var ' + d.ShortText + #39 +',';
              s := s+PtrName(d.ShortText)+'(PPointer(Params^['+IntToStr(pc)+'].Data)^)^';
            end else
            begin
              s := s+PtrName(d.ShortText)+'(Params^['+IntToStr(pc)+'].Data)^';
              tmp := tmp + #39 + d.ShortText + #39 +',';
            end;
            Inc(pc);
          end;
        end;
        if tmp <> '' then
          setlength(tmp,length(tmp)-1);

        if Fail then
          Continue;

        s := s+');'+LineEnding+'end;';
        if (i > 0) then
          s := LineEnding+s;
        Write(s);
        rutiss := Format(rutiss,[FixName(name.ShortText),'@RUTIS_' + name.ShortText,tmp]);
        if procnames <> nil then
          procnames.Add(rutiss);
        Debug('Done "'+Name.ShortText+'"!');
        Debug('Prog-name "' + rutiss + '"');
      end;
  finally
    m.Free;
    p.Free;

    Output.EndUpdate;
    Dbg.EndUpdate;
  end;
  Debug('Done :)');
end;

procedure TfrmMain.btnGoClick(Sender: TObject);
begin
  ConvertRT(eIn.Lines,eDebug.Lines,eOut.Lines);
end;

procedure TfrmMain.btnAdvancedClick(Sender: TObject);
begin
  WrapFilesForm.ShowModal;
end;

end.

