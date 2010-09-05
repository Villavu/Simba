unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, SynEdit, SynHighlighterPas;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnGo: TButton;
    pnlMain: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    eIn: TSynEdit;
    eOut: TSynEdit;
    eDebug: TSynEdit;
    PasHL: TSynPasSyn;
    procedure btnGoClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation

uses
  v_ideCodeParser;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.btnGoClick(Sender: TObject);

  procedure Debug(s: string); overload;
  begin
    if (Trim(eOut.Text) <> '') then
      eDebug.Lines.Append(s)
    else
      eDebug.Text := s;
  end;

  procedure Debug(v: Variant); overload;
  begin
    Debug(string(v));
  end;

  procedure Write(s: string); overload;
  begin
    if (Trim(eOut.Text) <> '') then
      eOut.Text := eOut.Text + s
    else
      eOut.Text := s;
  end;

  procedure Write(v: Variant); overload;
  begin
    Write(string(v));
  end;

var
  p: TCodeParser;
  m: TMemoryStream;
  a, b, c: TDeclarationArray;
  i, ii, iii, pc: Integer;
  s: string;
  d: TDeclaration;
  Fail: Boolean;
begin
  p := TCodeParser.Create;
  m := TMemoryStream.Create;

  try
    eOut.BeginUpdate;
    eOut.ClearAll;
    eDebug.BeginUpdate;
    eDebug.ClearAll;

    eIn.Lines.SaveToStream(m);

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

        s := 'procedure _RUTIS_'+Name.ShortText+
          '(Stack: TRutisStack; Params: PRutisParamInfoArray; Result: PRutisParamInfo);'+LineEnding+
          'begin'+LineEnding+'  ';

        d := Items.GetFirstItemOfClass(TciReturnType);
        if (d <> nil) then
          s := s+'P'+d.ShortText+'(Result^.Data)^ := ';

        s := s+Name.ShortText+'(';

        pc := 0;
        Fail := False;
        b := GetParamDeclarations();
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
            s := s+'P'+d.ShortText+'(Params^['+IntToStr(pc)+'].Data)^';
            Inc(pc);
          end;
        end;

        if Fail then
          Continue;

        s := s+');'+LineEnding+'end;';
        if (i > 0) then
          s := LineEnding+s;
        Write(s);
        Debug('Done "'+Name.ShortText+'"!');
      end;
  finally
    m.Free;
    p.Free;

    eOut.EndUpdate;
    eDebug.EndUpdate;
  end;
  Debug('Done :)');
end;

end.

