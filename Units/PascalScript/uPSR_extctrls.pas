
unit uPSR_extctrls;

{$I PascalScript.inc}
interface
uses
  uPSRuntime, uPSUtils;


procedure RIRegister_ExtCtrls(cl: TPSRuntimeClassImporter);

procedure RIRegisterTSHAPE(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTIMAGE(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTPAINTBOX(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTBEVEL(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTTIMER(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTCUSTOMPANEL(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTPANEL(Cl: TPSRuntimeClassImporter);
{$IFNDEF CLX}
procedure RIRegisterTPAGE(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTNOTEBOOK(Cl: TPSRuntimeClassImporter);
{$IFNDEF FPC}procedure RIRegisterTHEADER(Cl: TPSRuntimeClassImporter);{$ENDIF}
{$ENDIF}
procedure RIRegisterTCUSTOMRADIOGROUP(Cl: TPSRuntimeClassImporter);
procedure RIRegisterTRADIOGROUP(Cl: TPSRuntimeClassImporter);

implementation

uses
  {$IFDEF CLX}
  QExtCtrls, QGraphics;
  {$ELSE}
  ExtCtrls, Graphics,classes;
  {$ENDIF}

procedure RIRegisterTSHAPE(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TSHAPE) do
  begin
    {$IFNDEF PS_MINIVCL}
    RegisterMethod(@TSHAPE.STYLECHANGED, 'STYLECHANGED');
    {$ENDIF}
  end;
end;

procedure TIMAGECANVAS_R(Self: TIMAGE; var T: TCANVAS); begin T := Self.CANVAS; end;

procedure RIRegisterTIMAGE(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TIMAGE) do
  begin
    RegisterPropertyHelper(@TIMAGECANVAS_R, nil, 'CANVAS');
  end;
end;

procedure TPAINTBOXCANVAS_R(Self: TPAINTBOX; var T: TCanvas); begin T := Self.CANVAS; end;

procedure RIRegisterTPAINTBOX(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TPAINTBOX) do
  begin
    RegisterPropertyHelper(@TPAINTBOXCANVAS_R, nil, 'CANVAS');
  end;
end;

procedure RIRegisterTBEVEL(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TBEVEL);
end;

procedure RIRegisterTTIMER(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TTIMER);
end;

procedure RIRegisterTCUSTOMPANEL(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TCUSTOMPANEL);
end;

procedure RIRegisterTPANEL(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TPANEL);
end;
procedure TPagePageIndex_R(Self: TPAGE; var T: INTEGER); begin T := Self.PageIndex; end;
procedure TPagePageIndex_W(Self: TPAGE; T: INTEGER); begin Self.PageIndex := T; end;
procedure TPageOnShow_R(Self: TPAGE; var T: TNotifyEvent); begin T := Self.OnShow; end;
procedure TPageOnShow_W(Self: TPAGE; T: TNotifyEvent); begin Self.OnShow := T; end;
{$IFNDEF CLX}
procedure RIRegisterTPAGE(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TPAGE) do
  begin
    RegisterPropertyHelper(@TPagePageIndex_R,@TPagePageIndex_W,'PageIndex');
    RegisterEventPropertyHelper(@TPageOnShow_R,@TPageOnShow_W,'OnShow');
  end;
end;

procedure TNoteBookPageCount_R(Self: TNoteBook; var T: INTEGER); begin T := Self.PageCount; end;
procedure RIRegisterTNOTEBOOK(Cl: TPSRuntimeClassImporter);
begin
  with Cl.Add(TNOTEBOOK) do
  begin
    RegisterMethod(@TNoteBook.TabIndexAtClientPos,'TABINDEXATCLIENTPOS');
    RegisterPropertyHelper(@TNoteBookPageCount_R,nil,'PAGECOUNT');
  end;
end;

{$IFNDEF FPC}
procedure THEADERSECTIONWIDTH_R(Self: THEADER; var T: INTEGER; t1: INTEGER); begin T := Self.SECTIONWIDTH[t1]; end;
procedure THEADERSECTIONWIDTH_W(Self: THEADER; T: INTEGER; t1: INTEGER); begin Self.SECTIONWIDTH[t1] := T; end;

procedure RIRegisterTHEADER(Cl: TPSRuntimeClassImporter);
begin
	with Cl.Add(THEADER) do
	begin
		RegisterPropertyHelper(@THEADERSECTIONWIDTH_R, @THEADERSECTIONWIDTH_W, 'SECTIONWIDTH');
	end;
end;
{$ENDIF}
{$ENDIF}

procedure RIRegisterTCUSTOMRADIOGROUP(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TCUSTOMRADIOGROUP);
end;

procedure RIRegisterTRADIOGROUP(Cl: TPSRuntimeClassImporter);
begin
  Cl.Add(TRADIOGROUP);
end;

procedure RIRegister_ExtCtrls(cl: TPSRuntimeClassImporter);
begin
  {$IFNDEF PS_MINIVCL}
  RIRegisterTSHAPE(Cl);
  RIRegisterTIMAGE(Cl);
  RIRegisterTPAINTBOX(Cl);
  {$ENDIF}
  RIRegisterTBEVEL(Cl);
  {$IFNDEF PS_MINIVCL}
  RIRegisterTTIMER(Cl);
  {$ENDIF}
  RIRegisterTCUSTOMPANEL(Cl);
{$IFNDEF CLX}
  RIRegisterTPANEL(Cl);
{$ENDIF}
  {$IFNDEF PS_MINIVCL}
{$IFNDEF CLX}
  RIRegisterTPAGE(Cl);
	RIRegisterTNOTEBOOK(Cl);
 {$IFNDEF FPC}
	RIRegisterTHEADER(Cl);
 {$ENDIF}{FPC}
{$ENDIF}
  RIRegisterTCUSTOMRADIOGROUP(Cl);
  RIRegisterTRADIOGROUP(Cl);
  {$ENDIF}
end;

end.


