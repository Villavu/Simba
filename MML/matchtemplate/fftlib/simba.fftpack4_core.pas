unit FFTPACK4_core;
{==============================================================================]
  FFTPACK4_core.pas: A set of FFT routines in FPC
  Algorithmically based on Fortran-77 FFTPACK by Paul N. Swarztrauber (V4, 1985)
  
  As the original fortran libraries are public domain, the FPC Fourier routines 
  in this file are hereby released to the public domain as well.
  
  Brought to Free Pascal by Jarl `slacky` Holta
[==============================================================================}
{$I header.inc}
interface

uses
  SysUtils, mtCore;

type
  PSingle = ^Single;
  PInt32  = ^Int32;
  RealArrayRef = ^Single;
  IntArrayRef  = PInt32;


procedure cfftf(n: Int32; c, wsave: RealArrayRef);
procedure cfftb(n: Int32; c, wsave: RealArrayRef);
procedure cffti(n: Int32; wsave: RealArrayRef);

procedure rfftf(n: Int32; r, wsave: RealArrayRef);
procedure rfftb(n: Int32; r, wsave: RealArrayRef);
procedure rffti(n: Int32; wsave: RealArrayRef);


implementation

uses
  math; //used as a platform fallback

const
  TWOPI: Single = 6.28318530717959;
  MAXFAC   = 13;  // maximum number of factors in factorization of n
  NSPECIAL = 4;   // number of factors for which we have special-case routines

type
  TSPECIAL = array [0..NSPECIAL-1] of Int32; 

  
{$IF FPC_VERSION < 3}
  {$IF Defined(CPU386)}  //"overload" for i386, so we get a float32 version
    procedure SinCos(Theta: Single; out FSin, FCos: Single); assembler;
    asm
      fld Theta;
      fsincos;
      fstp [FCos];
      fstp [FSin];
      fwait;
    end;
  {$ELSEIF Defined(CPUX86_64)}
    procedure SinCos(Theta: Single; out FSin, FCos: Single); assembler;
    var t: Single;
    asm
      movss dword ptr t,xmm0
      fld dword ptr t
      fsincos
      fstp dword ptr [FCos]
      fstp dword ptr [FSin]
      fwait
    end;
  {$ELSE}
    procedure SinCos(Theta: Single; out FSin, FCos: Single); inline;
    begin
      FSin := Sin(Theta);
      FCos := Cos(Theta);
    end;
  {$ENDIF}
{$ENDIF}

(* ----------------------------------------------------------------------
   passf2, passf3, passf4, passf5, passf. Complex FFT passes fwd and bwd.
---------------------------------------------------------------------- *)

// isign = +1 for backward transform and -1 for forward transforms
procedure passf2(ido, l1: Int32; cc,ch,wa1: RealArrayRef; isign: Int32);
var
  i,k: Int32;
  re,im: Single;
  ah,ac: RealArrayRef;
begin
  if ido<=2 then
  begin
    for k:=0 to l1-1 do
    begin
      ah := ch + k*ido;
      ac := cc + k*ido*2;
      ah[0]       := ac[0] + ac[ido];   //re
      ah[1]       := ac[1] + ac[ido+1]; //im
      ah[ido*l1]  := ac[0] - ac[ido];   //re
      ah[ido*l1+1]:= ac[1] - ac[ido+1]; //im
    end;
  end else
  begin
    for k:=0 to l1-1 do
    begin
      i := 0;
      ah := ch + k*ido;
      ac := cc + 2*k*ido;
      while i < ido-1 do
      begin
        ah[0] := ac[0] + ac[ido];   //re
        ah[1] := ac[1] + ac[ido+1]; //im
        re    := ac[0] - ac[ido];
        im    := ac[1] - ac[ido+1];
        ah[l1*ido]   := wa1[i]*re - isign*wa1[i+1]*im; //re
        ah[l1*ido+1] := wa1[i]*im + isign*wa1[i+1]*re; //im
        Inc(i, 2);
        Inc(ac, 2); Inc(ah, 2);
      end;
    end;
  end;
end; // passf2


// isign = +1 for backward transform and -1 for forward transforms
procedure passf3(ido, l1: Int32; cc, ch, wa1,wa2: RealArrayRef; isign: Int32);
const
  taur: Single =-0.5;
  taui: Single = 0.866025403784439;
var
  i,k,ac,ah: Int32;
  ci2,ci3,di2,di3,cr2,cr3: Single;
  dr2,dr3: Single;
  ti2,tr2: Single;
begin
  if ido=2 then
  begin
    for k:=1 to l1 do
    begin
      ac  := (3*k-2) * ido;
      ah  := (k-1) * ido;
      tr2 := cc[ac] + cc[ac+ido];
      cr2 := cc[ac-ido] + taur*tr2;
      ch[ah] := cc[ac-ido] + tr2;
      ti2 := cc[ac+1] + cc[ac+ido+1];
      ci2 := cc[ac-ido+1] + taur*ti2;
      ch[ah+1] := cc[ac-ido+1]+ti2;
      cr3 := isign*taui * (cc[ac]-cc[ac+ido]);
      ci3 := isign*taui * (cc[ac+1]-cc[ac+ido+1]);
      ch[ah+l1*ido]     := cr2 - ci3;
      ch[ah+2*l1*ido]   := cr2 + ci3;
      ch[ah+l1*ido+1]   := ci2 + cr3;
      ch[ah+2*l1*ido+1] := ci2 - cr3;
    end;
  end else
  begin
    for k:=1 to l1 do
    begin
      i := 0;
      while i<ido-1 do
      begin
        ac  := i + (3*k-2) * ido;
        ah  := i + (k-1) * ido;
        tr2 := cc[ac] + cc[ac+ido];
        cr2 := cc[ac-ido] + taur*tr2;
        ch[ah] := cc[ac-ido] + tr2;
        ti2 := cc[ac+1] + cc[ac+ido+1];
        ci2 := cc[ac-ido+1] + taur*ti2;
        ch[ah+1] := cc[ac-ido+1] + ti2;
        cr3 := isign*taui * (cc[ac]-cc[ac+ido]);
        ci3 := isign*taui * (cc[ac+1]-cc[ac+ido+1]);
        dr2 := cr2 - ci3;
        dr3 := cr2 + ci3;
        di2 := ci2 + cr3;
        di3 := ci2 - cr3;
        ch[ah+l1*ido+1]   := wa1[i]*di2 + isign*wa1[i+1]*dr2;
        ch[ah+l1*ido]     := wa1[i]*dr2 - isign*wa1[i+1]*di2;
        ch[ah+2*l1*ido+1] := wa2[i]*di3 + isign*wa2[i+1]*dr3;
        ch[ah+2*l1*ido]   := wa2[i]*dr3 - isign*wa2[i+1]*di3;
      
        Inc(i, 2);
      end;
    end;
  end;
end; (* passf3 *)


// isign = +1 for backward transform and -1 for forward transforms
procedure passf4(o1, l1: Int32; cc, ch, wa1, wa2, wa3: RealArrayRef; isign: Int32);
var
  i,k,o2,o3,d1,d2,d3: Int32;
  ci2,ci3,ci4,cr2,cr3,cr4: Single;
  ti1,ti2,ti3,ti4,tr1,tr2,tr3,tr4: Single;
  ic,ih,ac,ah: RealArrayRef;
begin
  if o1=2 then
  begin
    d1 := l1*2;
    d2 := l1*4;
    d3 := l1*6;
    ac := cc;
    ah := ch;
    for k:=0 to l1-1 do
    begin
      ti1 := ac[1] - ac[5];
      ti2 := ac[1] + ac[5];
      tr4 := ac[7] - ac[3];
      ti3 := ac[7] + ac[3];
      tr1 := ac[0] - ac[4];
      tr2 := ac[0] + ac[4];
      ti4 := ac[2] - ac[6];
      tr3 := ac[2] + ac[6];
      ah[0]    := tr2 + tr3;
      ah[1]    := ti2 + ti3;
      ah[d2]   := tr2 - tr3;
      ah[d2+1] := ti2 - ti3;
      ah[d1]   := tr1 + isign*tr4;
      ah[d1+1] := ti1 + isign*ti4;
      ah[d3]   := tr1 - isign*tr4;
      ah[d3+1] := ti1 - isign*ti4;
      Inc(ac, 8);
      Inc(ah, 2);
    end;
  end else
  begin
    d1 := l1*o1;
    o2 := 2*o1; d2 := l1*o2;
    o3 := 3*o1; d3 := l1*o3;
    for k:=0 to l1-1 do
    begin
      i  := 0;
      ac := cc + (k*4*o1);
      ah := ch + (k*o1);
      while i < o1-1 do
      begin
        ic  := ac+1;
        ih  := ah+1;
        ti1 := ic[0]  - ic[o2];
        ti2 := ic[0]  + ic[o2];
        ti3 := ic[o1] + ic[o3];
        tr4 := ic[o3] - ic[o1];
        tr1 := ac[0]  - ac[o2];
        tr2 := ac[0]  + ac[o2];
        ti4 := ac[o1] - ac[o3];
        tr3 := ac[o1] + ac[o3];

        ah[0] := tr2 + tr3;
        ah[1] := ti2 + ti3;
        cr3   := tr2 - tr3;
        ci3   := ti2 - ti3;

        if isign = 1 then
        begin
          cr2 := tr1 + tr4;  ci2 := ti1 + ti4;
          cr4 := tr1 - tr4;  ci4 := ti1 - ti4;
          ah[d1] := wa1[i]*cr2 - wa1[i+1]*ci2;
          ih[d1] := wa1[i]*ci2 + wa1[i+1]*cr2;
          ah[d2] := wa2[i]*cr3 - wa2[i+1]*ci3;
          ih[d2] := wa2[i]*ci3 + wa2[i+1]*cr3;
          ah[d3] := wa3[i]*cr4 - wa3[i+1]*ci4;
          ih[d3] := wa3[i]*ci4 + wa3[i+1]*cr4;
        end else
        begin
          cr2 := tr1 - tr4;  ci2 := ti1 - ti4;
          cr4 := tr1 + tr4;  ci4 := ti1 + ti4;
          ah[d1] := wa1[i]*cr2 + wa1[i+1]*ci2;
          ih[d1] := wa1[i]*ci2 - wa1[i+1]*cr2;
          ah[d2] := wa2[i]*cr3 + wa2[i+1]*ci3;
          ih[d2] := wa2[i]*ci3 - wa2[i+1]*cr3;
          ah[d3] := wa3[i]*cr4 + wa3[i+1]*ci4;
          ih[d3] := wa3[i]*ci4 - wa3[i+1]*cr4;
        end;

        Inc(i, 2);
        Inc(ac, 2);
        Inc(ah, 2);
      end;
    end;
  end;
end; (* passf4 *)


// isign = +1 for backward transform and -1 for forward transforms
procedure passf5(ido, l1: Int32; cc,ch,wa1,wa2,wa3,wa4: RealArrayRef; isign: Int32);
const
  tr11: Single = 0.309016994374947;
  ti11: Single = 0.951056516295154;
  tr12: Single =-0.809016994374947;
  ti12: Single = 0.587785252292473;
var
  i,k,ac,ah,ido2,ido3,ido4,idl,idl2,idl3,idl4: Int32;
  ci2,ci3,ci4,ci5: Single;
  di2,di3,di4,di5: Single;
  cr2,cr3,cr5,cr4: Single;
  ti2,ti3,ti4,ti5: Single;
  dr2,dr3,dr4,dr5: Single;
  tr2,tr3,tr4,tr5: Single;
begin
  idl := l1*ido;
  ido2 := 2*ido;  idl2 := l1*ido2;
  ido3 := 3*ido;  idl3 := l1*ido3;
  ido4 := 4*ido;  idl4 := l1*ido4;
  if ido=2 then
  begin
    for k:=1 to l1 do
    begin
      ac  := (5*k-4)*ido+1;
      ti5 := cc[ac] - cc[ac+ido3];
      ti2 := cc[ac] + cc[ac+ido3];
      ti4 := cc[ac+ido] - cc[ac+ido2];
      ti3 := cc[ac+ido] + cc[ac+ido2];
      tr5 := cc[ac-1] - cc[ac+ido3-1];
      tr2 := cc[ac-1] + cc[ac+ido3-1];
      tr4 := cc[ac+ido-1] - cc[ac+ido2-1];
      tr3 := cc[ac+ido-1] + cc[ac+ido2-1];
      ah  := (k-1)*ido;
      ch[ah  ] := cc[ac-ido-1] + tr2 + tr3;
      ch[ah+1] := cc[ac-ido  ] + ti2 + ti3;
      cr2 := cc[ac-ido-1] + tr11*tr2 + tr12*tr3;
      ci2 := cc[ac-ido] + tr11*ti2 + tr12*ti3;
      cr3 := cc[ac-ido-1] + tr12*tr2 + tr11*tr3;
      ci3 := cc[ac-ido] + tr12*ti2 + tr11*ti3;
      cr5 := isign * (ti11*tr5 + ti12*tr4);
      ci5 := isign * (ti11*ti5 + ti12*ti4);
      cr4 := isign * (ti12*tr5 - ti11*tr4);
      ci4 := isign * (ti12*ti5 - ti11*ti4);
      ch[ah+idl]    := cr2 - ci5;
      ch[ah+idl4]   := cr2 + ci5;
      ch[ah+idl+1]  := ci2 + cr5;
      ch[ah+idl2+1] := ci3 + cr4;
      ch[ah+idl2]   := cr3 - ci4;
      ch[ah+idl3]   := cr3 + ci4;
      ch[ah+idl3+1] := ci3 - cr4;
      ch[ah+idl4+1] := ci2 - cr5;
    end;
  end else
  begin
    for k:=1 to l1 do
    begin
      i := 0;
      while i < ido-1 do
      begin
        ac  := i+1+(k*5-4)*ido;
        ti5 := cc[ac] - cc[ac+ido3];
        ti2 := cc[ac] + cc[ac+ido3];
        ti4 := cc[ac+ido] - cc[ac+ido2];
        ti3 := cc[ac+ido] + cc[ac+ido2];
        tr5 := cc[ac-1] - cc[ac+ido3-1];
        tr2 := cc[ac-1] + cc[ac+ido3-1];
        tr4 := cc[ac+ido-1] - cc[ac+ido2-1];
        tr3 := cc[ac+ido-1] + cc[ac+ido2-1];
        ah  := i+(k-1)*ido;
        ch[ah  ] := cc[ac-ido-1] + tr2 + tr3;
        ch[ah+1] := cc[ac-ido  ] + ti2 + ti3;
        cr2 := cc[ac-ido-1] + tr11*tr2 + tr12*tr3;
        ci2 := cc[ac-ido] + tr11*ti2 + tr12*ti3;
        cr3 := cc[ac-ido-1] + tr12*tr2 + tr11*tr3;
        ci3 := cc[ac-ido] + tr12*ti2 + tr11*ti3;
        cr5 := isign * (ti11*tr5 + ti12*tr4);
        ci5 := isign * (ti11*ti5 + ti12*ti4);
        cr4 := isign * (ti12*tr5 - ti11*tr4);
        ci4 := isign * (ti12*ti5 - ti11*ti4);
        dr3 := cr3 - ci4;
        dr4 := cr3 + ci4;
        di3 := ci3 + cr4;
        di4 := ci3 - cr4;
        dr5 := cr2 + ci5;
        dr2 := cr2 - ci5;
        di5 := ci2 - cr5;
        di2 := ci2 + cr5;
        ch[ah+idl]    := wa1[i]*dr2 - isign*wa1[i+1]*di2;
        ch[ah+idl+1]  := wa1[i]*di2 + isign*wa1[i+1]*dr2;
        ch[ah+idl2]   := wa2[i]*dr3 - isign*wa2[i+1]*di3;
        ch[ah+idl2+1] := wa2[i]*di3 + isign*wa2[i+1]*dr3;
        ch[ah+idl3]   := wa3[i]*dr4 - isign*wa3[i+1]*di4;
        ch[ah+idl3+1] := wa3[i]*di4 + isign*wa3[i+1]*dr4;
        ch[ah+idl4]   := wa4[i]*dr5 - isign*wa4[i+1]*di5;
        ch[ah+idl4+1] := wa4[i]*di5 + isign*wa4[i+1]*dr5;
      
        Inc(i, 2);
      end;
    end;
  end;
end; (* passf5 *)

// isign = +1 for backward transform and -1 for forward transforms
procedure passf(out nac: Boolean; ido,ip,l1,idl1: Int32;  cc,ch,wa: RealArrayRef; isign: Int32);
var
  idij,idlj, idot,ipph, i,j,k,l,jc,lc,ik, idj,idl,ic,idp, t1,t2,t3,t4: Int32;
  wai, war: Single;
begin
  idot := ido div 2;
  ipph := (ip+1) div 2;
  idp  := ip*ido;

  if ido>=l1 then
  begin
    for j:=1 to ipph-1 do
    begin
      jc := ip-j;
      for k:=0 to l1-1 do
      begin
        t1 := (j +k*ip)*ido;
        t2 := (jc+k*ip)*ido;
        for i:=0 to ido-1 do
        begin
          ch[i+(k+j *l1)*ido] := cc[i+t1] + cc[i+t2];
          ch[i+(k+jc*l1)*ido] := cc[i+t1] - cc[i+t2];
        end;
      end;
    end;
    for k:=0 to l1-1 do
      for i:=0 to ido-1 do
        ch[i+k*ido] := cc[i+k*idp];
  end else
  begin
    for j:=1 to ipph-1 do
    begin
      jc := ip-j;
      for i:=0 to ido-1 do
        for k:=0 to l1-1 do
        begin
          ch[i+(k+j*l1)*ido]  := cc[i+(j+k*ip)*ido]+cc[i+(jc+k*ip)*ido];
          ch[i+(k+jc*l1)*ido] := cc[i+(j+k*ip)*ido]-cc[i+(jc+k*ip)*ido];
        end;
    end;
    for i :=0 to ido-1 do
      for k :=0 to l1-1 do
        ch[i+k*ido] := cc[i+k*ip*ido];
  end;

  idl := 2-ido;
  ic  := 0;
  for l:=1 to ipph-1 do
  begin
    lc := ip-l;
    Inc(idl, ido);

    t1 := l*idl1;
    t2 := lc*idl1;
    for ik:=0 to idl1-1 do
    begin
      cc[ik+t1] := ch[ik] + wa[idl-2] * ch[ik+idl1];
      cc[ik+t2] := isign  * wa[idl-1] * ch[ik+(ip-1)*idl1];
    end;
  
    idlj := idl;
    Inc(ic, ido);
    for j:=2 to ipph-1 do
    begin
      //jc := ip-j;
      Inc(idlj, ic);
      if idlj > idp then Dec(idlj, idp);
      war := wa[idlj-2];
      wai := isign * wa[idlj-1];

      t1 := l*idl1;
      t2 := lc*idl1;
      t3 := j*idl1;
      t4 := (ip-j)*idl1;
      for ik :=0 to idl1-1 do
      begin
        cc[ik+t1] := cc[ik+t1] + war*ch[ik+t3];
        cc[ik+t2] := cc[ik+t2] + wai*ch[ik+t4];
      end;
    end;
  end;

  for j:=1 to ipph-1 do
  begin
    t3 := j*idl1;
    for ik:=0 to idl1-1 do
      ch[ik] := ch[ik] + ch[ik+t3];
  end;

  for j:=1 to ipph-1 do
  begin
    //jc := ip-j;
    ik := 1;
    t3 := j*idl1;
    t4 := (ip-j)*idl1;
    while ik<idl1 do
    begin
      ch[ik-1+t3] := cc[ik-1+t3] - cc[ik+t4];
      ch[ik-1+t4] := cc[ik-1+t3] + cc[ik+t4];
      ch[ik+t3]   := cc[ik+t3] + cc[ik-1+t4];
      ch[ik+t4]   := cc[ik+t3] - cc[ik-1+t4];

      Inc(ik, 2);
    end;
  end;

  nac := True;
  if ido=2 then Exit;
  nac := False;

  for ik:=0 to idl1-1 do
    cc[ik] := ch[ik];

  for j:=1 to ip-1 do
    for k:=0 to l1-1 do
    begin
      cc[(k+j*l1)*ido+0] := ch[(k+j*l1)*ido+0];
      cc[(k+j*l1)*ido+1] := ch[(k+j*l1)*ido+1];
    end;

  if idot<=l1 then
  begin
    idij := 0;
    for j:=1 to ip-1 do
    begin
      Inc(idij, 2);
      i := 3;
      while i<ido do
      begin
        Inc(idij, 2);
        k := 0;
        while k < l1 do
        begin
          t1 := (k + j * l1) * ido;
          cc[i-1+t1] := wa[idij-2] * ch[i-1+t1] - isign*wa[idij-1] * ch[i+t1];
          cc[i+t1]   := wa[idij-2] * ch[i+t1]   + isign*wa[idij-1] * ch[i-1+t1];
          Inc(k);
        end;
        Inc(i, 2);
      end;
    end;
  end else
  begin
    idj := 2-ido;
    for j:=1 to ip-1 do
    begin
      Inc(idj,ido);
      for k:=0 to l1-1 do
      begin
        idij := idj;
        i := 3;
        while i<ido do
        begin
          Inc(idij, 2);
          t1 := (k+j*l1)*ido;
          cc[i-1+t1] := wa[idij-2] * ch[i-1+t1] - isign*wa[idij-1] * ch[i+t1];
          cc[i+t1]   := wa[idij-2] * ch[i+t1] + isign*wa[idij-1] * ch[i-1+t1];
          Inc(i, 2);
        end;
      end;
    end;
  end;
end; (* passf *)


(* ----------------------------------------------------------------------
radf2,radb2, radf3,radb3, radf4,radb4, radf5,radb5, radfg,radbg.
Treal FFT passes fwd and bwd.
---------------------------------------------------------------------- *)


procedure radf2(ido, l1: Int32; cc,ch,wa1: RealArrayRef);
var
  i,k,ic: Int32;
  ti2,tr2: Single;
begin
  for k:=0 to l1-1 do
  begin
    ch[2*k*ido]           := cc[k*ido] + cc[(k + l1)*ido];
    ch[(2*k+1)*ido+ido-1] := cc[k*ido] - cc[(k + l1)*ido];
  end;

  if ido<2 then Exit;

  if ido<>2 then
  begin
    for k:=0 to l1-1 do
    begin
      i := 2;
      while i<ido do
      begin
        ic  := ido - i;
        tr2 := wa1[i-2] * cc[i-1+(k+l1)*ido] + wa1[i-1] * cc[i+(k+l1)*ido];
        ti2 := wa1[i-2] * cc[i  +(k+l1)*ido] - wa1[i-1] * cc[i-1+(k+l1)*ido];
        ch[i+2*k*ido]       := cc[i+k*ido] + ti2;
        ch[ic+(2*k+1)*ido]  := ti2 - cc[i+k*ido];
        ch[i-1+2*k*ido]     := cc[i-1+k*ido] + tr2;
        ch[ic-1+(2*k+1)*ido]:= cc[i-1+k*ido] - tr2;
        Inc(i, 2);
      end;
    end;
    if ido mod 2=1 then Exit;
  end;

  for k :=0 to l1-1 do
  begin
    ch[(2*k+1)*ido]   := -cc[ido-1+(k+l1)*ido];
    ch[ido-1+2*k*ido] :=  cc[ido-1+k*ido];
  end;
end; (* radf2 *)


procedure radb2(ido, l1: Int32; cc,ch,wa1: RealArrayRef);
var
  i,k,ic: Int32;
  ti2,tr2: Single;
begin
  for k:=0 to l1-1 do
  begin
    ch[k*ido]      := cc[2*k*ido] + cc[ido-1+(2*k+1)*ido];
    ch[(k+l1)*ido] := cc[2*k*ido] - cc[ido-1+(2*k+1)*ido];
  end;

  if ido < 2 then Exit;

  if ido <> 2 then
  begin
    for k :=0 to l1-1 do
    begin
      i := 2;
      while i<ido do
      begin
        ic := ido-i;
        ch[i-1+k*ido] := cc[i-1+2*k*ido] + cc[ic-1+(2*k+1)*ido];
        tr2           := cc[i-1+2*k*ido] - cc[ic-1+(2*k+1)*ido];
        ch[i+k*ido]   := cc[i+2*k*ido] - cc[ic+(2*k+1)*ido];
        ti2           := cc[i+(2*k)*ido] + cc[ic+(2*k+1)*ido];
        ch[i-1+(k+l1)*ido] := wa1[i-2]*tr2 - wa1[i-1]*ti2;
        ch[i+(k+l1)*ido]   := wa1[i-2]*ti2 + wa1[i-1]*tr2;
        Inc(i, 2);
      end;
    end;
    if ido mod 2=1 then
      Exit;
  end;
  for k :=0 to l1-1 do
  begin
    ch[ido-1+k*ido]      := 2*cc[ido-1+2*k*ido];
    ch[ido-1+(k+l1)*ido] := -2*cc[(2*k+1*ido)];
  end;
end; (* radb2 *)


procedure radf3(ido,l1: Int32; cc,ch,wa1,wa2: RealArrayRef);
const
  TAUR: Single = -0.5;
  TAUI: Single = 0.866025403784439;
var
  i,k,ic: Int32;
  ci2,di2,di3,cr2,dr2,dr3,ti2,ti3,tr2,tr3: Single;
begin      
  for k:=0 to l1-1 do
  begin
    cr2 := cc[(k+l1)*ido] + cc[(k+2*l1)*ido];
    ch[3*k*ido] := cc[k*ido] + cr2;
    ch[(3*k+2)*ido] := taui*(cc[(k+l1*2)*ido] - cc[(k+l1)*ido]);
    ch[ido-1+(3*k+1)*ido] := cc[k*ido] + taur*cr2;
  end;

  if ido=1 then Exit;
  
  for k:=0 to l1-1 do
  begin
    i  := 2;
    while i<ido do
    begin
      ic  := ido-i;
      dr2 := wa1[i-2]*cc[i-1+(k+l1)*ido] + wa1[i-1]*cc[i+(k+l1)*ido];
      di2 := wa1[i-2]*cc[i+(k+l1)*ido] - wa1[i-1]*cc[i-1+(k+l1)*ido];
      dr3 := wa2[i-2]*cc[i-1+(k+l1*2)*ido] + wa2[i-1]*cc[i+(k+l1*2)*ido];
      di3 := wa2[i-2]*cc[i+(k+l1*2)*ido] - wa2[i-1]*cc[i-1+(k+l1*2)*ido];
      cr2 := dr2 + dr3;
      ci2 := di2 + di3;
      ch[i-1+3*k*ido] := cc[i-1+k*ido] + cr2;
      ch[i+3*k*ido]   := cc[i+k*ido] + ci2;
      tr2 := cc[i-1+k*ido] + taur*cr2;
      ti2 := cc[i+k*ido] + taur*ci2;
      tr3 := taui * (di2 - di3);
      ti3 := taui * (dr3 - dr2);
      ch[i-1+(3*k+2)*ido]  := tr2 + tr3;
      ch[ic-1+(3*k+1)*ido] := tr2 - tr3;
      ch[i+(3*k+2)*ido]    := ti2 + ti3;
      ch[ic+(3*k+1)*ido]   := ti3 - ti2;
      Inc(i, 2);
    end;
  end;
end; (* radf3 *)


procedure radb3(ido,l1: Int32; cc,ch,wa1,wa2: RealArrayRef);
const
  TAUR: Single = -0.5;
  TAUI: Single = 0.866025403784439;
var
  i,k,ic: Int32;
  ci2,ci3,di2,di3,cr2,cr3,dr2,dr3,ti2,tr2: Single;
begin      
  for k:=0 to l1-1 do
  begin
    tr2 := 2*cc[ido-1+(3*k+1)*ido];
    cr2 := cc[3*k*ido] + taur*tr2;
    ch[k*ido] := cc[3*k*ido] + tr2;
    ci3 := 2*taui*cc[3*k+2*ido];
    ch[(k+  l1)*ido] := cr2 - ci3;
    ch[(k+2*l1)*ido] := cr2 + ci3;
  end;

  if ido=1 then Exit;

  for k:=0 to l1-1 do
  begin
    i  := 2;
    while i<ido do
    begin
      ic  := ido-i;
      tr2 := cc[i-1+(3*k+2)*ido] + cc[ic-1+(3*k+1)*ido];
      cr2 := cc[i-1+3*k*ido] + taur*tr2;
      ch[i-1+k*ido] := cc[i-1+3*k*ido] + tr2;
      ti2 := cc[i+(3*k+2*ido)] - cc[ic+(3*k+1)*ido];
      ci2 := cc[i+3*k*ido] + taur*ti2;
      ch[i+k*ido] := cc[i+3*k*ido] + ti2;
      cr3 := taui*(cc[i-1+(3*k+2)*ido] - cc[ic-1+(3*k+1)*ido]);
      ci3 := taui*(cc[i+(3*k+2)*ido] + cc[ic+(3*k+1)*ido]);
      dr2 := cr2 - ci3;
      dr3 := cr2 + ci3;
      di2 := ci2 + cr3;
      di3 := ci2 - cr3;
      ch[i-1+(k+l1)*ido]   := wa1[i-2]*dr2 - wa1[i-1]*di2;
      ch[i+(k+l1)*ido]     := wa1[i-2]*di2 + wa1[i-1]*dr2;
      ch[i-1+(k+2*l1)*ido] := wa2[i-2]*dr3 - wa2[i-1]*di3;
      ch[i+(k+2*l1)*ido]   := wa2[i-2]*di3 + wa2[i-1]*dr3;
    
      Inc(i, 2);
    end;
  end;
end; (* radb3 *)


procedure radf4(ido,l1: Int32; cc,ch,wa1,wa2,wa3: RealArrayRef);
const
  hsqt2: Single = 0.7071067811865475;
var
  i,k,ic: Int32;
  ci2,ci3,ci4: Single;
  cr2,cr3,cr4: Single;
  ti1,ti2,ti3,ti4: Single;
  tr1,tr2,tr3,tr4: Single;
begin
  for k:=0 to l1-1 do
  begin
    tr1 := cc[(k+l1)*ido] + cc[(k+3*l1)*ido];
    tr2 := cc[k*ido] + cc[(k+2*l1)*ido];
    ch[4*k*ido] := tr1+tr2;
    ch[ido-1+(4*k+3)*ido] := tr2-tr1;
    ch[ido-1+(4*k+1)*ido] := cc[k*ido] - cc[(k+2*l1)*ido];
    ch[(4*k+2)*ido] := cc[(k+3*l1)*ido] - cc[(k+l1)*ido];
  end;

  if ido<2 then exit;

  if ido<>2 then
  begin
    for k:=0 to l1-1 do
    begin
      i := 2;
      while i<ido do
      begin
        ic  := ido-i;
        cr2 := wa1[i-2] * cc[i-1+(k+l1)*ido] + wa1[i-1] * cc[i+(k+l1)*ido];
        ci2 := wa1[i-2] * cc[i+(k+l1)*ido] - wa1[i-1] * cc[i-1+(k+l1)*ido];
        cr3 := wa2[i-2] * cc[i-1+(k+2*l1)*ido] + wa2[i-1] * cc[i+(k+2*l1)*ido];
        ci3 := wa2[i-2] * cc[i+(k+2*l1)*ido] - wa2[i-1] * cc[i-1+(k+2*l1)*ido];
        cr4 := wa3[i-2] * cc[i-1+(k+3*l1)*ido] + wa3[i-1] * cc[i+(k+3*l1)*ido];
        ci4 := wa3[i-2] * cc[i+(k+3*l1)*ido] - wa3[i-1] * cc[i-1+(k+3*l1)*ido];
        tr1 := cr2+cr4;
        tr4 := cr4-cr2;
        ti1 := ci2+ci4;
        ti4 := ci2-ci4;
        ti2 := cc[i+k*ido] + ci3;
        ti3 := cc[i+k*ido] - ci3;
        tr2 := cc[i-1+k*ido] + cr3;
        tr3 := cc[i-1+k*ido] - cr3;
        ch[i-1+4*k*ido]      := tr1 + tr2;
        ch[ic-1+(4*k+3)*ido] := tr2 - tr1;
        ch[i+4*k*ido]        := ti1 + ti2;
        ch[ic+(4*k+3)*ido]   := ti1 - ti2;
        ch[i-1+(4*k+2)*ido]  := ti4 + tr3;
        ch[ic-1+(4*k+1)*ido] := tr3 - ti4;
        ch[i+(4*k+2)*ido]    := tr4 + ti3;
        ch[ic+(4*k+1)*ido]   := tr4 - ti3;
        Inc(i, 2);
      end;
    end;
    if ido mod 2=1 then
      exit;
  end;

  for k:=0 to l1-1 do
  begin
    ti1 := -hsqt2*(cc[ido-1+(k+l1)*ido] + cc[ido-1+(k+3*l1)*ido]);
    tr1 :=  hsqt2*(cc[ido-1+(k+l1)*ido] - cc[ido-1+(k+3*l1)*ido]);
    ch[ido-1+4*k*ido]     := tr1+cc[ido-1+k*ido];
    ch[ido-1+(4*k+2)*ido] := cc[ido-1+k*ido] - tr1;
    ch[(4*k+1)*ido] := ti1 - cc[ido-1+(k+2*l1)*ido];
    ch[(4*k+3)*ido] := ti1 + cc[ido-1+(k+2*l1)*ido];
  end;
end; (* radf4 *)


procedure radb4(ido,l1: Int32;  cc,ch,wa1,wa2,wa3: RealArrayRef);
const
  SQRT2: Single = 1.414213562373095;
var
  i,k,ic: Int32;
  ci2,ci3,ci4: Single;
  cr2,cr3,cr4: Single;
  ti1,ti2,ti3,ti4: Single;
  tr1,tr2,tr3,tr4: Single;
begin
  for k:=0 to l1-1 do
  begin
    tr1 := cc[4*k*ido] - cc[ido-1+(4*k+3)*ido];
    tr2 := cc[4*k*ido] + cc[ido-1+(4*k+3)*ido];
    tr3 := cc[ido-1+(4*k+1)*ido] + cc[ido-1+(4*k+1)*ido];
    tr4 := cc[(4*k+2)*ido] + cc[(4*k+2)*ido];
    ch[k*ido]        := tr2 + tr3;
    ch[(k+l1)*ido]   := tr1 - tr4;
    ch[(k+2*l1)*ido] := tr2 - tr3;
    ch[(k+3*l1)*ido] := tr1 + tr4;
  end;
  if ido<2 then
    exit;

  if ido<>2 then
  begin
    for k:=0 to l1-1 do
    begin
      i := 2;
      while i<ido do
      begin
        ic  := ido - i;
        ti1 := cc[i+4*k*ido] + cc[ic+(4*k+3)*ido];
        ti2 := cc[i+4*k*ido] - cc[ic+(4*k+3)*ido];
        ti3 := cc[i+(4*k+2)*ido] - cc[ic+(4*k+1)*ido];
        tr4 := cc[i+(4*k+2)*ido] + cc[ic+(4*k+1)*ido];
        tr1 := cc[i-1+4*k*ido] - cc[ic-1+(4*k+3)*ido];
        tr2 := cc[i-1+4*k*ido] + cc[ic-1+(4*k+3)*ido];
        ti4 := cc[i-1+(4*k+2)*ido] - cc[ic-1+(4*k+1)*ido];
        tr3 := cc[i-1+(4*k+2)*ido] + cc[ic-1+(4*k+1)*ido];
        ch[i-1+k*ido] := tr2 + tr3;
        cr3 := tr2 - tr3;
        ch[i+k*ido] := ti2 + ti3;
        ci3 := ti2 - ti3;
        cr2 := tr1 - tr4;
        cr4 := tr1 + tr4;
        ci2 := ti1 + ti4;
        ci4 := ti1 - ti4;
        ch[i-1+(k+l1)*ido]   := wa1[i-2]*cr2 - wa1[i-1] * ci2;
        ch[i  +(k+l1)*ido]   := wa1[i-2]*ci2 + wa1[i-1] * cr2;
        ch[i-1+(k+2*l1)*ido] := wa2[i-2]*cr3 - wa2[i-1] * ci3;
        ch[i  +(k+2*l1)*ido] := wa2[i-2]*ci3 + wa2[i-1] * cr3;
        ch[i-1+(k+3*l1)*ido] := wa3[i-2]*cr4 - wa3[i-1] * ci4;
        ch[i  +(k+3*l1)*ido] := wa3[i-2]*ci4 + wa3[i-1] * cr4;
        Inc(i, 2);
      end;
    end;
    if ido mod 2=1 then
      exit;
  end;

  for k:=0 to l1-1 do
  begin
    ti1 := cc[(4*k+1)*ido] + cc[(4*k+3)*ido];
    ti2 := cc[(4*k+3)*ido] - cc[(4*k+1)*ido];
    tr1 := cc[ido-1+4*k*ido] - cc[ido-1+(4*k+2)*ido];
    tr2 := cc[ido-1+4*k*ido] + cc[ido-1+(4*k+2)*ido];
    ch[ido-1+k*ido]        := tr2+tr2;
    ch[ido-1+(k+l1)*ido]   := SQRT2*(tr1-ti1);
    ch[ido-1+(k+2*l1)*ido] := ti2+ti2;
    ch[ido-1+(k+3*l1)*ido] :=-SQRT2*(tr1+ti1);
  end;
end;(* radb4 *)


procedure radf5(ido,l1: Int32; cc,ch,wa1,wa2,wa3,wa4: RealArrayRef);
const
  tr11: Single = 0.309016994374947;
  ti11: Single = 0.951056516295154;
  tr12: Single =-0.809016994374947;
  ti12: Single = 0.587785252292473;
var
  i,k,ic: Int32;
  ci2,ci3,ci4,ci5: Single;
  di2,di3,di4,di5: Single;
  cr2,cr3,cr4,cr5: Single;
  dr2,dr3,dr4,dr5: Single;
  ti2,ti3,ti4,ti5: Single;
  tr2,tr3,tr4,tr5: Single;
begin              
  for k:=0 to l1-1 do
  begin
    cr2 := cc[(k+4*l1)*ido] + cc[(k+l1)*ido];
    ci5 := cc[(k+4*l1)*ido] - cc[(k+l1)*ido];
    cr3 := cc[(k+3*l1)*ido] + cc[(k+2*l1)*ido];
    ci4 := cc[(k+3*l1)*ido] - cc[(k+2*l1)*ido];
  
    ch[5*k*ido]           := cc[k*ido] + cr2 + cr3;
    ch[ido-1+(5*k+1)*ido] := cc[k*ido] + tr11*cr2 + tr12*cr3;
    ch[(5*k+2)*ido]       := ti11*ci5  + ti12*ci4;
    ch[ido-1+(5*k+3)*ido] := cc[k*ido] + tr12*cr2+tr11*cr3;
    ch[(5*k+4)*ido]       := ti12*ci5  - ti11*ci4;
  end;

  if ido=1 then Exit;

  for k:=0 to l1-1 do
  begin
    i := 2;
    while i<ido do
    begin
      ic  := ido-i;
      dr2 := wa1[i-2]*cc[i-1+(k+l1)*ido] + wa1[i-1] * cc[i+(k+l1)*ido];
      di2 := wa1[i-2]*cc[i+(k+l1)*ido] - wa1[i-1] * cc[i-1+(k+l1)*ido];
      dr3 := wa2[i-2]*cc[i-1+(k+2*l1)*ido] + wa2[i-1] * cc[i+(k+2*l1)*ido];
      di3 := wa2[i-2]*cc[i+(k+2*l1)*ido] - wa2[i-1] * cc[i-1+(k+2*l1)*ido];
      dr4 := wa3[i-2]*cc[i-1+(k+3*l1)*ido] + wa3[i-1] * cc[i+(k+3*l1)*ido];
      di4 := wa3[i-2]*cc[i+(k+3*l1)*ido] - wa3[i-1] * cc[i-1+(k+3*l1)*ido];
      dr5 := wa4[i-2]*cc[i-1+(k+4*l1)*ido] + wa4[i-1] * cc[i+(k+4*l1)*ido];
      di5 := wa4[i-2]*cc[i+(k+4*l1)*ido] - wa4[i-1] * cc[i-1+(k+4*l1)*ido];
      cr2 := dr2 + dr5;
      ci5 := dr5 - dr2;
      cr5 := di2 - di5;
      ci2 := di2 + di5;
      cr3 := dr3 + dr4;
      ci4 := dr4 - dr3;
      cr4 := di3 - di4;
      ci3 := di3 + di4;
      ch[i-1+5*k*ido] := cc[i-1+k*ido] + cr2 + cr3;
      ch[i+5*k*ido]   := cc[i+k*ido] + ci2 + ci3;
      tr2 := cc[i-1+k*ido] + tr11*cr2 + tr12*cr3;
      ti2 := cc[i + k*ido] + tr11*ci2 + tr12*ci3;
      tr3 := cc[i-1+k*ido] + tr12*cr2 + tr11*cr3;
      ti3 := cc[i + k*ido] + tr12*ci2 + tr11*ci3;
      tr5 := ti11*cr5 + ti12*cr4;
      ti5 := ti11*ci5 + ti12*ci4;
      tr4 := ti12*cr5 - ti11*cr4;
      ti4 := ti12*ci5 - ti11*ci4;
      ch[i -1+(5*k+2)*ido] := tr2 + tr5;
      ch[ic-1+(5*k+1)*ido] := tr2 - tr5;
      ch[i  + (5*k+2)*ido] := ti2 + ti5;
      ch[ic + (5*k+1)*ido] := ti5 - ti2;
      ch[i -1+(5*k+4)*ido] := tr3 + tr4;
      ch[ic-1+(5*k+3)*ido] := tr3 - tr4;
      ch[i  + (5*k+4)*ido] := ti3 + ti4;
      ch[ic + (5*k+3)*ido] := ti4 - ti3;
    
      Inc(i, 2);
    end;
  end;
end;(* radf5 *)


procedure radb5(ido,l1: Int32; cc,ch,wa1,wa2,wa3,wa4: RealArrayRef);
const
  tr11: Single = 0.309016994374947;
  ti11: Single = 0.951056516295154;
  tr12: Single =-0.809016994374947;
  ti12: Single = 0.587785252292473;
var
  i,k,ic: Int32;
  ci2,ci3,ci4,ci5: Single;
  di2,di3,di4,di5: Single;
  cr2,cr3,cr4,cr5: Single;
  ti2,ti3,ti4,ti5: Single;
  dr2,dr3,dr4,dr5: Single;
  tr2,tr3,tr4,tr5: Single;
begin
  for k:=0 to l1-1 do
  begin
    ti5 := 2*cc[(5*k+2)*ido];
    ti4 := 2*cc[(5*k+4)*ido];
    tr2 := 2*cc[ido-1+(5*k+1)*ido];
    tr3 := 2*cc[ido-1+(5*k+3)*ido];
    ch[k*ido] := cc[5*k*ido] + tr2 + tr3;
    cr2 := cc[5*k*ido] + tr11*tr2 + tr12*tr3;
    cr3 := cc[5*k*ido] + tr12*tr2 + tr11*tr3;
    ci5 := ti11*ti5 + ti12*ti4;
    ci4 := ti12*ti5 - ti11*ti4;
    ch[(k+l1)*ido]   := cr2 - ci5;
    ch[(k+2*l1)*ido] := cr3 - ci4;
    ch[(k+3*l1)*ido] := cr3 + ci4;
    ch[(k+4*l1)*ido] := cr2 + ci5;
  end;

  if ido=1 then
    exit;

  for k:=0 to l1-1 do
  begin
    i := 2;
    while i<ido do
    begin
      ic  := ido-i;
      ti5 := cc[i+(5*k+2)*ido] + cc[ic+(5*k+1)*ido];
      ti2 := cc[i+(5*k+2)*ido] - cc[ic+(5*k+1)*ido];
      ti4 := cc[i+(5*k+4)*ido] + cc[ic+(5*k+3)*ido];
      ti3 := cc[i+(5*k+4)*ido] - cc[ic+(5*k+3)*ido];
      tr5 := cc[i-1+(5*k+2)*ido] - cc[ic-1+(5*k+1)*ido];
      tr2 := cc[i-1+(5*k+2)*ido] + cc[ic-1+(5*k+1)*ido];
      tr4 := cc[i-1+(5*k+4)*ido] - cc[ic-1+(5*k+3)*ido];
      tr3 := cc[i-1+(5*k+4)*ido] + cc[ic-1+(5*k+3)*ido];
      ch[i-1+k*ido] := cc[i-1+5*k*ido] + tr2 + tr3;
      ch[i  +k*ido] := cc[i  +5*k*ido] + ti2 + ti3;
      cr2 := cc[i-1+5*k*ido] + tr11*tr2 + tr12*tr3;
      ci2 := cc[i+5*k*ido]   + tr11*ti2 + tr12*ti3;
      cr3 := cc[i-1+5*k*ido] + tr12*tr2 + tr11*tr3;
      ci3 := cc[i+5*k*ido]   + tr12*ti2 + tr11*ti3;
      cr5 := ti11*tr5 + ti12*tr4;
      ci5 := ti11*ti5 + ti12*ti4;
      cr4 := ti12*tr5 - ti11*tr4;
      ci4 := ti12*ti5 - ti11*ti4;
      dr3 := cr3 - ci4;
      dr4 := cr3 + ci4;
      di3 := ci3 + cr4;
      di4 := ci3 - cr4;
      dr5 := cr2 + ci5;
      dr2 := cr2 - ci5;
      di5 := ci2 - cr5;
      di2 := ci2 + cr5;
      ch[i-1+(k+l1)*ido]   := wa1[i-2]*dr2 - wa1[i-1]*di2;
      ch[i  +(k+l1)*ido]   := wa1[i-2]*di2 + wa1[i-1]*dr2;
      ch[i-1+(k+2*l1)*ido] := wa2[i-2]*dr3 - wa2[i-1]*di3;
      ch[i  +(k+2*l1)*ido] := wa2[i-2]*di3 + wa2[i-1]*dr3;
      ch[i-1+(k+3*l1)*ido] := wa3[i-2]*dr4 - wa3[i-1]*di4;
      ch[i  +(k+3*l1)*ido] := wa3[i-2]*di4 + wa3[i-1]*dr4;
      ch[i-1+(k+4*l1)*ido] := wa4[i-2]*dr5 - wa4[i-1]*di5;
      ch[i  +(k+4*l1)*ido] := wa4[i-2]*di5 + wa4[i-1]*dr5;
      Inc(i, 2);
    end;
  end;
end; (* radb5 *)


procedure radfg(ido,ip,l1,idl1: Int32;  cc,ch,wa: RealArrayRef);
var
  idij,ipph,i,j,k,l,j2,ic,jc,lc,ik,iz,nbd: Int32;
  dc2,ds2,dcp,arg,dsp,ar1h,ar2h: Single;
  ar1,ai1,ar2,ai2: Single;
begin
  arg := TWOPI / ip;
  dcp := Cos(arg);
  dsp := Sin(arg);
  ipph := (ip+1) div 2;
  nbd := (ido-1) div 2;
  if ido<>1 then
  begin
    for ik:=0 to idl1-1 do
      ch[ik] := cc[ik];
  
    for j:=1 to ip-1 do
      for k:=0 to l1-1 do
        ch[(k+j*l1)*ido] := cc[(k+j*l1)*ido];
  
    if nbd<=l1 then
    begin
      iz := -ido;
      for j:=1 to ip-1 do
      begin
        Inc(iz,ido);
        idij := iz-1;
      
        i := 2;
        while i<ido do
        begin
          Inc(idij, 2);
          for k:=0 to l1-1 do
          begin
            ch[i-1+(k+j*l1)*ido] := wa[idij-1]*cc[i-1+(k+j*l1)*ido]+wa[idij]*cc[i+(k+j*l1)*ido];
            ch[i+(k+j*l1)*ido]   := wa[idij-1]*cc[i+(k+j*l1)*ido]-wa[idij]*cc[i-1+(k+j*l1)*ido];
          end;
          Inc(i, 2);
        end;
      end;
    end
    else
    begin
      iz := -ido;
      for j:=1 to ip-1 do
      begin
        Inc(iz,ido);
        for k:=0 to l1-1 do
        begin
          idij := iz-1;
          i  := 2;
          while i<ido do
          begin
            Inc(idij,2);
            ch[i-1+(k+j*l1)*ido] := wa[idij-1]*cc[i-1+(k+j*l1)*ido]+wa[idij]*cc[i+(k+j*l1)*ido];
            ch[i+(k+j*l1)*ido]   := wa[idij-1]*cc[i+(k+j*l1)*ido]-wa[idij]*cc[i-1+(k+j*l1)*ido];
            Inc(i, 2);
          end;
        end;
      end;
    end;
  
    if nbd>=l1 then
    begin
      for j:=1 to ipph-1 do
      begin
        jc := ip-j;
        for k:=0 to l1-1 do
        begin
          i := 2;
          while i<ido do
          begin
            cc[i-1+(k+j*l1)*ido]  := ch[i-1+(k+j*l1)*ido]+ch[i-1+(k+jc*l1)*ido];
            cc[i-1+(k+jc*l1)*ido] := ch[i+(k+j*l1)*ido]-ch[i+(k+jc*l1)*ido];
            cc[i+(k+j*l1)*ido]    := ch[i+(k+j*l1)*ido]+ch[i+(k+jc*l1)*ido];
            cc[i+(k+jc*l1)*ido]   := ch[i-1+(k+jc*l1)*ido]-ch[i-1+(k+j*l1)*ido];
            Inc(i, 2);
          end;
        end;
      end;
    end
    else
    begin
      for j:=1 to ipph-1 do
      begin
        jc := ip-j;
        i := 2;
        while i<ido do
        begin
          for k:=0 to l1-1 do
          begin
            cc[i-1+(k+j*l1)*ido]  := ch[i-1+(k+j*l1)*ido]+ch[i-1+(k+jc*l1)*ido];
            cc[i-1+(k+jc*l1)*ido] := ch[i+(k+j*l1)*ido]-ch[i+(k+jc*l1)*ido];
            cc[i+(k+j*l1)*ido]    := ch[i+(k+j*l1)*ido]+ch[i+(k+jc*l1)*ido];
            cc[i+(k+jc*l1)*ido]   := ch[i-1+(k+jc*l1)*ido]-ch[i-1+(k+j*l1)*ido];
          end;
          Inc(i, 2);
        end;
      end;
    end;
  end else // now ido == 1
    for ik:=0 to idl1-1 do
      cc[ik] := ch[ik];


  for j:=1 to ipph-1 do
  begin
    jc := ip-j;
    for k:=0 to l1-1 do
    begin
      cc[(k+j*l1)*ido]  := ch[(k+j*l1)*ido]+ch[(k+jc*l1)*ido];
      cc[(k+jc*l1)*ido] := ch[(k+jc*l1)*ido]-ch[(k+j*l1)*ido];
    end;
  end;
  ar1 := 1;
  ai1 := 0;
  for l:=1 to ipph-1 do
  begin
    lc := ip-l;
    ar1h := dcp*ar1-dsp*ai1;
    ai1 := dcp*ai1+dsp*ar1;
    ar1 := ar1h;
    for ik:=0 to idl1-1 do
    begin
      ch[ik+l*idl1]  := cc[ik]+ar1*cc[ik+idl1];
      ch[ik+lc*idl1] := ai1*cc[ik+(ip-1)*idl1];
    end;
    dc2 := ar1;
    ds2 := ai1;
    ar2 := ar1;
    ai2 := ai1;
    for j:=2 to ipph-1 do
    begin
      jc := ip-j;
      ar2h := dc2*ar2-ds2*ai2;
      ai2 := dc2*ai2+ds2*ar2;
      ar2 := ar2h;
      for ik:=0 to idl1-1 do
      begin
        ch[ik+l*idl1]  := ch[ik+l*idl1]  + (ar2*cc[ik+j*idl1]);
        ch[ik+lc*idl1] := ch[ik+lc*idl1] + (ai2*cc[ik+jc*idl1]);
      end;
    end;
  end;
  for j:=1 to ipph-1 do
    for ik:=0 to idl1-1 do
      ch[ik] := ch[ik] + (cc[ik+j*idl1]);

  if ido>=l1 then begin
    for k:=0 to l1-1 do
      for i:=0 to ido-1 do
        cc[i+k*ip*ido] := ch[i+k*ido];
  end else
    for i:=0 to ido-1 do
      for k:=0 to l1-1  do
        cc[i+k*ip*ido] := ch[i+k*ido];

  for j:=1 to ipph-1 do
  begin
    jc := ip-j;
    j2 := 2*j;
    for k:=0 to l1-1 do
    begin
      cc[ido-1 + (j2 - 1 + k*ip)*ido] := ch[(k+j*l1)*ido];
      cc[(j2 + k*ip)*ido] := ch[(k+jc*l1)*ido];
    end;
  end;
  if ido=1 then
    exit;

  if nbd>=l1 then
  begin
    for j:=1 to ipph-1 do
    begin
      jc := ip-j;
      j2 := 2*j;
      for k:=0 to l1-1 do
      begin
        i  := 2;
        while i<ido do
        begin
          ic := ido-i;
          cc[i-1+(j2+k*ip)*ido]    := ch[i-1+(k+j*l1)*ido] + ch[i-1+(k+jc*l1)*ido];
          cc[ic-1+(j2-1+k*ip)*ido] := ch[i-1+(k+j*l1)*ido] - ch[i-1+(k+jc*l1)*ido];
          cc[i+(j2+k*ip)*ido]      := ch[i+(k+j*l1)*ido ]  + ch[i+(k+jc*l1)*ido];
          cc[ic+(j2-1+k*ip)*ido]   := ch[i+(k+jc*l1)*ido]  - ch[i+(k+j*l1)*ido ];
          Inc(i, 2);
        end;
      end;
    end;
  end
  else
  begin
    for j:=1 to ipph-1 do
    begin
      jc := ip-j;
      j2 := 2*j;
      i  := 2;
      while i<ido do
      begin
        ic := ido-i;
        for k:=0 to l1-1 do
        begin
          cc[i-1+(j2+k*ip)*ido]    := ch[i-1+(k+j*l1)*ido] + ch[i-1+(k+jc*l1)*ido];
          cc[ic-1+(j2-1+k*ip)*ido] := ch[i-1+(k+j*l1)*ido] - ch[i-1+(k+jc*l1)*ido];
          cc[i+(j2+k*ip)*ido]      := ch[i+(k+j*l1)*ido ]  + ch[i+(k+jc*l1)*ido];
          cc[ic+(j2-1+k*ip)*ido]   := ch[i+(k+jc*l1)*ido]  - ch[i+(k+j*l1)*ido ];
        end;
        Inc(i, 2);
      end;
    end;
  end;
end;
(* radfg *)


procedure radbg(ido,ip,l1,idl1: Int32; cc,ch,wa: RealArrayRef);
var
  idij,ipph,i,j,k,l,j2,ic,jc,lc,ik,iz: Int32;
  dc2,ds2,nbd,dcp,arg,dsp,ar1h,ar2h: Single;
  ai1,ai2,ar1,ar2: Single;
begin          
  arg := TWOPI / ip;
  dcp := Cos(arg);
  dsp := Sin(arg);
  nbd := (ido-1) div 2;
  ipph := (ip+1) div 2;
  if ido>=l1 then begin
    for k:=0 to l1-1 do
      for i:=0 to ido-1 do
        ch[i+k*ido] := cc[i+k*ip*ido];
  end else
  begin
    for i:=0 to ido-1 do
      for k:=0 to l1-1 do
        ch[i+k*ido] := cc[i+k*ip*ido];
  end;

  for j:=1 to ipph-1 do
  begin
    jc := ip-j;
    j2 := 2*j;
    for k:=0 to l1-1 do
    begin
      ch[(k+j*l1)*ido]  := cc[ido-1+(j2-1+k*ip)*ido] + cc[ido-1+(j2-1+k*ip)*ido];
      ch[(k+jc*l1)*ido] := cc[(j2+k*ip)*ido] + cc[(j2+k*ip)*ido];
    end;
  end;

  if ido<>1 then
  begin
    if nbd>=l1 then
    begin
      for j:=1 to ipph-1 do
      begin
        jc := ip-j;
        for k:=0 to l1-1 do
        begin
          i := 2;
          while i<ido do
          begin
            ic := ido-i;
            ch[i-1+(k+j *l1)*ido] := cc[i-1+(2*j+k*ip)*ido] + cc[ic-1+(2*j-1+k*ip)*ido];
            ch[i-1+(k+jc*l1)*ido] := cc[i-1+(2*j+k*ip)*ido] - cc[ic-1+(2*j-1+k*ip)*ido];
            ch[i+(k+j *l1)*ido]   := cc[i+(2*j+k*ip)*ido] - cc[ic+(2*j-1+k*ip)*ido];
            ch[i+(k+jc*l1)*ido]   := cc[i+(2*j+k*ip)*ido] + cc[ic+(2*j-1+k*ip)*ido];
            Inc(i, 2);
          end;
        end;
      end;
    end else
    begin
      for j:=1 to ipph-1 do
      begin
        jc := ip-j;
        i := 2;
        while i<ido do
        begin
          ic := ido-i;
          for k:=0 to l1-1 do
          begin
            ch[i-1+(k+j*l1)*ido]  := cc[i-1+(2*j+k*ip)*ido] + cc[ic-1+(2*j-1+k*ip)*ido];
            ch[i-1+(k+jc*l1)*ido] := cc[i-1+(2*j+k*ip)*ido] - cc[ic-1+(2*j-1+k*ip)*ido];
            ch[i+(k+j*l1)*ido]    := cc[i+(2*j+k*ip)*ido] - cc[ic+(2*j-1+k*ip)*ido];
            ch[i+(k+jc*l1)*ido]   := cc[i+(2*j+k*ip)*ido] + cc[ic+(2*j-1+k*ip)*ido];
          end;
          Inc(i, 2);
        end;
      end;
    end;
  end;

  ar1 := 1;
  ai1 := 0;
  for l:=1 to ipph-1 do
  begin
    lc := ip-l;
    ar1h := dcp*ar1-dsp*ai1;
    ai1 := dcp*ai1+dsp*ar1;
    ar1 := ar1h;
    for ik:=0 to idl1-1 do
    begin
      cc[ik+l*idl1]  := ch[ik]+ar1*ch[ik+idl1];
      cc[ik+lc*idl1] := ai1*ch[ik+(ip-1)*idl1];
    end;
    dc2 := ar1;
    ds2 := ai1;
    ar2 := ar1;
    ai2 := ai1;
    for j:=2 to ipph-1 do
    begin
      jc := ip-j;
      ar2h := dc2*ar2-ds2*ai2;
      ai2 := dc2*ai2+ds2*ar2;
      ar2 := ar2h;
      for ik :=0 to idl1-1 do
      begin
        cc[ik+l*idl1]  := cc[ik+l*idl1] + (ar2*ch[ik+j*idl1]);
        cc[ik+lc*idl1] := cc[ik+lc*idl1] + (ai2*ch[ik+jc*idl1]);
      end;
    end;
  end;

  for j:=1 to ipph-1 do
    for ik:=0 to idl1-1 do
      ch[ik] := ch[ik] + ch[ik+j*idl1];

  for j:=1 to ipph-1 do
  begin
    jc := ip-j;
    for k:=0 to l1-1 do
    begin
      ch[(k+j*l1)*ido]  := cc[(k+j*l1)*ido]-cc[(k+jc*l1)*ido];
      ch[(k+jc*l1)*ido] := cc[(k+j*l1)*ido]+cc[(k+jc*l1)*ido];
    end;
  end;

  if ido = 1 then Exit;

  if nbd >= l1 then
  begin
    for j:=1 to ipph-1 do
    begin
      jc := ip-j;
      for k:=0 to l1-1 do
      begin
        i := 2;
        while i<ido do
        begin
          ch[i-1+(k+j*l1)*ido]  := cc[i-1+(k+j*l1)*ido] - cc[i+(k+jc*l1)*ido];
          ch[i-1+(k+jc*l1)*ido] := cc[i-1+(k+j*l1)*ido] + cc[i+(k+jc*l1)*ido];
          ch[i+(k+j*l1)*ido]    := cc[i+(k+j*l1)*ido] + cc[i-1+(k+jc*l1)*ido];
          ch[i+(k+jc*l1)*ido]   := cc[i+(k+j*l1)*ido] - cc[i-1+(k+jc*l1)*ido];
          Inc(i, 2);
        end;
      end;
    end;
  end else
  begin
    for j:=1 to ipph-1 do
    begin
      jc := ip-j;
      i := 2;
      while i<ido do
      begin
        for k:=0 to l1-1 do
        begin
          ch[i-1+(k+j *l1)*ido] := cc[i-1+(k+j*l1)*ido] - cc[i+(k+jc*l1)*ido];
          ch[i-1+(k+jc*l1)*ido] := cc[i-1+(k+j*l1)*ido] + cc[i+(k+jc*l1)*ido];
          ch[i+(k+j *l1)*ido]   := cc[i+(k+j*l1)*ido] + cc[i-1+(k+jc*l1)*ido];
          ch[i+(k+jc*l1)*ido]   := cc[i+(k+j*l1)*ido] - cc[i-1+(k+jc*l1)*ido];
        end;
        Inc(i, 2);
      end;
    end;
  end;

  for ik:=0 to idl1-1 do
    cc[ik] := ch[ik];

  for j:=1 to ip-1 do
    for k:=0 to l1-1 do
      cc[(k+j*l1)*ido] := ch[(k+j*l1)*ido];

  if nbd<=l1 then
  begin
    iz := -ido;
    for j:=1 to ip-1 do
    begin
      Inc(iz,ido);
      idij := iz-1;
      i  := 2;
      while i<ido do
      begin
        idij := idij + (2);
        for k:=0 to l1-1 do
        begin
          cc[i-1+(k+j*l1)*ido] := wa[idij-1] * ch[i-1+(k+j*l1)*ido] - wa[idij]*ch[i  +(k+j*l1)*ido];
          cc[i  +(k+j*l1)*ido] := wa[idij-1] * ch[i  +(k+j*l1)*ido] + wa[idij]*ch[i-1+(k+j*l1)*ido];
        end;
        Inc(i, 2);
      end;
    end;
  end
  else
  begin
    iz := -ido;
    for j:=1 to ip-1 do
    begin
      Inc(iz,ido);
      for k:=0 to l1-1 do
      begin
        idij := iz-1;
        i := 2;
        while i<ido do
        begin
          Inc(idij,2);
          cc[i-1+(k+j*l1)*ido] := wa[idij-1] * ch[i-1+(k+j*l1)*ido] - wa[idij]*ch[i  +(k+j*l1)*ido];
          cc[i  +(k+j*l1)*ido] := wa[idij-1] * ch[i  +(k+j*l1)*ido] + wa[idij]*ch[i-1+(k+j*l1)*ido];
          Inc(i, 2);
        end;
      end;
    end;
  end;
end; (* radbg *)

(* ----------------------------------------------------------------------
cfftf1, cfftf, cfftb, cffti1, cffti. Complex FFTs.
---------------------------------------------------------------------- *)


procedure cfftf1(n: Int32; c,ch,wa: RealArrayRef; ifac:IntArrayRef; isign: Int32);
var
  na, nac: Boolean;
  idot,i,k1,l1,l2,nf,ip,iw, ix2,ix3,ix4,ido,idl1: Int32;
  cinput, coutput: RealArrayRef;
begin
  nf := ifac[1];
  na := False;
  l1 := 1;
  iw := 0;
  for k1:=2 to nf+1 do
  begin
    ip := ifac[k1];
    l2 := ip*l1;
    ido := n div l2;
    idot := ido+ido;
    idl1 := idot*l1;
    if na then begin
      cinput := ch;
      coutput := c;
    end else begin
      cinput := c;
      coutput := ch;
    end;

    case ip of
      4:begin
          ix2 := iw+idot;
          ix3 := ix2+idot;
          passf4(idot,l1,cinput,coutput,@wa[iw],@wa[ix2],@wa[ix3],isign);
          na := not na;
        end;
      2:begin
          passf2(idot,l1,cinput,coutput,@wa[iw],isign);
          na := not na;
        end;
      3:begin
          ix2 := iw+idot;
          passf3(idot,l1,cinput,coutput,@wa[iw],@wa[ix2],isign);
          na := not na;
        end;
      5:begin
          ix2 := iw+idot;
          ix3 := ix2+idot;
          ix4 := ix3+idot;
          passf5(idot,l1,cinput,coutput,@wa[iw],@wa[ix2],@wa[ix3],@wa[ix4],isign);
          na := not na;
        end;
      else
      begin
        passf(nac,idot,ip,l1,idl1,cinput,coutput,@wa[iw],isign);
        if nac then
          na := not na;
      end;
    end;
    l1 := l2;
    Inc(iw, (ip - 1)*idot);
  end;
  if not na then Exit;

  for i:=0 to 2*n-1 do c[i] := ch[i];
end; (* cfftf1 *)



procedure cfftf(n: Int32; c, wsave: RealArrayRef);
var iw1,iw2: Int32;
begin
  if n=1 then Exit;
  iw1  := 2*n;
  iw2  := iw1+2*n;
  cfftf1(n,c,wsave,wsave+iw1, IntArrayRef(wsave+iw2), -1);
end; (* cfftf *)


procedure cfftb(n: Int32; c, wsave: RealArrayRef);
var iw1, iw2: Int32;
begin
  if n=1 then Exit;
  iw1 := 2*n;
  iw2 := iw1+2*n;
  cfftf1(n,c,wsave,wsave+iw1, IntArrayRef(wsave+iw2), +1);
end; (* cfftb *)


(*
  Factorize n in factors in ntryh and rest. On exit,
  ifac[0] contains n and ifac[1] contains number of factors,
  the factors start from ifac[2].
*)
                            //ifac[MAXFAC+2]     ntryh[NSPECIAL])
procedure Factorize(n: Int32; ifac: IntArrayRef; ntryh: TSPECIAL);
var
  ntry,i,j,ib,nf,nl,nq,nr: Int32;
label
  startloop;
begin
  ntry := 3;
  j  := 0;
  nf := 0;
  nl := n;

startloop:
  if(j < NSPECIAL) then
    ntry := ntryh[j]
  else
    Inc(ntry,2);

  Inc(j);
  repeat
    nq := nl div ntry;
    nr := nl-ntry*nq;
    if nr<>0 then
      goto startloop;
  
    Inc(nf);
    ifac[nf+1] := ntry;
    nl := nq;
    if (ntry=2) and (nf<>1) then
    begin
      for i :=2 to nf do
      begin
        ib := nf-i+2;
        ifac[ib+1] := ifac[ib];
      end;
      ifac[2] := 2;
    end;
  until nl = 1;
  ifac[0] := n;
  ifac[1] := nf;
end;


procedure cffti1(n: Int32;  wa: RealArrayRef;  ifac: IntArrayRef);
const
  twopi: Single = 2*PI;
  ntryh: TSPECIAL = (3,4,2,5); // Do not change the order of these.
var
  arg,argh,argld,fi: Single;
  idot,i,j,i1,k1,l1,l2,ld,ii,nf,ip,ido,ipm: Int32;
begin
  Factorize(n,ifac,ntryh);
  nf   := ifac[1];
  argh := twopi / n;
  i  := 1;
  l1 := 1;
  for k1:=1 to nf do
  begin
    ip := ifac[k1+1];
    ld := 0;
    l2 := l1*ip;
    ido := n div l2;
    idot := ido+ido+2;
    ipm := ip-1;
  
    for j:=1 to ipm do
    begin
      i1 := i;
      wa[i-1] := 1;
      wa[i] := 0;
      Inc(ld, l1);
      argld := ld*argh;
      fi := 0;
      ii := 4;
      while ii<=idot do
      begin
        Inc(i, 2);
        fi += 1;
        arg := fi*argld;
        SinCos(arg, wa[i], wa[i-1]);
        Inc(ii, 2);
      end;
      if ip>5 then
      begin
        wa[i1-1] := wa[i-1];
        wa[i1]   := wa[i];
      end;
    end;
    l1 := l2;
  end;
end; (* cffti1 *)


procedure cffti(n: Int32; wsave: RealArrayRef);
var
  iw1,iw2: Int32;
begin
  if n=1 then Exit;
  iw1 := 2*n;
  iw2 := iw1+2*n;
  cffti1(n,wsave+iw1, IntArrayRef(wsave+iw2));
end; (* cffti *)

(* ----------------------------------------------------------------------
rfftf1, rfftb1, rfftf, rfftb, rffti1, rffti. Treal FFTs.
---------------------------------------------------------------------- *)


procedure rfftf1(n: Int32; c,ch,wa: RealArrayRef; ifac: IntArrayRef);
var
  na: Boolean;
  i,k1,l1,l2,kh,nf,ip,iw,ix2,ix3,ix4,ido,idl1: Int32;
  cinput, coutput: RealArrayRef;
begin      
  nf := ifac[1];
  na := True;
  l2 := n;
  iw := n-1;
  for k1:=1 to nf do
  begin
    kh := nf-k1;
    ip := ifac[kh+2];
    l1 := l2 div ip;
    ido := n div l2;
    idl1 := ido*l1;
    iw := iw - ((ip-1)*ido);
    na := not na;
    if na then
    begin
      cinput := ch;
      coutput := c;
    end else
    begin
      cinput := c;
      coutput := ch;
    end;
  
    case ip of
      4:begin
          ix2 := iw+ido;
          ix3 := ix2+ido;
          radf4(ido,l1,cinput,coutput,@wa[iw],@wa[ix2],@wa[ix3]);
        end;
      2:begin
          radf2(ido,l1,cinput,coutput,@wa[iw]);
        end;
      3:begin
          ix2 := iw+ido;
          radf3(ido,l1,cinput,coutput,@wa[iw],@wa[ix2]);
        end;
      5:begin
          ix2 := iw+ido;
          ix3 := ix2+ido;
          ix4 := ix3+ido;
          radf5(ido,l1,cinput,coutput,@wa[iw],@wa[ix2],@wa[ix3],@wa[ix4]);
        end;
      else
      begin
        if(ido=1) then
        na := not na;
        if(not na) then
        begin
          radfg(ido,ip,l1,idl1,c,ch,@wa[iw]);
          na := True;
        end else
        begin
          radfg(ido,ip,l1,idl1,ch,c,@wa[iw]);
          na := False;
        end;
      end;
    end;
    l2 := l1;
  end;

  if na then exit;

  for i:=0 to n-1 do
    c[i] := ch[i];
end; (* rfftf1 *)


procedure rfftb1(n: Int32; c,ch,wa: RealArrayRef; ifac: IntArrayRef);
var
  na: Boolean;
  i,k1,l1,l2,nf,ip,iw,ix2,ix3,ix4,ido,idl1: Int32;
  cinput, coutput: RealArrayRef;
begin      
  nf := ifac[1];
  na := False;
  l1 := 1;
  iw := 0;
  for k1:=1 to nf do
  begin
    ip := ifac[k1+1];
    l2 := ip*l1;
    ido := n div l2;
    idl1 := ido*l1;
    if na then
    begin
      cinput := ch;
      coutput := c;
    end else
    begin
      cinput := c;
      coutput := ch;
    end;
  
    case ip of
      4:begin
          ix2 := iw+ido;
          ix3 := ix2+ido;
          radb4(ido,l1,cinput,coutput,@wa[iw],@wa[ix2],@wa[ix3]);
          na := not na;
        end;
      2:begin
          radb2(ido,l1,cinput,coutput,@wa[iw]);
          na := not na;
        end;
      3:begin
          ix2 := iw+ido;
          radb3(ido,l1,cinput,coutput,@wa[iw],@wa[ix2]);
          na := not na;
        end;
      5:begin
          ix2 := iw+ido;
          ix3 := ix2+ido;
          ix4 := ix3+ido;
          radb5(ido,l1,cinput,coutput,@wa[iw],@wa[ix2],@wa[ix3],@wa[ix4]);
          na := not na;
        end;
      else
      begin
        radbg(ido,ip,l1,idl1,cinput,coutput,@wa[iw]);
        if ido=1 then na := not na;
      end;
    end;
    l1 := l2;
    iw := iw + ((ip-1)*ido);
  end;

  if not na then
    exit;

  for i :=0 to n-1 do c[i] := ch[i];
end; (* rfftb1 *)


procedure rfftf(n: Int32; r, wsave: RealArrayRef);
begin
  if n=1 then Exit;
  rfftf1(n,r,wsave,wsave+n, IntArrayRef(wsave+2*n));
end; (* rfftf *)


procedure rfftb(n: Int32; r, wsave: RealArrayRef);
begin
  if n=1 then Exit;
  rfftb1(n,r,wsave, wsave+n, IntArrayRef(wsave+2*n));
end; (* rfftb *)


procedure rffti1(n: Int32; wa: RealArrayRef; ifac: IntArrayRef);
const
  ntryh: TSPECIAL = (4,2,3,5); (* Do not change the order of these. *)
var
  arg,argh,argld,fi: Single;
  i,j,k1,l1,l2,ld,ii,nf,ip,iz,ido,ipm,nfm1: Int32;
begin
  factorize(n,ifac,ntryh);
  nf := ifac[1];
  argh := twopi / n;
  iz := 0;
  nfm1 := nf-1;
  l1 := 1;
  if nfm1=0 then Exit;

  for k1 :=1 to nfm1 do
  begin
    ip := ifac[k1+1];
    ld := 0;
    l2 := l1*ip;
    ido := n div l2;
    ipm := ip-1;
    for j :=1 to ipm do
    begin
      Inc(ld, l1);
      i := iz;
      argld := ld*argh;
      fi := 0;
      ii := 3;
      while ii<=ido do
      begin
        Inc(i,2);
        fi += 1;
        arg := fi * argld;
        SinCos(arg, wa[i-1], wa[i-2]);
        Inc(ii, 2);
      end;
      Inc(iz,ido);
    end;
    l1 := l2;
  end;
end; (* rffti1 *)


procedure rffti(n: Int32; wsave: RealArrayRef);
begin
  if n=1 then Exit;
  rffti1(n, wsave+n, IntArrayRef(wsave+2*n));
end; (* rffti *)

end.
