{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by the Free Pascal development team

    QOI reader implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}{$h+}

unit fpqoi_simba;

interface

uses
  Classes, SysUtils, FPImage;

type
  PQoiHeader = ^TQoiHeader;
  TQoiHeader = packed record
     magic  : array [0..3] of char; { magic bytes 'qoif' }
     width  : DWord;                { image width in pixels (BE)}
     height : DWord;                { image height in pixels (BE)}
     channels   : byte;             { 3 = RGB, 4 = RGBA }
     colorspace : byte;             { 0 = sRGB with linear alpha }
                                    { 1 = all channels linear }
 end;

  PQoiPixel = ^TQoiPixel;
  TQoiPixel = packed record
     r,g,b,a : byte;
  end;

  TFPReaderQoi = class(TFPCustomImageReader)
  Private
    QoiHeader: TQoiHeader;  // The header as read from the stream.
    function getUseAlpha:Boolean;
  protected
    // required by TFPCustomImageReader
    procedure InternalRead  (Stream:TStream; Img:TFPCustomImage); override;
    function  InternalCheck (Stream:TStream) : Boolean; override;
  public
    property UseAlpha : Boolean read getUseAlpha;
  end;

  TFPWriterQoi = class(TFPCustomImageWriter)
  private
    QoiHeader: TQoiHeader;
    procedure setUseAlpha(useAlpha:Boolean);
    function getUseAlpha:Boolean;
  protected
    function  SaveHeader(Stream:TStream; Img: TFPCustomImage):Boolean; virtual;
    procedure InternalWrite (Stream:TStream; Img: TFPCustomImage); override;
  public
    constructor Create; override;
    property useAlpha:Boolean read getUseAlpha write setUseAlpha;
  end;

implementation

uses
  Graphics;

{$R-}
{$O-}

const
  qoChannelRGB  = 3;
  qoChannelRGBA = 4;

function Swap32(a: DWord): DWord;
var h, l : DWord;
begin
  a := RolDWord(a,16);
  h := a shr 8;
  h := h and $ff00ff;
  l := a and $ff00ff;
  l := l shl 8;

  Result := h or l;
end;

function QoiPixelIndex(const px: TQoiPixel): DWord; inline;
begin
  Result := (DWord(px.r)*3+DWord(px.g)*5+DWord(px.b)*7+DWord(px.a)*11) and 63;
end;

function RGBAToFPColor(const RGBA: TQoiPixel): TFPcolor; inline;
begin
  with Result, RGBA do
  begin
    Red   := (R shl 8) or R;
    Green := (G shl 8) or G;
    Blue  := (B shl 8) or B;
    Alpha := (A shl 8) or A;
  end;
end;

function TFPReaderQoi.getUseAlpha:boolean;
begin
  Result := (QoiHeader.channels=qoChannelRGBA);
end;

function  TFPReaderQoi.InternalCheck (Stream:TStream) : boolean;
var
  n: Int64;
begin
  Result:=False;
  if Stream=nil then
    exit;
  n:=SizeOf(TQoiHeader);
  Result:=Stream.Read(QoiHeader,n)=n;
  if Result then
  begin
    {$IFDEF ENDIAN_LITTLE}
    QoiHeader.width:=Swap32(QoiHeader.width);
     QoiHeader.height:=Swap32(QoiHeader.height);
   {$ENDIF}
    Result := (QoiHeader.magic = 'qoif'); // Just check magic number
  end;
end;

// NOTE: It is assumed that signature and IDHDR chunk already have been read.
procedure TFPReaderQoi.InternalRead (Stream:TStream; Img:TFPCustomImage);
var iP, q : qword;
    b, run : byte;
    g : shortint;
    px : TQoiPixel;
    arr : array [0..63] of TQoiPixel;
    iA : DWord; {index in pixel array}

    p: pbyte; orgSize, imgSize : qword;
    Row, Col, w, h : DWord;
    aLine : pbyte;


begin
     with QoiHeader do
     begin
       Img.SetSize (Width, Height);
       imgSize := Width * Height;
       w:=Width;
       h:=Height;
     end;


     orgSize:=Stream.size;
     orgSize:=orgSize-sizeof(TQoiHeader);
     getmem(aLine,orgSize);

     q:=Stream.Read(aLine^,orgSize);
     if orgSize>q then orgSize:=q;


     ip:=0;
     q:=0;
     p:=aLine;

     DWord(px):=0;
     px.a:=255;

     {initalize previosly seen pixel array}
     //fillchar(arr,sizeof(arr),0);
     iA:=QoiPixelIndex(px);
     //for iA:=0 to 63 do
     arr[iA]:=px;

     Row:=0;
     Col:=0;


     {actual decoding loop}
     while (orgSize> ip) and (imgSize>q) do
     begin
          b:=p^;
          inc(p);
          inc(ip);

          case (b shr 6) of
             0: begin  { pixel from previos pixel array}

                     if b = p^ then {deal with end of encoding}
                     begin
                          if b = 0 then
                          begin
                               dec(p);
                               for iA:=0 to 6 do
                               begin
                                    b:=p^;
                                    inc(p);
                                    inc(ip);
                                    if b<>0 then break;
                               end;
                               if b<>0 then
                               begin
                                    {invalid encoding}
                                    break;
                               end;
                               b:=p^;
                               inc(p);
                               inc(ip);
                               if b = 1 then
                               begin
                                    //writeln('end of encoding ');
                                    {success - no more encoded pixels}
                                    break;
                               end else
                               begin
                                    {invalid encoding}
                                    break;
                               end;

                          end else
                          begin
                               {invalid encoding}
                               break;
                          end;
                     end;

                     {pixel from array}
                     iA:= b and 63;
                     px:=arr[iA];
                     img.Colors[Col,Row] := RGBAToFPColor( px );
                     inc(q);
                     inc(Col);
                     if Col = w then begin inc(Row); Col:=0; if Row>=h then break; end;

                end;

             1: begin { diff }
                     b:=b and 63;
                     px.r:=px.r+ byte(b shr 4) and 3+shortint(-2);
                     px.g:=px.g+ byte(b shr 2) and 3+shortint(-2);
                     px.b:=px.b+ byte(b shr 0) and 3+shortint(-2);

                     img.Colors[Col,Row] := RGBAToFPColor( px );
                     inc(q);
                     inc(Col);
                     if Col = w then begin inc(Row); Col:=0; if Row>=h then break; end;
                     iA:=QoiPixelIndex(px);
                     arr[iA]:=px;

                end;

             2: begin { luma }
                     g:=b and 63 - 32;
                     b:=p^;
                     inc(p);
                     inc(ip);
                     px.g:=px.g + g;
                     px.r:=px.r+g+shortint((b shr 4)-8);
                     px.b:=px.b+g+shortint((b and 15)-8);
                     img.Colors[Col,Row] := RGBAToFPColor( px );
                     inc(q);
                     inc(Col);
                     if Col = w then begin inc(Row); Col:=0; if Row>=h then break; end;
                     iA:=QoiPixelIndex(px);
                     arr[iA]:=px;


                end;

             3: begin
                     run:=b and 63+1;
                     case run of
                       64: begin  { rgba }
                                px.r:=p^;
                                inc(p);
                                px.g:=p^;
                                inc(p);
                                px.b:=p^;
                                inc(p);
                                px.a:=p^;
                                inc(p);
                                inc(ip,4);
                                img.Colors[Col,Row] := RGBAToFPColor( px );
                                inc(q);
                                inc(Col);
                                if Col = w then begin inc(Row); Col:=0; if Row>=h then break; end;
                                iA:=QoiPixelIndex(px);
                                arr[iA]:=px;

                          end;
                       63: begin  { rgb  }
                                px.r:=p^;
                                inc(p);
                                px.g:=p^;
                                inc(p);
                                px.b:=p^;
                                inc(p);
                                inc(ip,3);
                                img.Colors[Col,Row] := RGBAToFPColor( px );
                                inc(q);
                                inc(Col);
                                if Col = w then begin inc(Row); Col:=0; if Row>=h then break; end;
                                iA:=QoiPixelIndex(px);
                                arr[iA]:=px;

                          end;
                       otherwise { run - repeat previos pixel}
                            repeat
                                img.Colors[Col,Row] := RGBAToFPColor( px );
                                inc(q);
                                inc(Col);
                                if Col = w then begin inc(Row); Col:=0; if Row>=h then break; end;
                                dec(run);

                            until run =0;
                       end;
                end;

          end;   {case of }
     end; { while do}
     freeMem(aLine);
end;

constructor TFPWriterQoi.create;
begin
  inherited create;
  with QoiHeader do
    begin
      magic:='qoif';
      channels:=qoChannelRGB;
      colorspace:=0;
    end;
end;

procedure TFPWriterQoi.setUseAlpha(useAlpha:boolean);
begin
   if useAlpha then QoiHeader.channels := qoChannelRGBA else QoiHeader.channels:=qoChannelRGB;
end;

function TFPWriterQoi.getUseAlpha:boolean;
begin
  Result:= (QoiHeader.channels=qoChannelRGBA);
end;

function TFPWriterQoi.SaveHeader(Stream:TStream; Img : TFPCustomImage):boolean;
begin
  Result:=False;
  with QoiHeader do
    begin
      Width:=Img.Width;
      Height:=Img.Height;
      //writeln('Save width ',width, '   height  ', height);
    end;

  {$IFDEF ENDIAN_LITTLE}
  QoiHeader.width:=Swap32(QoiHeader.width);
  QoiHeader.height:=Swap32(QoiHeader.height);
  {$ENDIF}

  //writeln('Save width 2 ',QoiHeader.width, '   height  ', QoiHeader.height);
  Stream.Write(QoiHeader,sizeof(TQoiHeader));

  {$IFDEF ENDIAN_LITTLE}
  QoiHeader.width:=Swap32(QoiHeader.width);
  QoiHeader.height:=Swap32(QoiHeader.height);
  {$ENDIF}
  Result:=true;
end;

procedure TFPWriterQoi.InternalWrite (Stream:TStream; Img:TFPCustomImage);
var
  Row,Col,RowSize:sizeuint;
  h, w, orgSize, mSize : sizeuint;
  aLine, p: PByte;

  color : TFPColor;


var iP,  iq, imgSize : qword;
    //q: PQoiPixel;
    b, run : byte;
    g, dr, dg, db, dr_dg, db_dg : byte;// shortint;
    px, cx : TQoiPixel;
    arr : array [0..63] of TQoiPixel;
    iA : DWord; {index in pixel array}
    //endOf : qword;

begin
    mSize:= img.Width * sizeof(TQoiPixel)+ img.Width;
    RowSize:= img.Width * sizeof(TQoiPixel)+ img.Width + 8+sizeof(TQoiPixel)*64+64;

    SaveHeader(Stream,Img); { write the headers }

    GetMem(aLine,RowSize);

    p:=aLine;

    DWord(px):=0;
    px.a:=255;

    {initalize previosly seen pixel array}
    fillchar(arr,sizeof(arr),0);
    iA:=QoiPixelIndex(px);
     //for iA:=0 to 63 do
     //arr[iA]:=px;

    Row:=0;
    Col:=0;
    h:=Img.Height;
    w:=Img.Width;
    iq:=0;
    ip:=0;
    imgSize:= h*w;
    if imgSize > 0 then
    while (imgSize)> iq do
    begin
         color:=img.colors[Col,Row];
         cx.r:=color.Red shr 8;
         cx.g:=color.Green shr (8);
         cx.b:=color.Blue shr (8);
         cx.a:=color.Alpha shr (8);

          iA:=QoiPixelIndex (cx);

          if DWord(cx)=DWord(px) then { run }
          begin
               run:=0;
               //inc (q);
               inc (iq);

               inc(col);
               if col = w then begin inc(row); col:=0; end;

               if (col < w) and  (row<h) then
               begin

                 color:=img.colors[Col,Row];
                 cx.r:=color.Red shr 8;
                 cx.g:=color.Green shr (8);
                 cx.b:=color.Blue shr (8);
                 cx.a:=color.Alpha shr (8);


               while (imgSize >= (iq+1))
                    and (DWord(cx)=DWord(px)) do
               begin
                    inc (run);
                    inc (iq);

                   inc(col);
               if col = w then begin
                    inc(row); col:=0;
                    if (col >= w) or  (row>=h) then break;
               end;



                 color:=img.colors[Col,Row];
                 cx.r:=color.Red shr 8;
                 cx.g:=color.Green shr (8);
                 cx.b:=color.Blue shr (8);
                 cx.a:=color.Alpha shr (8);



                    if run = 61 then break;
               end;
               end;

               b:=($ff xor 63) or run;
               p^:=b;
               inc(p);
               inc(ip);

          end else
          if DWord(arr[iA]) = DWord(cx) then { index }
          begin

               px:=cx;
               p^:=byte(iA);
               inc(p);
               inc(ip);
               //inc(q);
               inc(iq);

               inc(col);
               if col = w then begin inc(row); col:=0; end;


          end else
          if px.a <> cx.a then { rgba }
          begin
               b:=$ff;
               p^:=b;
               inc(p);
               px:=cx;
               //PQoiPixel(p)^:=cx;
               //inc(p,4);

               p^:=cx.r;inc(p);
               p^:=cx.g;inc(p);
               p^:=cx.b;inc(p);
               p^:=cx.a;inc(p);

               inc(ip,5);
               //inc(q);
               inc (iq);

               inc(col);
               if col = w then begin inc(row); col:=0; end;

               arr[iA]:=cx;

          end else
          begin
               dr := (cx.r - px.r);
               dg := (cx.g - px.g);
               db := (cx.b - px.b);

               px:=cx;

               dr_dg := dr-dg+8;
               db_dg := db-dg+8;

               dr:=dr+2;
               dg:=dg+2;
               db:=db+2;
               g:=dg+30;

               //inc(q);
               inc (iq);
               inc(col);
               if col = w then begin inc(row); col:=0; end;

               arr[iA]:=cx;

               if (dr and ($ff xor 3))+(dg and ($ff xor 3))+(db and ($ff xor 3)) = 0 then  { diff }
               begin
                    b:=64 or (dr shl 4) or (dg shl 2)or (db ) ;
                    p^:=b;
                    inc(p);
                    inc(ip);

               end else
               if ((g) and ($ff xor 63)) + (dr_dg and ($ff xor 15))+ (db_dg and ($ff xor 15))=0 then { luma }
               begin
                    b:=128 or g;
                    p^:=b;
                    inc(p);
                    b:=(dr_dg shl 4) or db_dg;
                    p^:=b;
                    inc(p);
                    inc(ip,2);

               end else {rgb}
               begin
                    b:=$fe;
                    p^:=b;
                    inc(p);
                    //PQoiPixel(p)^:=cx;
                    //inc(p,3);


                    p^:=cx.r;inc(p);
                    p^:=cx.g;inc(p);
                    p^:=cx.b;inc(p);

                    inc(ip,4);
               end;

          end;
          if ip >= mSize then
          begin
               {save data }
               orgSize:=ip;
               Stream.Write(aLine[0],orgSize);
               ip:=0;
               p:=aLine;
          end;
     end;


     {mark end of encoding}
     {
     endof:=qword(1) shl 56;
     pqword(p)^:=endof;
     inc(p,8);
     }
     p^:=0; inc(p);
     p^:=0; inc(p);
     p^:=0; inc(p);
     p^:=0; inc(p);
     p^:=0; inc(p);
     p^:=0; inc(p);
     p^:=0; inc(p);
     p^:=1; inc(p);

     inc(ip,8);

     orgSize:=ip;
     Stream.Write(aLine[0],orgSize);

     FreeMem(aLine);
end;

end.

