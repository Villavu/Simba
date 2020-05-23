{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    SSL support for ssockets

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit simba.sslsockets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sockets, simba.ssockets, simba.sslbase;

Const
  SUseCertData = 'use CertificateData instead';

Type
  ESSLSocketError = Class(ESocketError);
  TSSLSocketHandler = class;
  TVerifyCertificateEvent = Procedure(Sender : TObject; Allow : Boolean) of object;
  TSSLSocketHandlerClass = class of TSSLSocketHandler;

  { TSSLSocketHandler }

  TSSLSocketHandler = class(TSocketHandler)
  private
    FCertGenerator: TX509Certificate;
    FCertificateData: TCertificateData;
    FVerifyPeerCert: Boolean;
    FOnVerifyCertificate: TVerifyCertificateEvent;
    FSSLType: TSSLType;
    FSSLActive : Boolean;
    FSendHostAsSNI : Boolean;
    function GetSSLData(AIndex: Integer): TSSLData;
    function GetString(AIndex: Integer): string;
    procedure SetCertificateData(AValue: TCertificateData);
    procedure SetSSLData(AIndex: Integer; AValue: TSSLData);
    procedure SetString(AIndex: Integer; AValue: string);
  Private
    Class Var FDefaultHandlerClass : TSSLSocketHandlerClass;
  protected
    Procedure SetSSLActive(aValue : Boolean);
    function DoVerifyCert: boolean;
  public
    constructor Create; override;
    Destructor Destroy; override;
    // Class factory methods
    Class Procedure SetDefaultHandlerClass(aClass : TSSLSocketHandlerClass);
    Class Function GetDefaultHandlerClass : TSSLSocketHandlerClass;
    Class Function GetDefaultHandler : TSSLSocketHandler;
    // Socket methods
    Function CreateCertificateData : TCertificateData; virtual;
    Function CreateCertGenerator : TX509Certificate; virtual;
    function CreateSelfSignedCertificate: Boolean; virtual;
    Property CertGenerator : TX509Certificate Read FCertGenerator;
    Property SSLActive: Boolean read FSSLActive;
  published
    property SSLType: TSSLType read FSSLType write FSSLType;
    property VerifyPeerCert: Boolean read FVerifyPeerCert Write FVerifyPeerCert;
    Property SendHostAsSNI : Boolean Read FSendHostAsSNI Write FSendHostAsSNI;
    Property CertificateData : TCertificateData Read FCertificateData Write SetCertificateData;
    // Deprecated, use CertificateData instead.
    property KeyPassword: string Index 0 read GetString write SetString; deprecated 'use CertificateData instead';
    property CipherList: string Index 1 read GetString write SetString; deprecated 'use CertificateData instead';
    // In case a certificate must be generated as server, this is the hostname that will be used.
    property RemoteHostName : String Index 2 read GetString write SetString; deprecated 'use CertificateData instead';
    property Certificate : TSSLData Index 0 Read GetSSLData Write SetSSLData; deprecated 'use CertificateData instead';
    property TrustedCertificate : TSSLData Index 1 Read GetSSLData Write SetSSLData;deprecated 'use CertificateData instead';
    property PrivateKey : TSSLData Index 2 Read GetSSLData Write SetSSLData;deprecated 'use CertificateData instead';
    property PFX: TSSLData Index 3 Read GetSSLData Write SetSSLData;deprecated 'use CertificateData instead';
    property CertCA: TSSLData Index 4 Read GetSSLData Write SetSSLData;deprecated 'use CertificateData instead';
    property OnVerifyCertificate: TVerifyCertificateEvent read FOnVerifyCertificate write FOnVerifyCertificate;
  end;



implementation

Resourcestring
  SErrNoSSLSupport =
    'No SSL Socket support compiled in.'+sLineBreak+
    'Please include opensslsockets unit in program and recompile it.';
  SErrNoX509Certificate =
    'Cannot create a X509 certificate without SLL support';

{ TSSLSocketHandler }


function TSSLSocketHandler.GetSSLData(AIndex: Integer): TSSLData;
begin
  Case aIndex of
    0 : Result:=FCertificateData.Certificate;
    1 : Result:=FCertificateData.TrustedCertificate;
    2 : Result:=FCertificateData.PrivateKey;
    3 : Result:=FCertificateData.PFX;
    4 : Result:=FCertificateData.CertCA;
  end;
end;

function TSSLSocketHandler.GetString(AIndex: Integer): string;
begin
  Case AIndex of
    0 : Result:=FCertificateData.KeyPassword;
    1 : Result:=FCertificateData.CipherList;
    2 : Result:=FCertificateData.HostName;
  end;
end;

procedure TSSLSocketHandler.SetCertificateData(AValue: TCertificateData);
begin
  if FCertificateData=AValue then Exit;
  FCertificateData.Assign(AValue);
end;

procedure TSSLSocketHandler.SetSSLData(AIndex: Integer; AValue: TSSLData);
begin
  Case aIndex of
    0 : FCertificateData.Certificate:=AValue;
    1 : FCertificateData.TrustedCertificate:=AValue;
    2 : FCertificateData.PrivateKey:=AValue;
    3 : FCertificateData.PFX:=AValue;
    4 : FCertificateData.CertCA:=AValue;
  end;
end;



procedure TSSLSocketHandler.SetString(AIndex: Integer; AValue: string);
begin
  Case AIndex of
    0 : FCertificateData.KeyPassword:=AValue;
    1 : FCertificateData.CipherList:=AValue;
    2 : begin
        FCertificateData.HostName:=AValue;
        FCertGenerator.HostName:=aValue;
        end;
  end;
end;

procedure TSSLSocketHandler.SetSSLActive(aValue: Boolean);
begin
  FSSLActive:=aValue;
end;


function TSSLSocketHandler.DoVerifyCert: boolean;
begin
  Result:=True;
  If Assigned(OnVerifyCertificate) then
    OnVerifyCertificate(Self,Result);
end;

constructor TSSLSocketHandler.Create;

begin
  inherited Create;
  FSendHostAsSNI:=True;
  FCertGenerator:=CreateCertGenerator;
  FCertificateData:=CreateCertificateData;
end;

Destructor TSSLSocketHandler.Destroy;

begin
  FreeAndNil(FCertificateData);
  FreeAndNil(FCertGenerator);
  inherited Destroy;
end;

class procedure TSSLSocketHandler.SetDefaultHandlerClass(aClass: TSSLSocketHandlerClass);
begin
  FDefaultHandlerClass:=aClass;
end;

class function TSSLSocketHandler.GetDefaultHandlerClass: TSSLSocketHandlerClass;
begin
  Result:=FDefaultHandlerClass;
end;

class function TSSLSocketHandler.GetDefaultHandler: TSSLSocketHandler;
begin
  if FDefaultHandlerClass=Nil then
    Raise ESSLSocketError.Create(SErrNoSSLSupport);
  Result:=FDefaultHandlerClass.Create;
end;

function TSSLSocketHandler.CreateCertificateData: TCertificateData;
begin
  Result:=TCertificateData.Create;
end;

function TSSLSocketHandler.CreateCertGenerator: TX509Certificate;
begin
  Raise ESSLSocketError.Create(SErrNoX509Certificate);
end;

function TSSLSocketHandler.CreateSelfSignedCertificate: Boolean;

Var
  CK:TCertAndKey;

begin
  CK:=CertGenerator.CreateCertificateAndKey;
  CertificateData.Certificate.Value:=CK.Certificate;
  CertificateData.PrivateKey.Value:=CK.PrivateKey;
  Result:=(Length(CK.Certificate)<>0) and (Length(CK.PrivateKey)<>0);
end;


end.

