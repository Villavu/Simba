unit simba.sslbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TSSLType = (stAny,stSSLv2,stSSLv3,stTLSv1,stTLSv1_1,stTLSv1_2);

  { TSSLData }

  TSSLData = Class(TPersistent)
  private
    FFileName: String;
    FValue: TBytes;
  Public
    Function Empty : Boolean;
    Procedure Assign(Source : TPersistent);override;
    Property FileName : String Read FFileName Write FFileName;
    Property Value: TBytes Read FValue Write FValue;
  end;

Const
  SSLDataCount = 4; // 0 based.
  StrDataCount = 2; // 0 based.

Type
  { TSSLSocketHandler }

  { TCertificateData }

  TCertificateData = Class(TPersistent)
  Private
    FStrData : Array[0..StrDataCount] of string;
    FCertData : Array[0..SSLDataCount] of TSSLData;
    function GetSSLData(AIndex: Integer): TSSLData;
    procedure SetSSLData(AIndex: Integer; AValue: TSSLData);
    function GetString(AIndex: Integer): String;
    procedure SetString(AIndex: Integer; AValue: String);
  Public
    constructor Create;
    Destructor Destroy; override;
    Procedure Assign(Source : TPersistent); override;
    Function NeedCertificateData : Boolean;
  Published
    property KeyPassword: string Index 0 read GetString write SetString;
    property CipherList: string Index 1 read GetString write SetString;
    Property HostName : String Index 2 read GetString write SetString;
    property Certificate : TSSLData Index 0 Read GetSSLData Write SetSSLData;
    property TrustedCertificate : TSSLData Index 1 Read GetSSLData Write SetSSLData;
    property PrivateKey : TSSLData Index 2 Read GetSSLData Write SetSSLData;
    property PFX: TSSLData Index 3 Read GetSSLData Write SetSSLData;
    property CertCA: TSSLData Index 4 Read GetSSLData Write SetSSLData;
  end;

    { TX509Certificate }
  TCertAndKey = Record
    Certificate : TBytes;
    PrivateKey : TBytes;
  end;

  TX509Certificate = Class (TObject)
  private
    FCommonName: string;
    FCountry: String;
    FHostName: string;
    FKeySize: Integer;
    FOrganization: String;
    FSerial: Integer;
    FValidFrom: TDateTime;
    FValidTo: TDateTime;
    FVersion: Integer;
    function GetKeySize: Integer;
    function GetValidFrom: TDateTime;
    function GetValidTo: TDateTime;
    function GetVersion: Integer;
  Protected
    Function GetRealSerial : Integer;
  Public
    Function CreateCertificateAndKey : TCertAndKey; virtual; abstract;
    Procedure CreateCertificateAndKey(Var aCertificate,aKey : TBytes);
    Property Country : String Read FCountry Write FCountry;
    Property HostName : string Read FHostName Write FHostName;
    Property CommonName : string Read FCommonName Write FCommonName;
    Property Organization : String Read FOrganization Write FOrganization;
    Property KeySize : Integer Read GetKeySize Write FKeySize;
    // Valid from. Default today -1;
    Property ValidFrom : TDateTime Read GetValidFrom Write FValidFrom;
    // Valid To. Default today + 31;
    Property ValidTo : TDateTime Read GetValidTo Write FValidTo;
    // Version Default 1.
    Property Version : Integer Read GetVersion Write FVersion;
    // Serial. If zero, then a serial is generated.
    Property Serial : Integer Read FSerial Write FSerial;

  end;

implementation

{ TSSLData }

Function TSSLData.Empty: Boolean;
begin
  Result:=(Length(Value)=0) and (FileName='');
end;

Procedure TSSLData.Assign(Source: TPersistent);

begin
  if Source is TSSLData then
   With TSSLData(Source) do
    begin
    Self.FValue:=FValue;
    Self.FFileName:=FFileName;
    end
  else
    inherited Assign(Source);
end;

{ TCertificateData }

function TCertificateData.GetSSLData(AIndex: Integer): TSSLData;
begin
  Result:=FCertData[AIndex];
end;

procedure TCertificateData.SetSSLData(AIndex: Integer; AValue: TSSLData);
begin
  FCertData[AIndex].Assign(AValue);
end;

function TCertificateData.GetString(AIndex: Integer): String;
begin
  Result:=FStrData[AIndex];
  if (AIndex=2) and (result='') then
    Result:='localhost';
end;

procedure TCertificateData.SetString(AIndex: Integer; AValue: String);
begin
  FStrData[AIndex]:=aValue;
end;

constructor TCertificateData.Create;

Var
  I : Integer;

begin
  CipherList:='DEFAULT';
  HostName:='localhost';
  For I:=0 to SSLDataCount do
    FCertData[i]:=TSSLData.Create;
end;

destructor TCertificateData.Destroy;

Var
  I : Integer;

begin
  For I:=0 to SSLDataCount do
    FreeAndNil(FCertData[i]);
  inherited Destroy;
end;

procedure TCertificateData.Assign(Source: TPersistent);

Var
  CD : TCertificateData;
  I : Integer;

begin
  if Source is TCertificateData then
    begin
    CD:=Source as TCertificateData;
    For I:=0 to StrDataCount do
      FStrData[i]:=CD.FStrData[i];
    For I:=0 to SSLDataCount do
      FCertData[i].Assign(CD.FCertData[i])
    end
  else
    inherited Assign(Source);
end;

function TCertificateData.NeedCertificateData: Boolean;
begin
  Result:=Certificate.Empty and PFX.Empty;
end;

function TX509Certificate.GetKeySize: Integer;
begin
  Result:=FKeySize;
  if Result=0 then
    Result:=1024;
end;

function TX509Certificate.GetValidFrom: TDateTime;
begin
  Result:=FValidFrom;
  If Result=0 then
    Result:=Date-1;
end;

function TX509Certificate.GetValidTo: TDateTime;
begin
  Result:=FValidTo;
  If Result=0 then
    Result:=Date+31;
end;


function TX509Certificate.GetVersion: Integer;
begin
  Result:=FVersion;
  if FVersion=0 then
    FVersion:=1;
end;

function TX509Certificate.GetRealSerial: Integer;
begin
  Result:=FSerial;
  if Result=0 then
    Result:=10; // MinutesBetween(Now,EncodeDate(2019,1,1));
end;

procedure TX509Certificate.CreateCertificateAndKey(var aCertificate, aKey: TBytes);

Var
  CK : TCertAndKey;

begin
  CK:=CreateCertificateAndKey;
  aCertificate:=CK.Certificate;
  aKey:=CK.PrivateKey;
end;

end.

