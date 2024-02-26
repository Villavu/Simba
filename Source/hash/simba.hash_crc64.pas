// https://github.com/PascalVault/Lazarus_Hashing
//
// CRC-64 ECMA 182
// Author: domasz
// Licence: MIT
unit simba.hash_crc64;

{$i simba.inc}

interface

uses
  Classes, SysUtils,
  simba.hash;

type
  THasherCRC64 = class(TSimbaHasher)
  private
    FHash: QWord;
  public
    constructor Create; override;
    procedure Update(Msg: PByte; Length: Integer); override;
    function Final: String; override;
  end;

implementation

{$R-}
{$Q-}

var
  Table: array[0..255] of QWord = (
    $0000000000000000, $42F0E1EBA9EA3693, $85E1C3D753D46D26, $C711223CFA3E5BB5, $493366450E42ECDF, $0BC387AEA7A8DA4C, $CCD2A5925D9681F9, $8E224479F47CB76A,
    $9266CC8A1C85D9BE, $D0962D61B56FEF2D, $17870F5D4F51B498, $5577EEB6E6BB820B, $DB55AACF12C73561, $99A54B24BB2D03F2, $5EB4691841135847, $1C4488F3E8F96ED4,
    $663D78FF90E185EF, $24CD9914390BB37C, $E3DCBB28C335E8C9, $A12C5AC36ADFDE5A, $2F0E1EBA9EA36930, $6DFEFF5137495FA3, $AAEFDD6DCD770416, $E81F3C86649D3285,
    $F45BB4758C645C51, $B6AB559E258E6AC2, $71BA77A2DFB03177, $334A9649765A07E4, $BD68D2308226B08E, $FF9833DB2BCC861D, $388911E7D1F2DDA8, $7A79F00C7818EB3B,
    $CC7AF1FF21C30BDE, $8E8A101488293D4D, $499B3228721766F8, $0B6BD3C3DBFD506B, $854997BA2F81E701, $C7B97651866BD192, $00A8546D7C558A27, $4258B586D5BFBCB4,
    $5E1C3D753D46D260, $1CECDC9E94ACE4F3, $DBFDFEA26E92BF46, $990D1F49C77889D5, $172F5B3033043EBF, $55DFBADB9AEE082C, $92CE98E760D05399, $D03E790CC93A650A,
    $AA478900B1228E31, $E8B768EB18C8B8A2, $2FA64AD7E2F6E317, $6D56AB3C4B1CD584, $E374EF45BF6062EE, $A1840EAE168A547D, $66952C92ECB40FC8, $2465CD79455E395B,
    $3821458AADA7578F, $7AD1A461044D611C, $BDC0865DFE733AA9, $FF3067B657990C3A, $711223CFA3E5BB50, $33E2C2240A0F8DC3, $F4F3E018F031D676, $B60301F359DBE0E5,
    $DA050215EA6C212F, $98F5E3FE438617BC, $5FE4C1C2B9B84C09, $1D14202910527A9A, $93366450E42ECDF0, $D1C685BB4DC4FB63, $16D7A787B7FAA0D6, $5427466C1E109645,
    $4863CE9FF6E9F891, $0A932F745F03CE02, $CD820D48A53D95B7, $8F72ECA30CD7A324, $0150A8DAF8AB144E, $43A04931514122DD, $84B16B0DAB7F7968, $C6418AE602954FFB,
    $BC387AEA7A8DA4C0, $FEC89B01D3679253, $39D9B93D2959C9E6, $7B2958D680B3FF75, $F50B1CAF74CF481F, $B7FBFD44DD257E8C, $70EADF78271B2539, $321A3E938EF113AA,
    $2E5EB66066087D7E, $6CAE578BCFE24BED, $ABBF75B735DC1058, $E94F945C9C3626CB, $676DD025684A91A1, $259D31CEC1A0A732, $E28C13F23B9EFC87, $A07CF2199274CA14,
    $167FF3EACBAF2AF1, $548F120162451C62, $939E303D987B47D7, $D16ED1D631917144, $5F4C95AFC5EDC62E, $1DBC74446C07F0BD, $DAAD56789639AB08, $985DB7933FD39D9B,
    $84193F60D72AF34F, $C6E9DE8B7EC0C5DC, $01F8FCB784FE9E69, $43081D5C2D14A8FA, $CD2A5925D9681F90, $8FDAB8CE70822903, $48CB9AF28ABC72B6, $0A3B7B1923564425,
    $70428B155B4EAF1E, $32B26AFEF2A4998D, $F5A348C2089AC238, $B753A929A170F4AB, $3971ED50550C43C1, $7B810CBBFCE67552, $BC902E8706D82EE7, $FE60CF6CAF321874,
    $E224479F47CB76A0, $A0D4A674EE214033, $67C58448141F1B86, $253565A3BDF52D15, $AB1721DA49899A7F, $E9E7C031E063ACEC, $2EF6E20D1A5DF759, $6C0603E6B3B7C1CA,
    $F6FAE5C07D3274CD, $B40A042BD4D8425E, $731B26172EE619EB, $31EBC7FC870C2F78, $BFC9838573709812, $FD39626EDA9AAE81, $3A28405220A4F534, $78D8A1B9894EC3A7,
    $649C294A61B7AD73, $266CC8A1C85D9BE0, $E17DEA9D3263C055, $A38D0B769B89F6C6, $2DAF4F0F6FF541AC, $6F5FAEE4C61F773F, $A84E8CD83C212C8A, $EABE6D3395CB1A19,
    $90C79D3FEDD3F122, $D2377CD44439C7B1, $15265EE8BE079C04, $57D6BF0317EDAA97, $D9F4FB7AE3911DFD, $9B041A914A7B2B6E, $5C1538ADB04570DB, $1EE5D94619AF4648,
    $02A151B5F156289C, $4051B05E58BC1E0F, $87409262A28245BA, $C5B073890B687329, $4B9237F0FF14C443, $0962D61B56FEF2D0, $CE73F427ACC0A965, $8C8315CC052A9FF6,
    $3A80143F5CF17F13, $7870F5D4F51B4980, $BF61D7E80F251235, $FD913603A6CF24A6, $73B3727A52B393CC, $31439391FB59A55F, $F652B1AD0167FEEA, $B4A25046A88DC879,
    $A8E6D8B54074A6AD, $EA16395EE99E903E, $2D071B6213A0CB8B, $6FF7FA89BA4AFD18, $E1D5BEF04E364A72, $A3255F1BE7DC7CE1, $64347D271DE22754, $26C49CCCB40811C7,
    $5CBD6CC0CC10FAFC, $1E4D8D2B65FACC6F, $D95CAF179FC497DA, $9BAC4EFC362EA149, $158E0A85C2521623, $577EEB6E6BB820B0, $906FC95291867B05, $D29F28B9386C4D96,
    $CEDBA04AD0952342, $8C2B41A1797F15D1, $4B3A639D83414E64, $09CA82762AAB78F7, $87E8C60FDED7CF9D, $C51827E4773DF90E, $020905D88D03A2BB, $40F9E43324E99428,
    $2CFFE7D5975E55E2, $6E0F063E3EB46371, $A91E2402C48A38C4, $EBEEC5E96D600E57, $65CC8190991CB93D, $273C607B30F68FAE, $E02D4247CAC8D41B, $A2DDA3AC6322E288,
    $BE992B5F8BDB8C5C, $FC69CAB42231BACF, $3B78E888D80FE17A, $7988096371E5D7E9, $F7AA4D1A85996083, $B55AACF12C735610, $724B8ECDD64D0DA5, $30BB6F267FA73B36,
    $4AC29F2A07BFD00D, $08327EC1AE55E69E, $CF235CFD546BBD2B, $8DD3BD16FD818BB8, $03F1F96F09FD3CD2, $41011884A0170A41, $86103AB85A2951F4, $C4E0DB53F3C36767,
    $D8A453A01B3A09B3, $9A54B24BB2D03F20, $5D45907748EE6495, $1FB5719CE1045206, $919735E51578E56C, $D367D40EBC92D3FF, $1476F63246AC884A, $568617D9EF46BED9,
    $E085162AB69D5E3C, $A275F7C11F7768AF, $6564D5FDE549331A, $279434164CA30589, $A9B6706FB8DFB2E3, $EB46918411358470, $2C57B3B8EB0BDFC5, $6EA7525342E1E956,
    $72E3DAA0AA188782, $30133B4B03F2B111, $F7021977F9CCEAA4, $B5F2F89C5026DC37, $3BD0BCE5A45A6B5D, $79205D0E0DB05DCE, $BE317F32F78E067B, $FCC19ED95E6430E8,
    $86B86ED5267CDBD3, $C4488F3E8F96ED40, $0359AD0275A8B6F5, $41A94CE9DC428066, $CF8B0890283E370C, $8D7BE97B81D4019F, $4A6ACB477BEA5A2A, $089A2AACD2006CB9,
    $14DEA25F3AF9026D, $562E43B4931334FE, $913F6188692D6F4B, $D3CF8063C0C759D8, $5DEDC41A34BBEEB2, $1F1D25F19D51D821, $D80C07CD676F8394, $9AFCE626CE85B507
   );

constructor THasherCRC64.Create;
begin
  { nothing }
end;

procedure THasherCRC64.Update(Msg: PByte; Length: Integer);
var i: Integer;
    Index: Cardinal;
begin
  for i:=0 to Length-1 do
  begin
    Index := ((FHash shr 56) xor Msg^) and $FF;
    FHash := (FHash shl 8) xor Table[Index];
    Inc(Msg);
  end;
end;

function THasherCRC64.Final: String;
begin
  Result := Hex(FHash, 16);
end;

initialization
  RegisterHasher(EHashType.CRC64, THasherCRC64);

end.
