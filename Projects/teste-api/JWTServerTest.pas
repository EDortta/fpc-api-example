program JWTServerTest;

{$mode objfpc}{$H+}

{
  Este é um exemplo de implementação simples de um servidor JWT (JSON Web Token)
  utilizando a biblioteca OpenSSL.

  (C) 2025 Esteban D.Dortta
}

uses
  SysUtils, BrookHTTPRequest, BrookHTTPResponse, BrookHTTPServer,
  fpjson, jsonparser, base64, DateUtils, dynlibs;

const
  SECRET_KEY = 'chave-secreta-inventada-por-tua-pessoa';
  TOKEN_EXPIRY = 3600; // 1 hora
  EVP_MAX_MD_SIZE = 64; // SHA-256 produz 32 bytes, mas OpenSSL pode usar até 64
  DEBUG_MODE = True; // Habilita/Desabilita depuração

type
  TJWTServer = class(TBrookHTTPServer)
  protected
    procedure DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); override;
  end;

procedure DebugLog(const Msg: string);
begin
  if DEBUG_MODE then
    WriteLn('[DEBUG] ', Msg);
end;

function HMACSHA256(const Data, Key: string): string;
var
  HMAC: function(
    evp_md: Pointer;
    key: PAnsiChar;
    key_len: Integer;
    data: PAnsiChar;
    data_len: Integer;
    out_buf: PByte;
    out_len: PCardinal
  ): PByte; cdecl;

  LibHandle: TLibHandle;
  md: Pointer;
  hash: array[0..EVP_MAX_MD_SIZE - 1] of Byte;
  hash_len: Cardinal;
  I: Integer;
begin
  Result := '';

  // Load OpenSSL dynamically
  LibHandle := LoadLibrary('libcrypto.so');
  if LibHandle = 0 then
  begin
    DebugLog('Error: OpenSSL library (libcrypto.so) not found!');
    raise Exception.Create('OpenSSL library (libcrypto.so) not found!');
  end;

  try
    // Get HMAC function
    Pointer(HMAC) := GetProcedureAddress(LibHandle, 'HMAC');
    if not Assigned(HMAC) then
      raise Exception.Create('Failed to load HMAC function from OpenSSL.');

    md := GetProcedureAddress(LibHandle, 'EVP_sha256');
    if not Assigned(md) then
      raise Exception.Create('Failed to load EVP_sha256 from OpenSSL.');

    // Compute HMAC
    HMAC(md, PAnsiChar(Key), Length(Key), PAnsiChar(Data), Length(Data), @hash, @hash_len);

    // Convert result to hex
    for I := 0 to hash_len - 1 do
      Result := Result + IntToHex(hash[I], 2);

    DebugLog('HMAC-SHA256 computed successfully.');
  finally
    UnloadLibrary(LibHandle);
  end;
end;

{ Generate a JWT Token }
function GenerateJWT: string;
var
  Header, Payload, Signature, Token: string;
begin
  Header := '{"alg":"HS256","typ":"JWT"}';
  Payload := Format('{"sub":"test_user","iat":%d,"exp":%d}', 
    [DateTimeToUnix(Now), DateTimeToUnix(Now) + TOKEN_EXPIRY]);

  Token := EncodeStringBase64(Header) + '.' + EncodeStringBase64(Payload);
  Signature := EncodeStringBase64(HMACSHA256(Token, SECRET_KEY));

  DebugLog('JWT generated successfully.');
  Result := Token + '.' + Signature;
end;

{ Handle Requests }
procedure TJWTServer.DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
var
  JWTToken: string;
begin
  DebugLog('Request received: [' + ARequest.Method + '] ' + ARequest.Path);

  if ARequest.Path = '/get' then
  begin
    JWTToken := GenerateJWT;
    AResponse.Send(Format('{"token": "%s"}', [JWTToken]), 'application/json', 200);
    DebugLog('JWT token generated and sent.');
  end
  else
  begin
    AResponse.Send('{"error": "Endpoint not found"}', 'application/json', 404);
    DebugLog('Error: Unknown endpoint.');
  end;
end;

{ Run the Server }
begin
  with TJWTServer.Create(nil) do
  try
    Port := 8321;
    Open;
    if not Active then
      Exit;
    WriteLn('JWT Server running at http://localhost:', Port, '/get');
    ReadLn;
  finally
    Free;
  end;
end.