program AuthServer;

{$mode objfpc}{$H+}

uses
  SysUtils, BrookHTTPRequest, BrookHTTPResponse, BrookHTTPServer, 
  fpjson, jsonparser, base64, DateUtils, dynlibs, Classes;

const
  SECRET_KEY = 'chave-secreta-inventada-por-tua-pessoa';
  TOKEN_EXPIRY = 3600; // 1 hora
  EVP_MAX_MD_SIZE = 64; // SHA-256 produz 32 bytes, mas OpenSSL pode usar até 64
  DEBUG_MODE = True; // Habilita/Desabilita depuração

type
  TAuthServer = class(TBrookHTTPServer)
  protected
    procedure DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
      AResponse: TBrookHTTPResponse); override;
  end;

{ Simple Debugging Function }
procedure DebugLog(const Msg: string);
begin
  if DEBUG_MODE then
    WriteLn('[DEBUG] ', Msg);
end;

{ Compute HMAC-SHA256 using OpenSSL }
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
    DebugLog('ERROR: OpenSSL library (libcrypto.so) not found.');
    raise Exception.Create('OpenSSL library (libcrypto.so) not found.');
  end;

  try
    // Get HMAC function
    Pointer(HMAC) := GetProcedureAddress(LibHandle, 'HMAC');
    if not Assigned(HMAC) then
      raise Exception.Create('Failed to load HMAC function from OpenSSL.');

    md := GetProcedureAddress(LibHandle, 'EVP_sha256');
    if not Assigned(md) then
      raise Exception.Create('Failed to load EVP_sha256 from OpenSSL.');

    // Compute HMAC-SHA256
    HMAC(md, PAnsiChar(Key), Length(Key), PAnsiChar(Data), Length(Data), @hash, @hash_len);

    // Convert result to hex
    for I := 0 to hash_len - 1 do
      Result := Result + IntToHex(hash[I], 2);

    DebugLog('HMAC-SHA256 generated successfully.');
  finally
    UnloadLibrary(LibHandle);
  end;
end;

{ Generate a JWT Token }
function GenerateJWT(const Username: string): string;
var
  Header, Payload, Signature, Token: string;
begin
  Header := '{"alg":"HS256","typ":"JWT"}';
  Payload := Format('{"sub":"%s","iat":%d,"exp":%d}', 
    [Username, DateTimeToUnix(Now), DateTimeToUnix(Now) + TOKEN_EXPIRY]);

  Token := EncodeStringBase64(Header) + '.' + EncodeStringBase64(Payload);
  Signature := EncodeStringBase64(HMACSHA256(Token, SECRET_KEY));

  DebugLog('JWT generated for user: ' + Username);
  Result := Token + '.' + Signature;
end;

{ Validate JWT Token }
function ValidateJWT(const Token: string): Boolean;
var
  Parts: TStringArray;
  DecodedPayload: string;
  JSONData: TJSONData;
  Expiration: Int64;
begin
  Result := False;
  Parts := Token.Split('.');

  if Length(Parts) <> 3 then
  begin
    DebugLog('ERROR: Invalid JWT format.');
    Exit;
  end;

  try
    DecodedPayload := DecodeStringBase64(Parts[1]);
    JSONData := GetJSON(DecodedPayload);
    try
      Expiration := JSONData.FindPath('exp').AsInteger;
      Result := (Expiration > DateTimeToUnix(Now)); // Check expiration

      if Result then
        DebugLog('JWT is valid.')
      else
        DebugLog('ERROR: JWT is expired.');
    finally
      JSONData.Free;
    end;
  except
    DebugLog('ERROR: Error validating JWT.');
    Result := False;
  end;
end;

{ Extract Username from JWT Token }
function ExtractUsernameFromJWT(const Token: string): string;
var
  Parts: TStringArray;
  DecodedPayload: string;
  JSONData: TJSONData;
begin
  Result := '';
  Parts := Token.Split('.');

  if Length(Parts) <> 3 then Exit;

  try
    DecodedPayload := DecodeStringBase64(Parts[1]);
    JSONData := GetJSON(DecodedPayload);
    try
      Result := JSONData.FindPath('sub').AsString;
      DebugLog('Extracted username: ' + Result);
    finally
      JSONData.Free;
    end;
  except
    DebugLog('ERROR: Failed to extract username from JWT.');
    Result := '';
  end;
end;

{ Handle Requests }
procedure TAuthServer.DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
var
  JWTToken, RequestData, Username, Password, ReceivedToken, NewToken: string;
  JSONData: TJSONData;
begin
  DebugLog('Request received: [' + ARequest.Method + '] ' + ARequest.Path);

  if ARequest.Path = '/auth/login' then
  begin
    RequestData := TEncoding.UTF8.GetString(ARequest.Payload.Content);
    DebugLog('Received login request: ' + RequestData);

    JSONData := GetJSON(RequestData);
    try
      Username := JSONData.FindPath('username').AsString;
      Password := JSONData.FindPath('password').AsString;

      if (Username = 'admin') and (Password = 'password') then
      begin
        JWTToken := GenerateJWT(Username);
        AResponse.Send(Format('{"token": "%s"}', [JWTToken]), 'application/json', 200);
        DebugLog('Login successful, token sent.');
      end
      else
      begin
        AResponse.Send('{"error": "Invalid credentials"}', 'application/json', 401);
        DebugLog('ERROR: Invalid login attempt.');
      end;
    finally
      JSONData.Free;
    end;
  end
  else if ARequest.Path = '/auth/refresh' then
  begin
    ReceivedToken := ARequest.Headers.Values['Authorization'];

    if ValidateJWT(ReceivedToken) then
    begin
      NewToken := GenerateJWT(ExtractUsernameFromJWT(ReceivedToken));
      AResponse.Send(Format('{"token": "%s"}', [NewToken]), 'application/json', 200);
      DebugLog('Token refreshed successfully.');
    end
    else
    begin
      AResponse.Send('{"error": "Invalid or expired token"}', 'application/json', 401);
      DebugLog('ERROR: Invalid refresh attempt.');
    end;
  end
  else if ARequest.Path = '/auth/logout' then
  begin
    AResponse.Send('{"message": "Logout successful"}', 'application/json', 200);
    DebugLog('User logged out.');
  end
  else
  begin
    AResponse.Send('{"error": "Endpoint not found"}', 'application/json', 404);
    DebugLog('ERROR: Unknown endpoint.');
  end;
end;

{ Run the Authentication Server }
var
  AuthSvr: TAuthServer;
begin
  AuthSvr := TAuthServer.Create(nil);
  try
    AuthSvr.Port := 8321;
    AuthSvr.Open;
    if not AuthSvr.Active then
      Exit;
    WriteLn('Auth Server running at http://localhost:', AuthSvr.Port, '/auth');
    ReadLn;
  finally
    AuthSvr.Free;
  end;
end.
