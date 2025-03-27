program PingServer;

{$mode objfpc}{$H+}

uses
  SysUtils, BrookHTTPRequest, BrookHTTPResponse, BrookHTTPServer, fpjson, jsonparser;

const
  DEBUG_MODE = True; // Enable/Disable debugging

type
  THTTPServer = class(TBrookHTTPServer)
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

{ Handle Requests }
procedure THTTPServer.DoRequest(ASender: TObject; ARequest: TBrookHTTPRequest;
  AResponse: TBrookHTTPResponse);
begin
  DebugLog('Request received: [' + ARequest.Method + '] ' + ARequest.Path);

  if ARequest.Path = '/ping' then
  begin
    AResponse.Send('{"message": "Pong", "timestamp": "' + DateTimeToStr(Now) + '"}', 'application/json', 200);
    DebugLog('Response sent: Pong with timestamp.');
  end
  else
  begin
    AResponse.Send('{"error": "Endpoint not found"}', 'application/json', 404);
    DebugLog('ERROR: Unknown endpoint.');
  end;
end;

{ Run the Server }
begin
  with THTTPServer.Create(nil) do
  try
    Port := 8321;
    Open;
    if not Active then
      Exit;
    WriteLn('Ping Server running at http://localhost:', Port, '/ping');
    ReadLn;
  finally
    Free;
  end;
end.
