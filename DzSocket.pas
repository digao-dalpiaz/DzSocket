{------------------------------------------------------------------------------
TDzTCPServer and TDzTCPClient component
Developed by Rodrigo Depiné Dalpiaz (digao dalpiaz)
Non visual components to easy TCP Socket Asynchronous communication

https://github.com/digao-dalpiaz/DzSocket

Please, read the documentation at GitHub link.
------------------------------------------------------------------------------}

unit DzSocket;

interface

{TCP Socket Asynchronous communication / Non-Blocking

 The messages uses following syntax:
   [CHAR_IDENT_MSG][LENGTH][CMD][MSG]

 - The identification character is a security data to confirm the message
   buffer beginning.

 - The length is calculated by Cmd+Msg, represented by 4 bytes using an
   Integer structure stored as AnsiString.

 CACHE:
 When sending long messages or consecutive messages, it may be received by
 recipient in parts. So a cache is needed to store parts of message until
 the message is complete.

 KEEPALIVE:
 The Server and Client components have a KeepAlive native resource,
 allowing to send pings to ensure the connection is still alive.
 This will ensure a broken connection not stay active until the next data
 communication, closing the connection on ping time.

 Blocking method:
 O ScktComp supports Blocking mode too, but using this mode the messages
 are not received automatically. A reading loop is needed using Blocking mode.

 Winsock/Winsock2:
 All the socket communication is based on WinSock, which is either referenced
 in ScktComp. The Winsock2 is only used by KeepAlive function.
}

uses System.Classes, System.Win.ScktComp, Winapi.WinSock;

const DEF_KEEPALIVE_INTERVAL = 15000;

type
  TSocket = Winapi.WinSock.TSocket; {>IntPtr>NativeInt(Integer/Int64)} //force WinSock unit
  TErrorEvent = System.Win.ScktComp.TErrorEvent; //to not ask unit when use this class

  TDzTCPServer = class;

  TDzSocketCache = class
  private
    Data: AnsiString;
    Size: Integer;
  end;

  TDzServerClientSocket = class(TServerClientWinSocket) //class for Clients on Server
  private
    Comp: TDzTCPServer;
    Cache: TDzSocketCache;
  public
    destructor Destroy; override;
  end;

  {The class TDzSocket can't contain variables, because objects are not created
  using this class. This class is used only by reference for declarations.}
  TDzSocket = class(TCustomWinSocket)
  private
    function GetID: TSocket;
  public
    procedure Send(const Cmd: Char; const A: String = '');
    property ID: TSocket read GetID;
  end;

  TDzSocketEvent = procedure(Sender: TObject; Socket: TDzSocket) of object;
  TDzSocketReadEvent = procedure(Sender: TObject; Socket: TDzSocket; const Cmd: Char; const A: String) of object;
  TDzSocketErrorEvent = procedure(Sender: TObject; Socket: TDzSocket;
    const Event: TErrorEvent; const ErrorCode: Integer; const ErrorMsg: String) of object;
  TDzSocketDisconnectEvent = procedure(Sender: TObject; Socket: TDzSocket; const WasConnected: Boolean) of object;

  TDzSocketEnumerator = class
  private
    FIndex: Integer;
    FComp: TDzTCPServer;
  public
    constructor Create(Comp: TDzTCPServer);
    function GetCurrent: TDzSocket; inline;
    function MoveNext: Boolean;
    property Current: TDzSocket read GetCurrent;
  end;

  TDzTCPClient = class(TComponent)
  private
    C: TClientSocket;

    Cache: TDzSocketCache;

    FPort: Word;
    FHost: String;

    FKeepAlive: Boolean;
    FKeepAliveInterval: Integer;

    FOnConnect: TDzSocketEvent;
    FOnDisconnect: TDzSocketDisconnectEvent;
    FOnRead: TDzSocketReadEvent;
    FOnError: TDzSocketErrorEvent;
    FOnConnectionLost: TDzSocketEvent;

    MonConnectionLost: Boolean; //flag to connection lost monitoring

    procedure int_OnConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure int_OnDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure int_OnRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure int_OnError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);

    function GetConnected: Boolean;
    function GetSocketHandle: TSocket;

    procedure DoEvDisconnect(const WasConnected: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;

    procedure Send(const Cmd: Char; const A: String = '');

    property Connected: Boolean read GetConnected;
    property SocketHandle: TSocket read GetSocketHandle;
  published
    property Port: Word read FPort write FPort default 0;
    property Host: String read FHost write FHost;

    property KeepAlive: Boolean read FKeepAlive write FKeepAlive default False;
    property KeepAliveInterval: Integer read FKeepAliveInterval write FKeepAliveInterval
      default DEF_KEEPALIVE_INTERVAL;

    property OnConnect: TDzSocketEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TDzSocketDisconnectEvent read FOnDisconnect write FOnDisconnect;
    property OnRead: TDzSocketReadEvent read FOnRead write FOnRead;
    property OnError: TDzSocketErrorEvent read FOnError write FOnError;
    property OnConnectionLost: TDzSocketEvent read FOnConnectionLost write FOnConnectionLost;
  end;

  TDzTCPServer = class(TComponent)
  private
    S: TServerSocket;

    FPort: Word;

    FOnClientConnect: TDzSocketEvent;
    FOnClientDisconnect: TDzSocketEvent;
    FOnClientRead: TDzSocketReadEvent;
    FOnClientError: TDzSocketErrorEvent;

    FKeepAlive: Boolean;
    FKeepAliveInterval: Integer;

    procedure int_OnGetSocket(Sender: TObject; Socket: NativeInt; var SC: TServerClientWinSocket);

    procedure int_OnClientConnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure int_OnClientDisconnect(Sender: TObject; Socket: TCustomWinSocket);
    procedure int_OnClientRead(Sender: TObject; Socket: TCustomWinSocket);
    procedure int_OnClientError(Sender: TObject; Socket: TCustomWinSocket;
      ErrorEvent: TErrorEvent; var ErrorCode: Integer);

    function GetConnection(const Index: Integer): TDzSocket;
    function GetCount: Integer;
  public
    AutoFreeObjs: Boolean;
    SendAllOnlyWithData: Boolean;

    constructor Create(AOwner: TComponent); override;

    procedure Open;
    procedure Close;

    procedure Lock;
    procedure Unlock;

    property Connection[const Index: Integer]: TDzSocket read GetConnection;
    property Count: Integer read GetCount;

    function GetEnumerator: TDzSocketEnumerator;

    procedure Send(Socket: TDzSocket; const Cmd: Char; const A: String = '');
    procedure SendAll(const Cmd: Char; const A: String = '');
    procedure SendAllEx(Exclude: TDzSocket; const Cmd: Char; const A: String = '');

    function FindSocketHandle(const ID: TSocket): TDzSocket;
  published
    property Port: Word read FPort write FPort default 0;

    property OnClientConnect: TDzSocketEvent read FOnClientConnect write FOnClientConnect;
    property OnClientDisconnect: TDzSocketEvent read FOnClientDisconnect write FOnClientDisconnect;
    property OnClientRead: TDzSocketReadEvent read FOnClientRead write FOnClientRead;
    property OnClientError: TDzSocketErrorEvent read FOnClientError write FOnClientError;

    property KeepAlive: Boolean read FKeepAlive write FKeepAlive default False;
    property KeepAliveInterval: Integer read FKeepAliveInterval write FKeepAliveInterval
      default DEF_KEEPALIVE_INTERVAL;
  end;

procedure Register;

implementation

uses System.SysUtils, Winapi.Winsock2, System.Generics.Collections;

procedure Register;
begin
  RegisterComponents('Digao', [TDzTCPClient, TDzTCPServer]);
end;

//

{$REGION 'KeepAlive - uses WinSock 2'}
procedure EnableKeepAlive(Socket: TCustomWinSocket; const iTime: Integer);
type
  TTcpKeepAlive = record
    onoff, keepalivetime, keepaliveinterval: u_long;
  end;
var
  KeepAlive: TTcpKeepAlive;
  BytesRet: Cardinal;
begin
  KeepAlive.onoff := 1;
  KeepAlive.keepalivetime := iTime; //interval to send keepalive signal
  KeepAlive.keepaliveinterval := 1000; //interval to send signal after connection lost (default 1 sec)
  BytesRet := 0;
  if WSAIoctl(Socket.SocketHandle, IOC_IN or IOC_VENDOR or 4, @KeepAlive,
    SizeOf(KeepAlive), nil, 0, BytesRet, nil, nil)<>0 then
  begin
    Socket.Close;
    raise Exception.Create('Socket: Could not set KeepAlive');
  end;
end;
{$ENDREGION}

{$REGION 'Error Handling'}
function GetSocketErrorMsg(const Cod: Integer): String;
begin
  Result := SysErrorMessage(Cod);
end;
{$ENDREGION}

{$REGION 'Read/Send function'}
type PSockCache = ^TDzSocketCache;
function GetCachePointer(Comp: TComponent; Socket: TCustomWinSocket): PSockCache;
begin
  //Get cache pointer by component class
  if Comp is TDzTCPClient then
    Result := @TDzTCPClient(Comp).Cache
  else
  if Comp is TDzTCPServer then
    Result := @TDzServerClientSocket(Socket).Cache
  else
    raise Exception.Create('Socket: Unknown class to get cache pointer');
end;

type TSockMsgSize = Integer;
const STRUCT_MSGSIZE = SizeOf(TSockMsgSize); //4 bytes
function SizeToString(const Value: TSockMsgSize): AnsiString;
begin
  SetLength(Result, STRUCT_MSGSIZE);
  Move(Value, Result[1], STRUCT_MSGSIZE);
end;

function StringToSize(const Value: AnsiString): TSockMsgSize;
var mv_Value: AnsiString;
begin
  mv_Value := Value;
  Move(mv_Value[1], Result, STRUCT_MSGSIZE);
end;

const CHAR_IDENT_MSG = #2;
procedure SockRead(Comp: TComponent; Socket: TCustomWinSocket;
   EvRead: TDzSocketReadEvent; EvError: TDzSocketErrorEvent);

  procedure ReadPart(const A: AnsiString);
  var
    SU, SA: TStringStream;
    W: String;
  begin
    SU := TStringStream.Create('', TEncoding.Unicode);
    try
      SA := TStringStream.Create(A);
      try
        SU.LoadFromStream(SA); //convert to Unicode
      finally
        SA.Free;
      end;

      W := SU.DataString;
    finally
      SU.Free;
    end;

    if Assigned(EvRead) then
      EvRead(Comp, TDzSocket(Socket), W[1], W.Remove(0{0-based}, 1));
  end;

var
  PCache: PSockCache;
  Cache: TDzSocketCache;
  Buf, A: AnsiString;
  LMsgs: TList<AnsiString>;
  ReadSize: Integer;
begin
  Buf := Socket.ReceiveText; //read socket buffer

  PCache := GetCachePointer(Comp, Socket); //get cache pointer
  if not Assigned(PCache^) then
    PCache^ := TDzSocketCache.Create;

  Cache := PCache^;

  LMsgs := TList<AnsiString>.Create;
  try
    try
      while Buf<>EmptyAnsiStr do
      begin
        if Cache.Size>0 then //should be here first
        begin
          ReadSize := Length(Buf);
          if Cache.Size<ReadSize then ReadSize := Cache.Size;
          Cache.Data := Cache.Data + Copy(Buf, 1, ReadSize);
          Delete(Buf, 1, ReadSize);
          Dec(Cache.Size, ReadSize);

          if Cache.Size=0 then //message completed
          begin
            LMsgs.Add(Cache.Data);
            Cache.Data := '';
          end;
        end else
        if Buf[1]=CHAR_IDENT_MSG then
        begin
          Delete(Buf, 1, 1);

          Cache.Size := StringToSize(Copy(Buf, 1, STRUCT_MSGSIZE));
          Delete(Buf, 1, STRUCT_MSGSIZE);

          if Cache.Size<=0 then raise Exception.Create('Invalid size');

        end else
          raise Exception.Create('Invalid data');
      end;
    except
      on E: Exception do
        if Assigned(EvError) then
          EvError(Comp, TDzSocket(Socket), eeReceive, -1, Format('Error on buffer reading (%s)', [E.Message]));
    end;

    {The final Read event is not together with buffer receive because it
    can't determine how long the Read event will take, depending on programmer
    codes, so its only fired after all buffer receiving is done, avoiding
    messages parts overload.}

    for A in LMsgs do
      ReadPart(A);

  finally
    LMsgs.Free;
  end;
end;

procedure SockSend(Socket: TCustomWinSocket; const Cmd: Char; const A: String);
var
  SU, SA: TStringStream;
  ansiData: AnsiString;
begin
  SA := TStringStream.Create;
  try
    SU := TStringStream.Create(Cmd+A, TEncoding.Unicode);
    try
      SA.LoadFromStream(SU); //convert to Ansi
    finally
      SU.Free;
    end;
    ansiData := AnsiString(SA.DataString);
  finally
    SA.Free;
  end;

  Socket.SendText(CHAR_IDENT_MSG+SizeToString(Length(ansiData))+ansiData);
end;
{$ENDREGION}

{$REGION 'TDzSocket'}
function TDzSocket.GetID: TSocket;
begin
  Result := SocketHandle;
end;

procedure TDzSocket.Send(const Cmd: Char; const A: String);
begin
  SockSend(Self, Cmd, A);
end;
{$ENDREGION}

{$REGION 'TDzSocketEnumerator'}
constructor TDzSocketEnumerator.Create(Comp: TDzTCPServer);
begin
  inherited Create;
  FIndex := -1;
  FComp := Comp;
end;

function TDzSocketEnumerator.GetCurrent: TDzSocket;
begin
  Result := FComp.Connection[FIndex];
end;

function TDzSocketEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FComp.Count-1;
  if Result then
    Inc(FIndex);
end;
{$ENDREGION}

{$REGION 'TDzTCPClient'}
constructor TDzTCPClient.Create(AOwner: TComponent);
begin
  inherited;
  FKeepAliveInterval := DEF_KEEPALIVE_INTERVAL;
end;

destructor TDzTCPClient.Destroy;
begin
  if Assigned(Cache) then Cache.Free;

  inherited;
end;

procedure TDzTCPClient.Connect;
begin
  if Connected then Exit;

  if FHost='' then
    raise Exception.Create('Host not specified');
  if FPort=0 then
    raise Exception.Create('Port not specified');

  if Assigned(C) then C.Free;
  {This is needed because after a connection error, when retry connection the
  socket component returns a different error and uses old host.
  So I always recreate the internal socket to fix this bug.}

  MonConnectionLost := False; //clear

  C := TClientSocket.Create(Self);

  C.OnConnect := int_OnConnect;
  C.OnDisconnect := int_OnDisconnect;
  C.OnRead := int_OnRead;
  C.OnError := int_OnError;

  C.Host := FHost;
  C.Port := FPort;
  C.Open;
end;

procedure TDzTCPClient.Disconnect;
begin
  if not Connected then Exit;

  MonConnectionLost := False; //disable because is own client command
  C.Close;
end;

function TDzTCPClient.GetConnected: Boolean;
begin
  Result := Assigned(C) and C.Socket.Connected;
end;

function TDzTCPClient.GetSocketHandle: TSocket;
begin
  if Connected then
    Result := C.Socket.SocketHandle
  else
    Result := Winapi.WinSock.INVALID_SOCKET;
end;

procedure TDzTCPClient.Send(const Cmd: Char; const A: String);
begin
  if not Connected then
    raise Exception.Create('Socket is not connected to send');

  SockSend(C.Socket, Cmd, A);
end;

procedure TDzTCPClient.int_OnConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  MonConnectionLost := True; //enable connection lost monitoring

  if FKeepAlive then
    EnableKeepAlive(Socket, FKeepAliveInterval);

  if Assigned(FOnConnect) then
    FOnConnect(Self, TDzSocket(Socket));
end;

procedure TDzTCPClient.int_OnDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  DoEvDisconnect(True);
end;

procedure TDzTCPClient.DoEvDisconnect(const WasConnected: Boolean);
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self, TDzSocket(C.Socket), WasConnected);

  if MonConnectionLost then //disconnect command not by client
  begin
    if Assigned(FOnConnectionLost) then
      FOnConnectionLost(Self, TDzSocket(C.Socket));
  end;
end;

procedure TDzTCPClient.int_OnRead(Sender: TObject; Socket: TCustomWinSocket);
begin
  SockRead(Self, Socket, FOnRead, FOnError);
end;

procedure TDzTCPClient.int_OnError(Sender: TObject; Socket: TCustomWinSocket;
  ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  case ErrorEvent of
    eeConnect: DoEvDisconnect(False); //error on connection
    eeDisconnect: C.Close; //this error caused a disconnection
    eeLookup:
    begin
      //when Lookup error, the socket will fire OnDisconnect, but we need to know that was not connected yet.
      C.OnDisconnect := nil; //after this the internal socket will be recreated anyway.
      DoEvDisconnect(False);
    end;
  end;

  if Assigned(FOnError) then
  begin
    FOnError(Self, TDzSocket(Socket), ErrorEvent, ErrorCode, GetSocketErrorMsg(ErrorCode));

    ErrorCode := 0;
  end;
end;
{$ENDREGION}

{$REGION 'TDzTCPServer'}
constructor TDzTCPServer.Create(AOwner: TComponent);
begin
  inherited;
  FKeepAliveInterval := DEF_KEEPALIVE_INTERVAL;

  S := TServerSocket.Create(Self);

  S.OnGetSocket := int_OnGetSocket; //event to create socket object

  S.OnClientConnect := int_OnClientConnect;
  S.OnClientDisconnect := int_OnClientDisconnect;
  S.OnClientRead := int_OnClientRead;
  S.OnClientError := int_OnClientError;
end;

procedure TDzTCPServer.Open;
begin
  if FPort=0 then
    raise Exception.Create('Port not specified');

  S.Port := FPort;
  try
    S.Open;
  except
    on E: ESocketError do
      if E.Message.Contains('(10048)') then {WSAEADDRINUSE}
        raise Exception.Create('Port already in use')
      else
        raise;
  end;
end;

procedure TDzTCPServer.Close;
begin
  S.Close;
end;

function TDzTCPServer.FindSocketHandle(const ID: TSocket): TDzSocket;
var Sock: TDzSocket;
begin
  Result := nil;

  Lock;
  try
    for Sock in Self do
      if Sock.SocketHandle = ID then
      begin
        Result := Sock;
        Break;
      end;
  finally
    Unlock;
  end;
end;

procedure TDzTCPServer.Send(Socket: TDzSocket; const Cmd: Char; const A: String);
begin
  SockSend(Socket, Cmd, A);
end;

procedure TDzTCPServer.SendAll(const Cmd: Char; const A: String);
begin
  SendAllEx(nil, Cmd, A);
end;

procedure TDzTCPServer.SendAllEx(Exclude: TDzSocket; const Cmd: Char; const A: String);
var Sock: TDzSocket;
begin
  Lock;
  try
    for Sock in Self do
    begin
      if Sock=Exclude then Continue;

      if SendAllOnlyWithData then
        if Sock.Data=nil then Continue;

      Send(Sock, Cmd, A);
    end;
  finally
    Unlock;
  end;
end;

procedure TDzTCPServer.int_OnGetSocket(Sender: TObject; Socket: NativeInt; var SC: TServerClientWinSocket);
begin
  SC := TDzServerClientSocket.Create(Socket, S.Socket);
  TDzServerClientSocket(SC).Comp := Self;
end;

destructor TDzServerClientSocket.Destroy; // !!!
begin
  //Comp.OnClientDisconnect(nil, Self); - here the socket object does not exist anymore!!!

  if Assigned(Cache) then Cache.Free;

  if Comp.AutoFreeObjs then
    if Assigned(Data) then TObject(Data).Free;

  inherited;
end;

procedure TDzTCPServer.int_OnClientConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  if FKeepAlive then
    EnableKeepAlive(Socket, FKeepAliveInterval);

  if Assigned(FOnClientConnect) then
    FOnClientConnect(Self, TDzSocket(Socket));
end;

procedure TDzTCPServer.int_OnClientDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  if Assigned(FOnClientDisconnect) then
    FOnClientDisconnect(Self, TDzSocket(Socket));
end;

procedure TDzTCPServer.int_OnClientRead(Sender: TObject; Socket: TCustomWinSocket);
begin
  SockRead(Self, Socket, FOnClientRead, FOnClientError);
end;

procedure TDzTCPServer.int_OnClientError(Sender: TObject; Socket: TCustomWinSocket;
  ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  if Assigned(FOnClientError) then
  begin
    FOnClientError(Self, TDzSocket(Socket), ErrorEvent, ErrorCode, GetSocketErrorMsg(ErrorCode));

    ErrorCode := 0;
  end;

  if ErrorEvent=eeDisconnect then Socket.Close; //this error caused disconnection
end;

procedure TDzTCPServer.Lock;
begin
  S.Socket.Lock;
end;

procedure TDzTCPServer.Unlock;
begin
  S.Socket.Unlock;
end;

function TDzTCPServer.GetConnection(const Index: Integer): TDzSocket;
begin
  Result := TDzSocket( S.Socket.Connections[Index] );
end;

function TDzTCPServer.GetCount: Integer;
begin
  Result := S.Socket.ActiveConnections;
end;

function TDzTCPServer.GetEnumerator: TDzSocketEnumerator;
begin
  Result := TDzSocketEnumerator.Create(Self);
end;
{$ENDREGION}

end.
