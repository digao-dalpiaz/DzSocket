unit UFrm;

interface

uses Vcl.Forms, Vcl.ExtCtrls, DzSocket, Vcl.StdCtrls, Vcl.Controls, Vcl.Buttons,
  System.Classes,
  //
  System.Types;

type
  TFrm = class(TForm)
    BtnOnServer: TBitBtn;
    BtnOffServer: TBitBtn;
    BtnClientConnect: TBitBtn;
    BtnClientDisconnect: TBitBtn;
    S_Memo: TMemo;
    C_Memo: TMemo;
    S_Cmd: TEdit;
    S_Msg: TEdit;
    C_Cmd: TEdit;
    C_Msg: TEdit;
    LCon: TListBox;
    EdHost: TEdit;
    BtnSendAll: TBitBtn;
    C: TDzTCPClient;
    S: TDzTCPServer;
    BtnKill: TBitBtn;
    EdServerPort: TEdit;
    EdName: TEdit;
    EdClientPort: TEdit;
    BtnSendPrint: TBitBtn;
    CkLoopPrint: TCheckBox;
    LUsers: TListBox;
    LbServerLog: TLabel;
    LbClientLog: TLabel;
    Divider: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure BtnOnServerClick(Sender: TObject);
    procedure BtnOffServerClick(Sender: TObject);
    procedure BtnClientConnectClick(Sender: TObject);
    procedure BtnClientDisconnectClick(Sender: TObject);
    procedure C_MsgKeyPress(Sender: TObject; var Key: Char);
    procedure S_MsgKeyPress(Sender: TObject; var Key: Char);
    procedure BtnSendAllClick(Sender: TObject);
    procedure BtnKillClick(Sender: TObject);
    procedure LConDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure LUsersDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure BtnSendPrintClick(Sender: TObject);
    procedure SClientConnect(Sender: TObject; Socket: TDzSocket);
    procedure SClientDisconnect(Sender: TObject; Socket: TDzSocket);
    procedure SClientError(Sender: TObject; Socket: TDzSocket;
      const Event: TErrorEvent; const ErrorCode: Integer;
      const ErrorMsg: string);
    procedure SClientRead(Sender: TObject; Socket: TDzSocket; const Cmd: Char;
      const A: string);
    procedure CConnect(Sender: TObject; Socket: TDzSocket);
    procedure CConnectionLost(Sender: TObject; Socket: TDzSocket);
    procedure CDisconnect(Sender: TObject; Socket: TDzSocket;
      const WasConnected: Boolean);
    procedure CError(Sender: TObject; Socket: TDzSocket;
      const Event: TErrorEvent; const ErrorCode: Integer;
      const ErrorMsg: string);
    procedure CRead(Sender: TObject; Socket: TDzSocket; const Cmd: Char;
      const A: string);
    procedure FormDestroy(Sender: TObject);
    procedure CLoginRequest(Sender: TObject; Socket: TDzSocket;
      var Data: string);
    procedure CLoginResponse(Sender: TObject; Socket: TDzSocket;
      Accepted: Boolean; const Data: string);
    procedure SClientLoginCheck(Sender: TObject; Socket: TDzSocket;
      var Accept: Boolean; const RequestData: string; var ResponseData: string);
    procedure SClientLoginSuccess(Sender: TObject; Socket: TDzSocket);
  private
    procedure LogServer(const A: string);
    procedure LogClient(const A: string);

    procedure DoSound;
    function GetClientsList: string;
    procedure ClientsListReceived(const A: string);
    procedure AddUser(const A: string);
    procedure DelUser(const A: string);
    procedure ClearClientUsers;
  end;

var
  Frm: TFrm;

implementation

{$R *.dfm}

uses System.SysUtils, System.StrUtils, Vcl.Graphics, Winapi.Windows,
  Winapi.MMSystem, UPrint, UClasses;

procedure TFrm.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;

  S.AutoFreeObjs := True; //auto free server connections Data property
  S.EnumeratorOnlyAuth := True;

  //--Disable WordWrap because of long text result on slow performance
  S_Memo.WordWrap := False;
  C_Memo.WordWrap := False;
  //--

  if FindCmdLineSwitch('server_auto') then //app parameter to auto on server
    BtnOnServer.Click
  else
  if FindCmdLineSwitch('client_auto_con') then //app parameter to auto connnect client
  begin
    EdHost.Text := ParamStr(1);
    EdName.Text := ParamStr(2);

    BtnClientConnect.Click;
  end;
end;

procedure TFrm.FormDestroy(Sender: TObject);
begin
  //When destroying, ensure client users list objects release
  ClearClientUsers;

  //Important: the server connections Data objects are not released here because using AutoFreeObjs in Server
end;

procedure TFrm.DoSound;
begin
  PlaySound('PLING', HInstance, SND_RESOURCE or SND_ASYNC);
end;

procedure TFrm.LConDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);

var
  Sock: TDzSocket;
  C: TClient;
begin
  LCon.Canvas.Font.Color := clBlack;
  if odSelected in State then LCon.Canvas.Brush.Color := clYellow;
  LCon.Canvas.FillRect(Rect);

  Sock := TDzSocket(LCon.Items.Objects[Index]);
  C := Sock.Data;

  LCon.Canvas.TextOut(3, Rect.Top+2, Format('%d: %s [%s] > %s',
    [Sock.ID, C.DNS, Sock.RemoteAddress, C.Name]));
end;

procedure TFrm.LUsersDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
  State: TOwnerDrawState);

var U: TUser;
begin
  LUsers.Canvas.Font.Color := clBlack;
  if odSelected in State then LUsers.Canvas.Brush.Color := clYellow;
  LUsers.Canvas.FillRect(Rect);

  U := GetUser(Index);

  LUsers.Canvas.TextOut(3, Rect.Top+2, U.ID.ToString);
  LUsers.Canvas.TextOut(50, Rect.Top+2, U.Name);
end;

procedure TFrm.LogServer(const A: string);
begin
  S_Memo.Lines.Add(FormatDateTime('hh:nn:ss', Now)+' '+A);
end;

procedure TFrm.LogClient(const A: string);
begin
  C_Memo.Lines.Add(FormatDateTime('hh:nn:ss', Now)+' '+A);
end;

// SERVER

procedure TFrm.BtnOnServerClick(Sender: TObject);
begin
  S.Port := StrToInt(EdServerPort.Text);
  S.Open;

  LogServer('Server up');

  if S.KeepAlive then
    LogServer(Format('KeepAlive enabled [%d ms]', [S.KeepAliveInterval]));

  EdServerPort.Enabled := False;
  BtnOnServer.Enabled := False;
  BtnOffServer.Enabled := True;
  BtnKill.Enabled := True;
  BtnSendAll.Enabled := True;
end;

procedure TFrm.BtnOffServerClick(Sender: TObject);
begin
  S.Close;

  LogServer('Server down');

  LCon.Clear; //clear connections list
  //Important: the Data object of connections are not released here beacuse using AutoFreeObjs in Server

  EdServerPort.Enabled := True;
  BtnOnServer.Enabled := True;
  BtnOffServer.Enabled := False;
  BtnKill.Enabled := False;
  BtnSendAll.Enabled := False;
end;

procedure TFrm.SClientConnect(Sender: TObject; Socket: TDzSocket);
begin
  LogServer(Format('Socket %d connected', [Socket.ID]));
end;

procedure TFrm.SClientDisconnect(Sender: TObject; Socket: TDzSocket);
var Index: Integer;
begin
  LogServer(Format('Socket %d disconnected', [Socket.ID]));

  Index := LCon.Items.IndexOfObject(Socket);
  if Index<>-1 then LCon.Items.Delete(Index);

  S.SendAll('D', IntToStr(Socket.ID));
end;

procedure TFrm.SClientError(Sender: TObject; Socket: TDzSocket;
  const Event: TErrorEvent; const ErrorCode: Integer; const ErrorMsg: string);
begin
  LogServer(Format('Socket %d error: %s', [Socket.ID, ErrorMsg]));
end;

procedure TFrm.SClientLoginCheck(Sender: TObject; Socket: TDzSocket;
  var Accept: Boolean; const RequestData: string; var ResponseData: string);
var
  Sock: TDzSocket;
  C: TClient;
begin
  S.Lock;
  try
    for Sock in S do
    begin
      C := Sock.Data;
      if C.Name = RequestData then
      begin
        Accept := False;
        ResponseData := 'User name already in use!';
        Exit;
      end;
    end;
  finally
    S.Unlock;
  end;

  //create client object
  C := TClient.Create;
  C.Name := RequestData;
  Socket.Data := C;
  C.DNS := Socket.RemoteHost; //slow function

  LCon.Items.AddObject('', Socket);
end;

procedure TFrm.SClientLoginSuccess(Sender: TObject; Socket: TDzSocket);
var C: TClient;
begin
  C := Socket.Data;

  S.SendAllEx(Socket, 'C', Format('%d/%s', [Socket.ID, C.Name])); //send connection to other clients
  Socket.Send('L', GetClientsList); //send clients list to the client
end;

procedure TFrm.SClientRead(Sender: TObject; Socket: TDzSocket; const Cmd: Char;
  const A: string);
var C: TClient;
begin
  C := Socket.Data;

  case Cmd of
    'P': //recebeu screenshot
    begin
      DecodePrint(A);
      if CkLoopPrint.Checked then Socket.Send('P'); //request new screenshot to the client
    end;
    else
    begin
      LogServer(Format('Message received from %s (%d): %s/%s {Size=%d}',
       [C.Name, Socket.ID, Cmd, A, A.Length]));
      DoSound;
    end;
  end;
end;

procedure TFrm.S_MsgKeyPress(Sender: TObject; var Key: Char);
var
  Cmd: Char;
  Msg: string;
  Sock: TDzSocket;
  C: TClient;
begin
  if Key = #13 then
  begin
    Key := #0;

    if LCon.ItemIndex=-1 then
      raise Exception.Create('No client selected');

    Sock := GetSelSocketInServer;
    C := Sock.Data;

    Cmd := S_Cmd.Text[1];
    Msg := S_Msg.Text;

    S.Send(Sock, Cmd, Msg);
    S_Msg.Text := '';

    LogServer(Format('Send to %s (%d): %s/%s {Size=%d}',
      [C.Name, Sock.ID, Cmd, Msg, Msg.Length]));
  end;
end;

procedure TFrm.BtnSendAllClick(Sender: TObject);
var
  Cmd: Char;
  Msg: string;
begin
  Cmd := S_Cmd.Text[1];
  Msg := S_Msg.Text;
  S.SendAll(Cmd, Msg);

  LogServer(Format('Send to all: %s/%s', [Cmd, Msg]));
end;

procedure TFrm.BtnKillClick(Sender: TObject);
var Sock: TDzSocket;
begin
  if LCon.ItemIndex=-1 then
    raise Exception.Create('No client selected');

  Sock := GetSelSocketInServer;
  LogServer(Format('Kill socket %d', [Sock.ID]));
  Sock.Close;
end;

function TFrm.GetClientsList: string;
var
  Lst: TStringList;
  Sock: TDzSocket;
  C: TClient;
begin
  Lst := TStringList.Create;
  try
    S.Lock;
    try
      for Sock in S do
      begin
        C := Sock.Data;
        Lst.Add(Format('%d/%s', [Sock.ID, C.Name]));
      end;
    finally
      S.Unlock;
    end;
    Result := Lst.Text;
  finally
    Lst.Free;
  end;
end;

// CLIENT

procedure TFrm.BtnClientConnectClick(Sender: TObject);
begin
  BtnClientConnect.Enabled := False;
  EdHost.Enabled := False;
  EdClientPort.Enabled := False;
  EdName.Enabled := False;

  LogClient('Connecting...');

  C.Host := EdHost.Text;
  C.Port := StrToInt(EdClientPort.Text);
  C.Connect;
end;

procedure TFrm.BtnClientDisconnectClick(Sender: TObject);
begin
  C.Disconnect;
end;

procedure TFrm.CConnect(Sender: TObject; Socket: TDzSocket);
begin
  LogClient('Connected');
  if C.KeepAlive then
    LogClient(Format('KeepAlive enabled [%d ms]', [C.KeepAliveInterval]));

  BtnClientDisconnect.Enabled := True;
end;

procedure TFrm.CLoginRequest(Sender: TObject; Socket: TDzSocket;
  var Data: string);
begin
  LogClient('Login request');
  Data := EdName.Text;
end;

procedure TFrm.CLoginResponse(Sender: TObject; Socket: TDzSocket;
  Accepted: Boolean; const Data: string);
begin
  LogClient(Format('Login response (Accepted: %s / Data: "%s")',
    [BoolToStr(Accepted, True), Data]))
end;

procedure TFrm.CConnectionLost(Sender: TObject; Socket: TDzSocket);
begin
  LogClient('Connection lost!');
end;

procedure TFrm.CDisconnect(Sender: TObject; Socket: TDzSocket;
  const WasConnected: Boolean);
begin
  if WasConnected then
    LogClient('Disconnected');

  BtnClientConnect.Enabled := True;
  BtnClientDisconnect.Enabled := False;

  EdHost.Enabled := True;
  EdClientPort.Enabled := True;
  EdName.Enabled := True;

  ClearClientUsers;
end;

procedure TFrm.CError(Sender: TObject; Socket: TDzSocket;
  const Event: TErrorEvent; const ErrorCode: Integer; const ErrorMsg: string);
begin
  LogClient('ERROR: '+ErrorMsg);
end;

procedure TFrm.CRead(Sender: TObject; Socket: TDzSocket; const Cmd: Char;
  const A: string);
begin
  case Cmd of
    'P': BtnSendPrint.Click; //server requested screenshot
    'L': ClientsListReceived(A);
    'C': AddUser(A);
    'D': DelUser(A);
    else
    begin
      LogClient(Format('Received from Server: %s/%s {Size=%d}', [Cmd, A, A.Length]));
      DoSound;
    end;
  end;
end;

procedure TFrm.C_MsgKeyPress(Sender: TObject; var Key: Char);
var
  Cmd: Char;
  Msg: string;
begin
  if Key = #13 then
  begin
    Key := #0;

    Cmd := C_Cmd.Text[1];
    Msg := C_Msg.Text;

    C.Send(Cmd, Msg);
    LogClient(Format('Send to Server: %s/%s {Size=%d}', [Cmd, Msg, Msg.Length]));
    C_Msg.Text := '';
  end;
end;

procedure TFrm.ClientsListReceived(const A: string);
var
  Lst: TStringList;
  Line: string;
begin
  Lst := TStringList.Create;
  try
    Lst.Text := A;
    for Line in Lst do
      AddUser(Line);
  finally
    Lst.Free;
  end;
end;

procedure TFrm.AddUser(const A: string);
var
  U: TUser;
  Parts: TArray<string>;
begin
  Parts := A.Split(['/']);

  U := TUser.Create;
  U.ID := Parts[0].ToInteger;
  U.Name := Parts[1];
  LUsers.Items.AddObject('', U);
end;

procedure TFrm.DelUser(const A: string);
var I: Integer;
begin
  I := FindUser(StrToInt(A));
  if I<>-1 then
  begin
    LUsers.Items.Objects[I].Free;
    LUsers.Items.Delete(I);
  end;
end;

procedure TFrm.ClearClientUsers;
var I: Integer;
begin
  for I := 0 to LUsers.Count-1 do
    LUsers.Items.Objects[I].Free;

  LUsers.Clear;
end;

procedure TFrm.BtnSendPrintClick(Sender: TObject);
begin
  SendPrint;
end;

end.
