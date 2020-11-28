unit UFrmClient;

interface

uses FMX.Forms, DzSocket, FMX.StdCtrls, FMX.Edit, System.Classes, FMX.Types,
  FMX.Controls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TConnectionState = (csDisconnected, csConnecting, csConnected);

  TFrmClient = class(TForm)
    C: TDzTCPClient;
    EdLog: TMemo;
    EdMessage: TEdit;
    BtnConnect: TButton;
    BtnDisconnect: TButton;
    BoxConfig: TPanel;
    EdNickName: TEdit;
    LbNickName: TLabel;
    EdHost: TEdit;
    LbHost: TLabel;
    procedure BtnConnectClick(Sender: TObject);
    procedure BtnDisconnectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure CDisconnect(Sender: TObject; Socket: TDzSocket;
      const WasConnected: Boolean);
    procedure CError(Sender: TObject; Socket: TDzSocket;
      const Event: TErrorEvent; const ErrorCode: Integer;
      const ErrorMsg: string);
    procedure CLoginRequest(Sender: TObject; Socket: TDzSocket;
      var Data: string);
    procedure CLoginResponse(Sender: TObject; Socket: TDzSocket;
      Accepted: Boolean; const Data: string);
    procedure CConnectionLost(Sender: TObject; Socket: TDzSocket);
    procedure EdMessageKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure CRead(Sender: TObject; Socket: TDzSocket; const Cmd: Char;
      const A: string);
  private
    procedure UpdateVisualConnectionState(State: TConnectionState);
    procedure Log(const Text: string);
  end;

var
  FrmClient: TFrmClient;

implementation

{$R *.fmx}

uses System.SysUtils, FMX.DialogService;

procedure TFrmClient.FormCreate(Sender: TObject);
begin
  UpdateVisualConnectionState(csDisconnected);
end;

procedure TFrmClient.Log(const Text: string);
begin
  EdLog.Lines.Add(FormatDateTime('hh:nn:ss', Now)+' - '+Text);
end;

procedure TFrmClient.BtnConnectClick(Sender: TObject);
begin
  EdNickName.Text := Trim(EdNickName.Text);
  if EdNickName.Text=string.Empty then
  begin
    TDialogService.ShowMessage('Please, type user nick name.');
    EdNickName.SetFocus;
    Exit;
  end;

  EdHost.Text := Trim(EdHost.Text);
  if EdHost.Text=string.Empty then
  begin
    TDialogService.ShowMessage('Please, type server address.');
    EdHost.SetFocus;
    Exit;
  end;

  C.Host := EdHost.Text;

  UpdateVisualConnectionState(csConnecting);
  Log('Connecting...');
  C.Connect;
end;

procedure TFrmClient.BtnDisconnectClick(Sender: TObject);
begin
  C.Disconnect;
end;

procedure TFrmClient.CConnectionLost(Sender: TObject; Socket: TDzSocket);
begin
  Log('Connection lost.');
end;

procedure TFrmClient.CDisconnect(Sender: TObject; Socket: TDzSocket;
  const WasConnected: Boolean);
begin
  Log('Disconnected.');
  UpdateVisualConnectionState(csDisconnected);
end;

procedure TFrmClient.CError(Sender: TObject; Socket: TDzSocket;
  const Event: TErrorEvent; const ErrorCode: Integer; const ErrorMsg: string);
begin
  Log('ERROR: '+ErrorMsg);
end;

procedure TFrmClient.CLoginRequest(Sender: TObject; Socket: TDzSocket;
  var Data: string);
begin
  //send nick name when server requests login data
  Data := EdNickName.Text;
end;

procedure TFrmClient.CLoginResponse(Sender: TObject; Socket: TDzSocket;
  Accepted: Boolean; const Data: string);
begin
  if Accepted then
  begin
    Log('Connected.');
    UpdateVisualConnectionState(csConnected);
  end
  else
    Log('Login rejected: '+Data);
end;

procedure TFrmClient.CRead(Sender: TObject; Socket: TDzSocket; const Cmd: Char;
  const A: string);
var
  D: TMsgArray;
begin
  case Cmd of
    'M': //message received
    begin
      D := DataToArray(A);
      Log(D[0]+': '+D[1]);

      //first array value = sender nick name
      //second array value = message text
    end;
  end;
end;

procedure TFrmClient.EdMessageKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key=13 then
  begin
    Key := 0;
    C.Send('M', EdMessage.Text);
    EdMessage.Text := string.Empty;
  end;
end;

procedure TFrmClient.UpdateVisualConnectionState(State: TConnectionState);
begin
  BoxConfig.Enabled := State=csDisconnected;
  BtnConnect.Enabled := State=csDisconnected;
  BtnDisconnect.Enabled := State=csConnected;
  EdMessage.Enabled := State=csConnected;
end;

end.
