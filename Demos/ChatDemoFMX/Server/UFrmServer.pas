unit UFrmServer;

interface

uses FMX.Forms, DzSocket, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls,
  FMX.Controls.Presentation, System.Classes, FMX.Types, FMX.Controls,
  FMX.Layouts, FMX.ListBox;

type
  TFrmServer = class(TForm)
    S: TDzTCPServer;
    L: TListBox;
    LbClientsList: TLabel;
    BtnKill: TButton;
    EdLog: TMemo;
    LbLogs: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SClientDisconnect(Sender: TObject; Socket: TDzSocket);
    procedure SClientError(Sender: TObject; Socket: TDzSocket;
      const Event: TErrorEvent; const ErrorCode: Integer;
      const ErrorMsg: string);
    procedure SClientLoginCheck(Sender: TObject; Socket: TDzSocket;
      var Accept: Boolean; const RequestData: string; var ResponseData: string);
    procedure SClientLoginSuccess(Sender: TObject; Socket: TDzSocket);
    procedure SClientRead(Sender: TObject; Socket: TDzSocket; const Cmd: Char;
      const A: string);
    procedure BtnKillClick(Sender: TObject);
  private
    procedure Log(const Text: string);
    function NickNameAlreadyExists(const NickName: string): Boolean;
  end;

var
  FrmServer: TFrmServer;

implementation

{$R *.fmx}

uses System.SysUtils, UClient;

procedure TFrmServer.FormCreate(Sender: TObject);
begin
  S.AutoFreeObjs := True;
  S.EnumeratorOnlyAuth := True;
  S.Open;
end;

procedure TFrmServer.Log(const Text: string);
begin
  EdLog.Lines.Add(FormatDateTime('hh:nn:ss', Now)+' - '+Text);
end;

procedure TFrmServer.SClientDisconnect(Sender: TObject; Socket: TDzSocket);
var
  C: TClient;
  Idx: Integer;
begin
  if not Socket.Auth then Exit;

  C := Socket.Data;
  Log(Format('%s disconnected', [C.NickName]));

  Idx := L.Items.IndexOfObject(Socket);
  if Idx<>-1 then
    L.Items.Delete(Idx);
end;

procedure TFrmServer.SClientError(Sender: TObject; Socket: TDzSocket;
  const Event: TErrorEvent; const ErrorCode: Integer; const ErrorMsg: string);
begin
  Log(Format('Client socket error (ID %d): %s', [Socket.ID, ErrorMsg]));
end;

procedure TFrmServer.SClientLoginCheck(Sender: TObject; Socket: TDzSocket;
  var Accept: Boolean; const RequestData: string; var ResponseData: string);
var
  NickName: string;
  C: TClient;
begin
  NickName := RequestData;
  if NickNameAlreadyExists(NickName) then
  begin
    Accept := False;
    ResponseData := 'Another user is already connected with the same nick name';
    Exit;
  end;

  //login accepted
  C := TClient.Create;
  C.NickName := NickName;

  Socket.Data := C;
end;

function TFrmServer.NickNameAlreadyExists(const NickName: string): Boolean;
var
  Socket: TDzSocket;
  C: TClient;
begin
  for Socket in S do
  begin
    C := Socket.Data;
    if SameText(C.NickName, NickName) then Exit(True);
  end;

  Exit(False);
end;

procedure TFrmServer.SClientLoginSuccess(Sender: TObject; Socket: TDzSocket);
var
  C: TClient;
begin
  C := Socket.Data;
  Log(Format('%s connected', [C.NickName]));

  L.Items.AddObject(C.NickName, Socket);
end;

procedure TFrmServer.SClientRead(Sender: TObject; Socket: TDzSocket;
  const Cmd: Char; const A: string);
var
  C: TClient;
begin
  C := Socket.Data;

  case Cmd of
    'M': S.SendAll('M', ArrayToData([C.NickName, A]));
  end;
end;

procedure TFrmServer.BtnKillClick(Sender: TObject);
var
  Socket: TDzSocket;
begin
  if L.ItemIndex=-1 then Exit;

  Socket := TDzSocket(L.Items.Objects[L.ItemIndex]);
  Socket.Close;
end;

end.
