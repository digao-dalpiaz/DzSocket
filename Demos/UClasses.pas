unit UClasses;

interface

uses DzSocket;

type
  //Class for list of clients in Server
  TClient = class
    Name, DNS: string;
  end;

  //Class for list of clients in Client
  TUser = class
    ID: TSocket;
    Name: string;
  end;

function GetSelSocketInServer: TDzSocket;

function GetUser(Index: Integer): TUser;
function FindUser(ID: TSocket): Integer;

implementation

uses UFrm;

function GetSelSocketInServer: TDzSocket;
begin
  Result := TDzSocket(Frm.LCon.Items.Objects[Frm.LCon.ItemIndex]);
end;

function GetUser(Index: Integer): TUser;
begin
  Result := TUser(Frm.LUsers.Items.Objects[Index]);
end;

function FindUser(ID: TSocket): Integer;
var I: Integer;
begin
  Result := -1;

  for I := Frm.LUsers.Count-1 downto 0 do
  begin
    if GetUser(I).ID = ID then
    begin
      Result := I;
      Break;
    end;
  end;
end;

end.
