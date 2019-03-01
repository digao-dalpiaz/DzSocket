unit UClasses;

interface

uses DzSocket;

type
  TClient = class
    Name, DNS: String;
  end;

  TUser = class
    ID: TSocket;
    Name: String;
  end;

function GetSelSock: TDzSocket;

function GetUser(Index: Integer): TUser;
function FindUser(ID: TSocket): Integer;

implementation

uses UFrm;

function GetSelSock: TDzSocket;
begin
  Result := TDzSocket(Frm.L.Items.Objects[Frm.L.ItemIndex]);
end;

function GetUser(Index: Integer): TUser;
begin
  Result := TUser(Frm.LUsers.Items.Objects[Index]);
end;

function FindUser(ID: TSocket): Integer;
var I: Integer;
begin
  Result := -1;

  for I := 0 to Frm.LUsers.Count-1 do
  begin
    if GetUser(I).ID = ID then
    begin
      Result := I;
      Break;
    end;
  end;
end;

end.
