unit UPrint;

interface

procedure SendPrint;
procedure DecodePrint(const A: string);

implementation

uses Winapi.Windows, Vcl.Graphics, System.Classes, System.ZLib, System.SysUtils,
  UFrm, UFrmPrint;

procedure SendPrint;
var
  DC: HDC;
  B: TBitmap;
  Mem: TMemoryStream;
  MS: TStringStream;
  R: TRect;
begin
  MS := TStringStream.Create;
  try
    Mem := TMemoryStream.Create;
    try
      B := TBitmap.Create;
      try
        DC := GetDC(0);
        try
          Winapi.Windows.GetClientRect(WindowFromDC(DC), R);
          B.SetSize(R.Width div 2, R.Height div 2);
          B.PixelFormat := pf16bit;

          SetStretchBltMode(B.Canvas.Handle, HALFTONE);
          StretchBlt(B.Canvas.Handle, 0, 0, B.Width, B.Height, DC, 0, 0,
            R.Width, R.Height, SRCCOPY);
        finally
          ReleaseDC(0, DC);
        end;

        B.SaveToStream(Mem);
      finally
        B.Free;
      end;
      Mem.Position := 0;
      ZCompressStream(Mem, MS);
    finally
      Mem.Free;
    end;
    Frm.C.Send('P', MS.DataString);
  finally
    MS.Free;
  end;
end;

procedure DecodePrint(const A: string);
var
  MS: TStringStream;
  Mem: TMemoryStream;
  B: TBitmap;
begin
  B := TBitmap.Create;
  try
    Mem := TMemoryStream.Create;
    try
      MS := TStringStream.Create(A);
      try
        ZDecompressStream(MS, Mem);
      finally
        MS.Free;
      end;
      Mem.Position := 0;
      B.LoadFromStream(Mem);
    finally
      Mem.Free;
    end;

    FrmPrint.Img.Picture.Assign(B);
    FrmPrint.Caption := Format('Screenshot received - %s / Buffer Length: %d',
      [FormatDateTime('hh:nn:ss.zzz', Now), A.Length]);
    FrmPrint.Show;

  finally
    B.Free;
  end;
end;

end.
