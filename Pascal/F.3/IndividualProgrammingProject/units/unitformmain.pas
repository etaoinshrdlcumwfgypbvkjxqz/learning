unit UnitFormMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  UnitWorld;

type

  { TFormMain }

  TFormMain = class(TForm)
    procedure FormCreate(const Sender: TFormMain);
    procedure Draw(Sender: TObject);
  private
    GLBox: TGLBox;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(const Sender: TFormMain);
begin
  GLBox:=TGLBox.Create(Self);
  with GLBox do
  begin
    AutoResizeViewport:=true;
    Parent:=Self;
    MultiSampling:=4;
    Align:=alClient;
    OnPaint:=@Draw;
    invalidate;
  end;
end;

procedure TFormMain.Draw(Sender: TObject);
begin
  Draw0(TGLBox(Sender));
end;

end.

