unit UnitFormPopup;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TFormPopup = class(TForm)
    RichEdit1: TRichEdit;
    procedure FormDeactivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormPopup: TFormPopup;

implementation

{$R *.dfm}

procedure TFormPopup.FormDeactivate(Sender: TObject);
begin
    Hide;
end;

end.
