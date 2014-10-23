unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, Pkg.Delegates, generics.collections;

type

  TForm2 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    del1: IPkgDelegate<TNotifyEvent>;
    del1_: IPkgSafeDelegate<TNotifyEvent>;
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
var
  LMethod: TNotifyEvent;
begin

  for LMethod in del1 do LMethod(self);


  del1.invoke(
    procedure(AMethod: TNotifyEvent)
    begin
      AMethod(self);
    end
  );
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  memo1.Lines.Add('b2');
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  memo1.Lines.Add('b3');
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  button1.SetFocus;
  del1 := TPkgDelegate<TNotifyEvent>.Create;
  del1_ := del1.toSafeDelegate;
  del1_.add(Button2Click);
  del1_.add(Button3Click);
end;



end.
