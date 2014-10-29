unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, Pkg.Delegates, generics.collections, typInfo;

type

  TButtonX = class(TButton)
    public
      constructor Create(AOwner: TComponent); override;
      procedure MyClick(Sender: TObject);
  end;

  TForm2 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    del1: IPkgDelegate<TNotifyEvent>;
    del1_: IPkgSafeDelegate<TNotifyEvent>;
    da: IPkgDelegate<TProc<TObject>>;
    bx: TButtonX;
    procedure ButtonXClick(Sender: TObject);
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
  exit;

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
  del1.Add(Button5Click);
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  memo1.Lines.Add('b3');
  //del1_.Remove(Button3Click);
//  bx.Free;
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  bx.Free;
end;

procedure TForm2.Button5Click(Sender: TObject);
begin
  memo1.Lines.Add('b5');
  del1.Remove(Button5Click);
  del1.Remove(Button2Click);
end;

procedure TForm2.ButtonXClick(Sender: TObject);
begin
  memo1.Lines.Add('bX');
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  tp: TProc<TObject>;
begin
  button1.SetFocus;

//  da := TPkgDelegate<TProc<TObject>>.Create;
//  da.Add(
//    procedure(Sender: TObject)
//    begin
//      memo1.Lines.Add('ANNO!');
//    end
//  );
//
//  exit;



//  bx := TButtonX.Create(nil);
//  bx.Parent := self;

  del1 := TPkgDelegate<TNotifyEvent>.Create(nil);
  del1_ := del1.toSafeDelegate;
  del1_.add(Button2Click);
  del1_.add(Button3Click);
  //del1_.Add(bx.MyClick);
//  del1_.Remove(Button3Click);
end;



{ TButtonX }

constructor TButtonX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnClick := MyClick;
end;

procedure TButtonX.MyClick(Sender: TObject);
begin
  Form2.Memo1.Lines.Add('MyClick');
end;

end.
