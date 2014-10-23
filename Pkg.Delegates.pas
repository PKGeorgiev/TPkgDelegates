unit Pkg.Delegates;

interface
uses sysUtils, generics.collections, RTTI, windows;

type
  IPkgDelegate<T> = interface;

  IPkgSafeDelegate<T> = interface ['{6233C0FE-175E-4EC0-91DC-17C4460219B3}']
    procedure Add(AMethod: T);
    procedure Remove(AMethod: T);
  end;

  IPkgDelegate<T> = interface(IPkgSafeDelegate<T>) ['{18EB7EC7-5843-459D-B147-2A2C1CA96539}']
    procedure Invoke(AInvokerMethod: TProc<T>);
    function  ToSafeDelegate: IPkgDelegate<T>;
    function  GetEnumerator: TEnumerator<T>;
  end;

  TPkgDelegate<T> = class(TInterfacedObject, IPkgSafeDelegate<T>, IPkgDelegate<T>)
    private
    protected
      FPadlock: TObject;
      FDelegates: TList<T>;

    public type
      TDelegateEnumerator = class(TList<T>.TEnumerator)
      private
        FDelegate: TPkgDelegate<T>;
      public
        constructor Create(ADelegate: TPkgDelegate<T>);
        destructor Destroy; override;
      end;

    public
      constructor Create;
      destructor Destroy; override;
      procedure add(AMethod: T);
      procedure remove(AMethod: T);
      function GetEnumerator: TEnumerator<T>;
      procedure invoke(AInvokerMethod: TProc<T>);
      function toSafeDelegate: IPkgDelegate<T>;
  end;

implementation

{ TPkgDelegate<T> }

procedure TPkgDelegate<T>.add(AMethod: T);
begin
  TMonitor.Enter(FPadlock);
  try
    FDelegates.Add(AMethod);
  finally
    TMonitor.Exit(FPadlock);
  end;
end;

constructor TPkgDelegate<T>.Create;
begin
  inherited Create;
  FPadlock := TObject.Create;
  FDelegates := TList<T>.Create;
end;

destructor TPkgDelegate<T>.Destroy;
begin
  FreeAndNil(FDelegates);
  FreeAndNil(FPadlock);
  inherited;
end;

function TPkgDelegate<T>.GetEnumerator: TEnumerator<T>;
begin
  result := TDelegateEnumerator.Create(self);
end;

procedure TPkgDelegate<T>.invoke(AInvokerMethod: TProc<T>);
var
  LMethod: T;
begin
  for LMethod in FDelegates do
  begin
    AInvokerMethod(LMethod);
  end;
end;

procedure TPkgDelegate<T>.remove(AMethod: T);
begin
  TMonitor.Enter(FPadlock);
  try
    FDelegates.Remove(AMethod);
  finally
    TMonitor.Exit(FPadlock);
  end;
end;

function TPkgDelegate<T>.toSafeDelegate: IPkgDelegate<T>;
begin
  result := self as IPkgDelegate<T>;
end;

{ TPkgDelegate<T>.TDelegateEnumerator<T> }

//  Magic: http://stackoverflow.com/a/5039775/1022219

constructor TPkgDelegate<T>.TDelegateEnumerator.Create(ADelegate: TPkgDelegate<T>);
begin
  inherited Create(ADelegate.FDelegates);
  FDelegate := ADelegate;
  TMonitor.Enter(FDelegate.FPadlock);
end;

destructor TPkgDelegate<T>.TDelegateEnumerator.Destroy;
begin
  TMonitor.Exit(FDelegate.FPadlock);
  inherited;
end;


end.
