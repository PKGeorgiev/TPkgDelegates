unit Pkg.Delegates;

interface
uses sysUtils, generics.collections, RTTI, windows;

type
  IPkgDelegate<T> = interface;


  //  IPkgSafeDelegate<T> is passed for external access
  //  External users can only add/remove delegates, but cannot execute it
  IPkgSafeDelegate<T> = interface ['{6233C0FE-175E-4EC0-91DC-17C4460219B3}']
    //  Adds a method to execution queue
    procedure Add(AMethod: T);
    //  Removes a method from execution queue
    procedure Remove(AMethod: T);
  end;

  //  IPkgDelegate<T> is the *MAIN* interface.
  //  Use a reference to it in your classes
  //  It is like a container for methods
  IPkgDelegate<T> = interface(IPkgSafeDelegate<T>) ['{18EB7EC7-5843-459D-B147-2A2C1CA96539}']
    //  Invokes methods in the execution queue using Invoker Anonymous method
    procedure Invoke(AInvokerMethod: TProc<T>);
    //  Easy casting to IPkgSafeDelegate<T>
    function  ToSafeDelegate: IPkgSafeDelegate<T>;
    //  Used to invoke each method from execution queue using for-in construct
    function  GetEnumerator: TEnumerator<T>;
  end;

  //  TPkgDelegate<T> is used only to create IPkgDelegate<T> instance
  //  DO NOT USE IT'S methods and DO NOT CAST to it!
  TPkgDelegate<T> = class(TInterfacedObject, IPkgSafeDelegate<T>, IPkgDelegate<T>)
    private
    protected
      FPadlock: TObject;
      FDelegates: TList<T>;
    public type
      TDelegateEnumerator = class(TList<T>.TEnumerator)
      private
        //  Keeps the instance of IPkgDelegate<T> alive
        //  for the period of for-in loop
        FDelegate: IPkgDelegate<T>;
        //  The PadLock object of the container instance
        FEnumeratorPadLock: TObject;
      public
        //  We lock parent container in the Constructor
        constructor Create(ADelegate: IPkgDelegate<T>; ADelegates: TList<T>; APadlock: TObject);
        //  We Unlock parent container in the Destructor
        destructor Destroy; override;
      end;

    public
      constructor Create;
      destructor  Destroy; override;

      procedure   Add(AMethod: T);
      procedure   Remove(AMethod: T);
      function    GetEnumerator: TEnumerator<T>;
      procedure   Invoke(AInvokerMethod: TProc<T>);
      function    ToSafeDelegate: IPkgSafeDelegate<T>;
  end;

implementation

{ TPkgDelegate<T> }

procedure TPkgDelegate<T>.Add(AMethod: T);
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
  result := TDelegateEnumerator.Create(self, FDelegates, FPadlock);
end;

procedure TPkgDelegate<T>.Invoke(AInvokerMethod: TProc<T>);
var
  LMethod: T;
begin
  //  Self's GetEnumerator is thread-safe
  for LMethod in self do
  begin
    AInvokerMethod(LMethod);
  end;
end;

procedure TPkgDelegate<T>.Remove(AMethod: T);
begin
  TMonitor.Enter(FPadlock);
  try
    FDelegates.Remove(AMethod);
  finally
    TMonitor.Exit(FPadlock);
  end;
end;

function TPkgDelegate<T>.ToSafeDelegate: IPkgSafeDelegate<T>;
begin
  result := self as IPkgSafeDelegate<T>;
end;

{ TPkgDelegate<T>.TDelegateEnumerator<T> }

//  Magic:
//  http://stackoverflow.com/a/5039775/1022219
//  http://stackoverflow.com/a/26525631/1022219
//  i.e. the Enumerator is guaranteed to be Created and Destroyed within forin's body
//  (the Compiler "injects" try/finally block)
//  So we can use locking wihtin the Enumerator

constructor TPkgDelegate<T>.TDelegateEnumerator.Create(ADelegate: IPkgDelegate<T>; ADelegates: TList<T>; APadlock: TObject);
begin
  inherited Create(ADelegates);
  FDelegate := ADelegate;
  FEnumeratorPadLock := APadlock;
  //  Locks Delegates container
  TMonitor.Enter(FEnumeratorPadLock);
end;

destructor TPkgDelegate<T>.TDelegateEnumerator.Destroy;
begin
  //  UnLocks Delegates container
  TMonitor.Exit(FEnumeratorPadLock);
  inherited;
end;


end.
