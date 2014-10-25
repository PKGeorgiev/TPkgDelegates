unit Pkg.Delegates;

interface
uses sysUtils, generics.collections;

type
  IPkgDelegate<T> = interface;


  //  IPkgSafeDelegate<T> is used for external access
  //  External users can only add/remove delegates, but cannot execute them
  IPkgSafeDelegate<T> = interface ['{6233C0FE-175E-4EC0-91DC-17C4460219B3}']
    //  Adds a method to the execution queue
    procedure Add(AMethod: T);
    //  Removes a method from the execution queue
    procedure Remove(AMethod: T);
  end;

  //  IPkgDelegate<T> is the *MAIN* interface.
  //  Use a reference to it in your classes
  //  It is like a container for methods
  IPkgDelegate<T> = interface(IPkgSafeDelegate<T>) ['{18EB7EC7-5843-459D-B147-2A2C1CA96539}']
    //  Invokes methods in the execution queue using Invoker Anonymous method
    //  It's thread-safe!
    procedure Invoke(AInvokerMethod: TProc<T>);
    //  Easy casting to IPkgSafeDelegate<T>
    function  ToSafeDelegate: IPkgSafeDelegate<T>;
    //  Used to invoke each method from execution queue using for-in construct
    //  It's thread-safe!
    function  GetEnumerator: TEnumerator<T>;
  end;

  //  TPkgDelegate<T> is used only to create IPkgDelegate<T> instance
  //  DO NOT USE IT'S methods and DO NOT CAST to it!
  TPkgDelegate<T> = class(TInterfacedObject, IPkgSafeDelegate<T>, IPkgDelegate<T>)
    private
    protected
      //FPadlock: TObject;
      FDelegates: TList<T>;
    public type
      TDelegateEnumerator = class(TList<T>.TEnumerator)
      private
        //  Keeps the instance of IPkgDelegate<T> alive
        //  for the period of for-in loop
        FDelegate: IPkgDelegate<T>;
        //  List of methods i.e. execution queue
        //  It's used for locking
        FContainerDelegates: TList<T>;
      public
        //  We lock parent container in the Constructor
        constructor Create(ADelegate: IPkgDelegate<T>; ADelegates: TList<T>);
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
  TMonitor.Enter(FDelegates);
  try
    FDelegates.Add(AMethod);
  finally
    TMonitor.Exit(FDelegates);
  end;
end;

constructor TPkgDelegate<T>.Create;
begin
  inherited Create;
  FDelegates := TList<T>.Create;
end;

destructor TPkgDelegate<T>.Destroy;
begin
  FreeAndNil(FDelegates);
  inherited;
end;

function TPkgDelegate<T>.GetEnumerator: TEnumerator<T>;
begin
  result := TDelegateEnumerator.Create(self, FDelegates);
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
  TMonitor.Enter(FDelegates);
  try
    FDelegates.Remove(AMethod);
  finally
    TMonitor.Exit(FDelegates);
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

constructor TPkgDelegate<T>.TDelegateEnumerator.Create(ADelegate: IPkgDelegate<T>; ADelegates: TList<T>);
begin
  inherited Create(ADelegates);
  FDelegate := ADelegate;
  FContainerDelegates := ADelegates;
  //  Locks Delegates container
  TMonitor.Enter(FContainerDelegates);
end;

destructor TPkgDelegate<T>.TDelegateEnumerator.Destroy;
begin
  //  UnLocks Delegates container
  TMonitor.Exit(FContainerDelegates);
  inherited;
end;


end.
