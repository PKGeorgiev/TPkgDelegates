unit Pkg.Delegates;

interface
uses sysUtils, generics.collections, typInfo, classes, {$IFDEF VER210}rtti,{$ENDIF}generics.defaults;

type



  //  Represents the lifetime of a handler
  THandlerLifetime = (hlPermanent, hlOneTime);
  //  Represents handler's status
  THandlerStatus = (hsActive, hsPendingDeletion);

  TTypeCast = class
  public
    // ReinterpretCast does a hard type cast
    class function ReinterpretCast<ReturnT>(const Value): ReturnT;
    // StaticCast does a hard type cast but requires an input type
    class function StaticCast<T, ReturnT>(const Value: T): ReturnT;
  end;

  //  Used to remove handlers of destroyed owner components
  //  I.e. owners that support FreeNotification method
  TFreeNotificationSink = class(TComponent)
  private
    FOwnerComp: TComponent;
    FEvent: TProc<TObject>;
    function getOnFreeNotification: TProc<TObject>;
    procedure setOnFreNotification(const Value: TProc<TObject>);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent; handler: TProc<TObject>); reintroduce;
    destructor Destroy; override;
    property onFreeNotification: TProc<TObject> read getOnFreeNotification write setOnFreNotification;
  end;

  IPkgDelegate<T> = interface;

  //  Holds conrete handler and it's attributes
  TSysHandlerItem<T> = class
    private
      FPadLock: TObject;
      FHandler: T;
      FLifeTime: THandlerLifetime;
      FStatus: THandlerStatus;
      FFreeNotificationSink: TFreeNotificationSink;
      function getStatus: THandlerStatus;
      procedure setStatus(const Value: THandlerStatus);
    protected
    public
      constructor Create(AHandler: T; AHandlerLifeTime: THandlerLifetime = hlPermanent);
      destructor Destroy; override;
      property Handler: T read FHandler;
      property LifeTime: THandlerLifetime read FLifeTime;
      property Status: THandlerStatus read getStatus write setStatus;
  end;

  //  IPkgSafeDelegate<T> is used for external access
  //  External users can only add/remove delegates, but cannot execute them
  IPkgSafeDelegate<T> = interface ['{6233C0FE-175E-4EC0-91DC-17C4460219B3}']
    function getIsExecuting: boolean;
    //  Adds a handler to the delegate
    procedure Add(AHandler: T; AHandlerLifeTime: THandlerLifetime = hlPermanent);
    //  Removes a handler from the delegate
    //  If the delegate is currently executing the handler is marked for deletion
    procedure Remove(AMethod: T);
    procedure RemoveAll;
    property IsExecuting: boolean read getIsExecuting;
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
    //  Cleans handlers marked for deletion
    procedure CleanupHandlers;
  end;

  //  TPkgDelegate<T> is used only to create IPkgDelegate<T> instance
  //  DO NOT USE IT'S methods and DO NOT CAST to it!
  TPkgDelegate<T> = class(TInterfacedObject, IPkgSafeDelegate<T>, IPkgDelegate<T>)
    private
      function getIsExecuting: boolean;
    protected
      //  List of handlers
      FHandlers: TList<TSysHandlerItem<T>>;
      FIsExecuting: boolean;
      FOwnerFreeNotificationSink: TFreeNotificationSink;
      //  Default Comparer is broken in Delphi 2009!!!
      //  We need own implementation
      function AreEqual(ALeft, ARight: T): boolean;
    public type
      TDelegateEnumerator = class(TEnumerator<T>)
      private
        //  Keeps the instance of IPkgDelegate<T> alive
        //  for the period of for-in loop
        FDelegate: IPkgDelegate<T>;
        FTDelegate: TPkgDelegate<T>;
        //  List of handlers i.e. execution queue
        //  It's used for locking
        //  NB: The Enumerator returns only active handlers!
        FDelegateHandlers: TList<TSysHandlerItem<T>>;
        FIndex: integer;
      protected
        function DoGetCurrent: T; override;
        function DoMoveNext: Boolean; override;
        function GetCurrent: T;
      public
        //  We lock parent container in the Constructor
        constructor Create(ADelegate: IPkgDelegate<T>; ADelegates: TList<TSysHandlerItem<T>>);
        //  We Unlock parent container in the Destructor
        destructor Destroy; override;

        property Current: T read DoGetCurrent;
        function MoveNext: Boolean;
      end;

    public
      constructor Create(AOwner: TComponent = nil);
      destructor  Destroy; override;

      procedure   Add(AHandler: T; AHandlerLifeTime: THandlerLifetime = hlPermanent);
      procedure   Remove(AMethod: T);
      procedure   RemoveAll;
      function    GetEnumerator: TEnumerator<T>;
      procedure   Invoke(AInvokerMethod: TProc<T>);
      function    ToSafeDelegate: IPkgSafeDelegate<T>;
      property    IsExecuting: boolean read getIsExecuting;
      procedure   CleanupHandlers;
  end;

implementation

{ TPkgDelegate<T> }

procedure TPkgDelegate<T>.Add(AHandler: T; AHandlerLifeTime: THandlerLifetime);
var
  LDelegateRec: TSysHandlerItem<T>;
begin
  TMonitor.Enter(FHandlers);
  try
    LDelegateRec := TSysHandlerItem<T>.Create(AHandler, AHandlerLifeTime);
    FHandlers.Add(LDelegateRec);
  finally
    TMonitor.Exit(FHandlers);
  end;
end;

function TPkgDelegate<T>.AreEqual(ALeft, ARight: T): boolean;
var
  LTypeInfo: PTypeInfo;
  LMethod, RMethod: TMethod;
  LLeft, LRight: IInterface;
begin
  LTypeInfo := TypeInfo(T);
  if not assigned(LTypeInfo) then
    raise Exception.Create('Missing RTTI!');

  case LTypeInfo^.Kind of
    tkMethod:
    begin
      LMethod := TMethod((@ALeft)^);
      RMethod := TMethod((@ARight)^);
      result := (LMethod.Code = RMethod.Code) AND (LMethod.Data = RMethod.Data);
    end;

    tkInterface:
    begin
      LLeft := IInterface((@ALeft)^);
      LRight := IInterface((@ARight)^);
      result := LLeft = LRight;
    end;

    else
      raise Exception.Create('Unknown method type!');
  end;

end;

procedure TPkgDelegate<T>.CleanupHandlers;
var
  k: integer;
  LHandler: TSysHandlerItem<T>;
begin
  TMonitor.Enter(FHandlers);
  try
    for k := FHandlers.Count - 1 downto 0 do
    begin
      LHandler := FHandlers[k];
      if LHandler.Status = hsPendingDeletion then
      begin
        FHandlers.Delete(k);
        LHandler.Free;
      end;
    end;
  finally
    TMonitor.Exit(FHandlers);
  end;
end;

constructor TPkgDelegate<T>.Create(AOwner: TComponent);
begin
  inherited Create;
  FHandlers := TList<TSysHandlerItem<T>>.Create;
  if assigned(AOwner) then
  begin
    FOwnerFreeNotificationSink := TFreeNotificationSink.Create(AOwner,
      procedure(Sender: TObject)
      begin
        RemoveAll;
      end
    );
  end;
end;

destructor TPkgDelegate<T>.Destroy;
begin
  FreeAndNil(FOwnerFreeNotificationSink);
  FreeAndNil(FHandlers);
  inherited;
end;

function TPkgDelegate<T>.GetEnumerator: TEnumerator<T>;
begin
  result := TDelegateEnumerator.Create(self, FHandlers);
end;

function TPkgDelegate<T>.getIsExecuting: boolean;
begin
  TMonitor.Enter(FHandlers);
  try
    result := FIsExecuting;
  finally
    TMonitor.Exit(FHandlers);
  end;
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
var
  LHandler: TSysHandlerItem<T>;
  k: integer;
  LComparer: IComparer<T>;
begin
  TMonitor.Enter(FHandlers);
  try
    LComparer := TComparer<T>.Default;
    for k := FHandlers.Count - 1 downto 0 do
    begin
      LHandler := FHandlers[k];
      if AreEqual(LHandler.Handler, AMethod) then
      begin
        if FIsExecuting then
          //  If the delegate is currently executing we mark the handler for deletion
          LHandler.Status := hsPendingDeletion
        else
        begin
          //  Completely remove the handler
          FHandlers.Remove(LHandler);
          LHandler.Free;
        end;
        exit;
      end;
    end;
  finally
    TMonitor.Exit(FHandlers);
  end;
end;

procedure TPkgDelegate<T>.RemoveAll;
var
  LHandler: TSysHandlerItem<T>;
  k: integer;
begin
  TMonitor.Enter(FHandlers);
  try
    for k := FHandlers.Count - 1 downto 0 do
    begin
      LHandler := FHandlers[k];
      if FIsExecuting then
        //  If the delegate is currently executing we mark the handler for deletion
        LHandler.Status := hsPendingDeletion
      else
      begin
        //  Completely remove the handler
        FHandlers.Remove(LHandler);
        LHandler.Free;
      end;
    end;
  finally
    TMonitor.Exit(FHandlers);
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

constructor TPkgDelegate<T>.TDelegateEnumerator.Create(ADelegate: IPkgDelegate<T>; ADelegates: TList<TSysHandlerItem<T>>);
begin
  inherited Create;
  FDelegate := ADelegate;
  FTDelegate := ADelegate as TPkgDelegate<T>;
  FDelegateHandlers := ADelegates;
  FIndex := -1;
  //  Locks Delegates container
  TMonitor.Enter(FDelegateHandlers);
  FTDelegate.FIsExecuting := true;
end;

destructor TPkgDelegate<T>.TDelegateEnumerator.Destroy;
var
  LHandler: TSysHandlerItem<T>;
  k: integer;
begin

  //  Remove stale handlers (i.e. handlers to be removed)
  FDelegate.CleanupHandlers;

  FTDelegate.FIsExecuting := false;
  //  UnLocks Delegates container
  TMonitor.Exit(FDelegateHandlers);
  inherited;
end;

function TPkgDelegate<T>.TDelegateEnumerator.DoGetCurrent: T;
begin
  result := GetCurrent;
end;

function TPkgDelegate<T>.TDelegateEnumerator.DoMoveNext: Boolean;
begin
  result := MoveNext;
end;

function TPkgDelegate<T>.TDelegateEnumerator.GetCurrent: T;
begin
  if FDelegateHandlers[FIndex].LifeTime = hlOneTime then
    FDelegateHandlers[FIndex].Status := hsPendingDeletion;

  result := FDelegateHandlers[FIndex].Handler;
end;

function TPkgDelegate<T>.TDelegateEnumerator.MoveNext: Boolean;
begin
  if FIndex >= FDelegateHandlers.Count then
    Exit(False);

  Inc(FIndex);

  //  Find an active handler
  while FIndex < FDelegateHandlers.Count do
  begin
    if FDelegateHandlers[FIndex].Status = hsActive then exit(true);
    Inc(FIndex);
  end;

  Result := FIndex < FDelegateHandlers.Count;
end;

{ TSysDelegateItem<T> }

constructor TSysHandlerItem<T>.Create(AHandler: T;
  AHandlerLifeTime: THandlerLifetime);
var
  LTypeInfo: PTypeInfo;
  LTypeData: PTypeData;
  LPropInfo: PPropInfo;
  LMethod: ^TMethod;
  FSelf: TObject;

  {$IFDEF VER210}
  LObj: TObject;
  LRttiContext: TRttiContext;
  LRttyType: TRttiType;
  LRttiField: TRttiField;
  {$ENDIF}

begin
  inherited Create;
  FPadLock := TObject.Create;
  FHandler := AHandler;
  FStatus := hsActive;
  FLifeTime := AHandlerLifeTime;

  LTypeInfo := TypeInfo(T);
  if assigned(LTypeInfo) then
  begin
    if LTypeInfo^.Kind = tkMethod then
    begin
      FSelf := TMethod((@FHandler)^).Data;
      if FSelf is TComponent then
        FFreeNotificationSink := TFreeNotificationSink.Create(FSelf as TComponent,
          procedure(Sender: TObject)
          begin
            Status := hsPendingDeletion;
          end
        );
    end;

    {$IFDEF VER210}
    if LTypeInfo^.Kind = tkInterface then
    begin
      LTypeData := PTypeData(LTypeInfo);
      LObj := TTypeCast.ReinterpretCast<IInterface>(FHandler) as TObject;
      LRttiContext.Create;
      try
        LRttyType := LRttiContext.GetType(LObj.ClassType);
        if assigned(LRttyType) then
        begin
          LRttiField := LRttyType.GetField('Self');
          if assigned(LRttiField) then
          begin
            FSelf := LRttiField.GetValue(LObj).AsObject;

            if FSelf is TComponent then
              FNotificationSink := TNotificationSink.Create(FSelf as TComponent,
                procedure(Sender: TObject)
                begin
                  Status := hsPendingDeletion;
                end
              );
          end;
        end;

      finally
        LRttiContext.Free;
      end;
    end;
    {$ENDIF}

  end;
end;

destructor TSysHandlerItem<T>.Destroy;
begin
  FreeAndNil(FFreeNotificationSink);
  FreeAndNil(FPadLock);
  inherited;
end;

function TSysHandlerItem<T>.getStatus: THandlerStatus;
begin
  TMonitor.Enter(FPadLock);
  try
    result := FStatus;
  finally
    TMonitor.Exit(FPadLock);
  end;
end;

procedure TSysHandlerItem<T>.setStatus(const Value: THandlerStatus);
begin
  TMonitor.Enter(FPadLock);
  try
    FStatus := value;
  finally
    TMonitor.Exit(FPadLock);
  end;
end;

{ TNotificationSink }

constructor TFreeNotificationSink.Create(AOwner: TComponent;
  handler: TProc<TObject>);
begin
  FOwnerComp := AOwner;
  FOwnerComp.FreeNotification(self);
  FEvent := handler;
end;

destructor TFreeNotificationSink.Destroy;
begin
  if Assigned(FOwnerComp) then
    FOwnerComp.RemoveFreeNotification(self);
  inherited;
end;

function TFreeNotificationSink.getOnFreeNotification: TProc<TObject>;
begin
  result := FEvent;
end;

procedure TFreeNotificationSink.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if Operation = opRemove then
    if Assigned(FEvent) then
    begin
      AComponent.RemoveFreeNotification(self);
      FOwnerComp := nil;
      FEvent(AComponent);
    end;
end;

procedure TFreeNotificationSink.setOnFreNotification(const Value: TProc<TObject>);
begin
  FEvent := Value;
end;

{ TTypeCast }

class function TTypeCast.ReinterpretCast<ReturnT>(const Value): ReturnT;
begin
  Result := ReturnT(Value);
end;

class function TTypeCast.StaticCast<T, ReturnT>(const Value: T): ReturnT;
begin
  Result := ReinterpretCast<ReturnT>(Value);
end;

end.
