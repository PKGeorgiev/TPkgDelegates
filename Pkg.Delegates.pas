unit Pkg.Delegates;

interface
uses
  sysUtils, generics.collections, typInfo,
  {$IF CompilerVersion >= 21.0}rtti,{$ENDIF}classes;

procedure syncStart(ASyncObject: TObject);
procedure syncEnd(ASyncObject: TObject);  
  
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

  ISysHandlerItem<T> = interface ['{E7E777D7-259F-4A6E-8548-A263E4882A07}']
    function getStatus: THandlerStatus;
    procedure setStatus(const Value: THandlerStatus);
    function getLifeTime: THandlerLifetime;
    function getHandler: T;

    property Handler: T read getHandler;
    property LifeTime: THandlerLifetime read getLifeTime;
    property Status: THandlerStatus read getStatus write setStatus;
  end;

  //  Holds conrete handler and it's attributes
  TSysHandlerItem<T> = class(TInterfacedObject, ISysHandlerItem<T>)
    private
      FPadLock: TObject;
      FHandler: T;
      FLifeTime: THandlerLifetime;
      FStatus: THandlerStatus;
      FFreeNotificationSink: TFreeNotificationSink;
      function getStatus: THandlerStatus;
      procedure setStatus(const Value: THandlerStatus);
      function getLifeTime: THandlerLifetime;
      function getHandler: T;
    protected
    public
      constructor Create(AHandler: T; AHandlerLifeTime: THandlerLifetime = hlPermanent);
      destructor Destroy; override;
      property Handler: T read getHandler;
      property LifeTime: THandlerLifetime read getLifeTime;
      property Status: THandlerStatus read getStatus write setStatus;
  end;

  TDelegateEnumerator<T> = class(TEnumerator<T>)
  private
    //  Keeps the instance of IPkgDelegate<T> alive
    //  for the period of for-in loop
    FDelegate: IPkgDelegate<T>;
    //  List of handlers i.e. execution queue
    //  It's used for locking
    //  NB: The Enumerator returns only active handlers!
    FDelegateHandlers,
    FCopyOfDelegateHandlers: TList<ISysHandlerItem<T>>;
    FIndex: integer;
    FWasCreatedInMainThread: boolean;
  protected
    function DoGetCurrent: T; override;
    function DoMoveNext: Boolean; override;
    function GetCurrent: T;
    procedure internalDestroy;
  public
    //  We lock parent container in the Constructor
    constructor Create(ADelegate: IPkgDelegate<T>; ADelegates: TList<ISysHandlerItem<T>>);
    //  We Unlock parent container in the Destructor
    destructor Destroy; override;

    property Current: T read DoGetCurrent;
    function MoveNext: Boolean;

    function GetEnumerator: TEnumerator<T>;
  end;  

  //  IPkgSafeDelegate<T> is used for external access
  //  External users can only add/remove delegates, but cannot execute them
  IPkgSafeDelegate<T> = interface ['{6233C0FE-175E-4EC0-91DC-17C4460219B3}']
    //  Adds a handler to the delegate
    procedure Add(AHandler: T; AHandlerLifeTime: THandlerLifetime = hlPermanent);
    //  Removes a handler from the delegate
    //  If the delegate is currently executing the handler is marked for deletion
    procedure Remove(AMethod: T);
    procedure RemoveAll;
    //  Cleans handlers marked for deletion
    procedure CleanupHandlers;
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
    function  GetEnumerator: TDelegateEnumerator<T>;
    function  InMainThread: TDelegateEnumerator<T>;
    function getCount: integer;
    property Count: integer read getCount;
  end;

  //  TPkgDelegate<T> is used only to create IPkgDelegate<T> instance
  //  DO NOT USE IT'S methods and DO NOT CAST to it!
  TPkgDelegate<T> = class(TInterfacedObject, IPkgSafeDelegate<T>, IPkgDelegate<T>)
    private

    protected
      //  List of handlers
      FHandlers: TList<ISysHandlerItem<T>>;
      FOwnerFreeNotificationSink: TFreeNotificationSink;
      //  Default Comparer is broken in Delphi 2009!!!
      //  We need own implementation
      function AreEqual(ALeft, ARight: T): boolean;
      //  These should be protected to compile in Delphi 2009
      function getCount: integer;


    public
      constructor Create(AOwner: TComponent = nil); overload;
      constructor Create(AHandler: T; AHandlerLifeTime: THandlerLifetime = hlPermanent; AOwner: TComponent = nil); overload;
      destructor  Destroy; override;

      procedure   Add(AHandler: T; AHandlerLifeTime: THandlerLifetime = hlPermanent);
      procedure   Remove(AMethod: T);
      procedure   RemoveAll;
      function    GetEnumerator: TDelegateEnumerator<T>;
      function    InMainThread: TDelegateEnumerator<T>;
      procedure   Invoke(AInvokerMethod: TProc<T>); virtual;
      function    ToSafeDelegate: IPkgSafeDelegate<T>;
      procedure   CleanupHandlers;
      property    Count: integer read getCount;
  end;

implementation

{ TPkgDelegate<T> }

procedure TPkgDelegate<T>.Add(AHandler: T; AHandlerLifeTime: THandlerLifetime);
var
  LDelegateRec: TSysHandlerItem<T>;
begin
  syncStart(FHandlers);
  try
    LDelegateRec := TSysHandlerItem<T>.Create(AHandler, AHandlerLifeTime);
    FHandlers.Add(LDelegateRec);
  finally
    syncEnd(FHandlers);
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
  LHandler: ISysHandlerItem<T>;
begin
  syncStart(FHandlers);
  try
    for k := FHandlers.Count - 1 downto 0 do
    begin
      LHandler := FHandlers[k];
      if LHandler.Status = hsPendingDeletion then
        FHandlers.Delete(k);
    end;
  finally
    syncEnd(FHandlers);
  end;
end;

constructor TPkgDelegate<T>.Create(AHandler: T;
  AHandlerLifeTime: THandlerLifetime; AOwner: TComponent);
begin
  Create(AOwner);
  add(AHandler, AHandlerLifeTime);
end;

constructor TPkgDelegate<T>.Create(AOwner: TComponent);
begin
  inherited Create;
  FHandlers := TList<ISysHandlerItem<T>>.Create;
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
  CleanupHandlers;
  FreeAndNil(FOwnerFreeNotificationSink);
  FreeAndNil(FHandlers);
  inherited;
end;

function TPkgDelegate<T>.getCount: integer;
begin
  syncStart(FHandlers);
  try
    result := FHandlers.Count;
  finally
    syncEnd(FHandlers);
  end;
end;

function TPkgDelegate<T>.GetEnumerator: TDelegateEnumerator<T>;
begin
  result := TDelegateEnumerator<T>.Create(self, FHandlers);
end;

function TPkgDelegate<T>.InMainThread: TDelegateEnumerator<T>;
var
  LEnumerator: TDelegateEnumerator<T>;
begin

  TThread.Synchronize(nil, 
    procedure
    begin
      LEnumerator := GetEnumerator;
    end
  );

  result := LEnumerator;

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
  LHandler: ISysHandlerItem<T>;
  k: integer;
begin
  syncStart(FHandlers);
  try
    for k := FHandlers.Count - 1 downto 0 do
    begin
      LHandler := FHandlers[k];
      if AreEqual(LHandler.Handler, AMethod) then
      begin
        //  Completely remove the handler
        LHandler.Status := hsPendingDeletion;
        FHandlers.Remove(LHandler);
        exit;
      end;
    end;
  finally
    syncEnd(FHandlers);
  end;
end;

procedure TPkgDelegate<T>.RemoveAll;
var
  LHandler: ISysHandlerItem<T>;
  k: integer;
begin
  syncStart(FHandlers);
  try
    for k := FHandlers.Count - 1 downto 0 do
    begin
      LHandler := FHandlers[k];
      //  Completely remove the handler
      LHandler.Status := hsPendingDeletion;
      FHandlers.Remove(LHandler);
    end;
  finally
    syncEnd(FHandlers);
  end;
end;

procedure syncEnd(ASyncObject: TObject);
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    TMonitor.Exit(ASyncObject);
end;

procedure syncStart(ASyncObject: TObject);
begin
  if TThread.CurrentThread.ThreadID <> MainThreadID then
    TMonitor.Enter(ASyncObject);
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

constructor TDelegateEnumerator<T>.Create(ADelegate: IPkgDelegate<T>; ADelegates: TList<ISysHandlerItem<T>>);
begin
  inherited Create;
  FDelegate := ADelegate;
  FDelegateHandlers := ADelegates;
  FIndex := -1;
  FWasCreatedInMainThread := TThread.CurrentThread.ThreadID = MainThreadID;
  //  Locks Delegates container
  syncStart(FDelegateHandlers);
  FCopyOfDelegateHandlers := TList<ISysHandlerItem<T>>.Create;
  //  Create a copy of original FDelegateHandlers
  FCopyOfDelegateHandlers.AddRange(FDelegateHandlers);
end;

procedure TDelegateEnumerator<T>.internalDestroy;
var
  LHandler: TSysHandlerItem<T>;
  k: integer;
begin
  //  Remove stale handlers (i.e. handlers to be removed)
  FDelegate.CleanupHandlers;

  FreeAndNil(FCopyOfDelegateHandlers);
  //  UnLocks Delegates container
  syncEnd(FDelegateHandlers);
end;


destructor TDelegateEnumerator<T>.Destroy;

begin

  if FWasCreatedInMainThread then
    TThread.Synchronize(nil,
      procedure
      begin
        internalDestroy;
      end
    )
  else
    internalDestroy;

  inherited;
end;

function TDelegateEnumerator<T>.DoGetCurrent: T;
begin
  result := GetCurrent;
end;

function TDelegateEnumerator<T>.DoMoveNext: Boolean;
begin
  result := MoveNext;
end;

function TDelegateEnumerator<T>.GetCurrent: T;
begin
  if FCopyOfDelegateHandlers[FIndex].LifeTime = hlOneTime then
    FCopyOfDelegateHandlers[FIndex].Status := hsPendingDeletion;

  result := FCopyOfDelegateHandlers[FIndex].Handler;
end;

function TDelegateEnumerator<T>.GetEnumerator: TEnumerator<T>;
begin
  result := self;
end;


function TDelegateEnumerator<T>.MoveNext: Boolean;
begin
  if FIndex >= FCopyOfDelegateHandlers.Count then
    Exit(False);

  Inc(FIndex);

  //  Find an active handler
  while FIndex < FCopyOfDelegateHandlers.Count do
  begin
    if FCopyOfDelegateHandlers[FIndex].Status = hsActive then exit(true);
    Inc(FIndex);
  end;

  Result := FIndex < FCopyOfDelegateHandlers.Count;
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

  {$IF CompilerVersion >= 21.0}
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

    //  Delphi 2010 and later support getting SELF for anonymous methods
    {$IF CompilerVersion >= 21.0}
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
              FFreeNotificationSink := TFreeNotificationSink.Create(FSelf as TComponent,
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

function TSysHandlerItem<T>.getHandler: T;
begin
  result := FHandler;
end;

function TSysHandlerItem<T>.getLifeTime: THandlerLifetime;
begin
  result := FLifeTime;
end;

function TSysHandlerItem<T>.getStatus: THandlerStatus;
begin
  syncStart(FPadLock);
  try
    result := FStatus;
  finally
    syncEnd(FPadLock);
  end;
end;

procedure TSysHandlerItem<T>.setStatus(const Value: THandlerStatus);
begin
  syncStart(FPadLock);
  try
    FStatus := value;
  finally
    syncEnd(FPadLock);
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
