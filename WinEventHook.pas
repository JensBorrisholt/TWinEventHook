unit WinEventHook;

interface

uses
  Windows, System.Classes, Generics.Collections;

TYPE
  TWinEventHookHandle = THandle;

  TWinEventStruct = record
    Hook: TWinEventHookHandle;
    Event: DWORD;
    hWnd: THandle;
    idObject: Longint;
    idChild: Longint;
    idEventThread: DWORD;
    dwmsEventTime: DWORD;
    EventTime: TDateTime;
    EventClass: String;
  end;
{$M+}

Type
  TWinEventHook = class;
  TWinEventHookNotify = reference to procedure(Hook: TWinEventHook; const HookMsg: TWinEventStruct);
  TWinEventHookProcs = TDictionary<TWinEventHookHandle, TWinEventHook>;

  TWinEventHook = class
  private
    class var FWinEventHookProcs: TWinEventHookProcs;
  private
    FHook: hHook;
    FOnExecute: TWinEventHookNotify;
    FActive: Boolean;
    FEventMin: DWORD;
    FEventMax: DWORD;
    FContext: DWORD;
    procedure SetActive(const Value: Boolean);
  published
    property Active: Boolean read FActive write SetActive;
  public
    constructor Create(eventMin: DWORD; eventMax: DWORD = 0; aContext: DWORD = WINEVENT_OUTOFCONTEXT); reintroduce;
    destructor Destroy; override;
    class constructor Create;
    class Destructor Destroy;
    property OnExecute: TWinEventHookNotify read FOnExecute write FOnExecute;
  end;

type
  THookContainer = class(TComponent)
  private
    FHook: TWinEventHook;
  public
    destructor Destroy; override;
    constructor Create(AOwner: TComponent; eventMin: DWORD; eventMax: DWORD = 0; aContext: DWORD = WINEVENT_OUTOFCONTEXT); reintroduce;
    property Hook: TWinEventHook read FHook;
  end;

implementation

uses
  System.SysUtils, System.DateUtils;

{ TWinEventHook }

procedure WinEventProcCallBack(HWINEVENTHOOK: THandle; Event: DWORD; hWnd: hWnd; idObject, idChild: Longint; idEventThread, dwmsEventTime: DWORD); stdcall;
var
  WinEventHook: TWinEventHook;
  WinEventStruct: TWinEventStruct;
  ClassName: String;
begin
  if not TWinEventHook.FWinEventHookProcs.TryGetValue(HWINEVENTHOOK, WinEventHook) then
    exit;

  if not WinEventHook.Active then
    exit;

  WinEventStruct.Hook := HWINEVENTHOOK;
  WinEventStruct.Event := Event;
  WinEventStruct.hWnd := hWnd;
  WinEventStruct.idObject := idObject;
  WinEventStruct.idChild := idChild;
  WinEventStruct.idEventThread := idEventThread;
  WinEventStruct.dwmsEventTime := dwmsEventTime;

  WinEventStruct.EventTime := IncMilliSecond(Now, dwmsEventTime - GetTickCount);

  SetLength(ClassName, 255);
  SetLength(ClassName, GetClassName(hWnd, pChar(ClassName), 255));
  WinEventStruct.EventClass := ClassName;

  WinEventHook.FOnExecute(WinEventHook, WinEventStruct);
end;

destructor TWinEventHook.Destroy;
begin
  Active := False;
  inherited;
end;

constructor TWinEventHook.Create(eventMin: DWORD; eventMax: DWORD = 0; aContext: DWORD = WINEVENT_OUTOFCONTEXT);
begin
  inherited Create;
  FHook := 0;
  FActive := False;
  FEventMin := eventMin;
  if eventMax = 0 then
    eventMax := eventMin;

  FEventMax := eventMax;
  FContext := aContext;
end;

class destructor TWinEventHook.Destroy;
begin
  FWinEventHookProcs.Free;
end;

procedure TWinEventHook.SetActive(const Value: Boolean);
begin
  if FActive = Value then
    exit;

  FActive := Value;

  case FActive of
    True:
      begin
        FHook := SetWinEventHook(FEventMin, FEventMax, 0, WinEventProcCallBack, 0, 0, FContext);
        if (FHook = 0) then
        begin
          FActive := False;
          raise Exception.Create(ClassName + ' CREATION FAILED!');
        end;
        FWinEventHookProcs.Add(FHook, Self);
      end;

    False:
      begin
        if (FHook <> 0) then
        begin
          UnhookWinEvent(FHook);
          FWinEventHookProcs.Remove(FHook);
        end;
        FHook := 0;
      end;
  end;
end;

class constructor TWinEventHook.Create;
begin
  FWinEventHookProcs := TDictionary<TWinEventHookHandle, TWinEventHook>.Create();
end;

{ THookContainer }

constructor THookContainer.Create(AOwner: TComponent; eventMin: DWORD; eventMax: DWORD; aContext: DWORD);
begin
  inherited Create(AOwner);
  FHook := TWinEventHook.Create(eventMin, eventMax, aContext);
end;

destructor THookContainer.Destroy;
begin
  FHook.Free;
  inherited;
end;

end.
