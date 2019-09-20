# TWinEventHook
 TWinEventHook is a Delphi wrapper arrounf the Win32 function SetWinEventHook, which  allows you to hook into certain Windows events.

The included Demo catches all Windows Events and logs the EventTime and the current Timestamp It's all shown in a ListView

![Demo Application](https://github.com/JensBorrisholt/TWinEventHook/blob/master/Capture.png)

## Example of use:

```delphi

uses
  WinEventHook;
  
  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure HookProc(Hook: TWinEventHook; const HookMsg: TWinEventStruct);
  public
    { Public declarations }
  end;
  
procedure TForm1.FormCreate(Sender: TObject);
begin
  with THookContainer.Create(Self, EVENT_OBJECT_CREATE).Hook do
  begin
    OnExecute := HookProc;
    Active := True;
  end;

  with THookContainer.Create(Self, EVENT_OBJECT_DESTROY).Hook do
  begin
    OnExecute := HookProc;
    Active := True;
  end;
end;
 
 procedure TForm1.HookProc(Hook: TWinEventHook; const HookMsg: TWinEventStruct);
begin
  //Do Stuff with   HookMsg
end;
