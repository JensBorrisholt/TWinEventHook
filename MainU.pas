unit MainU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Generics.Collections,
  Vcl.ExtCtrls,

  WinEventHook;

type
  TEventClass = class
  strict private
    FEventClass: string;
    FEventTime: TDateTime;
    FTimeStamp: TDateTime;
  public
    property EventClass: string read FEventClass write FEventClass;
    property EventTime: TDateTime read FEventTime write FEventTime;
    property TimeStamp: TDateTime read FTimeStamp write FTimeStamp;
  end;

  TForm1 = class(TForm)
    ListView1: TListView;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ListView1DrawItem(Sender: TCustomListView; Item: TListItem; Rect: TRect; State: TOwnerDrawState);
    procedure FormResize(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    FEventsTracker: TObjectDictionary<string, TEventClass>;
    procedure HookProc(Hook: TWinEventHook; const HookMsg: TWinEventStruct);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  DateUtils, Math;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FEventsTracker.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FEventsTracker := TObjectDictionary<string, TEventClass>.Create([doOwnsValues]);

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

procedure TForm1.FormResize(Sender: TObject);
begin
  ListView1.Column[0].Width := ListView1.ClientWidth - 210;
  ListView1.Column[1].Width := 100;
  ListView1.Column[2].Width := 100;
end;

procedure TForm1.HookProc(Hook: TWinEventHook; const HookMsg: TWinEventStruct);
var
  EventClass: TEventClass;
begin
  if not FEventsTracker.TryGetValue(HookMsg.EventClass, EventClass) then
  begin
    EventClass := TEventClass.Create;
    EventClass.EventClass := HookMsg.EventClass;
    EventClass.EventTime := HookMsg.EventTime;
    EventClass.TimeStamp := now;
    FEventsTracker.Add(HookMsg.EventClass, EventClass);
  end
  else
    EventClass.TimeStamp := now;
end;

procedure TForm1.ListView1DrawItem(Sender: TCustomListView; Item: TListItem; Rect: TRect; State: TOwnerDrawState);
var
  i: Integer;
  x1, x2: Integer;
  r: TRect;
  S: string;
  EventClass: TEventClass;
const
  cStripe = $CCFFCC;
const
  DT_ALIGN: array [TAlignment] of Integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
begin
  EventClass := TEventClass(Item.Data);

  if (EventClass <> nil) and (SecondsBetween(EventClass.TimeStamp, now) < 1) then
  begin
    Sender.Canvas.Font.Color := clBlack;
    Sender.Canvas.Brush.Color := cStripe;
  end
  else
  begin
    Sender.Canvas.Font.Color := clBlack;
    Sender.Canvas.Brush.Color := clWhite;
  end;

  Sender.Canvas.Brush.Style := bsSolid;
  Sender.Canvas.FillRect(Rect);

  x1 := 0;
  x2 := 0;
  r := Rect;
  Sender.Canvas.Brush.Style := bsClear;
  for i := 0 to ListView1.Columns.Count - 1 do
  begin
    inc(x2, ListView1.Columns[i].Width);
    r.Left := x1;
    r.Right := x2;
    if i = 0 then
      S := Item.Caption
    else
      S := Item.SubItems[i - 1];

    DrawText(Sender.Canvas.Handle, S, length(S), r, DT_SINGLELINE or DT_ALIGN[ListView1.Columns[i].Alignment] or DT_VCENTER or DT_END_ELLIPSIS);
    x1 := x2;
  end;

end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  Element: TPair<string, TEventClass>;
  EventClass: TEventClass;
  Item: TListItem;
begin
  SendMessage(ListView1.Handle, WM_SETREDRAW, WPARAM(False), 0);
  // ListView1.Items.Clear;

  for Element in FEventsTracker do
  begin
    EventClass := Element.Value;

    Item := ListView1.FindCaption(0, EventClass.EventClass, False, True, False);

    if Item <> nil then
    begin
      if Item.SubItems[0] <> TimeToStr(Element.Value.EventTime) then
        Item.SubItems[0] := TimeToStr(Element.Value.EventTime);

      if Item.SubItems[1] <> TimeToStr(Element.Value.TimeStamp) then
        Item.SubItems[1] := TimeToStr(Element.Value.TimeStamp)
    end
    else
    begin

      with ListView1.Items.Add do
      begin
        Caption := Element.Key;
        SubItems.Add(TimeToStr(Element.Value.TimeStamp));
        SubItems.Add(TimeToStr(Element.Value.EventTime));

        Data := Element.Value;

      end;
    end;
  end;

  SendMessage(ListView1.Handle, WM_SETREDRAW, WPARAM(True), 0);
  ListView1.Invalidate;
end;

end.
