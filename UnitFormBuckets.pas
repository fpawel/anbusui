unit UnitFormBuckets;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, System.ImageList,
    Vcl.ImgList, Vcl.ExtCtrls, Vcl.StdCtrls, VclTee.TeeGDIPlus, VclTee.TeEngine,
    VclTee.TeeProcs, VclTee.Chart,
    UnitFormChartSeries, VclTee.Series;

type

    TNodeKind = (trdYear, trdMonth, trdDay, trdBucket);

    PTreeData = ^RTreeData;

    RTreeData = record
        Value: int64;
        NodeKind: TNodeKind;
        BucketCreatedAt: TDateTime;
        BucketUpdatedAt: TDateTime;
    end;

    TFormBuckets = class(TForm)
        TreeView1: TVirtualStringTree;
        ImageList1: TImageList;
        Splitter1: TSplitter;
        procedure FormCreate(Sender: TObject);
        procedure TreeView1Collapsed(Sender: TBaseVirtualTree;
          Node: PVirtualNode);
        procedure TreeView1GetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
          Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
        procedure TreeView1GetImageIndex(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
          var Ghosted: Boolean; var ImageIndex: TImageIndex);
        procedure TreeView1Expanding(Sender: TBaseVirtualTree;
          Node: PVirtualNode; var Allowed: Boolean);
        procedure TreeView1Change(Sender: TBaseVirtualTree; Node: PVirtualNode);
        procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
          WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    private
        { Private declarations }

        function GetTreeData(Node: PVirtualNode): PTreeData;
        procedure CreateYearsNodes;
        procedure CreateMonthsNodes(ParentNode: PVirtualNode);
        procedure CreateDaysNodes(ParentNode: PVirtualNode);
        procedure CreatePartiesNodes(ParentNode: PVirtualNode);

        property TreeData[Node: PVirtualNode]: PTreeData read GetTreeData;

    public
        FFormChartSeries: TFormChartSeries;
        { Public declarations }
        procedure ValidateData;

    end;

    TBucket = class
        FBucketID: int64;
        FCreatedAt: TDateTime;
        FUpdatedAt: TDateTime;
    end;

    TSeriesRecord = class
        FStoredAt: TDateTime;
        FValue: double;
        FVar: Integer;
        FAddr: Integer;
    end;

var
    FormBuckets: TFormBuckets;

implementation

{$R *.dfm}

uses superobject, stringutils, rest.json,
    dateutils, UnitServerApp, stringgridutils, vclutils;

procedure TFormBuckets.FormCreate(Sender: TObject);
begin
    TreeView1.NodeDataSize := SizeOf(RTreeData);
    FFormChartSeries := TFormChartSeries.Create(self);

    with FFormChartSeries do
    begin
        Parent := self;
        Align := alClient;
        BorderStyle := bsNone;
        Visible := true;
        Font.Assign(self.Font);
        Visible := false;
    end;

end;

procedure TFormBuckets.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
    FFormChartSeries.ChangeAxisOrder(GetVCLControlAtPos(self, MousePos),
      WheelDelta);
end;

procedure TFormBuckets.TreeView1Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    i: ISuperObject;
    j: TSuperArray;
    s: string;
    ser: TFastLineSeries;
    t: TDateTime;
begin

    if Assigned(TreeData[Node]) AND (TreeData[Node].NodeKind = trdBucket) then
    begin
        FFormChartSeries.NewChart;
        FFormChartSeries.FBucketID := TreeData[Node].Value;

        for i in ServerApp.MustGetResult('ChartSvc.Vars',
          SO(Format('{"BucketID":%d}', [TreeData[Node].Value]))) do
        begin
            FFormChartSeries.ListBox1.Items.Add(inttostr(i.AsInteger));
        end;

        for i in ServerApp.MustGetResult('ChartSvc.Addresses',
          SO(Format('{"BucketID":%d}', [TreeData[Node].Value]))) do
        begin
            FFormChartSeries.ListBox2.Items.Add(inttostr(i.AsInteger));
        end;

        for i in ServerApp.MustGetResult('ChartSvc.Records',
          SO(Format('{"BucketID":%d}', [TreeData[Node].Value]))) do
        begin
            j := i.AsArray;
            t := EncodeDateTime(j[2].AsInteger, j[3].AsInteger, j[4].AsInteger,
              j[5].AsInteger, j[6].AsInteger, j[7].AsInteger, j[8].AsInteger);

            FFormChartSeries.SeriesOf(j[0].AsInteger, j[1].AsInteger)
              .AddXY(t, j[9].AsDouble);;;

        end;

        FFormChartSeries.Visible := true;

    end
    else
    begin
        FFormChartSeries.Visible := false;
    end;

end;

procedure TFormBuckets.TreeView1Collapsed(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    // freeNodeData(VirtualStringTree1, Node.FirstChild);
    TreeView1.DeleteChildren(Node);
end;

procedure TFormBuckets.TreeView1Expanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: Boolean);
var
    p: PTreeData;
    NewNode: PVirtualNode;
    NewData: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    case p.NodeKind of
        trdYear:
            CreateMonthsNodes(Node);
        trdMonth:
            CreateDaysNodes(Node);
        trdDay:
            CreatePartiesNodes(Node);
        trdBucket:
            ;
    end;

end;

procedure TFormBuckets.TreeView1GetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: TImageIndex);
var
    p: PTreeData;
begin
    if (Column = 0) AND (Kind in [ikNormal, ikSelected]) then
    begin
        ImageIndex := Integer(TreeData[Node].NodeKind);
    end;
end;

procedure TFormBuckets.TreeView1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    case p.NodeKind of
        trdYear:
            CellText := inttostr(p.Value);
        trdMonth:
            CellText := inttostr2(p.Value) + ' ' + month_name(p.Value);

        trdDay:
            CellText := inttostr2(p.Value);
        trdBucket:
            CellText := FormatDateTime('hh:nn', p.BucketCreatedAt) + ' - ' +
              FormatDateTime('hh:nn', p.BucketUpdatedAt);
    end;
end;

procedure TFormBuckets.CreateYearsNodes;
var
    Node: PVirtualNode;
    i: ISuperObject;
begin
    TreeView1.Clear;
    for i in ServerApp.MustGetResult('ChartSvc.Years', SO('{}')) do
    begin
        Node := TreeView1.AddChild(nil);
        TreeData[Node].Value := i.AsInteger;
        TreeData[Node].NodeKind := trdYear;
        TreeView1.HasChildren[Node] := true;
    end;
end;

procedure TFormBuckets.CreateMonthsNodes(ParentNode: PVirtualNode);
var
    Node: PVirtualNode;
    i: ISuperObject;
begin
    for i in ServerApp.MustGetResult('ChartSvc.Months',
      SO(Format('{"Year":%d}', [TreeData[ParentNode].Value]))) do
    begin
        Node := TreeView1.AddChild(ParentNode);
        TreeData[Node].Value := i.AsInteger;
        TreeData[Node].NodeKind := trdMonth;
        TreeView1.HasChildren[Node] := true;
    end;
end;

procedure TFormBuckets.CreateDaysNodes(ParentNode: PVirtualNode);
var
    Node: PVirtualNode;
    i: ISuperObject;
begin
    for i in ServerApp.MustGetResult('ChartSvc.Days',
      SO(Format('{"Year":%d, "Month":%d}', [TreeData[ParentNode.Parent].Value,
      TreeData[ParentNode].Value]))) do
    begin
        Node := TreeView1.AddChild(ParentNode);
        TreeData[Node].Value := i.AsInteger;
        TreeData[Node].NodeKind := trdDay;
        TreeView1.HasChildren[Node] := true;
    end;
end;

procedure TFormBuckets.CreatePartiesNodes(ParentNode: PVirtualNode);
var
    Node: PVirtualNode;
    i: ISuperObject;
    s: string;
    b: TBucket;
begin
    for i in ServerApp.MustGetResult('ChartSvc.Buckets',
      SO(Format('{"Year":%d, "Month":%d, "Day":%d}', [TreeData[ParentNode.Parent.Parent].Value,
      TreeData[ParentNode.Parent].Value, TreeData[ParentNode].Value]))) do
    begin
        Node := TreeView1.AddChild(ParentNode);
        s := i.AsJSon;
        b := TJson.JsonToObject<TBucket>(s);
        TreeData[Node].Value := b.FBucketID;
        TreeData[Node].NodeKind := trdBucket;
        TreeData[Node].BucketCreatedAt := b.FCreatedAt;
        TreeData[Node].BucketUpdatedAt := b.FUpdatedAt;
        TreeView1.HasChildren[Node] := false;
        b.Free;
    end;
end;

function TFormBuckets.GetTreeData(Node: PVirtualNode): PTreeData;
begin
    Result := PTreeData(TreeView1.GetNodeData(Node));
end;

procedure TFormBuckets.ValidateData;
begin
    TreeView1.Clear;
    CreateYearsNodes;

end;

end.
