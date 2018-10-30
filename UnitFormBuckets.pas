unit UnitFormBuckets;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VirtualTrees, System.ImageList,
    Vcl.ImgList, Vcl.ExtCtrls;

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
    private
        { Private declarations }


        function GetTreeData(Node: PVirtualNode): PTreeData;
        procedure CreateYearsNodes;
        procedure CreateMonthsNodes(ParentNode: PVirtualNode);
        procedure CreateDaysNodes(ParentNode: PVirtualNode);
        procedure CreatePartiesNodes(ParentNode: PVirtualNode);

        property TreeData[Node: PVirtualNode]: PTreeData read GetTreeData;
    public
        { Public declarations }
        procedure ValidateData;

    end;

var
    FormBuckets: TFormBuckets;

implementation

{$R *.dfm}

uses superobject, stringutils,
    dateutils, UnitServerApp;

procedure TFormBuckets.FormCreate(Sender: TObject);
begin
    TreeView1.NodeDataSize := SizeOf(RTreeData);
end;

procedure TFormBuckets.TreeView1Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
    // if Assigned(TreeData[Node]) AND (TreeData[Node].NodeKind = trdParty) then
    // begin
    // FormPartyView.party := TParty.Create(DataModule1.FDConnectionElcheseDB,
    // TreeData[Node].Value);
    // Node := TreeView1.GetFirst;
    // while Assigned(Node) do
    // begin
    // if TreeView1.IsVisible[Node] then
    // TreeView1.RepaintNode(Node);
    // Node := TreeView1.GetNext(Node);
    // end;
    //
    // end;

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
        ImageIndex := integer(TreeData[Node].NodeKind);
    end;
end;

procedure TFormBuckets.TreeView1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    case Column of
        0:
            begin
                case p.NodeKind of
                    trdYear:
                        CellText := inttostr(p.Value);
                    trdMonth:
                        CellText := inttostr2(p.Value) + ' ' +
                          month_name(p.Value);

                    trdDay:
                        CellText := inttostr2(p.Value);
                    trdBucket:
                        CellText := DateToStr(p.BucketCreatedAt);
                end;
            end;
        1:
            begin
                case p.NodeKind of
                    trdBucket:
                        CellText := inttostr(p.Value);
                end;
            end;
    end;
end;

procedure TFormBuckets.CreateYearsNodes;
var
    Node: PVirtualNode;
    i: ISuperObject;
begin
    TreeView1.Clear;
    for i in ServerApp.MustGetResult('BucketsSvc.Years', SO('{}')) do
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
    for i in ServerApp.MustGetResult('BucketsSvc.Months',
      SO(format('[%d]', [TreeData[ParentNode].Value]))) do
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
    for i in ServerApp.MustGetResult('BucketsSvc.Days',
      SO(format('[%d, %d]', [TreeData[ParentNode.Parent].Value,
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
    NodeData: PTreeData;
begin
    // with TFDQuery.Create(nil) do
    // begin
    // Connection := DataModule1.FDConnectionElcheseDB;
    // SQL.Text :=
    // 'SELECT * FROM party_info WHERE year = :year AND month = :month AND day = :day ORDER BY created_at; ';
    // ParamByName('day').Value := TreeData[ParentNode].Value;
    // ParamByName('month').Value := TreeData[ParentNode.Parent].Value;
    // ParamByName('year').Value := TreeData[ParentNode.Parent.Parent].Value;
    // open;
    // First;
    // while not Eof do
    // begin
    // Node := TreeView1.AddChild(ParentNode);
    // NodeData := TreeData[Node];
    // NodeData.Value := FieldValues['party_id'];
    // NodeData.NodeKind := trdParty;
    // NodeData.PartyInfo := TPartyInfo.Create;
    // NodeData.PartyInfo.FProdType := FieldValues['product_type_name'];
    // NodeData.PartyInfo.FNote := FieldValues['note'];
    // Next;
    // end;
    // Close;
    // Free
    // end;
end;

function TFormBuckets.GetTreeData(Node: PVirtualNode): PTreeData;
begin
    result := PTreeData(TreeView1.GetNodeData(Node));
end;

procedure TFormBuckets.ValidateData;
begin
    TreeView1.Clear;
    CreateYearsNodes;

end;

end.
